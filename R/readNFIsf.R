.nfi_first_col <- function(x, candidates) {
    nm <- names(x)
    hit <- match(tolower(candidates), tolower(nm))
    hit <- hit[!is.na(hit)]
    if (!length(hit))
        return(NA_character_)
    nm[hit[1L]]
}

.nfi_numeric <- function(x) {
    if (is.factor(x))
        x <- as.character(x)
    if (is.character(x))
        x <- gsub(",", ".", trimws(x), fixed = TRUE)
    suppressWarnings(as.numeric(x))
}

.nfi_suppress_dbf_grepl_warning <- function(w) {
    msg <- conditionMessage(w)
    call_txt <- paste(deparse(conditionCall(w)), collapse = " ")

    dbf_encoding_warning <- grepl("unable to translate|input string .* is invalid", msg)
    from_grepl <- grepl("grepl", call_txt, fixed = TRUE)

    if (isTRUE(dbf_encoding_warning) && isTRUE(from_grepl))
        invokeRestart("muffleWarning")
}

.nfi_readNFI_quiet_dbf <- function(args) {
    withCallingHandlers(
        do.call(readNFI, args),
        warning = .nfi_suppress_dbf_grepl_warning
    )
}

.nfi_bind_rows_fill <- function(x) {
    x <- x[!vapply(x, is.null, logical(1))]
    if (!length(x))
        return(NULL)
    if (length(x) == 1L)
        return(x[[1L]])

    nm <- unique(unlist(lapply(x, names), use.names = FALSE))
    x <- lapply(x, function(z) {
        miss <- setdiff(nm, names(z))
        for (m in miss)
            z[[m]] <- NA
        z[, nm, drop = FALSE]
    })

    out <- do.call(rbind, x)
    rownames(out) <- NULL
    out
}

.nfi_input_province <- function(nfi) {
    if (is.numeric(nfi) && length(nfi) == 1L && !is.na(nfi))
        return(as.integer(nfi))

    if (is.character(nfi) && length(nfi) == 1L) {
        z <- trimws(nfi)
        if (grepl("^[0-9]+$", z))
            return(as.integer(z))
    }

    NA_integer_
}

.nfi_postprocess_local_table <- function(dset, nfi.nr, province = NA_integer_,
                                         pr.from = NULL) {
    if (is.null(dset))
        return(NULL)

    dset <- .nfi_unwrap_table(dset)
    dset <- .nfi_convert_factors_to_numeric_safe(dset)

    nfi.nr <- as.integer(nfi.nr)[1L]
    pr_col <- .nfi_first_col(
        dset,
        c("pr", "Provincia", "PROVINCIA", "NPROV", "nprov", "prov")
    )

    if (!is.na(pr_col)) {
        names(dset)[names(dset) == pr_col] <- "pr"
    } else {
        pr_val <- if (!is.null(pr.from) && length(pr.from)) pr.from[1L] else province
        dset <- data.frame(pr = pr_val, dset, check.names = FALSE)
    }

    if (!"nfi.nr" %in% names(dset))
        dset <- data.frame(nfi.nr = nfi.nr, dset, check.names = FALSE)

    attr(dset, "pr.") <- unique(dset$pr)
    attr(dset, "nfi.nr") <- nfi.nr
    class(dset) <- unique(c("readNFI", class(dset)))
    dset
}

.nfi_detect_sep <- function(fi, n = 5L) {
    hdr <- tryCatch(readLines(fi, n = n, warn = FALSE),
                    error = function(e) character(0))
    hdr <- hdr[nzchar(trimws(hdr))]
    if (!length(hdr))
        return(",")
    hdr <- hdr[1L]
    cand <- c(";", ",", "\t", "|")
    cnt <- vapply(cand, function(sep)
        length(strsplit(hdr, sep, fixed = TRUE)[[1L]]) - 1L,
        integer(1))
    if (all(cnt <= 0L))
        return(",")
    cand[which.max(cnt)]
}

.nfi_read_one_csv <- function(fi) {
    sep <- .nfi_detect_sep(fi)
    tryCatch(
        utils::read.table(fi,
                          header = TRUE,
                          sep = sep,
                          quote = '"',
                          dec = '.',
                          fill = TRUE,
                          comment.char = '',
                          stringsAsFactors = FALSE,
                          check.names = FALSE),
        error = function(e) NULL
    )
}

.nfi_dbf_target <- function(dt.nm, nfi.nr) {
    if (identical(as.integer(nfi.nr), 2L) && dt.nm %in% "PCMayores")
        return("PIESMA")
    dt.nm
}

.nfi_select_dbf_paths <- function(files, dt.nm, nfi.nr) {
    files <- files[tolower(tools::file_ext(files)) == "dbf"]
    if (!length(files))
        return(character(0))

    stems <- tools::file_path_sans_ext(basename(files))
    stems_low <- tolower(stems)
    target <- .nfi_dbf_target(dt.nm, nfi.nr)
    target_low <- tolower(target)

    if (target_low == "datest") {
        keep <- grepl("^datest[0-9]*$", stems_low)
    } else {
        keep <- stems_low == target_low
        if (!any(keep))
            keep <- grepl(target_low, stems_low, fixed = TRUE)
    }

    files[keep]
}

.nfi_read_local_dbf <- function(files, nfi.nr, dt.nm, province = NA_integer_) {
    sel <- .nfi_select_dbf_paths(files, dt.nm = dt.nm, nfi.nr = nfi.nr)
    if (!length(sel))
        return(NULL)

    out <- lapply(sel, function(fi) {
        d <- tryCatch(foreign::read.dbf(fi), error = function(e) NULL)
        .nfi_postprocess_local_table(d, nfi.nr = nfi.nr, province = province)
    })
    .nfi_bind_rows_fill(out)
}

.nfi_windows_access_driver <- function() {
    if (!identical(unname(Sys.info()[["sysname"]]), "Windows"))
        return(FALSE)

    if (!requireNamespace("odbc", quietly = TRUE))
        return(NA)

    drv <- tryCatch(odbc::odbcListDrivers(), error = function(e) NULL)
    if (is.null(drv) || !"name" %in% names(drv))
        return(NA)

    any(grepl("access", drv$name, ignore.case = TRUE))
}

.nfi_assert_access_backend <- function(backend = c("odbc", "mdbtools")) {
    backend <- match.arg(backend)

    if (backend == "odbc") {
        if (!requireNamespace("RODBC", quietly = TRUE)) {
            stop("Missing package 'RODBC'. Install it before reading Access files on Windows.",
                 call. = FALSE)
        }
        drv <- .nfi_windows_access_driver()
        if (identical(drv, FALSE)) {
            stop(
                paste(
                    "Windows Access driver not found.",
                    "Install Microsoft 365 Access Runtime or another Microsoft Access driver,",
                    "restart R, and try readNFIcoords() again."
                ),
                call. = FALSE
            )
        }
        return(invisible(TRUE))
    }

    if (!requireNamespace("Hmisc", quietly = TRUE)) {
        stop("Missing package 'Hmisc'. Install it before reading Access files on Unix-like systems.",
             call. = FALSE)
    }
    if (!all(nzchar(Sys.which(c("mdb-tables", "mdb-export"))))) {
        sys <- unname(Sys.info()[["sysname"]])
        install_hint <- if (identical(sys, "Darwin")) {
            "Install it with Homebrew: brew install mdbtools"
        } else if (file.exists("/etc/arch-release")) {
            "Install it with pacman: sudo pacman -S mdbtools"
        } else {
            "Install it with your system package manager, for example: sudo apt install mdbtools"
        }
        stop(paste("External tool 'mdbtools' not found.", install_hint),
             call. = FALSE)
    }

    invisible(TRUE)
}

.nfi_read_access_one <- function(file, tables) {
    is_win <- identical(unname(Sys.info()["sysname"]), "Windows")
    if (is_win) {
        .nfi_assert_access_backend("odbc")
        con <- RODBC::odbcConnectAccess2007(file, rows_at_time = 1)
        on.exit(RODBC::odbcClose(con), add = TRUE)
        out <- lapply(tables, function(tb)
            tryCatch(RODBC::sqlFetch(con, sqtable = tb), error = function(e) NULL))
    } else {
        .nfi_assert_access_backend("mdbtools")
        out <- tryCatch(Hmisc::mdb.get(file, tables = tables),
                        error = function(e) NULL)
        if (!is.list(out) || is.data.frame(out))
            out <- list(out)
    }

    names(out) <- tables[seq_along(out)]
    out
}

.nfi_read_local_access <- function(files, nfi.nr, dt.nm,
                                   province = NA_integer_) {
    files <- files[tolower(tools::file_ext(files)) %in% c("mdb", "accdb")]
    if (!length(files))
        return(NULL)

    may <- grepl("mayores", dt.nm, ignore.case = TRUE)
    tables <- if (may) unique(c(dt.nm, "PCDatosMap")) else dt.nm

    one <- lapply(files, function(fi) {
        z <- .nfi_read_access_one(fi, tables = tables)
        if (is.null(z))
            return(NULL)

        pr_from_map <- NULL
        if ("PCDatosMap" %in% names(z) && is.data.frame(z[["PCDatosMap"]])) {
            pc <- .nfi_convert_factors_to_numeric_safe(z[["PCDatosMap"]])
            pc_pr <- .nfi_first_col(pc, c("Provincia", "PROVINCIA", "pr"))
            if (!is.na(pc_pr))
                pr_from_map <- unique(pc[[pc_pr]])
        }

        main <- if (dt.nm %in% names(z)) z[[dt.nm]] else z[[1L]]
        .nfi_postprocess_local_table(main,
                                     nfi.nr = nfi.nr,
                                     province = province,
                                     pr.from = pr_from_map)
    })

    .nfi_bind_rows_fill(one)
}

.nfi_read_local_csv <- function(files, nfi.nr, dt.nm,
                                province = NA_integer_) {
    files <- files[tolower(tools::file_ext(files)) == "csv"]
    if (!length(files))
        return(NULL)

    stems <- tolower(tools::file_path_sans_ext(basename(files)))
    keep <- stems == tolower(dt.nm)
    if (!any(keep))
        keep <- grepl(tolower(dt.nm), stems, fixed = TRUE)
    files <- files[keep]
    if (!length(files))
        return(NULL)

    out <- lapply(files, function(fi) {
        d <- .nfi_read_one_csv(fi)
        .nfi_postprocess_local_table(d, nfi.nr = nfi.nr, province = province)
    })
    .nfi_bind_rows_fill(out)
}

.nfi_read_local_table <- function(files, nfi.nr, dt.nm,
                                  province = NA_integer_) {
    files <- as.character(files)
    files <- files[!is.na(files) & nzchar(files) & file.exists(files)]
    if (!length(files))
        return(NULL)

    ext <- unique(tolower(tools::file_ext(files)))

    out <- NULL
    if ("dbf" %in% ext)
        out <- .nfi_read_local_dbf(files, nfi.nr = nfi.nr,
                                   dt.nm = dt.nm, province = province)
    if (is.null(out) && any(ext %in% c("mdb", "accdb")))
        out <- .nfi_read_local_access(files, nfi.nr = nfi.nr,
                                      dt.nm = dt.nm, province = province)
    if (is.null(out) && "csv" %in% ext)
        out <- .nfi_read_local_csv(files, nfi.nr = nfi.nr,
                                   dt.nm = dt.nm, province = province)

    out
}

.nfi_factor_is_numeric <- function(x) {
    if (!is.factor(x))
        return(FALSE)

    chr <- as.character(x)
    if (!length(chr))
        return(FALSE)

    ## IFN2 DBF files may contain invalid bytes in text fields.  Convert
    ## through iconv() and test bytes to avoid wide-string warnings.
    chr <- iconv(chr, from = "", to = "UTF-8", sub = "byte")
    chr <- trimws(chr)
    chr <- gsub(",", ".", chr, fixed = TRUE)
    ok <- !is.na(chr) & nzchar(chr)

    if (!any(ok))
        return(FALSE)

    all(grepl("^-?\\d*\\.?\\d+$", chr[ok], useBytes = TRUE))
}

.nfi_convert_factors_to_numeric_safe <- function(x) {
    if (!is.data.frame(x))
        return(x)

    is_num_factor <- vapply(x, .nfi_factor_is_numeric, logical(1))
    for (nm in names(x)[is_num_factor])
        x[[nm]] <- .nfi_numeric(x[[nm]])

    x
}

.nfi_key_part <- function(x) {
    x0 <- x
    x <- trimws(as.character(x))
    xn <- .nfi_numeric(x)
    ok <- !is.na(xn)
    out <- x
    out[ok] <- format(xn[ok], scientific = FALSE, trim = TRUE)
    out[is.na(x0)] <- NA_character_
    out
}

.nfi_plot_keys <- function(x, allow_missing_province = FALSE) {
    pr_col <- .nfi_first_col(
        x,
        c("pr", "Provincia", "PROVINCIA", "NPROV", "nprov", "prov")
    )

    plot_col <- .nfi_first_col(
        x,
        c("Estadillo", "ESTADILLO", "estadillo", "NUMPAR", "numpar",
          "plot", "plot_id", "idp")
    )

    if (is.na(plot_col)) {
        stop(
            paste(
                "Cannot attach coordinates because the plot identifier",
                "was not found. Expected a column such as 'Estadillo',",
                "'ESTADILLO', 'NUMPAR', 'plot', or 'idp'."
            ),
            call. = FALSE
        )
    }

    if (is.na(pr_col) && !allow_missing_province) {
        stop(
            paste(
                "Cannot attach coordinates because the province identifier",
                "was not found. Expected a column such as 'pr', 'Provincia',",
                "'PROVINCIA', or 'NPROV'."
            ),
            call. = FALSE
        )
    }

    data.frame(
        .nfi_pr = if (is.na(pr_col)) NA_character_ else .nfi_key_part(x[[pr_col]]),
        .nfi_plot = .nfi_key_part(x[[plot_col]]),
        stringsAsFactors = FALSE
    )
}

.nfi_coord_columns <- function(coords, nfi.nr, x.col = NULL, y.col = NULL,
                               huso.col = NULL) {
    if (!is.null(x.col)) {
        if (!x.col %in% names(coords))
            stop("Column '", x.col, "' was not found in 'coords'.", call. = FALSE)
        x_col <- x.col
    } else {
        x_col <- .nfi_first_col(
            coords,
            if (identical(as.integer(nfi.nr), 2L)) {
                c("COORDEX", "CX", "CoorX", "CoorXC", "CoordX", "X", "x")
            } else {
                c("CoorX", "CoorXC", "CoordX", "COORDEX", "CX", "X", "x")
            }
        )
    }

    if (!is.null(y.col)) {
        if (!y.col %in% names(coords))
            stop("Column '", y.col, "' was not found in 'coords'.", call. = FALSE)
        y_col <- y.col
    } else {
        y_col <- .nfi_first_col(
            coords,
            if (identical(as.integer(nfi.nr), 2L)) {
                c("COORDEY", "CY", "CoorY", "CoorYC", "CoordY", "Y", "y")
            } else {
                c("CoorY", "CoorYC", "CoordY", "COORDEY", "CY", "Y", "y")
            }
        )
    }

    if (is.na(x_col) || is.na(y_col)) {
        stop(
            paste(
                "Coordinate columns were not found in 'coords'.",
                "For IFN2 expected COORDEX/COORDEY or CX/CY;",
                "for IFN3/IFN4 expected CoorX/CoorY or CoorXC/CoorYC."
            ),
            call. = FALSE
        )
    }

    if (!is.null(huso.col)) {
        if (!huso.col %in% names(coords))
            stop("Column '", huso.col, "' was not found in 'coords'.", call. = FALSE)
        h_col <- huso.col
    } else {
        h_col <- .nfi_first_col(coords, c("Huso", "HUSO", "huso", "Zone", "zone"))
    }

    list(x = x_col, y = y_col, huso = h_col)
}

.nfi_unwrap_table <- function(x, preferred = NULL) {
    if (is.data.frame(x))
        return(x)

    if (is.list(x)) {
        if (!is.null(preferred) && preferred %in% names(x) && is.data.frame(x[[preferred]]))
            return(x[[preferred]])

        is_df <- vapply(x, is.data.frame, logical(1))
        if (sum(is_df) == 1L)
            return(x[[which(is_df)[1L]]])
    }

    stop("The coordinate table could not be converted to a data.frame.", call. = FALSE)
}

.nfi_guess_coord_table <- function(tables, nfi.nr) {
    if (is.null(tables) || !is.data.frame(tables) || !"dt.nm" %in% names(tables))
        return(NULL)

    dt <- as.character(tables$dt.nm)
    dt_low <- tolower(dt)

    if (identical(as.integer(nfi.nr), 2L)) {
        ## DATEST is the generic table stem used by readNFI() for IFN2.
        ## listNFI_tables() may report province-specific files such as
        ## DATEST45, DATEST28, etc.; we only use those names to validate
        ## that a DATEST table exists. The value passed to readNFI() must
        ## remain province-independent.
        cand <- grep("^datest[0-9]*$", dt_low)
        if (!length(cand))
            return(NULL)
        return("DATEST")
    }

    cand <- c(which(dt_low == "pcdatosmap"),
              which(dt_low == "listado definitivo"),
              grep("listado", dt_low))

    cand <- unique(cand)
    if (!length(cand))
        return(NULL)

    dt[cand[1L]]
}

.nfi_coord_factor <- function(nfi.nr, coord.factor = NULL) {
    if (!is.null(coord.factor))
        return(as.numeric(coord.factor)[1L])

    if (identical(as.integer(nfi.nr), 2L))
        return(1000)

    1
}


.nfi_to_integer <- function(x) {
    xx <- .nfi_numeric(x)
    suppressWarnings(as.integer(xx))
}

.nfi_huso_from_province <- function(pr) {
    pr <- sprintf("%02d", .nfi_to_integer(pr))
    out <- rep(NA_integer_, length(pr))

    ## Package-level fallback for IFN coordinates when no Huso column is
    ## available: Península/Baleares in UTM zone 30; Canarias in zone 28.
    ## A Huso column in the coordinate table always takes precedence.
    out[!is.na(pr)] <- 30L
    out[pr %in% c("35", "38")] <- 28L
    out
}

.nfi_ifn2_huso <- function(pr) {
    .nfi_huso_from_province(pr)
}

.nfi_ifn2_datum <- function(pr) {
    pr <- sprintf("%02d", .nfi_to_integer(pr))
    ifelse(pr %in% c("35", "38"), "WGS84", "ED50")
}

.nfi_ifn4_datum <- function(pr) {
    pr <- sprintf("%02d", .nfi_to_integer(pr))

    canarias <- c("35", "38")
    ed50 <- c(
        "31",                         # Navarra
        "15", "27", "32", "36",     # Galicia
        "33",                         # Asturias
        "39",                         # Cantabria
        "30",                         # Murcia
        "07",                         # Islas Baleares
        "01", "20", "48",             # País Vasco
        "26",                         # La Rioja
        "28",                         # Madrid
        "08", "17", "25", "43"       # Cataluña
    )

    out <- rep(NA_character_, length(pr))
    out[pr %in% canarias] <- "WGS84"
    out[pr %in% ed50] <- "ED50"
    out[is.na(out) & !is.na(pr)] <- "ETRS89"
    out
}

.nfi_epsg <- function(datum, huso) {
    hz <- .nfi_to_integer(huso)
    datum <- toupper(trimws(as.character(datum)))
    out <- rep(NA_integer_, length(datum))

    ok <- !is.na(hz) & datum == "ED50"
    out[ok] <- 23000L + hz[ok]

    ok <- !is.na(hz) & datum == "ETRS89"
    out[ok] <- 25800L + hz[ok]

    ok <- !is.na(hz) & datum == "WGS84"
    out[ok] <- 32600L + hz[ok]

    out
}

.nfi_reference_values <- function(nfi.nr, pr, huso_from_table = NULL,
                                  validate = TRUE,
                                  infer.huso = FALSE) {
    n <- length(pr)
    if (is.null(huso_from_table))
        huso_from_table <- rep(NA, n)

    huso <- .nfi_to_integer(huso_from_table)
    datum <- rep(NA_character_, n)
    huso_source <- ifelse(is.na(huso), NA_character_, "table")
    missing_huso <- is.na(huso)

    if (identical(as.integer(nfi.nr), 2L)) {
        ## IFN2 coordinates come from DATEST and are converted from km to m.
        ## DATEST does not normally carry Huso. Province-based Huso filling is
        ## therefore optional and marked as province_inferred.
        datum <- .nfi_ifn2_datum(pr)
        if (any(missing_huso) && isTRUE(infer.huso)) {
            huso[missing_huso] <- .nfi_ifn2_huso(pr[missing_huso])
            huso_source[missing_huso & !is.na(huso)] <- "province_inferred"
        }
    } else if (identical(as.integer(nfi.nr), 3L)) {
        datum[] <- "ED50"
        ## IFN3 documentation says coordinates are in the cartographic zone of
        ## each MTN50 sheet. Province inference can be useful, but it is not
        ## equivalent to sheet-level Huso reconstruction, so mark it clearly.
        if (any(missing_huso) && isTRUE(infer.huso)) {
            huso[missing_huso] <- .nfi_huso_from_province(pr[missing_huso])
            huso_source[missing_huso & !is.na(huso)] <- "province_inferred"
        }
        if (any(is.na(huso)) && isTRUE(validate)) {
            warning(
                paste(
                    "Some IFN3 rows have no Huso in the coordinate table.",
                    "EPSG remains NA unless infer.huso = TRUE or a Huso column is supplied."
                ),
                call. = FALSE
            )
        }
    } else if (identical(as.integer(nfi.nr), 4L)) {
        datum <- .nfi_ifn4_datum(pr)
        ## IFN4 normally carries Huso. Fill missing values only when explicitly
        ## requested, and mark the source of those values.
        if (any(missing_huso) && isTRUE(infer.huso)) {
            huso[missing_huso] <- .nfi_huso_from_province(pr[missing_huso])
            huso_source[missing_huso & !is.na(huso)] <- "province_inferred"
        }
        if (any(is.na(huso)) && isTRUE(validate)) {
            warning(
                paste(
                    "Some IFN4 rows have missing Huso in the coordinate table.",
                    "EPSG remains NA unless infer.huso = TRUE or a Huso column is supplied."
                ),
                call. = FALSE
            )
        }
    }

    epsg <- .nfi_epsg(datum, huso)
    crs <- ifelse(is.na(epsg), NA_character_, paste0("EPSG:", epsg))

    data.frame(
        huso = huso,
        huso_source = huso_source,
        datum = datum,
        epsg = epsg,
        crs = crs,
        stringsAsFactors = FALSE
    )
}

addNFIcoords <- structure(function
### addNFIcoords: attach NFI plot coordinates as ordinary columns.
###
### Join a processed Spanish National Forest Inventory table with a
### plot-level coordinate table and append UTM coordinate metadata. This
### function is intentionally independent from \code{readNFI()}, so it can
### enrich already loaded tables without creating circular calls.
(
    nfi, ##<< \code{data.frame}. Inventory table, commonly an object returned
         ## by \code{\link{readNFI}}.
    coords, ##<< \code{data.frame} or one-table \code{list}. Coordinate table.
            ## For IFN2 this is usually \code{DATESTXX.DBF} or the parcel
            ## layer table with \code{CX}/\code{CY}. For IFN3/IFN4 this is
            ## usually \code{PCDatosMap} or \code{Listado definitivo}.
    nfi.nr = attr(nfi, "nfi.nr"), ##<< \code{integer}. Inventory stage: 2,
                                  ## 3, or 4. IFN2 coordinates are converted
                                  ## from kilometres to metres by default.
    x.col = NULL, ##<< Optional source X-coordinate column in \code{coords}.
    y.col = NULL, ##<< Optional source Y-coordinate column in \code{coords}.
    huso.col = NULL, ##<< Optional source UTM-zone column in \code{coords}.
    x.name = "x", ##<< Output X-coordinate column name, in metres.
    y.name = "y", ##<< Output Y-coordinate column name, in metres.
    huso.name = "huso", ##<< Output UTM-zone column name when available.
    huso.source.name = "huso_source", ##<< Output column describing whether
                                      ## Huso comes from the coordinate table
                                      ## or was inferred from province code.
    datum.name = "datum", ##<< Output geodetic datum column name.
    epsg.name = "epsg", ##<< Output EPSG code column name when known.
    crs.name = "crs", ##<< Output CRS label column name when known.
    overwrite = FALSE, ##<< \code{logical}. Allow overwriting existing output
                       ## coordinate columns.
    keep.raw.coords = FALSE, ##<< \code{logical}. Keep raw source coordinate
                             ## values before unit conversion.
    coord.factor = NULL, ##<< Numeric multiplier applied to raw coordinates.
                         ## Defaults to 1000 for IFN2 and 1 for IFN3/IFN4.
    validate = TRUE, ##<< \code{logical}. Warn about duplicate coordinate keys
                     ## and unmatched plots.
    infer.huso = FALSE ##<< \code{logical}. Fill missing UTM zones from the
                       ## province code. A Huso column in \code{coords} has
                       ## precedence when present. Province-filled values are
                       ## marked in \code{huso_source}.
) {
    ##details<< The function uses province and plot identifiers to match
    ## rows in \code{nfi} with records in \code{coords}. It searches common
    ## IFN column names such as \code{pr}, \code{Provincia},
    ## \code{ESTADILLO}, \code{Estadillo}, and \code{NUMPAR}. The main
    ## table keeps its original row order and number of rows; tree-level
    ## tables therefore receive repeated plot coordinates.
    ##details<< Coordinate values are treated as UTM coordinates. IFN2
    ## coordinate columns, normally \code{COORDEX}/\code{COORDEY} or
    ## \code{CX}/\code{CY}, are interpreted as kilometres and multiplied by
    ## 1000 by default. IFN3 and IFN4 coordinate columns, normally
    ## \code{CoorX}/\code{CoorY} or \code{CoorXC}/\code{CoorYC}, are
    ## interpreted as metres.
    ##details<< A \code{Huso} column in the coordinate table always has
    ## priority. When \code{infer.huso = TRUE}, missing UTM zones are filled
    ## from the province code and marked as \code{province_inferred} in
    ## \code{huso_source}; otherwise the corresponding CRS fields remain
    ## \code{NA}.
    if (is.null(nfi))
        return(nfi)

    if (!is.data.frame(nfi))
        stop("'nfi' must be a data.frame.", call. = FALSE)

    if (is.null(nfi.nr) || is.na(nfi.nr))
        stop("'nfi.nr' is required when it is not stored in attr(nfi, 'nfi.nr').",
             call. = FALSE)

    nfi.nr <- as.integer(nfi.nr)[1L]
    if (!nfi.nr %in% c(2L, 3L, 4L))
        stop("'nfi.nr' must be 2, 3, or 4.", call. = FALSE)

    coords <- .nfi_unwrap_table(coords)
    coords <- .nfi_convert_factors_to_numeric_safe(coords)

    out_names <- c(x.name, y.name)
    for (nm0 in c(huso.name, huso.source.name, datum.name, epsg.name, crs.name)) {
        if (!is.null(nm0) && nzchar(nm0))
            out_names <- c(out_names, nm0)
    }

    hit <- out_names[out_names %in% names(nfi)]
    if (length(hit) && !isTRUE(overwrite)) {
        stop(
            "Output coordinate column(s) already exist: ",
            paste(hit, collapse = ", "),
            ". Use overwrite = TRUE or different output names.",
            call. = FALSE
        )
    }

    key_nfi <- .nfi_plot_keys(nfi, allow_missing_province = FALSE)
    key_coords <- .nfi_plot_keys(coords, allow_missing_province = TRUE)

    use_pr <- any(!is.na(key_coords$.nfi_pr))
    if (!use_pr && isTRUE(validate)) {
        warning(
            paste(
                "Coordinate table has no province column.",
                "Joining by plot identifier only. This is safe only for one province."
            ),
            call. = FALSE
        )
    }

    make_key <- function(k, use_pr) {
        if (use_pr)
            paste(k$.nfi_pr, k$.nfi_plot, sep = "\r")
        else
            k$.nfi_plot
    }

    k1 <- make_key(key_nfi, use_pr = use_pr)
    k2 <- make_key(key_coords, use_pr = use_pr)

    if (isTRUE(validate)) {
        dup <- unique(k2[duplicated(k2) & !is.na(k2)])
        if (length(dup)) {
            warning(
                length(dup),
                " duplicated plot key(s) found in the coordinate table. ",
                "The first coordinate record for each key will be used.",
                call. = FALSE
            )
        }
    }

    keep <- !duplicated(k2)
    k2 <- k2[keep]
    coords <- coords[keep, , drop = FALSE]

    cc <- .nfi_coord_columns(
        coords = coords,
        nfi.nr = nfi.nr,
        x.col = x.col,
        y.col = y.col,
        huso.col = huso.col
    )

    factor <- .nfi_coord_factor(nfi.nr, coord.factor = coord.factor)
    x_raw <- .nfi_numeric(coords[[cc$x]])
    y_raw <- .nfi_numeric(coords[[cc$y]])

    idx <- match(k1, k2)
    out <- nfi

    if (isTRUE(keep.raw.coords)) {
        out[[paste0(x.name, "_raw")]] <- x_raw[idx]
        out[[paste0(y.name, "_raw")]] <- y_raw[idx]
    }

    out[[x.name]] <- x_raw[idx] * factor
    out[[y.name]] <- y_raw[idx] * factor

    huso_src <- if (!is.na(cc$huso)) coords[[cc$huso]][idx] else NULL
    ref <- .nfi_reference_values(
        nfi.nr = nfi.nr,
        pr = key_nfi$.nfi_pr,
        huso_from_table = huso_src,
        validate = validate,
        infer.huso = infer.huso
    )

    if (!is.null(huso.name) && nzchar(huso.name))
        out[[huso.name]] <- ref$huso
    if (!is.null(huso.source.name) && nzchar(huso.source.name))
        out[[huso.source.name]] <- ref$huso_source
    if (!is.null(datum.name) && nzchar(datum.name))
        out[[datum.name]] <- ref$datum
    if (!is.null(epsg.name) && nzchar(epsg.name))
        out[[epsg.name]] <- ref$epsg
    if (!is.null(crs.name) && nzchar(crs.name))
        out[[crs.name]] <- ref$crs

    if (isTRUE(validate)) {
        miss <- sum(is.na(idx))
        if (miss > 0L) {
            warning(
                miss,
                " row(s) in 'nfi' did not match the coordinate table.",
                call. = FALSE
            )
        }
    }

    attr(out, "coord_source_columns") <- cc
    attr(out, "coord_factor") <- factor
    attr(out, "coord_units") <- "m"
    attr(out, "coord_type") <- "UTM"
    attr(out, "huso_source_values") <- unique(ref$huso_source)
    attr(out, "coord_source_units") <- if (identical(nfi.nr, 2L)) "km" else "m"
    attr(out, "nfi.nr") <- nfi.nr
    class(out) <- unique(c("readNFIcoords", class(out)))
    out
    ##value<< A data frame with the same rows and row order as \code{nfi},
    ## plus coordinate columns. Output \code{x} and \code{y} are always in
    ## metres. Attributes record the source coordinate columns, source units,
    ## and multiplier used during conversion.
}, ex = function() {
    trees <- data.frame(
        nfi.nr = 2,
        pr = 45,
        ESTADILLO = c(1, 1, 2),
        ESPECIE = c(21, 21, 25)
    )
    attr(trees, "nfi.nr") <- 2

    coords <- data.frame(
        PROVINCIA = 45,
        ESTADILLO = c(1, 2),
        COORDEX = c(412, 413),
        COORDEY = c(4411, 4412)
    )

    x <- addNFIcoords(trees, coords, infer.huso = TRUE)
    x[, c("pr", "ESTADILLO", "x", "y", "huso", "datum", "epsg")]

    ## Keep raw IFN2 kilometre coordinates for checking.
    addNFIcoords(trees, coords, keep.raw.coords = TRUE)
})

readNFIcoords <- structure(function
### readNFIcoords: read an NFI table and append coordinate columns.
###
### Read one Spanish National Forest Inventory table, locate the matching
### plot-coordinate table, and append UTM coordinates and CRS metadata as
### ordinary columns. The wrapper discovers files with
### \code{listNFI_tables()} once and then reuses the local files present in
### \code{dir}; it does not intentionally download the same source twice.
(
    nfi, ##<< Input accepted by \code{\link{listNFI_tables}} and
         ## \code{\link{readNFI}}: province identifier, zip archive,
         ## decompressed files, or a previously loaded data frame.
    nfi.nr = 4, ##<< \code{integer}. Inventory stage: 2, 3, or 4.
    dt.nm = "PCMayores", ##<< Table to import and preserve as the main output.
    coord.nm = NULL, ##<< Optional coordinate table name. If \code{NULL},
                     ## the function validates \code{DATEST*} in
                     ## \code{listNFI_tables()} but passes the generic
                     ## \code{DATEST} stem for IFN2, and uses
                     ## \code{PCDatosMap}/\code{Listado definitivo} for
                     ## IFN3/IFN4 when available.
    file_ext = NULL, ##<< Optional file extension passed to
                     ## \code{listNFI_tables()}.
    file_name = NULL, ##<< Optional file name filter for the main-table
                      ## read. Coordinate discovery ignores this argument.
    validate = TRUE, ##<< \code{logical}. Validate coordinate-table discovery
                     ## and warn about unmatched plot keys.
    ..., ##<< Additional arguments passed to \code{listNFI_tables()}, such
         ## as \code{dir} or \code{timeOut}. The resulting local paths are
         ## then passed to \code{readNFI()}.
    x.name = "x", ##<< Output X-coordinate column name, in metres.
    y.name = "y", ##<< Output Y-coordinate column name, in metres.
    huso.name = "huso", ##<< Output UTM-zone column name when available.
    huso.source.name = "huso_source", ##<< Output column describing whether
                                      ## Huso comes from the coordinate table
                                      ## or was inferred from province code.
    datum.name = "datum", ##<< Output geodetic datum column name.
    epsg.name = "epsg", ##<< Output EPSG code column name when known.
    crs.name = "crs", ##<< Output CRS label column name when known.
    overwrite = FALSE, ##<< Allow overwriting existing coordinate columns.
    keep.raw.coords = FALSE, ##<< Keep raw source coordinates before conversion.
    infer.huso = FALSE ##<< Fill missing UTM zones from province code only when
                       ## requested. A Huso column in the coordinate table has
                       ## precedence. Province-filled values are marked in
                       ## huso_source.
) {
    ##details<< The wrapper first calls \code{listNFI_tables()} to discover
    ## and, when necessary, fetch the available files. After that discovery
    ## step, the main table and the coordinate table are read from the local
    ## paths returned by \code{listNFI_tables()}, respecting the package
    ## cache philosophy and avoiding a second intentional download.
    ##details<< The argument \code{dt.nm} controls the main table. The
    ## coordinate table only supplies plot positions. For IFN2 the coordinate
    ## table is detected as the generic \code{DATEST} stem after validating
    ## available \code{DATESTXX} files. For IFN3 and IFN4 the function looks
    ## for \code{PCDatosMap} or \code{Listado definitivo}, unless
    ## \code{coord.nm} is supplied explicitly.
    ##details<< The output remains a regular \code{data.frame} enriched with
    ## coordinate and CRS metadata columns. Use \code{readNFIsf()} when an
    ## \code{sf} geometry column is preferred.
    if (is.data.frame(nfi)) {
        stop(
            paste(
                "When 'nfi' is already a data.frame, use addNFIcoords()",
                "with an explicit coordinate table. readNFIcoords() is",
                "intended for sources that can also provide a coordinate table."
            ),
            call. = FALSE
        )
    }

    ## Discover the available tables once. For province/URL/ZIP inputs this
    ## is the only step that can call fetchNFI(). If the files already exist
    ## in 'dir', fetchNFI() returns the cached extracted paths.
    tab_args <- c(
        list(nfi = nfi, nfi.nr = nfi.nr),
        list(...)
    )
    if (!is.null(file_ext))
        tab_args$file_ext <- file_ext
    ## Do not pass file_name here: coordinate discovery needs to see DATEST,
    ## PCDatosMap, or other plot-level tables even when the user reads only
    ## one main table.

    tabs <- tryCatch(do.call(listNFI_tables, tab_args),
                     error = function(e) e)

    if (inherits(tabs, "error"))
        stop(conditionMessage(tabs), call. = FALSE)

    if (is.null(tabs) || !is.data.frame(tabs) || !nrow(tabs)) {
        stop(
            paste(
                "No local NFI tables were found.",
                "Check 'nfi', 'nfi.nr', 'dir', 'file_ext', and connection/cache status."
            ),
            call. = FALSE
        )
    }

    if (!"path" %in% names(tabs))
        stop("listNFI_tables() did not return a 'path' column.", call. = FALSE)

    local_files <- unique(as.character(tabs$path))
    local_files <- local_files[!is.na(local_files) & nzchar(local_files)]

    if (!length(local_files))
        stop("No local file paths were returned by listNFI_tables().", call. = FALSE)

    if (is.null(coord.nm)) {
        coord.nm <- .nfi_guess_coord_table(tabs, nfi.nr = nfi.nr)

        if (is.null(coord.nm)) {
            msg <- paste(
                "No coordinate table was detected.",
                "Use coord.nm to specify one explicitly.",
                "Check listNFI_tables() for available dt.nm values."
            )
            if (isTRUE(validate))
                stop(msg, call. = FALSE)
            warning(msg, call. = FALSE)
            return(NULL)
        }
    }

    ## Read the main table from local files, not from the province id/URL.
    ## That avoids a second fetchNFI() call and preserves readNFI() behavior.
    main_files <- local_files
    if (!is.null(file_name)) {
        base_x <- tolower(basename(main_files))
        stem_x <- tolower(tools::file_path_sans_ext(basename(main_files)))
        req <- tolower(file_name)
        keep <- base_x %in% req | stem_x %in% req
        if (any(keep))
            main_files <- main_files[keep]
    }

    province <- .nfi_input_province(nfi)
    x <- .nfi_read_local_table(
        files = main_files,
        nfi.nr = nfi.nr,
        dt.nm = dt.nm,
        province = province
    )

    if (is.null(x) || !is.data.frame(x))
        return(x)

    coords <- .nfi_read_local_table(
        files = local_files,
        nfi.nr = nfi.nr,
        dt.nm = coord.nm,
        province = province
    )
    coords <- .nfi_unwrap_table(coords, preferred = coord.nm)

    out <- addNFIcoords(
        nfi = x,
        coords = coords,
        nfi.nr = nfi.nr,
        x.name = x.name,
        y.name = y.name,
        huso.name = huso.name,
        huso.source.name = huso.source.name,
        datum.name = datum.name,
        epsg.name = epsg.name,
        crs.name = crs.name,
        overwrite = overwrite,
        keep.raw.coords = keep.raw.coords,
        validate = validate,
        infer.huso = infer.huso
    )

    attr(out, "coord_table") <- coord.nm
    attr(out, "nfi_tables") <- tabs
    attr(out, "local_files") <- local_files
    out
    ##value<< A \code{readNFI} data frame enriched with plot-level spatial
    ## coordinates. The imported table named by \code{dt.nm} remains the main
    ## output; coordinate fields are joined by province and plot identifier.
}, ex = function() {
    ## Synthetic example for the joining helper.
    trees <- data.frame(
        nfi.nr = 3,
        pr = 28,
        Estadillo = c(10, 10, 11),
        Especie = c(21, 21, 25)
    )
    attr(trees, "nfi.nr") <- 3

    coords <- data.frame(
        Provincia = 28,
        Estadillo = c(10, 11),
        CoorX = c(440000, 441000),
        CoorY = c(4488000, 4489000),
        Huso = 30
    )

    addNFIcoords(trees, coords)

    ## Real use with a persistent cache directory. This may download data
    ## the first time and reuse the local files later.
    ## cache <- tools::R_user_dir("basifoR", "cache")
    ## x <- readNFIcoords(45, nfi.nr = 4, dt.nm = "PCMayores", dir = cache)
    ## unique(x[, c("huso", "huso_source", "datum", "epsg")])
})

.nfi_tmp_spatial_names <- function(existing) {
    seed <- paste0("..nfi_sf_", format(Sys.getpid(), scientific = FALSE), "_")
    base <- c("x", "y", "huso", "huso_source", "datum", "epsg", "crs")
    out <- paste0(seed, base)
    i <- 0L
    while (any(out %in% existing)) {
        i <- i + 1L
        out <- paste0(seed, i, "_", base)
    }
    stats::setNames(out, base)
}

.nfi_drop_columns <- function(x, cols) {
    cols <- intersect(cols, names(x))
    if (length(cols))
        x[cols] <- NULL
    x
}

.nfi_coord_ref_summary <- function(x, huso.name, huso.source.name,
                                   datum.name, epsg.name, crs.name) {
    n <- nrow(x)
    val <- function(nm) {
        if (is.null(nm) || !nzchar(nm) || !nm %in% names(x))
            return(rep(NA, n))
        x[[nm]]
    }

    out <- data.frame(
        huso = val(huso.name),
        huso_source = val(huso.source.name),
        datum = val(datum.name),
        epsg = val(epsg.name),
        crs = val(crs.name),
        stringsAsFactors = FALSE
    )
    unique(out)
}

.nfi_sf_crs_value <- function(x, epsg.name = NULL, crs = NULL,
                              mixed.crs = c("na", "error")) {
    mixed.crs <- match.arg(mixed.crs)

    if (!is.null(crs))
        return(crs)

    if (is.null(epsg.name) || !nzchar(epsg.name) || !epsg.name %in% names(x))
        return(NA_integer_)

    epsg <- .nfi_to_integer(x[[epsg.name]])
    epsg <- unique(epsg[!is.na(epsg)])

    if (!length(epsg))
        return(NA_integer_)

    if (length(epsg) == 1L)
        return(epsg)

    msg <- paste(
        "More than one EPSG code is present in the coordinate metadata:",
        paste(epsg, collapse = ", "),
        ". An sf geometry column can store only one active CRS."
    )

    if (mixed.crs == "error")
        stop(msg, call. = FALSE)

    warning(paste(msg, "The sf object will be created with crs = NA."),
            call. = FALSE)
    NA_integer_
}

.nfi_as_sf <- function(x, x.name, y.name,
                       huso.name = NULL,
                       huso.source.name = NULL,
                       datum.name = NULL,
                       epsg.name = NULL,
                       crs.name = NULL,
                       crs = NULL,
                       keep.coord.meta = FALSE,
                       mixed.crs = c("na", "error"),
                       na.action = c("keep", "drop", "error"),
                       validate = TRUE) {
    mixed.crs <- match.arg(mixed.crs)
    na.action <- match.arg(na.action)

    if (!requireNamespace("sf", quietly = TRUE)) {
        stop("Package 'sf' is required. Install it before using readNFIsf().",
             call. = FALSE)
    }

    if (!x.name %in% names(x) || !y.name %in% names(x))
        stop("Internal coordinate columns were not found before sf conversion.",
             call. = FALSE)

    missing_xy <- is.na(x[[x.name]]) | is.na(x[[y.name]])
    if (any(missing_xy)) {
        msg <- paste0(sum(missing_xy), " row(s) have missing coordinates.")
        if (na.action == "error")
            stop(msg, call. = FALSE)
        if (na.action == "drop") {
            if (isTRUE(validate))
                warning(paste(msg, "These rows were dropped before sf conversion."),
                        call. = FALSE)
            x <- x[!missing_xy, , drop = FALSE]
        } else if (isTRUE(validate)) {
            warning(paste(msg, "They are kept with missing point geometries."),
                    call. = FALSE)
        }
    }

    ref_summary <- .nfi_coord_ref_summary(
        x = x,
        huso.name = huso.name,
        huso.source.name = huso.source.name,
        datum.name = datum.name,
        epsg.name = epsg.name,
        crs.name = crs.name
    )

    crs_value <- .nfi_sf_crs_value(
        x = x,
        epsg.name = epsg.name,
        crs = crs,
        mixed.crs = mixed.crs
    )

    out <- sf::st_as_sf(
        x,
        coords = c(x.name, y.name),
        crs = crs_value,
        remove = TRUE,
        na.fail = FALSE
    )

    if (!isTRUE(keep.coord.meta)) {
        out <- .nfi_drop_columns(
            out,
            c(huso.name, huso.source.name, datum.name, epsg.name, crs.name)
        )
    }

    attr(out, "coord_type") <- "UTM"
    attr(out, "coord_units") <- "m"
    attr(out, "coord_reference") <- ref_summary
    attr(out, "sf_crs_assigned") <- crs_value
    class(out) <- unique(c("readNFIsf", class(out)))
    out
}

addNFIsf <- structure(function
### addNFIsf: convert an NFI table and coordinate table to sf points.
###
### Join an already loaded inventory table to an already loaded coordinate
### table and return an \code{sf} point data frame. The inventory table is
### preserved as the attribute table, while coordinates are used to build a
### geometry column instead of being returned as ordinary x/y columns.
(
    nfi, ##<< \code{data.frame}. Inventory table, commonly returned by
         ## \code{\link{readNFI}}.
    coords, ##<< \code{data.frame} or one-table \code{list}. Coordinate table.
    nfi.nr = attr(nfi, "nfi.nr"), ##<< \code{integer}. Inventory stage: 2,
                                  ## 3, or 4.
    x.col = NULL, ##<< Optional source X-coordinate column in \code{coords}.
    y.col = NULL, ##<< Optional source Y-coordinate column in \code{coords}.
    huso.col = NULL, ##<< Optional source UTM-zone column in \code{coords}.
    crs = NULL, ##<< Optional CRS passed to \code{sf::st_as_sf}. When NULL,
                ## the function uses the unique EPSG derived from coordinate
                ## metadata, if one is available.
    keep.coord.meta = FALSE, ##<< Keep Huso/datum/EPSG metadata columns in
                             ## the sf attribute table. When FALSE, these are
                             ## stored only in attributes.
    mixed.crs = c("na", "error"), ##<< What to do when rows imply more than
                                   ## one EPSG code. The default creates sf
                                   ## with CRS NA and warns.
    na.action = c("keep", "drop", "error"), ##<< Handling of rows that do not
                                             ## match a coordinate record.
    overwrite = FALSE, ##<< Allow overwriting coordinate metadata columns
                       ## when \code{keep.coord.meta = TRUE}.
    keep.raw.coords = FALSE, ##<< Passed to \code{addNFIcoords}; mainly useful
                             ## for debugging and ignored in the final sf
                             ## output unless \code{keep.coord.meta = TRUE}.
    coord.factor = NULL, ##<< Numeric multiplier applied to raw coordinates.
                         ## Defaults to 1000 for IFN2 and 1 for IFN3/IFN4.
    validate = TRUE, ##<< \code{logical}. Warn about duplicate coordinate
                     ## keys and unmatched plots.
    infer.huso = NULL ##<< Fill missing UTM zones from province code. By
                      ## default, TRUE for IFN2 and FALSE for IFN3/IFN4.
) {
    ##details<< This helper is the spatial analogue of \code{addNFIcoords()}.
    ## It first creates temporary coordinate and CRS metadata columns, then
    ## converts the result to an \code{sf} point object with
    ## \code{sf::st_as_sf()}. Temporary coordinate columns are removed from
    ## the final object unless \code{keep.coord.meta = TRUE}.
    ##details<< An \code{sf} geometry column can store only one active CRS.
    ## If rows imply more than one EPSG code, \code{mixed.crs = "na"} creates
    ## the object with an undefined CRS and stores the detected metadata in
    ## \code{attr(x, "coord_reference")}; \code{mixed.crs = "error"}
    ## stops instead.
    ##details<< Rows without matching coordinates can be kept as missing
    ## point geometries, dropped, or rejected with \code{na.action}. This is
    ## useful when external CSV files contain partial plot coverage.
    if (is.null(nfi.nr) || is.na(nfi.nr))
        stop("'nfi.nr' is required when it is not stored in attr(nfi, 'nfi.nr').",
             call. = FALSE)

    nfi.nr <- as.integer(nfi.nr)[1L]
    if (is.null(infer.huso))
        infer.huso <- identical(nfi.nr, 2L)

    meta_names <- c(
        huso = "huso",
        huso_source = "huso_source",
        datum = "datum",
        epsg = "epsg",
        crs = "crs"
    )

    if (isTRUE(keep.coord.meta)) {
        nm <- c(x = "..nfi_sf_x", y = "..nfi_sf_y", meta_names)
    } else {
        nm <- .nfi_tmp_spatial_names(names(nfi))
    }

    z <- addNFIcoords(
        nfi = nfi,
        coords = coords,
        nfi.nr = nfi.nr,
        x.col = x.col,
        y.col = y.col,
        huso.col = huso.col,
        x.name = nm[["x"]],
        y.name = nm[["y"]],
        huso.name = nm[["huso"]],
        huso.source.name = nm[["huso_source"]],
        datum.name = nm[["datum"]],
        epsg.name = nm[["epsg"]],
        crs.name = nm[["crs"]],
        overwrite = overwrite,
        keep.raw.coords = keep.raw.coords,
        coord.factor = coord.factor,
        validate = validate,
        infer.huso = infer.huso
    )

    out <- .nfi_as_sf(
        x = z,
        x.name = nm[["x"]],
        y.name = nm[["y"]],
        huso.name = nm[["huso"]],
        huso.source.name = nm[["huso_source"]],
        datum.name = nm[["datum"]],
        epsg.name = nm[["epsg"]],
        crs.name = nm[["crs"]],
        crs = crs,
        keep.coord.meta = keep.coord.meta,
        mixed.crs = mixed.crs,
        na.action = na.action,
        validate = validate
    )

    attr(out, "coord_source_columns") <- attr(z, "coord_source_columns")
    attr(out, "coord_factor") <- attr(z, "coord_factor")
    attr(out, "coord_source_units") <- attr(z, "coord_source_units")
    attr(out, "nfi.nr") <- nfi.nr
    out
    ##value<< An \code{sf} data frame with point geometries. The geometry
    ## coordinates are UTM metres. CRS is assigned only when a unique EPSG is
    ## available or when \code{crs} is supplied explicitly.
}, ex = function() {
    trees <- data.frame(
        nfi.nr = 2,
        pr = 45,
        ESTADILLO = c(1, 1, 2),
        ESPECIE = c(21, 21, 25)
    )
    attr(trees, "nfi.nr") <- 2

    coords <- data.frame(
        PROVINCIA = 45,
        ESTADILLO = c(1, 2),
        COORDEX = c(412, 413),
        COORDEY = c(4411, 4412)
    )

    if (requireNamespace("sf", quietly = TRUE)) {
        x <- addNFIsf(trees, coords, infer.huso = TRUE)
        sf::st_crs(x)
        sf::st_geometry(x)
    }
})

readNFIsf <- structure(function
### readNFIsf: read a Spanish NFI table as an sf point data frame.
###
### Read one Spanish National Forest Inventory table and return an
### \code{sf} object with plot geometries. The table requested by
### \code{dt.nm} becomes the attribute table. Coordinates come from the
### appropriate plot-coordinate table detected from \code{listNFI_tables()}.
(
    nfi, ##<< Input accepted by \code{\link{listNFI_tables}}: province
         ## identifier, zip archive, URL, or decompressed files.
    nfi.nr = 4, ##<< \code{integer}. Inventory stage: 2, 3, or 4.
    dt.nm = "PCMayores", ##<< Table to import and preserve as the sf
                          ## attribute table.
    coord.nm = NULL, ##<< Optional coordinate table name. If \code{NULL},
                     ## the function detects DATEST for IFN2 and
                     ## PCDatosMap/Listado definitivo for IFN3/IFN4.
    file_ext = NULL, ##<< Optional file extension passed to
                     ## \code{listNFI_tables()}.
    file_name = NULL, ##<< Optional file name filter for the main-table read.
                      ## Coordinate discovery ignores this argument.
    validate = TRUE, ##<< \code{logical}. Validate coordinate-table discovery
                     ## and warn about unmatched plot keys.
    ..., ##<< Additional arguments passed to \code{listNFI_tables()}, such as
         ## \code{dir} or \code{timeOut}.
    crs = NULL, ##<< Optional CRS passed to \code{sf::st_as_sf}. When NULL,
                ## the function uses the unique EPSG derived from metadata.
    keep.coord.meta = FALSE, ##<< Keep Huso/datum/EPSG metadata columns in
                             ## the sf attribute table. Otherwise they are
                             ## stored in \code{attr(x, "coord_reference")}.
    mixed.crs = c("na", "error"), ##<< What to do when rows imply more than
                                   ## one EPSG code.
    na.action = c("keep", "drop", "error"), ##<< Handling of rows that do not
                                             ## match a coordinate record.
    infer.huso = NULL ##<< Fill missing UTM zones from province code. By
                      ## default, TRUE for IFN2 and FALSE for IFN3/IFN4.
) {
    ##details<< This is the high-level spatial reader. It mirrors the usual
    ## \code{readNFI()} workflow for the selected \code{dt.nm} table, but
    ## returns an \code{sf} point data frame. It should complement rather than
    ## replace \code{readNFI()}, because the return type changes and a
    ## coordinate table must be available.
    ##details<< The function calls \code{listNFI_tables()} once, reuses the
    ## local files in \code{dir}, reads the requested table as attributes,
    ## reads the coordinate table separately, and delegates the geometry
    ## construction to \code{addNFIsf()}.
    ##details<< By default, \code{infer.huso} is \code{TRUE} for IFN2 and
    ## \code{FALSE} for IFN3/IFN4. This prevents silently assigning uncertain
    ## UTM zones for inventories where a province-level fallback may be less
    ## precise than the original map-sheet reference.
    mixed.crs <- match.arg(mixed.crs)
    na.action <- match.arg(na.action)

    if (is.data.frame(nfi)) {
        stop(
            paste(
                "When 'nfi' is already a data.frame, use addNFIsf()",
                "with an explicit coordinate table. readNFIsf() is intended",
                "for sources that can also provide a coordinate table."
            ),
            call. = FALSE
        )
    }

    nfi.nr <- as.integer(nfi.nr)[1L]
    if (is.null(infer.huso))
        infer.huso <- identical(nfi.nr, 2L)

    tab_args <- c(
        list(nfi = nfi, nfi.nr = nfi.nr),
        list(...)
    )
    if (!is.null(file_ext))
        tab_args$file_ext <- file_ext

    tabs <- tryCatch(do.call(listNFI_tables, tab_args), error = function(e) e)
    if (inherits(tabs, "error"))
        stop(conditionMessage(tabs), call. = FALSE)

    if (is.null(tabs) || !is.data.frame(tabs) || !nrow(tabs)) {
        stop(
            paste(
                "No local NFI tables were found.",
                "Check 'nfi', 'nfi.nr', 'dir', 'file_ext', and connection/cache status."
            ),
            call. = FALSE
        )
    }

    if (!"path" %in% names(tabs))
        stop("listNFI_tables() did not return a 'path' column.", call. = FALSE)

    local_files <- unique(as.character(tabs$path))
    local_files <- local_files[!is.na(local_files) & nzchar(local_files)]
    if (!length(local_files))
        stop("No local file paths were returned by listNFI_tables().", call. = FALSE)

    if (is.null(coord.nm)) {
        coord.nm <- .nfi_guess_coord_table(tabs, nfi.nr = nfi.nr)
        if (is.null(coord.nm)) {
            stop(
                paste(
                    "No coordinate table was detected.",
                    "Use coord.nm to specify one explicitly.",
                    "Check listNFI_tables() for available dt.nm values."
                ),
                call. = FALSE
            )
        }
    }

    main_files <- local_files
    if (!is.null(file_name)) {
        base_x <- tolower(basename(main_files))
        stem_x <- tolower(tools::file_path_sans_ext(basename(main_files)))
        req <- tolower(file_name)
        keep <- base_x %in% req | stem_x %in% req
        if (any(keep))
            main_files <- main_files[keep]
    }

    province <- .nfi_input_province(nfi)
    x <- .nfi_read_local_table(
        files = main_files,
        nfi.nr = nfi.nr,
        dt.nm = dt.nm,
        province = province
    )

    if (is.null(x) || !is.data.frame(x))
        return(x)

    coords <- .nfi_read_local_table(
        files = local_files,
        nfi.nr = nfi.nr,
        dt.nm = coord.nm,
        province = province
    )
    coords <- .nfi_unwrap_table(coords, preferred = coord.nm)

    out <- addNFIsf(
        nfi = x,
        coords = coords,
        nfi.nr = nfi.nr,
        crs = crs,
        keep.coord.meta = keep.coord.meta,
        mixed.crs = mixed.crs,
        na.action = na.action,
        validate = validate,
        infer.huso = infer.huso
    )

    attr(out, "coord_table") <- coord.nm
    attr(out, "nfi_tables") <- tabs
    attr(out, "local_files") <- local_files
    out
    ##value<< An \code{sf} data frame. The imported table named by
    ## \code{dt.nm} remains the attribute table; plot coordinates are stored
    ## in the geometry column.
	}, ex = function() {
	    if (interactive()) {
	        ## Real use with a persistent cache directory. This may download data
	        ## the first time and reuse the local files later.
	        cache <- tools::R_user_dir("basifoR", "cache")
	        x <- readNFIsf(45, nfi.nr = 4, dt.nm = "PCMayores", dir = cache)
	        sf::st_crs(x)
	        attr(x, "coord_reference")
	    }

	    ## Visual check against a province boundary, if optional packages exist.
	    ## if (interactive() &&
	    ##     requireNamespace("geodata", quietly = TRUE) &&
	    ##     requireNamespace("terra", quietly = TRUE)) {
	    ##     gadm <- geodata::gadm("ESP", level = 2, path = tempdir())
	    ##     gadm_sf <- sf::st_as_sf(gadm)
	    ##     plot(sf::st_geometry(gadm_sf))
	    ##     plot(sf::st_geometry(sf::st_transform(x, sf::st_crs(gadm_sf))),
	    ##          add = TRUE, pch = 16, cex = 0.3)
	    ## }
	})
