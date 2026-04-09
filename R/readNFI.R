readNFI <- structure(function
### Read raw tables from the Spanish National Forest Inventory (SNFI)
### and compatible inventory exports. The function accepts province
### identifiers that are resolved to official SNFI download URLs, local
### or remote \code{.zip} archives, and direct paths to decompressed
### \code{.csv}, \code{.dbf}, \code{.mdb}, or \code{.accdb} files.
                       ##title<< Read raw SNFI tables from archives, URLs, or local files
                       ##description<< Import raw inventory tables from
                       ## the Spanish National Forest Inventory (SNFI)
                       ## or compatible tabular exports. Use
                       ## \code{readNFI()} when you need the original
                       ## table structure before computing metrics with
                       ## higher-level workflows.
                       ##details<< The input \code{nfi} can be supplied
                       ## in three main forms. First, it can be a
                       ## province name or code; in that case,
                       ## \code{readNFI()} resolves the identifier to
                       ## an official SNFI download URL according to
                       ## \code{nfi.nr}. Second, it can be a local or
                       ## remote \code{.zip} archive; the function then
                       ## delegates extraction to \code{\link{fetchNFI}}.
                       ## Third, it can be one or more already
                       ## decompressed file paths.
                       ##
                       ## When the selected files are \code{.csv}, the
                       ## function detects the field separator
                       ## automatically and returns either one data
                       ## frame or a named list of data frames. When the
                       ## selected files are Access or DBF tables from
                       ## the SNFI, the function reads the requested
                       ## table, converts numeric-looking factors back to
                       ## numeric values, preserves character columns,
                       ## and adds province and inventory-stage metadata.
                       ##
                       ## Access backends are platform dependent. On
                       ## Windows, reading \code{.mdb} or \code{.accdb}
                       ## files requires package \pkg{RODBC} and an
                       ## installed Microsoft Access driver. On
                       ## Unix-like systems, it requires package
                       ## \pkg{Hmisc} together with the external
                       ## \code{mdbtools} utilities. Use
                       ## \code{file_ext = "csv"} to bypass those
                       ## dependencies when you work with zipped CSV
                       ## exports.
(
    nfi,  ##<< \code{character}. Inventory source to read. Accepted
          ## values are: (i) a province name or province code to be
          ## resolved to an official SNFI download URL; (ii) a local or
          ## remote \code{.zip} archive; or (iii) one or more direct
          ## paths to decompressed \code{.csv}, \code{.dbf},
          ## \code{.mdb}, or \code{.accdb} files.
    nfi.nr = 4, ##<< \code{integer}. SNFI stage used when \code{nfi}
                ## is given as a province identifier or when the
                ## inventory stage cannot be inferred from file names.
                ## Use \code{2}, \code{3}, or \code{4}.
    dt.nm = 'PCMayores', ##<< \code{character}. Table name or names to
                         ## import from the selected inventory source.
                         ## For second-stage DBF inputs,
                         ## \code{"PCMayores"} is internally remapped to
                         ## \code{"PIESMA"}. For many third- and
                         ## fourth-stage tree workflows,
                         ## \code{"PCMayores"} is the main tree table.
    file_ext = NULL, ##<< \code{character}. Optional file extension or
                     ## extensions forwarded to \code{\link{fetchNFI}}
                     ## when \code{nfi} is a province identifier or a
                     ## \code{.zip} archive. Leave \code{NULL} to use
                     ## the default Access/DBF extensions handled by
                     ## \code{fetchNFI()}. Use \code{"csv"} for zipped
                     ## CSV exports.
    file_name = NULL, ##<< \code{character}. Optional file name or stem
                      ## passed to \code{\link{fetchNFI}} to keep only
                      ## specific files inside a \code{.zip} archive.
    ... ##<< Additional arguments passed to \code{\link{fetchNFI}},
        ## such as \code{dir} or \code{timeOut}.
) {
    imp <- nfi

    if (is.data.frame(imp))
        return(imp)

    if (is.factor(imp))
        imp <- as.character(imp)

    if (is.numeric(imp)) {
        if (length(imp) != 1L || is.na(imp)) {
            stop(
                "Numeric 'nfi' inputs must be a single non-missing province code.",
                call. = FALSE
            )
        }
        imp <- as.character(imp)
    }

    if (!is.character(imp))
        stop(
            paste(
                "'nfi' must be a province identifier, file path/URL,",
                "or a data.frame already loaded in memory."
            ),
            call. = FALSE
        )

    is.ifn4 <- nfi.nr == 4

    is_zip_path <- function(x) {
        is.character(x) &&
            length(x) == 1L &&
            !is.na(x) &&
            tolower(tools::file_ext(x)) == "zip"
    }

    is_csv <- function(x) {
        is.character(x) &&
            length(x) > 0L &&
            all(grepl("\\.csv$", x, ignore.case = TRUE))
    }

    detect_sep <- function(fi, n = 5L) {
        hdr <- tryCatch(readLines(fi, n = n, warn = FALSE),
                        error = function(e) character(0))
        hdr <- hdr[nzchar(trimws(hdr))]
        if (length(hdr) == 0L)
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

    read_one_csv <- function(fi) {
        sep <- detect_sep(fi)
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

    read_csv_files <- function(x) {
        out <- lapply(x, read_one_csv)
        ok <- !vapply(out, is.null, logical(1))
        out <- out[ok]
        x <- x[ok]
        if (length(out) == 0L)
            return(NULL)
        names(out) <- tools::file_path_sans_ext(basename(x))
        if (length(out) == 1L)
            return(out[[1L]])
        out
    }

    has_mdbtools_backend <- function() {
        all(nzchar(Sys.which(c("mdb-tables", "mdb-export"))))
    }

    has_windows_access_driver <- function() {
        if (!identical(unname(Sys.info()[["sysname"]]), "Windows")) {
            return(FALSE)
        }

        if (!requireNamespace("odbc", quietly = TRUE)) {
            return(NA)
        }

        drv <- tryCatch(odbc::odbcListDrivers(), error = function(e) NULL)
        if (is.null(drv) || !"name" %in% names(drv)) {
            return(NA)
        }

        any(grepl("access", drv$name, ignore.case = TRUE))
    }

    assert_access_backend <- function(backend = c("odbc", "mdbtools")) {
        backend <- match.arg(backend)

        if (backend == "odbc") {
            if (!requireNamespace("RODBC", quietly = TRUE)) {
                stop(
                    "Missing package 'RODBC'. Install it before reading Access files on Windows.",
                    call. = FALSE
                )
            }

            drv <- has_windows_access_driver()
            if (identical(drv, FALSE)) {
                stop(
                    paste(
                        "Windows Access driver not found.",
                        "Install Microsoft 365 Access Runtime or another Microsoft Access driver,",
                        "restart R, and try readNFI() again."
                    ),
                    call. = FALSE
                )
            }

            if (is.na(drv)) {
                warning(
                    paste(
                        "Could not verify the Windows Access driver because package 'odbc' is not installed.",
                        "basifoR will still try the RODBC connection."
                    ),
                    call. = FALSE
                )
            }

            return(invisible(TRUE))
        }

        if (!requireNamespace("Hmisc", quietly = TRUE)) {
            stop(
                "Missing package 'Hmisc'. Install it before reading Access files on Unix-like systems.",
                call. = FALSE
            )
        }

        if (!has_mdbtools_backend()) {
            sys <- unname(Sys.info()[["sysname"]])
            install_hint <- if (identical(sys, "Darwin")) {
                "Install it with Homebrew: brew install mdbtools"
            } else if (file.exists("/etc/arch-release")) {
                "Install it with pacman: sudo pacman -S mdbtools"
            } else {
                "Install it with your system package manager, for example: sudo apt install mdbtools"
            }

            stop(
                paste(
                    "External tool 'mdbtools' not found.",
                    install_hint
                ),
                call. = FALSE
            )
        }

        invisible(TRUE)
    }

    fetch_args <- c(list(url. = NULL), list(...))
    if (!is.null(file_ext))
        fetch_args$file_ext <- file_ext
    if (!is.null(file_name))
        fetch_args$file_name <- file_name

    code_match <- find_code__(imp, is.ifn4 = is.ifn4, df = procods)
    pr. <- find_code__(imp, FALSE, df = procods)

    if (!is.na(code_match) && !is_zip_path(imp)) {
        nfi. <- paste0("nfi", nfi.nr)
        imp <- do.call(nfi., list(prov = imp))
        fetch_args$url. <- imp
        imp <- do.call(fetchNFI, fetch_args)
    } else if (is_zip_path(imp)) {
        fetch_args$url. <- imp
        imp <- do.call(fetchNFI, fetch_args)
        if (!is.null(imp) && !is_csv(imp))
            nfi.nr  <- get_ifn_nr(imp)
    }

    if (is.null(imp))
        return(imp)

    if (is_csv(imp))
        return(read_csv_files(imp))

    fwin <- function(x, dt.nm) {
        ife <- RODBC::odbcConnectAccess2007(x, rows_at_time = 1)
        on.exit(RODBC::odbcClose(ife))
        ifc <- Map(function(x)
            RODBC::sqlFetch(ife, sqtable = x), dt.nm)
        return(ifc)
    }
    fmdb <- function(x, dt.nm) {
        tryCatch(Hmisc::mdb.get(x, tables = dt.nm),
                 error = function(e) NULL)
    }
    fdbf <- function(x, dt.nm) {
        x <- x[grepl(dt.nm, x)]
        foreign::read.dbf(x)
    }
    is_dbf <- all(grepl('\\.dbf$', imp, ignore.case = TRUE))
    is_mdb <- all(grepl('\\.mdb$|\\.accdb$', imp, ignore.case = TRUE))
    is_win <- identical(unname(Sys.info()['sysname']), 'Windows')

    if (is_mdb) {
        if (is_win) {
            assert_access_backend("odbc")
            fnim <- 'fwin'
        } else {
            assert_access_backend("mdbtools")
            fnim <- 'fmdb'
        }
    }
    if (is_dbf) {
        fnim <- 'fdbf'
    }
    dt.nm. <- dt.nm
    may. <- grepl('mayores', dt.nm, ignore.case = TRUE)
    may2. <- grepl('dbf', fnim)
    if (may. & !may2.) {
        dt.nm. <- unique(c(dt.nm, 'PCDatosMap'))
    }
    if (may2. & dt.nm %in% 'PCMayores')
        dt.nm. <- 'PIESMA'
    dset <- tryCatch(do.call(fnim, list(imp, dt.nm.)),
                     error = function(e) NULL)
    if (is.null(dset))
        return(dset)
    if (!may. & !dt.nm.[1] %in% 'PIESMA')
        return(dset)
    if (may. & !may2.) {
        pr. <- unique(dset$'PCDatosMap'$'Provincia')
        dset <- dset[[dt.nm]]
    }
    if (dt.nm.[1] %in% 'PIESMA')
        pr. <- unique(dset$'PROVINCIA')
    dset <- convert_factors_to_numeric(dset)
    attributes(dset) <- c(attributes(dset), list(pr. = pr.))
    attr(dset, "nfi.nr") <- nfi.nr
    if ('provincia' %in% tolower(names(dset))) {
        dset <- data.frame(nfi.nr = nfi.nr, dset)
        names(dset)[tolower(names(dset)) == "provincia"] <- "pr"
    } else {
        dset <- data.frame(nfi.nr = nfi.nr, pr = pr., dset)
    }
    class(dset) <- append('readNFI', class(dset))
    return(dset)
##value<< Returns one of three object types, depending on the input.
## First, when \code{nfi} resolves to \code{.csv} file paths, the
## function returns a single \code{data.frame} for one file or a named
## \code{list} of \code{data.frame}s for several files. Second, when
## it reads Access or DBF tables from the SNFI, it returns a
## \code{data.frame} of class \code{c("readNFI", "data.frame")}.
## This object includes leading columns \code{nfi.nr} and \code{pr},
## stores the province vector in \code{attr(x, "pr.")}, and stores the
## inferred inventory stage in \code{attr(x, "nfi.nr")}. Third, the
## function returns \code{NULL} when fetching, extraction, or import
## fails, or when no requested file can be read.
}, ex = function(){
    ## Minimal example using a local CSV file created on the fly
    tmp <- tempfile(fileext = ".csv")
    utils::write.table(
        data.frame(
            plot = 1:2,
            species = c("sp1", "sp2"),
            dbh_cm = c(12.5, 18.0)
        ),
        file = tmp,
        sep = ";",
        row.names = FALSE,
        quote = FALSE
    )

    x <- readNFI(tmp)
    str(x)

    unlink(tmp)
})
