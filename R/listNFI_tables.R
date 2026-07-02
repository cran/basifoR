listNFI_tables <- structure(function
                         ##title<< List available raw SNFI tables
                         ##description<< Discover the raw tables or
                         ## files available in Spanish National Forest
                         ## Inventory (SNFI) downloads, local archives,
                         ## or already decompressed files. The function
                         ## helps users inspect available table names
                         ## before reading data with \code{readNFI()}.
                         ##details<< The input \code{nfi} can be a
                         ## province name or province code, a local or
                         ## remote \code{.zip} archive, or one or more
                         ## already decompressed \code{.dbf}, \code{.mdb},
                         ## \code{.accdb}, or \code{.csv} files.
                         ##
                         ## When \code{nfi} is a province identifier, the
                         ## function resolves it with the internal URL
                         ## resolver selected by \code{nfi.nr}: \code{nfi2()},
                         ## \code{nfi3()}, or \code{nfi4()}. It then calls
                         ## \code{fetchNFI()} to download or reuse the
                         ## corresponding local files.
                         ##
                         ## For second-stage SNFI data and other DBF-based
                         ## sources, the function lists one row per DBF file
                         ## and uses the DBF file stem as \code{dt.nm}. For
                         ## CSV sources, it lists one row per CSV file and
                         ## also uses the file stem as \code{dt.nm}. For
                         ## Access sources, it lists one row per table found
                         ## inside each \code{.mdb} or \code{.accdb} file.
                         ##
                         ## The function does not read full data tables. It
                         ## only discovers table or file names. However, it
                         ## must inspect local files. Therefore, when \code{nfi}
                         ## is remote or province-based, at least one download
                         ## or extraction step can be necessary unless the
                         ## files already exist in the same \code{dir} cache.
                         ##
                         ## The arguments \code{file_ext} and \code{file_name}
                         ## are forwarded to \code{fetchNFI()} only when they
                         ## are not \code{NULL}. This keeps \code{fetchNFI()}
                         ## as the single authority for default downloadable
                         ## extensions and matches the forwarding style used
                         ## by \code{readNFI()}.
                         ##
                         ## Access table listing is platform dependent. On
                         ## Windows it requires \pkg{RODBC} and a Microsoft
                         ## Access ODBC driver. Package \pkg{odbc} is optional
                         ## and is used only to check whether such a driver is
                         ## visible. On Unix-like systems, table listing
                         ## requires the external \code{mdbtools} command
                         ## \code{mdb-tables}.
(
    nfi,  ##<< \code{character} or \code{numeric}. Inventory source to
          ## inspect. Accepted values are: (i) a province name or
          ## province code to be resolved to an official SNFI download
          ## URL; (ii) a local or remote \code{.zip} archive; or (iii)
          ## one or more direct paths to decompressed \code{.dbf},
          ## \code{.mdb}, \code{.accdb}, or \code{.csv} files.
    nfi.nr = 4, ##<< \code{integer}. SNFI stage used when \code{nfi}
                ## is given as a province identifier. Use \code{2},
                ## \code{3}, or \code{4}. The value selects the internal
                ## URL resolver \code{nfi2()}, \code{nfi3()}, or
                ## \code{nfi4()}.
    dir = tempdir(), ##<< \code{character}. Directory used by
                     ## \code{fetchNFI()} to store downloaded archives
                     ## and extracted files. Use the same \code{dir} in
                     ## later calls to \code{readNFI()} to reuse cached
                     ## files and avoid unnecessary downloads.
    file_ext = NULL, ##<< \code{character} or \code{NULL}. Optional file
                     ## extension or extensions forwarded to
                     ## \code{\link{fetchNFI}}. Leave \code{NULL} to use
                     ## the default extensions defined by \code{fetchNFI()}.
    file_name = NULL, ##<< \code{character} or \code{NULL}. Optional file
                      ## name or bare stem forwarded to
                      ## \code{\link{fetchNFI}} to keep only specific files
                      ## inside a compressed archive.
    ... ##<< Additional arguments passed to \code{\link{fetchNFI}}, such
        ## as \code{timeOut = httr::timeout(120)}.
) {
    nfi.nr <- as.integer(nfi.nr)[1L]
    if (!nfi.nr %in% c(2L, 3L, 4L))
        stop("'nfi.nr' must be 2, 3, or 4.", call. = FALSE)

    is_url <- function(x) {
        is.character(x) && length(x) > 0L && all(grepl("^https?://", x))
    }

    is_zip <- function(x) {
        is.character(x) && length(x) == 1L &&
            grepl("\\.zip$", x, ignore.case = TRUE)
    }

    is_decompressed_file <- function(x) {
        is.character(x) && length(x) > 0L && all(file.exists(x)) && !is_zip(x)
    }

    empty_answer <- function() {
        data.frame(
            source = character(0),
            dt.nm = character(0),
            file = character(0),
            path = character(0),
            stringsAsFactors = FALSE
        )
    }

    fetch_one <- function(x) {
        ## Match readNFI(): let fetchNFI() own its defaults unless the
        ## user explicitly supplies file_ext or file_name.
        fetch_args <- c(list(url. = x, dir = dir), list(...))

        if (!is.null(file_ext))
            fetch_args$file_ext <- file_ext

        if (!is.null(file_name))
            fetch_args$file_name <- file_name

        do.call(fetchNFI, fetch_args)
    }

    if (is_decompressed_file(nfi)) {
        files <- nfi
    } else {
        if (is_zip(nfi) || is_url(nfi)) {
            src <- nfi
        } else {
            url_fun <- switch(
                as.character(nfi.nr),
                "2" = nfi2,
                "3" = nfi3,
                "4" = nfi4
            )
            src <- url_fun(nfi)
        }

        if (is.null(src) || length(src) == 0L)
            return(empty_answer())

        files <- unlist(lapply(src, fetch_one), use.names = FALSE)
    }

    files <- files[!is.na(files) & nzchar(files)]
    if (!length(files))
        return(empty_answer())

    files <- normalizePath(files, mustWork = FALSE)
    ext <- tolower(tools::file_ext(files))
    ans <- list()

    ## IFN2 and similar sources: one DBF table per file.
    dbf <- files[ext == "dbf"]
    if (length(dbf)) {
        stems <- tools::file_path_sans_ext(basename(dbf))

        ans$dbf <- data.frame(
            source = "DBF",
            dt.nm = stems,
            file = basename(dbf),
            path = dbf,
            stringsAsFactors = FALSE
        )
    }

    ## CSV sources: one table per file.
    csv <- files[ext == "csv"]
    if (length(csv)) {
        stems <- tools::file_path_sans_ext(basename(csv))

        ans$csv <- data.frame(
            source = "CSV",
            dt.nm = stems,
            file = basename(csv),
            path = csv,
            stringsAsFactors = FALSE
        )
    }

    ## IFN3/IFN4 and similar sources: many tables inside one Access file.
    acc <- files[ext %in% c("mdb", "accdb")]
    if (length(acc)) {
        has_windows_access_driver <- function() {
            if (!identical(unname(Sys.info()[["sysname"]]), "Windows"))
                return(FALSE)

            if (!requireNamespace("odbc", quietly = TRUE))
                return(NA)

            drv <- tryCatch(odbc::odbcListDrivers(), error = function(e) NULL)
            if (is.null(drv) || !"name" %in% names(drv))
                return(NA)

            any(grepl("access", drv$name, ignore.case = TRUE))
        }

        list_access_tables <- function(f) {
            is_windows <- identical(unname(Sys.info()[["sysname"]]), "Windows")

            if (is_windows) {
                if (!requireNamespace("RODBC", quietly = TRUE)) {
                    stop(
                        paste(
                            "Missing package 'RODBC'.",
                            "Install it before listing Access tables on Windows."
                        ),
                        call. = FALSE
                    )
                }

                drv <- has_windows_access_driver()
                if (identical(drv, FALSE)) {
                    stop(
                        paste(
                            "Windows Access driver not found.",
                            "Install Microsoft 365 Access Runtime or another",
                            "Microsoft Access driver, restart R, and try",
                            "listNFI_tables() again."
                        ),
                        call. = FALSE
                    )
                }

                if (is.na(drv)) {
                    warning(
                        paste(
                            "Could not verify the Windows Access driver because",
                            "package 'odbc' is not installed. The function will",
                            "still try the RODBC connection, matching readNFI()."
                        ),
                        call. = FALSE
                    )
                }

                con <- RODBC::odbcConnectAccess2007(f, rows_at_time = 1)
                on.exit(RODBC::odbcClose(con), add = TRUE)

                tbl <- RODBC::sqlTables(con)
                if ("TABLE_NAME" %in% names(tbl)) {
                    if ("TABLE_TYPE" %in% names(tbl))
                        tbl <- tbl[toupper(tbl$TABLE_TYPE) == "TABLE", , drop = FALSE]
                    tabs <- tbl$TABLE_NAME
                } else {
                    tabs <- character(0)
                }
            } else {
                if (!nzchar(Sys.which("mdb-tables"))) {
                    sys <- unname(Sys.info()[["sysname"]])
                    hint <- if (identical(sys, "Darwin")) {
                        "Install mdbtools with Homebrew: brew install mdbtools"
                    } else if (file.exists("/etc/arch-release")) {
                        "Install mdbtools with pacman: sudo pacman -S mdbtools"
                    } else {
                        paste(
                            "Install mdbtools with your system package manager,",
                            "e.g. sudo apt install mdbtools"
                        )
                    }
                    stop(paste("External tool 'mdb-tables' not found.", hint),
                         call. = FALSE)
                }

                tabs <- system2("mdb-tables", c("-1", f), stdout = TRUE)
            }

            tabs <- tabs[nzchar(tabs)]
            tabs <- tabs[!grepl("^MSys", tabs, ignore.case = TRUE)]
            tabs <- unique(tabs)

            data.frame(
                source = "Access",
                dt.nm = tabs,
                file = basename(f),
                path = f,
                stringsAsFactors = FALSE
            )
        }

        acc_out <- lapply(acc, list_access_tables)
        ans$access <- do.call(rbind, acc_out)
    }

    if (!length(ans))
        return(empty_answer())

    ans <- do.call(rbind, ans)
    rownames(ans) <- NULL
    ans <- ans[order(ans$source, ans$file, ans$dt.nm), ]
    rownames(ans) <- NULL
    ans
    ##value<< A \code{data.frame} with one row per discovered raw table
    ## or file. The columns are \code{source}, which identifies the
    ## backend as \code{"DBF"}, \code{"CSV"}, or \code{"Access"};
    ## \code{dt.nm}, the table or file stem that users can pass to
    ## \code{readNFI()} when appropriate; \code{file}, the local file
    ## basename; and \code{path}, the normalized local file path. The
    ## function returns an empty data frame with these columns when no
    ## matching files or tables are found.
}, ex = function() {
    ## Self-contained example using a temporary DBF file.
    tmp <- tempfile(fileext = ".dbf")
    foreign::write.dbf(
        data.frame(
            plot = 1:2,
            tree = 1:2,
            dbh = c(15.2, 31.8)
        ),
        file = tmp
    )

    tabs <- listNFI_tables(tmp)
    tabs[, c("source", "dt.nm", "file")]

    unlink(tmp)

    ## Typical SNFI use with a persistent cache directory.
    ## This can download data and may require mdbtools or an Access driver,
    ## so it is intentionally left as commented example code.
    ## cache <- tools::R_user_dir("basifoR", "cache")
    ## tabs4 <- listNFI_tables(28, nfi.nr = 4, dir = cache)
    ## head(tabs4)
    ## x4 <- readNFI(28, nfi.nr = 4, dt.nm = "PCMayores", dir = cache)
})
