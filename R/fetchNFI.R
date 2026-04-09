fetchNFI <- structure(function#Fetch SNF Data 
### This function can download and decompress data sets from the Spanish
### National Forest Inventory (SNF) stored in \code{.zip} files,
### whether they are local or remote (URL-based).
                      ##details<< The data should be in files with
                      ##extensions specified in the \code{file_ext}
                      ##argument. Use \code{file_name} to restrict the
                      ##returned files to specific names inside the
                      ##decompressed content. It accepts either full
                      ##file names or bare stems without extensions.
(
    url.,  ##<<\code{character}. Specifies the URL/path to a
           ##compressed SNF (.zip).
    dir = tempdir(), ##<<\code{character}. Directory where the fetched
                     ##file will be stored.
    file_ext = c('mdb','DBF', 'accdb'), ##<<\code{character}. Supported
                                        ##file extensions.
    file_name = NULL, ##<<\code{character}. Optional file names inside
                      ##the decompressed content to keep. It accepts
                      ##either full file names or bare stems without
                      ##extensions.
    timeOut = timeout(60) ##<<\code{request}. Maximum request time,
                          ##see \code{\link[httr]{timeout}}. Default is
                          ##\code{timeout(60)}.
) {
    if(is.null(url.))
        return(NULL)
    is.remote <- grepl("^https?://", url.)

    if (is.remote) {
        gf <- gracefully_fail(url., timeOut = timeOut)
        if (is.null(gf))
            return(gf)
        zipfile <- file.path(dir, basename(url.))
    } else {
        zipfile <- normalizePath(url., mustWork = FALSE)
    }

    outdir <- file.path(
        dir,
        tools::file_path_sans_ext(basename(zipfile))
    )

    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

    pat <- paste0("\\.(", paste(file_ext, collapse = "|"), ")$")

    keep_requested <- function(x, file_name = NULL) {
        if (is.null(file_name))
            return(x)
        base_x <- tolower(basename(x))
        stem_x <- tolower(tools::file_path_sans_ext(basename(x)))
        req <- tolower(file_name)
        x[base_x %in% req | stem_x %in% req]
    }

    as_extracted_paths <- function(x, outdir) {
        if (length(x) == 0L)
            return(x)
        x_abs <- tryCatch(normalizePath(x, mustWork = FALSE), error = function(e) x)
        is_abs <- grepl("^(/|[A-Za-z]:[/\\\\])", x)
        out <- ifelse(is_abs, x, file.path(outdir, x))
        out <- ifelse(file.exists(x_abs), x_abs, out)
        out
    }

    cached <- list.files(
        outdir,
        pattern = pat,
        full.names = TRUE,
        recursive = TRUE,
        ignore.case = TRUE
    )

    cached <- keep_requested(cached, file_name)

    if (length(cached) > 0L)
        return(cached)

    zip_ok <- function(zf) {
        if (!file.exists(zf))
            return(FALSE)
        out <- tryCatch(unzip(zf, list = TRUE), error = function(e) NULL)
        !is.null(out) && nrow(out) > 0L
    }

    if (is.remote) {
        if (file.exists(zipfile) && !zip_ok(zipfile))
            unlink(zipfile)

        if (!file.exists(zipfile))
            download.file(url., zipfile, mode = "wb")
    }

    if (!zip_ok(zipfile))
        return(NULL)

    con <- tryCatch(
        unzip(zipfile, exdir = outdir, files = NULL),
        error = function(e) NULL
    )

    if (is.null(con))
        return(NULL)

    con <- as_extracted_paths(con, outdir)
    tos <- grepl(pat, con, ignore.case = TRUE)
    con <- con[tos]
    con <- keep_requested(con, file_name)

    if (length(con) == 0L)
        return(NULL)

    return(con)
### \code{character}. Returns the path to the fetched and decompressed
### NFI data (.mdb, .DBF, .accdb, or other extensions requested in
### \code{file_ext}) stored in a temporary file.
}, ex = function(){
## Process SNF data for Toledo stored locally
# Path to Toledo data file in 'basifoR' package
ifn4p45 <- system.file("Ifn4_Toledo.zip", package="basifoR")

# Decompress SNF data from the specified file path or URL
fetch_ifn4p45 <- fetchNFI(ifn4p45)
print(fetch_ifn4p45)

## French NFI tree table read from the official web resource
## f <- "https://inventaire-forestier.ign.fr/dataifn/data/export_dataifn_2024_en.zip"
## fnfi <- fetchNFI(f, file_ext = "csv", file_name = "ARBRE")
## print(fnfi)
    
})
