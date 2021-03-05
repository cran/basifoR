urlToTemp <- structure(function#Temporary NFI data 
### This function is implemented by other routines of the package to
### decompress \code{.zip} files with data sets of the SNFI. 
                       ##details<< The data sets are decompressed in a
                       ##temporary file of the local
                       ##machine. Compressed data containing file
                       ##extensions other than \code{.mdb} or
                       ##\code{.dbf} are not supported.
(
    url.  ##<<\code{character}.  URL/path to a compressed file of the
              ##SNFI (.zip) having data of either .dbf or .mdb file
              ##extensions..
) {
    if(is.null(url.))
        return(NULL)
    temp <- tempfile()
    is.remote <- grepl('www.mapama.gob.es',url.)
    if(is.remote)
        download.file(url.,temp)
    if(!is.remote)
        file.copy(url.,temp)
    con <- unzip(temp,
                 exdir = tempdir(),
                 list = TRUE)
    con <- unzip(temp,
                 exdir = tempdir(),
                 files = NULL)
    supr.only <- c('mdb','DBF')
    tos <- grepl(paste(supr.only,
                       collapse = "|"), con)
    con <- con[tos]
    return(tryCatch(
        con,error = function(e) NULL))
### \code{character}. Path to the NFI data (.mdb or .dbf) stored in a
### temporary file
}, ex = function(){
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
tfmad <- urlToTemp(madridNFI)
tfmad
})
