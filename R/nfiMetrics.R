nfiMetrics <- structure(function#Tree metrics from NFI data
### This function recursively implements \code{\link{dbhMetric}} on
### data bases of the Spanish National Forest Inventory (NFI) to
### derive a variety of tree metrics. Use \code{\link{metrics2Vol}} to
### recursively derive the metrics plus over bark volumes.
(
    nfi,  ##<<\code{character} or \code{data.frame}.  URL/path to a
          ##compressed file of the NFI (.zip) having data of either
          ##.dbf or .mdb file extensions, or a data frame such as that
          ##produced by \code{\link{readNFI}}.
    var = c('d','h','ba','n','Hd'), ##<<\code{character}. Metrics. These
                                         ##can be five: \code{(1)} the
                                         ##mean diameter \code{'d'};
                                         ##\code{(2)} the tree height
                                         ##\code{'h'}; \code{(3)} the
                                         ##basal area \code{'ba'};
                                         ##code{(4)} the number of
                                         ##trees per hectare
                                         ##\code{'n'}; and \code{(5)}
                                         ##the dominant height
                                         ##\code{'Hd'}, see Details
                                         ##section in
                                         ##\code{\link{dbhMetric}} for
                                         ##better understanding of the
                                         ##metrics units. Default
                                         ##\code{c('pr','d','h','ba','n','Hd')}.
    levels = c('esta','espe') ##<<\code{character}. levels at which
                              ##the metrics are computed. Pattern
                              ##matching is supported. Cases are
                              ##ignored. Default
                              ##\code{c('esta','espe')} matches both
                              ##the sample plot \code{'Estadillos'}
                              ##and tree species \code{'Especie'}.
) {
        if(is.null(nfi)|is.character(nfi)){
            nfi. <- nfi
        nfi <- readNFI(nfi)
        if(is.null(nfi.))
            return(nfi)}
    fc <- function(dt, cl.){
        nt. <- paste(cl., collapse = '|')
        nt.. <- grep(nt., names(dt),
                     ignore.case = TRUE)
        cl.nm <- sort(names(dt)[nt..],
                      decreasing = TRUE)
        return(cl.nm)}

        ## var.. <- getOption('units')
        ## mn.un <- names(var..[var..%in%var])

        var. <- var[!var%in%'Hd']
    fdn <- function(dbh, var){
        if(var%in%c('d','n','ba'))
            dm <- apply(dbh[,fc(dbh,c('Dn','Diamet'))],1,
                        function(x)dbhMetric(x,var))
        if(var%in%'h'){
            ht <- fc(dbh,c('altura','Ht'))
            ## dbh[,ht] <- as.numeric(as.character(dbh[,ht]))
            ## dm <- conv_unit(dbh[,ht],
            ##                 from = 'm', to = 'dm')}
            dm <- as.numeric(as.character(dbh[,ht]))}
        ## if(var%in%'pr'){
        ##     dm <- rep(attr(dbh,'pr.'), nrow(dbh))
        ## }
        return(dm)}
        dmt <- mapply(function(y)
            fdn(nfi,y), y = var.)
        if(!is.null(attr(nfi,'pr.')))
        dmt <- cbind(pr = attr(nfi,'pr.'), dmt)
        ## nma <- names(nfi)
        ## app <- paste(levels, collapse = '|')
        ## gap <- grepl(app,nma, ignore.case = TRUE)
        ## nms <- nma[gap]
        nms <- flev(nfi, levels)
        nm.. <- c(nms, colnames(dmt))
        dmt <- data.frame(nfi[,nms], dmt)
        names(dmt) <- nm..
        
        if('Hd'%in%var){
            needed <- c('h','d','n')
            nd <- paste(needed, collapse = '?,')
            if(!all(needed%in%var))
                stop(paste0('Hd: missing variables: var = c(',nd,'?, ...)'))
            spl <- split(dmt, dmt[,nms], drop = TRUE)
            dmhe <- Map(function(y)
                cbind(y, Hd = tryCatch(domheight(y$'h',y$'d',y$'n'),
                                       error = function(e) NA)), spl)
            dmt <- do.call('rbind', dmhe) 
            rownames(dmt) <- NULL}

        dmt <- conv_units(dmt)
        
    return(dmt)
### \code{data.frame} containing columns which match the strings in
### \code{levels}, plus the variables defined in \code{var}, including
### the province \code{pr} (\code{dimensionless}), the diameter
### \code{d} (\code{'mm'}), the tree height \code{h} (\code{'dm'}),
### the basal area \code{ba} (\code{'m2 tree-1'}), the number of trees
### by hectare \code{n} (\code{dimensionless}), and the tree dominant
### height \code{Hd} (\code{'m'}).
}, ex = function(){
    ## seconf NFI
    madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
    rmad <- readNFI(madridNFI)[1:10,]
    mmad <- nfiMetrics(rmad)
    head(rmad,3)
    ## see metric units
    attr(rmad,'units')
})
