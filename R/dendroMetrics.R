dendroMetrics <- structure(function#Summarize dendrometrics
###This function can summarize dendrometric data of the Spanish
###National Forest Inventory (SNFI). It can also control most other
###functions of the package. Dendrometric variables in the outputs are
###transformed into stand units, see the Details section.
                           ##details<< Dendrometric variables are
                           ## summarized according to the levels of
                           ## the argument \code{summ.vr}. The summary
                           ## outputs include the categorical columns
                           ## formulated in \code{summ.vr} and the
                           ## variables defined using
                           ## arguments/defaults in
                           ## \code{\link{nfiMetrics}}. These
                           ## variables include the tree basal area
                           ## \code{ba} (\code{'m2 ha-1'}), the
                           ## average diameter at breast height
                           ## \code{d} (\code{'cm'}), the quadratic
                           ## mean diameter \code{dg} (\code{'cm'}),
                           ## the average tree height \code{h}
                           ## (\code{'m'}), the number of trees by
                           ## hectare \code{n} ('dimensionless'), and
                           ## the over bark volume \code{v} (\code{'m3
                           ## ha-1'}). Subsets of the output summary
                           ## are extracted using logical expressions
                           ## in argument \code{'cut.dt'}, see syntax
                           ## in \code{\link{Logic}}.
(
    nfi, ##<<\code{character} or \code{data.frame}.  URL/path to a
          ##compressed SNFI file (.zip) having data of either
          ##.dbf or .mdb file extensions; or data frame such as that
          ##produced by \code{\link{nfiMetrics}}; or data frame such
          ##as that produced by \code{\link{readNFI}}.
    summ.vr = 'Estadillo', ##<< \code{character} or \code{NULL}. Name
                           ##of a Categorical variables in the SNFI
                           ##data used to summarize the outputs. If
                           ##\code{NULL} then output from
                           ##\code{\link{metrics2Vol}} is
                           ##returned. Default \code{'Estadillo'}
                           ##processes sample plots.
    cut.dt = 'd == d', ##<< \code{character}. Logical condition used
                       ##to subset the output. Default \code{'d == d'}
                       ##avoids subsetting.
    report = FALSE, ##<< \code{logical}. Print a report of the output
                    ##in the current working directory.
    ... ##<< Additional arguments in \code{\link{metrics2Vol}} or
        ##\code{\link{nfiMetrics}} or \code{\link{readNFI}}.
) {
    if(is.null(nfi) | is.character(nfi) | inherits(nfi, 'readNFI')){
        nfi. <- nfi
        nfi <- metrics2Vol(nfi, ...)
        if(is.null(nfi.))
            return(nfi)
    }
    frm. <- attr(nfi, 'units')
    if(is.null(summ.vr)){
        nfi <- subset(nfi,
                      eval(parse(text = cut.dt)))
        attributes(nfi) <- c(attributes(nfi), list(units = frm.))
        if(report)
            write.csv(nfi, file = 'report.csv', row.names = FALSE)
        return(nfi)
    }
    summ.vr <- flev(nfi, summ.vr)
    var <- getOption('units1')[getOption('units1')%in%names(nfi)]
    frm. <- names(attr(nfi, 'units'))
    to. <- names(var)
    var. <- var[var!='n']
    nfi <- conv_units(nfi, var = var, un = to.)
    msp <- split(nfi, nfi[summ.vr])
    msp <- Filter('nrow', msp)

    fsum <- function(dt){
        dt[,var.] <- dt[,var.] * dt[,'n'] 
        summ <- apply(dt[,var], 2,
                      sum, na.rm = TRUE)
        summ[c('d','h', 'Hd')] <- summ[c('d','h','Hd')]/summ['n'] 
        summ['dg'] <- sqrt((4E4 * summ['ba']/summ['n'])/pi)
        summ <- summ[order(names(summ))]
        summ <- sapply(summ,function(x) round(x,3))
        summ <- t(as.matrix(summ))
        fcs. <- names(dt)[!names(dt)%in%var]
        fcs <- dt[1,fcs.]
        resd <- cbind(fcs, summ)}

    resm <- Map(function(x)
        fsum(x), x= msp)
    resm <- Reduce('rbind',resm)
    resm <- data.frame(resm)
    resm <- subset(resm,
                   eval(parse(text = cut.dt)))
    rownames(resm) <- NULL
    if(report)
        write.csv(resm, file = 'report.csv', row.names = FALSE)

    dgcm <- 'dg'
    names(dgcm) <- 'cm'
    attr. <- c(attr(nfi,'units'), dgcm)
    attributes(resm) <- c(attributes(resm), list(units = attr.))

    return(resm)
### \code{data.frame}. Depending on \code{summ.vr = NULL}, an output from
### \code{\link{metrics2Vol}}, or a summary of the variables, see
### Details section.
}, ex = function(){
    ## SNFI Data from the province of Madrid
    madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
    rmad <- readNFI(madridNFI)[1:100,]
    mmad <- nfiMetrics(rmad)
    vmad <- metrics2Vol(mmad)
    dmad <- dendroMetrics(vmad, cut.dt = 'h > 8')
    head(dmad)
    ## see metric units
    attr(dmad,'units')

    ## Retrieval of SNFI data in 'www.miteco.gob.es' and computation
    ## of the corresponding dendrometric summary:
    
    ## donttest{
    ## path <- '/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip'
    ## url2 <- httr::modify_url("https://www.miteco.gob.es", path = path)
    ## dmad <- dendroMetrics(url2, cut.dt = 'h >= 11')
    ## head(dmad)
    ## attr(dmad, 'units')
    ## }
    
})
