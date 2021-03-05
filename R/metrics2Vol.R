metrics2Vol <- structure(function#Tree volumes in NFI data
### This function computes over bark volumes (\code{'m3'}) processing
### tree metrics from databases of the SNFI data and using volume
### equations established in 2nd NFI, see Details section. To derive
### dendrometric summaries use \code{\link{dendroMetrics}}.
                         ##details<< The quations from the second SNFI
                         ##used have the forms \code{'v ~ par1 + par2
                         ##* (d^2) * h'}, and \code{'v ~ par1 *
                         ##(d^par2) * (h^par3)'}. These equations
                         ##estimate over bark volumes in \code{'dcm3'}
                         ##but the function transform the units so the
                         ##output volumes have \code{'m3'} units. The
                         ##function assign equation forms and
                         ##parameters depending on several variables,
                         ##including the provincial unit, the tree
                         ##species, the tree diameter, and the tree
                         ##height. Consequently, objects from
                         ##\code{\link{nfiMetrics}} must have these
                         ##variables.
(
    nfi,  ##<<\code{character} or \code{data.frame}.  URL/path to a
          ##compressed file of the NFI (.zip) having data of either
          ##.dbf or .mdb file extensions; or data frame such as that
          ##produced by \code{\link{nfiMetrics}}; or data frame such
          ##as that produced by \code{\link{readNFI}}.
    cub.met = 'freq', ##<< \code{character}. Cubication
                      ##method. Default \code{'freq'} implements the
                      ##equation form that most frequently match the
                      ##data, see \code{details} section. Additional
                      ##methods have not yet been implemented.
    keep.var = FALSE, ##<< \code{logical}. Keep the variables used to
                      ##compute the volumes. Default \code{FALSE}.
    ... ##<< Depending on \code{'nfi'}, additional arguments in
        ##\code{\link{metrics2Vol}} or \code{\link{nfiMetrics}} or
        ##\code{\link{readNFI}}.
) {
    if(is.null(nfi) | is.character(nfi) | inherits(nfi, 'readNFI')){
        nfi. <- nfi
        nfi <- nfiMetrics(nfi, ...)
    if(is.null(nfi.))
        return(nfi)}
    spec. <- names(nfi)[grepl('spec', names(nfi), ignore.case = TRUE)]
    var <- c('pr','h','d')
    needed <- c('Especie/ESPECIE', var)
    nd <- paste(needed, collapse = '?,')
    if(!all(length(spec.) != 0 & var%in%names(nfi))){
        warning("nfiMetrics: change arguments 'var'and/or 'levels'")
        stop(paste0('v: missing variables: nfi[,c(',nd,'?, ...)]'))
    }

    var.. <- getOption('units')
    var.. <- var..[var..%in%names(nfi)]
    attr_un <- attr(nfi,'units')
    if(!is.null(attr_un))
        names(var..)[var..%in%attr_un] <- names(attr_un)
    nfi. <- nfi

    nfi <- conv_units(nfi, var = c('d','h'), un = c('mm','dm'))
    
    mds <- c('1'  = 'v ~ par1 + par2 * (d^2) * h',
             '11' = 'v ~ par1 * (d^par2) * (h^par3)')
    fc <- function(dt, cl.){
        nt. <- paste(cl., collapse = '|')
        nt.. <- grep(nt., names(dt),
                     ignore.case = TRUE)
        cl.nm <- sort(names(dt)[nt..],
                      decreasing = TRUE)
        return(cl.nm)}
    fmdV <- function(mdb2, ntm = c('pr','spec')){
        ## data(parEqVcc, envir = environment())
        ## load('parEqVcc.RData')
        vt <- merge(mdb2, parEqVcc,
                    by.x = fc(mdb2, ntm),
                    by.y = fc(parEqVcc, ntm),
                    all.x = TRUE)
        return(vt)}
    feV <- function(vt, md){
        fvarin <- function(fun,ind = TRUE){
            fun <- formula(fun)
            allv <- all.vars(fun,
                             functions = FALSE) 
            yvar <- all.vars(update(fun, . ~ 1),
                             functions = FALSE)
            inds <- allv[!allv%in%yvar]
            if(!ind)inds <- yvar
            return(inds)}
        fev <- function(fun, md){
            e <- list2env(as.list(md))
            y <- eval(parse(text=fun), e)
            return(y)}
        ind <- fvarin(md)
        dep <- fvarin(md, F)
        sbs <- paste(dep,'~', sep = '|') 
        md. <- gsub(sbs,'',md)
        md. <- gsub(' ','',md.)
        vt. <- vt[,ind]
        vl <- apply(vt.,1,function(x)fev(md.,x))
        vl <- cbind(vt, vl)
        names(vl) <- c(names(vt),dep)
        return(vl)}

    vt <- fmdV(nfi)
    lvs <- levels(as.factor(vt$'Modelo'))
    spm <- split(vt, vt[,'Modelo'])
    nms. <- names(spm)
    mds. <- mds[nms.]
    mmod <- Map(function(x,y)
        feV(x,y),x = spm, y = mds.)
    mmd <- do.call('rbind', mmod)
    tex <- fc(mmd,c('mod','par')) 
    if(!keep.var)
        mmd <- mmd[,!names(mmd)%in%tex]
    ffreq <- function(df){
        tm <- data.frame(table(df$'fc'))
        tm <- subset(tm,get('Freq')%in%max(get('Freq')))
        tm <- as.character(tm$'Var1')[1]
        return(tm)    
    }
    if(cub.met%in%'freq')
        cub.met <- ffreq(mmd)
    mmd <- subset(mmd, fc%in%as.factor(cub.met))
    if(!keep.var)
        mmd <- mmd[,!names(mmd)%in%'fc']
    vun <- getOption('units')[getOption('units')=='v']
    attr(mmd, 'units') <- c(attr(nfi, 'units'), vun) 
    mmd <- conv_units(mmd, var = c('d','h','v'), un = c('cm','m','m3'))        
    rownames(mmd) <- NULL
    return(mmd)
### \code{data.frame}. Depending on \code{keep.var}, short or expanded
###  data set.  Short data sets contain the volumes \code{v}
###  (\code{'m3'}) plus the metrics defined in
###  \code{\link{nfiMetrics}}. The expanded data contains additional
###  columns with the variables used to compute the volumes.
}, ex = function(){
    madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
    rmad <- readNFI(madridNFI)[1:10,]
    vmad <- metrics2Vol(rmad)
    head(vmad)
    ## see metric units
    attr(vmad,'units')
})
