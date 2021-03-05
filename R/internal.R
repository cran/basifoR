## Internal utility functions used by basifoR

conv_units <- function(nfi, var = c('d','h'), un = c('cm','m')){
    units. <- getOption('units')
    if(!is.null(attr(nfi,'units')))
        units.  <- attr(nfi,'units')
    cols <- units.[units.%in%names(nfi)]
    units_ini <- units_out <- names(cols)
    matches <- sapply(var,function(m) paste0("^",m,"$"))
    pos. <- sapply(matches,function(m) grep(m, cols))
    units_out[pos.]  <- un
    f_conv_unit <- function(x,y,z){
        if(y == "" | z == ""){
            return(x)
        }else{
            conv_unit(x,y,z)}}
    nfi[,cols] <- data.frame(
        mapply(function(x,y,z)
            f_conv_unit(x,y,z),
            nfi[,cols],
            units_ini,
            units_out))
    un_attr <- cols 
    names(un_attr) <- units_out
    attributes(nfi) <- c(attributes(nfi), list(units = un_attr))
    return(nfi)}

flev <- function(vmad, levels){
nma <- names(vmad)
app <- paste(levels, collapse = '|')
gap <- grepl(app,nma, ignore.case = TRUE)
nms <- nma[gap]
return(nms)}



units. <- c('d','h','ba','n','Hd','v')
names(units.) <- c('mm','m','m2','','m','dm3')

units.. <- units.
names(units..) <- c('cm','m','m2','','m','m3')



## /IFNdyn-master/ github proyect with dominantHeight function for NFI
## https://github.com/miquelcaceres/IFNdyn
domheight<-function(h, d, n) {
  o <-order(d, decreasing=TRUE)
  h = h[o]
  n = n[o]
  ncum = 0 
  for(i in 1:length(h)) {
    ncum = ncum + n[i]
    if(!is.na(ncum)&&ncum>100){
        return(sum(h[1:i]*n[1:i], na.rm=TRUE)/sum(h[1:i]*n[1:i]/h[1:i], na.rm=TRUE))}
    ## if(ncum>100) return(sum(h[1:i]*n[1:i], na.rm=TRUE)/sum(h[1:i]*n[1:i]/h[1:i], na.rm=TRUE)) ## this produces an error message if the condition is NA
  }
  return(sum(h*n)/sum(n))
}


.onAttach <- function(lib, pkg)
{
  version <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
  
  if(interactive())
    { # > figlet basifoR
        packageStartupMessage(
          "basifoR
version: ", version)
}
else
    { packageStartupMessage(
          "Package 'basifoR' version ", version) } 

  packageStartupMessage("Type 'citation(\"basifoR\")' for citing this R package in publications.")
  invisible()
}


.onLoad <- function(libname, pkgname){
    op <- options()
    op.FC <- list(url2 = "http://www.mapama.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip",
                  url3 = "http://www.mapama.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/ifn3p01_tcm30-293907.zip",
                  utm = "+proj=utm +zone=utm.z +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                  utm1 = "+proj=utm +zone=utm.z +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                  longlat = '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs',
                  fapp = 'mcmapply',
                  units = units.,
                  units1 = units..)

toset <- !(names(op.FC) %in% names(op))
  if(any(toset)) options(op.FC[toset])
invisible()

}
