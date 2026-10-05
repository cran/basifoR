# R/internal.R

has_mdbtools_backend <- function() {
    all(nzchar(Sys.which(c("mdb-tables", "mdb-export"))))
}

has_windows_access_driver <- function() {
    if (!identical(unname(Sys.info()[["sysname"]]), "Windows")) {
        return(FALSE)
    }
    TRUE
}

## assert_access_backend <- function(...) {
##     ...
## }

## `%||%` <- function(x, y) if (is.null(x)) y else x
null_or <- function(x, y) if (is.null(x)) y else x
    `%||%` <- function(a, b) if (is.null(a)) b else a


# Cleaner Hd computation helpers for basifoR external workflows
#
# Use with:
# external_dendroMetrics(..., domheight_fun = domheight_strict)
# external_dendroMetrics(..., domheight_fun = domheight_mean_fallback)
# external_dendroMetrics(..., domheight_fun = domheight_flexible)

prepare_domheight_inputs <- function(h, d, n) {
    h <- as.numeric(h)
    d <- as.numeric(d)
    n <- as.numeric(n)

    if (!(length(h) == length(d) && length(d) == length(n))) {
        stop("'h', 'd', and 'n' must have the same length.", call. = FALSE)
    }

    ok <- is.finite(h) & is.finite(d) & is.finite(n) & h > 0 & d > 0 & n > 0
    if (!any(ok)) {
        return(list(h = numeric(0), d = numeric(0), n = numeric(0)))
    }

    h <- h[ok]
    d <- d[ok]
    n <- n[ok]

    o <- order(d, decreasing = TRUE)
    list(h = h[o], d = d[o], n = n[o])
}

weighted_height_mean <- function(h, n) {
    sw <- sum(n, na.rm = TRUE)
    if (!is.finite(sw) || sw <= 0) {
        return(NA_real_)
    }
    sum(h * n, na.rm = TRUE) / sw
}

select_domheight_slice <- function(n, threshold = 100) {
    threshold <- as.numeric(threshold)[1]
    if (!is.finite(threshold) || threshold <= 0) {
        stop("'threshold' must be a single positive number.", call. = FALSE)
    }

    i <- which(cumsum(n) >= threshold)[1]
    if (is.na(i)) {
        return(integer(0))
    }

    seq_len(i)
}

# Strict dominant height:
# returns NA when the observed trees with valid h, d, n do not reach the threshold.
domheight_strict <- function(h, d, n, threshold = 100) {
    x <- prepare_domheight_inputs(h, d, n)
    if (!length(x$h)) {
        return(NA_real_)
    }

    idx <- select_domheight_slice(x$n, threshold = threshold)
    if (!length(idx)) {
        return(NA_real_)
    }

    weighted_height_mean(x$h[idx], x$n[idx])
}

# Backward-compatible dominant height:
# returns the weighted mean height of all valid trees when the threshold is not reached.
domheight_mean_fallback <- function(h, d, n, threshold = 100) {
    x <- prepare_domheight_inputs(h, d, n)
    if (!length(x$h)) {
        return(NA_real_)
    }

    idx <- select_domheight_slice(x$n, threshold = threshold)
    if (!length(idx)) {
        return(weighted_height_mean(x$h, x$n))
    }

    weighted_height_mean(x$h[idx], x$n[idx])
}

# Flexible dominant height:
# fallback = "mean" reproduces the current package logic more clearly.
# fallback = "na" is the safer option for sparse external NFI height data.
domheight_flexible <- function(h, d, n, threshold = 100,
                               fallback = c("mean", "na")) {
    fallback <- match.arg(fallback)
    x <- prepare_domheight_inputs(h, d, n)
    if (!length(x$h)) {
        return(NA_real_)
    }

    idx <- select_domheight_slice(x$n, threshold = threshold)
    if (!length(idx)) {
        if (identical(fallback, "na")) {
            return(NA_real_)
        }
        return(weighted_height_mean(x$h, x$n))
    }

    weighted_height_mean(x$h[idx], x$n[idx])
}


metrics2Vol_legacy <- structure(function#Tree volumes in NFI data
### This function computes over bark volumes (\code{'m3'}) processing
### tree metrics from databases of the SNF data and using volume
### equations established in 2nd NFI, see Details section. To compute
### all in-package metrics, run function \code{\link{dendroMetrics}}.
                         ##details<< The quations from the second SNF
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
        ##\code{\link{nfiMetrics}} or
        ##\code{\link{readNFI}}.
) {

    nfi. <- nfi
    if (is.null(nfi.))
        return(nfi)

    if (!inherits(nfi., "nfiMetrics"))
        nfi <- nfiMetrics(nfi, ...)

    nfi_nr <- attr(nfi, "nfi.nr")
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

    ## The parEqVcc equations take diameter in mm and height in m and return
    ## volume in dm3, as the official IFN equations do. Evaluating them with
    ## height in dm overstated volume about tenfold (fixed in 0.8.0). If older
    ## inputs arrive without unit metadata, assume the current package
    ## convention (d in mm, h in m) and make the conversion explicit.
    units_in <- attr(nfi, "units")
    if (is.null(units_in))
        units_in <- c(d = "mm", h = "m")
    if (is.null(names(units_in)))
        stop("Legacy metrics2Vol requires named unit metadata.", call. = FALSE)
    if ("d" %in% names(nfi) && !"d" %in% names(units_in))
        units_in["d"] <- "mm"
    if ("h" %in% names(nfi) && !"h" %in% names(units_in))
        units_in["h"] <- "m"
    attr(nfi, "units") <- units_in

    nfi <- conv_units(nfi, var = c('d','h'), un = c('mm','m'))
    
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
    ## load('/home/wihe/Documents/tuh32536/bfRdevel/basifoR/R/sysdata.rda')
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
    ## Form class ('freq'): choose one form class within each province and
    ## species group, not one for the whole dataset. A dataset-wide choice
    ## dropped every species that lacks that form class. Within a group each
    ## tree joins every form-class row of its species, so the group itself
    ## cannot rank them; the tie is broken by frequency in the whole dataset,
    ## which keeps the previous choice for groups that have the dataset mode.
    if(cub.met%in%'freq'){
        fc_chr <- as.character(mmd$'fc')
        rank_fc <- names(sort(table(fc_chr), decreasing = TRUE))
        keys <- fc(nfi, c('pr','spec'))
        keys <- keys[keys %in% names(mmd)]
        grp <- if (length(keys)) {
            interaction(mmd[keys], drop = TRUE, lex.order = TRUE)
        } else {
            factor(rep('all', nrow(mmd)))
        }
        pick <- tapply(fc_chr, grp, function(f) {
            f <- unique(f[!is.na(f)])
            if (!length(f)) NA_character_ else rank_fc[rank_fc %in% f][1L]
        })
        chosen <- unname(pick[as.character(grp)])
        mmd <- mmd[!is.na(fc_chr) & !is.na(chosen) & fc_chr == chosen, , drop = FALSE]
    } else {
        mmd <- subset(mmd, fc%in%as.factor(cub.met))
    }
    if(!keep.var)
        mmd <- mmd[,!names(mmd)%in%'fc']
    ## vun <- getOption('units')[getOption('units')=='v']
    ## attr(mmd, 'units') <- c(attr(nfi, 'units'), vun) 
    ## mmd <- conv_units(mmd, var = c('d','h','v'), un = c('cm','m','m3'))        

## fix
u <- attr(nfi, "units")
if (is.null(u))
    u <- c(d = "mm", h = "m")
if ("d" %in% names(mmd) && !"d" %in% names(u))
    u["d"] <- "mm"
if ("h" %in% names(mmd) && !"h" %in% names(u))
    u["h"] <- "m"
u["v"] <- "dm3"
attr(mmd, "units") <- u

mmd <- conv_units(
    mmd,
    var = c("d", "h", "v"),
    un  = c("cm", "m", "m3"))


    rownames(mmd) <- NULL

n <- names(mmd)
first <- c("nfi.nr","pr", "estadillo","especie")
i <- match(first, tolower(n))
i <- i[!is.na(i)]
mmd <- mmd[, c(n[i], n[-i]), drop = FALSE]
    
    attr(mmd, "nfi.nr") <- nfi_nr
    class(mmd) <- append('metrics2vol',class(mmd))
    return(mmd)
### \code{data.frame}. Depending on \code{keep.var}, short or expanded
###  data set.  Short data sets contain the volumes \code{v}
###  (\code{'m3'}) plus the metrics defined in
###  \code{\link{nfiMetrics}}. The expanded data contains additional
###  columns with the variables used to compute the volumes.
}, ex = function(){
## Process SNF data for Toledo stored locally
# Path to Toledo data file in 'basifoR' package
ifn4p45 <- system.file("Ifn4_Toledo.zip", package="basifoR")

# Decompress SNF data from the specified file path or URL
fetch_ifn4p45 <- fetchNFI(ifn4p45)

# Read and process the data (first 100 rows)
get_ifn4p45 <- readNFI(fetch_ifn4p45)[1:100,]

# Compute some metrics
metrics_ifn4p45 <- nfiMetrics(get_ifn4p45)

# Calculate volume metrics 
vol_ifn4p45 <- metrics2Vol(metrics_ifn4p45)

## see metric units
    attr(vol_ifn4p45,'units')
})


find_code__ <- function(input_value, is.ifn4, df) {
    x <- trimws(as.character(input_value))

    norm <- function(z) {
        z <- as.character(z)
        z <- iconv(z, from = "", to = "ASCII//TRANSLIT")
        tolower(trimws(z))
    }

    x_norm <- norm(x)

    is_num <- grepl("^[0-9]+$", x_norm)

    if (is_num) {
        hit <- which(
            norm(df$codigo)  == x_norm |
            norm(df$codigo2) == x_norm
        )[1L]
    } else {
        hit <- which(
            norm(df$provincia)   == x_norm |
            norm(df$provincia_0) == x_norm |
            norm(df$provincia_1) == x_norm |
            norm(df$codigo)      == x_norm |
            norm(df$codigo2)     == x_norm
        )[1L]

        if (is.na(hit)) {
            hit <- which(
                grepl(x_norm, norm(df$provincia),   fixed = TRUE) |
                grepl(x_norm, norm(df$provincia_0), fixed = TRUE) |
                grepl(x_norm, norm(df$provincia_1), fixed = TRUE)
            )[1L]
        }
    }

    if (is.na(hit))
        return(NA_character_)

    if (is.ifn4) {
        out <- df$provincia_1[hit]
    } else {
        out <- df$codigo[hit]
    }

    as.character(out)
}

get_ifn_nr <- function(x) {
  nm <- basename(x)
  
  if (all(grepl("\\.DBF", nm, ignore.case = TRUE))) {
    return(2L)
  }
  
  m <- regexpr("ifn([0-9]+)", nm, ignore.case = TRUE, perl = TRUE)
  
  if (any(m > 0)) {
    hit <- regmatches(nm, m)[m > 0][1]
    return(as.integer(sub("(?i)ifn([0-9]+).*", "\\1", hit, perl = TRUE)))
  }
  
  NA_integer_
}

## Testing functions in basifoR
nfi4 <- function(prov, complain = TRUE){
## Function to download ifn4 data using a province code
    if(is.null(prov))
        return(invisible(NULL))
    ## dt <- read.csv('procods_Cristobal.csv')
    dt <- procods
prov. <- prov
    ## prov <- find_code(dt, prov)
        prov <- find_code_(prov, is.ifn4 = TRUE, df = dt)
    if(is.null(prov))
        return(invisible(NULL))
u <- miteco_urls_from_paths('path41')
    all_links. <- unlist(Map(function(x)
        inspect_links(x, prov, ignore.case = TRUE), u))
    pattern <- "[iI]fn4[_\\-p]?"
  exclude <- "[tT]ablas|[sS]ig"
  matches <- grep(pattern, all_links., value = TRUE) # Find strings matching 'ifn4'
  all_links <- matches[!grepl(exclude, matches)]    # Exclude unwanted patterns
    parsed <- mapply(function(x)
        httr::modify_url(getOption('server'), path = x),
        all_links, USE.NAMES = FALSE)
if(length(parsed) == 0){
        if(complain)
            warning(paste0("URL for spanish province '", prov., "' not found!\n"),
                    call. = FALSE)
    return(invisible(NULL))
}
return(parsed)}


find_code_ <- function(input_value, is.ifn4 = TRUE, df, complain = TRUE) {
  result <- df$codigo[
    grepl(input_value, df$codigo) | 
    grepl(input_value, df$provincia) | 
    grepl(input_value, df$codigo2) |
    grepl(input_value, df$provincia_0) |
    grepl(input_value, df$provincia_1)
    ][1L]
  if(is.ifn4){
      result <- df$provincia_1[
                       grepl(paste0('^',result,'$'), df$codigo,
                             ignore.case = TRUE)]}
  if(length(result) == 0)
      result <- NA
      if(is.na(result) & complain){
          warning(paste0("Spanish province '", input_value, "' not found!\n"),
                  call. = FALSE)
        return(invisible(NULL))}
  ## }
  # Return the result
  return(result)
}

check_extension_in_zip <- function(url, extension){
  temp_file <- tempfile(fileext=".zip")
  suppressWarnings(
    tryCatch({
      download.file(url, temp_file, mode="wb", quiet=TRUE)
      zip_contents <- unzip(temp_file, list=TRUE)$Name
      ## has_extension <- any(grepl(extension, zip_contents, ignore.case=TRUE))
      has_extension <- grepl(extension, zip_contents, ignore.case=TRUE)
      has_extension <- url[has_extension]
      unlink(temp_file)
      return(has_extension)
    }, error=function(e){
      ## message("An error occurred:", e$message)
      return(FALSE)
    })
  )
}

#----------------------------------------------------------------
## Internal utility functions used by basifoR

# Function to replace a row based on two indices
replace_provincia <- function(df, row1, row2) {
    if(is.character(row1))
        row1 <- find_provincia_or_codigo(row1)
    if(is.character(row2))
        row1 <- find_provincia_or_codigo(row2)
  # Check if row indices are within the data frame bounds
  if (any(row1 > nrow(df) | row2 > nrow(df))) {
    stop("Row indices are out of bounds")
  }
  
  # Replace the row corresponding to row1 with the row corresponding to row2
  df[row1, ] <- df[row2, ]
  
  return(df)
}

# Function to test response from a URL
test_url_response <- function(url) {
  # Send GET request
  response <- GET(url)
  
  # Check the status code
  status_code <- status_code(response)
  ## print(paste("Status Code:", status_code))
return(status_code)  
  ## # Check the content type
  ## content_type <- headers(response)$`content-type`
  ## print(paste("Content Type:", content_type))
  
  ## # Check the content of the response
  ## content <- content(response, as = "text", encoding = "UTF-8")
  ## print(paste("Content:", substr(content, 1, 500)))  # Print the first 500 characters
}

.onAttach <- function(lib, pkg)
{
  version <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
  if(interactive())
  { # > figlet basifoR
      msg <- basifoR_figlet()
      packageStartupMessage(msg)
    }
    else
    { packageStartupMessage(
          "Package 'basifoR' version ", version) }
    packageStartupMessage("Type 'citation(\"basifoR\")' for citing this R package in publications\n")
    invisible()
}

.onLoad <- function(libname, pkgname){
    op <- options()
    op.FC <- list(
        server = "http://www.miteco.gob.es",
        path21 = "es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn2_parcelas_1_25.html",
        path22 = "es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn2_parcelas_26_50.html",
        path31 = "es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn3_base_datos_1_25.html",
        path32 = "es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn3_base_datos_26_50.html",
        path41 = "es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional/cuarto_inventario.html", 
        utm = "+proj=utm +zone=utm.z +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
        utm1 = "+proj=utm +zone=utm.z +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
        longlat = '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs',
        fapp = 'mcmapply',
        dt.ext = c('mdb','DBF', 'accdb'),
        units = units.,
        units1 = units..,
        basifoR.units.tree = units.,
        basifoR.units.stand = units..)
    toset <- !(names(op.FC) %in% names(op))
    if(any(toset)) options(op.FC[toset])
    invisible()
}

basifoR_figlet <- function(){
msg <- cat(
"
 _           _ ___     _____ 
| |_ ___ ___|_|  _|___| __  |
| . | .'|_ -| |  _| . |    -|
|___|__,|___|_|_| |___|__|__|\n
"
)
vrs <- paste0('basifoR version ',packageVersion("basifoR"),'\n')
cat(vrs)
}


conv_units <- function(nfi, var = c("d", "h"), un = c("cm", "m")) {
    units. <- attr(nfi, "units")

    if (is.null(units.)) {
        units. <- getOption("basifoR.units.tree")

        legacy_units <- getOption("units")
        if (!is.null(legacy_units) && !is.null(names(legacy_units))) {
            overlaps <- names(legacy_units) %in% names(nfi)
            if (any(overlaps))
                units. <- legacy_units
        }
    }

    if (is.null(units.) || is.null(names(units.)))
        return(nfi)

    cols <- names(units.)[names(units.) %in% names(nfi)]
    if (!length(cols)) {
        attr(nfi, "units") <- units.[0]
        return(nfi)
    }

    units_ini <- units_out <- unname(units.[cols])

    pos. <- match(var, cols)
    ok <- !is.na(pos.)
    units_out[pos.[ok]] <- un[ok]

    convert_unit_label <- function(x, from, to) {
        if (from == "" || to == "" || from == to)
            return(x)

        if (!grepl(" ", from, fixed = TRUE) && !grepl(" ", to, fixed = TRUE))
            return(conv_unit(x, from, to))

        rx <- "^(.+) ha-1$"
        from_m <- regexec(rx, from)
        to_m   <- regexec(rx, to)

        from_cap <- regmatches(from, from_m)[[1]]
        to_cap   <- regmatches(to, to_m)[[1]]

        if (length(from_cap) == 2L && length(to_cap) == 2L) {
            from_num <- from_cap[2]
            to_num   <- to_cap[2]
            return(conv_unit(x, from_num, to_num))
        }

        stop("Unsupported unit conversion from '", from, "' to '", to, "'.")
    }

    nfi[, cols] <- data.frame(
        mapply(function(x, y, z) convert_unit_label(x, y, z),
               nfi[, cols, drop = FALSE],
               units_ini,
               units_out,
               SIMPLIFY = FALSE),
        check.names = FALSE
    )

    un_attr <- units_out
    names(un_attr) <- cols
    attributes(nfi) <- c(attributes(nfi), list(units = un_attr))
    return(nfi)
}


convert_factors_to_numeric <- function(df) {
# Function to convert factor columns to numeric while preserving
# character columns
  df[] <- lapply(df, function(col) {
    if (is.factor(col) && all(grepl("^-?\\d*\\.?\\d+$", as.character(col)))) {
      return(as.numeric(as.character(col)))
    } else {
      return(col)
    }
  })
  return(df)
}

domheight<-function(h, d, n) {
## /IFNdyn-master/ github proyect with dominantHeight function for NFI
## https://github.com/miquelcaceres/IFNdyn
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

file_exten <- function(texts)
    sapply(texts, function(x) sub(".*\\.(.*)", "\\1", x),
           USE.NAMES = FALSE)


find_provincia_or_codigo <- function(input) { #
# Function to find provincia if input is numeric, or codigo/codigo2 if
# input is character (case insensitive)
    ## to comment:
    ## load('/home/wihe/Documents/tuh32536/bfRdevel/basifoR/R/sysdata.rda')
    data <- procods
    if (is.numeric(input)) {  # Check if input is numeric
    result <- data$provincia[grepl(paste0("^", input, "$"), data$codigo, ignore.case = TRUE)]
  } else if (is.character(input)) {  # Assume input is character
      result <- data$codigo[grepl(input, data$provincia,
                                  ignore.case = TRUE)]
    if (length(result) == 0) {
        result <- data$codigo2[grepl(paste0("^", input, "$"),
                                     data$provincia, ignore.case = TRUE)]
    }
  } else {
    result <- NA
  }
  if (length(result) == 0) {
    result <- NA
  }
    ## if(is.na(result))
    ## cat(paste0("Warning: provincia or codigo '", input, "' was not found!\n"))
  return(result)
}

flev <- function(vmad, levels){
nma <- names(vmad)
app <- paste(levels, collapse = '|')
gap <- grepl(app,nma, ignore.case = TRUE)
nms <- nma[gap]
return(nms)}

gracefully_fail <- function(remote_file, timeOut = timeout(50)) {
## source:
## https://community.rstudio.com/t/internet-resources-should-fail-gracefully/49199/11
  try_GET <- function(x, ...) {
    tryCatch(
      GET(url = x, timeOut, ...),
      ## GET(url = x, timeout(50), ...),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }
  is_response <- function(x) {
    class(x) == "response"
  }
  # First check internet connection
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }
  # Then try for timeout problems
  resp <- try_GET(remote_file)
  if (!is_response(resp)) {
    message(resp)
    return(invisible(NULL))
  }
  # Then stop if status > 400
  if (httr::http_error(resp)) { 
    message_for_status(resp)
    return(invisible(NULL))
  }
return(TRUE)
}

insert_ifn_ifn4 <- function(input_string) {
  # Define the pattern to match "ifn4" followed by "_" or "-"
  pattern <- "nacionales/ifn4(?=[_-])"
  # Use gregexpr to find all matches
  match_positions <- gregexpr(pattern, input_string, perl = TRUE)
  # Check if there is exactly one match
  if (length(match_positions[[1]]) == 1 && match_positions[[1]][1] != -1) {
    # Find the position of the match
    start_pos <- match_positions[[1]][1]
    # Insert "ifn/ifn4" before the match
    result_string <- paste0(substr(input_string, 1, start_pos - 1), 
                            "ifn/ifn4/", 
                            substr(input_string, start_pos, nchar(input_string)))
  } else {
    # If the pattern is not found exactly once, return the original string
    result_string <- input_string
  }
      return(result_string)
}

inspect_links <- function(url, pattern = NULL, ...) {
# Function to inspect links
    webpage <- rvest::read_html(url)
    links <- rvest::html_nodes(webpage, 'a')
    links <- rvest::html_attr(links, "href")
  if (!is.null(pattern)) {
    links <- links[grepl(pattern, links, perl = TRUE, ...)]
  }
  return(links)
}

is_decompressed <- function(x)
    grepl(paste0(getOption('dt.ext'), collapse = '|'), x)

miteco_urls_from_paths <- function(paths = c('path21', 'path22')) 
    mapply(function(x)
        httr::modify_url(getOption('server'), path = getOption(x)),
        paths,
        USE.NAMES = FALSE)

msg <- basifoR_figlet()

nfi2 <- function(prov, complain = TRUE){
## Function to download ifn2 data using a province code
    if(is.null(prov))
        return(invisible(NULL))
u <- miteco_urls_from_paths(c('path21', 'path22'))
all_links <- unlist(Map(function(x)inspect_links(x,'zip'), u), use.names = FALSE)
parsed <- mapply(function(x)
    httr::modify_url(getOption('server'), path = x), all_links, USE.NAMES = FALSE)
    prov. <- prov
    prov <- find_code_(prov, is.ifn4 = FALSE, df = procods)
    ## if(is.na(prov) | length(prov) == 0){
if(is.null(prov)){
        ## if(complain)
    ## warning(paste0("Spanish province '", prov., "' not found!\n"))
        return(invisible(NULL))}
    ptt <- prov + 5
if(ptt < 10)
    ptt <- paste0('0', ptt)
ptt <- paste0(ptt, '.zip')
## return(ptt)
parsed. <- parsed[grepl(ptt, parsed)]
if(length(parsed.) == 0){
    warning(paste0("URL for spanish province '", prov., "' not found!\n"),
            call. = FALSE)
    return(invisible(NULL))
}
return(parsed.)}

nfi3 <- function(prov){
## Function to download ifn3 data using a province code
    if(is.null(prov))
        return(invisible(NULL))
u <- miteco_urls_from_paths(c('path31', 'path32'))
all_links <- unlist(Map(function(x)inspect_links(x,"fn3.*\\.zip"), u), use.names = FALSE)
parsed <- mapply(function(x)httr::modify_url(getOption('server'), path = x), all_links, USE.NAMES = FALSE)
prov. <- prov
msg <- paste0("Warning: Data for codigo '", prov., "' was not found!\n")
## if(is.character(prov))
##  prov <- find_provincia_or_codigo(prov)
prov <- find_code_(prov, is.ifn4 = FALSE, df = procods)
## if(is.na(prov)){
##     cat(msg)
##     return(invisible(NULL))}
if(is.null(prov)){
        ## if(complain)
    ## warning(paste0("Spanish province '", prov., "' not found!\n"))
        return(invisible(NULL))}
if(prov < 10)
    prov <- paste0('0', prov)
prov <- paste0(prov, '.zip')
parsed. <- parsed[grepl(prov, parsed)]
if(length(parsed.) == 0){
    warning(paste0("URL for spanish province '", prov., "' not found!\n"),
            call. = FALSE)
    return(invisible(NULL))
}
return(parsed.)}

units. <- c(
    d = 'mm',
    h = 'm',
    ba = 'm2 tree-1',
    n = 'ha-1',
    Hd = 'm',
    v = 'm3 tree-1'
)

units.. <- c(
    d = 'cm',
    h = 'm',
    ba = 'm2 ha-1',
    n = 'ha-1',
    Hd = 'm',
    v = 'm3 ha-1'
)
