nfiMetrics <- structure(function#Tree-level metrics for Spanish NFI inputs
### Compute tree-level diameter, height, basal area, trees per
### hectare, and optional dominant height from Spanish National Forest
### Inventory inputs. Supply either an object returned by
### \code{\link{readNFI}} or a path/URL that \code{\link{readNFI}}
### can import. The function returns the requested metrics together
### with the matched grouping columns and attaches unit metadata to the
### result.
(
    nfi,  ##<< \code{character(1)} or a \code{"readNFI"} object.
          ##<< Supply either a path/URL that \code{\link{readNFI}} can
          ##<< import or an already imported object returned by
          ##<< \code{\link{readNFI}}. The input should contain the SNFI
          ##<< diameter and height fields, usually aliases such as
          ##<< \code{Dn} and \code{altura}.
    var = c('d','h','ba','n','Hd'), ##<< \code{character}. Metrics to
                                    ##<< compute. Supported values are
                                    ##<< \code{'d'} (diameter in
                                    ##<< \code{mm}), \code{'h'}
                                    ##<< (height in \code{dm}),
                                    ##<< \code{'ba'} (basal area per
                                    ##<< tree in \code{m^2}),
                                    ##<< \code{'n'} (trees per hectare),
                                    ##<< and \code{'Hd'} (dominant
                                    ##<< height in \code{dm}).
                                    ##<< Request \code{'h'},
                                    ##<< \code{'d'}, and \code{'n'}
                                    ##<< together when you request
                                    ##<< \code{'Hd'}.
    levels = c('esta','espe'), ##<< \code{character}. Column-name
                               ##<< patterns used to keep grouping
                               ##<< variables in the output. Matching
                               ##<< ignores case and accepts partial
                               ##<< matches. The default usually keeps
                               ##<< plot and species identifiers when
                               ##<< those fields are present.
    design = snfi_design(), ##<< Sampling design used to derive
                            ##<< \code{'n'} and any dependent
                            ##<< \code{'Hd'} calculation. Supply the
                            ##<< default \code{\link{snfi_design}()},
                            ##<< another \code{"concentric_design"},
                            ##<< or any \code{"inventory_design"}
                            ##<< supported by \code{\link{trees_per_ha}}.
                            ##<< The returned object stores a summary of
                            ##<< the design in \code{attr(x,
                            ##<< "design_meta")} when relevant.
    ... ##<< Additional arguments passed to \code{\link{readNFI}} when
        ##<< \code{nfi} is not already a \code{"readNFI"} object.

) {
    ns_fun <- function(name) {
        fn <- get0(name, mode = "function", inherits = TRUE)
        if (is.null(fn)) {
            fn <- tryCatch(
                getFromNamespace(name, "basifoR"),
                error = function(e) NULL
            )
        }
        fn
    }

    ##details<< When \code{nfi} is not already a \code{"readNFI"}
    ##details<< object, \code{nfiMetrics()} first calls
    ##details<< \code{\link{readNFI}(nfi, ...)}.
    ##details<<
    ##details<< The function resolves diameter columns from common SNFI
    ##details<< aliases such as \code{Dn}, \code{Diamet}, and
    ##details<< \code{Diametro}, and height columns from
    ##details<< \code{altura} or \code{Ht}. When numbered repeated
    ##details<< measurements are present, it averages them row-wise
    ##details<< after converting zeros to \code{NA}.
    ##details<<
    ##details<< For \code{'ba'}, the function returns basal area per
    ##details<< tree. For \code{'n'}, it applies the supplied sampling
    ##details<< design to each tree diameter and returns trees per
    ##details<< hectare.
    ##details<<
    ##details<< If you request \code{'Hd'}, the function computes
    ##details<< dominant height within the groups selected by
    ##details<< \code{levels} by using \code{'h'}, \code{'d'}, and
    ##details<< \code{'n'}.
    ##seealso<< dendroMetrics, dbhMetric, readNFI, snfi_design, trees_per_ha

    ## Return early on NULL input to preserve the previous behaviour.
    nfi. <- nfi
    if(is.null(nfi.))
        return(nfi)

    ## Import the data only when the input is not already a readNFI object.
    if(!inherits(nfi., "readNFI"))
        nfi <- readNFI(nfi, ...)

    nfi_nr <- attr(nfi, "nfi.nr")

    ## Find columns whose names match one or more search patterns.
    fc <- function(dt, cl.){
        nt. <- paste(cl., collapse = '|')
        nt.. <- grep(nt., names(dt),
                     ignore.case = TRUE)
        cl.nm <- sort(names(dt)[nt..],
                      decreasing = TRUE)
        return(cl.nm)
    }

    ## Resolve measurement columns conservatively to avoid averaging
    ## unrelated DBH or height fields before tier assignment. Prefer an
    ## exact alias first; otherwise allow numbered repeats of the same
    ## base field (for example Dn1, Dn2) and only then average them.
    resolve_measure_cols <- function(dt, aliases) {
        nm0 <- names(dt)
        nml <- tolower(nm0)
        ali <- tolower(aliases)

        ## Prefer an exact alias, in declared order.
        ii <- match(ali, nml)
        ii <- ii[!is.na(ii)]
        if(length(ii) > 0L)
            return(nm0[ii[1L]])

        ## Otherwise allow numbered repeats of the same base field.
        hits <- integer(0)
        for(a in ali) {
            rx <- paste0('^', a, '([._]?[0-9]+)?$')
            hits <- c(hits, grep(rx, nml, perl = TRUE))
        }
        hits <- unique(hits)

        if(length(hits) == 0L)
            return(character(0))

        cols <- nm0[hits]
        base <- sub('([._]?[0-9]+)$', '', tolower(cols))

        ## Only average if all matched columns are numbered repeats of
        ## one field. If the match is ambiguous, keep the first column
        ## and warn instead of averaging different sources.
        if(length(unique(base)) > 1L) {
            warning(
                'Ambiguous measurement columns matched: ',
                paste(cols, collapse = ', '),
                '. Using ', cols[1L],
                call. = FALSE
            )
            return(cols[1L])
        }

        cols[order(cols)]
    }

    ## Dominant height is computed later from h, d, and n.
    var. <- var[!var %in% 'Hd']

    diam_cols <- character(0)
    ht_cols <- character(0)

    ## Resolve the raw diameter and height columns only when needed.
    if(any(var. %in% c('d', 'n', 'ba')))
        diam_cols <- resolve_measure_cols(nfi, c('Dn', 'Diamet', 'Diametro'))

    if(any(var. %in% 'h'))
        ht_cols <- resolve_measure_cols(nfi, c('altura', 'Ht'))

    ## Convert selected columns to a numeric matrix while preserving order.
    get_numeric_matrix <- function(dt, cols) {
        if(length(cols) == 0L)
            return(NULL)

        x <- dt[, cols, drop = FALSE]
        x <- lapply(x, function(z) suppressWarnings(as.numeric(as.character(z))))
        x <- as.data.frame(x, check.names = FALSE,
                           stringsAsFactors = FALSE)
        as.matrix(x)
    }

    ## Vectorised trees-per-hectare calculation for concentric designs,
    ## with fallback to trees_per_ha() for other supported designs.
    trees_per_ha_vec <- function(dbh_cm, design) {

        if (inherits(design, "concentric_design")) {
            out <- rep(NA_real_, length(dbh_cm))
            ok <- !is.na(dbh_cm) & dbh_cm >= design$min_dbh_cm[1]
            if (any(ok)) {
                idx <- findInterval(dbh_cm[ok], design$min_dbh_cm)
                out[ok] <- design$sf[idx]
            }
            return(out)
        }

        vapply(dbh_cm,
               function(x) trees_per_ha(design = design, dbh_cm = x),
               numeric(1))
    }

    ## Pre-allocate derived vectors.
    diam_mm <- NULL
    diam_cm <- NULL
    trees_ha <- NULL
    ht_dm <- NULL

    ## Diameter-based metrics: treat zeros as missing values and average
    ## repeated diameter columns row-wise when several measurements exist.
    if(length(diam_cols) > 0L) {
        diam_mat <- get_numeric_matrix(nfi, diam_cols)
        diam_mat[diam_mat == 0] <- NA_real_

        if(ncol(diam_mat) == 1L) {
            diam_mm <- diam_mat[, 1L]
        } else {
            nn <- rowSums(!is.na(diam_mat))
            diam_mm <- rowMeans(diam_mat, na.rm = TRUE)
            diam_mm[nn == 0L] <- NA_real_
        }

        if(any(var. %in% c('ba', 'n')))
            diam_cm <- conv_unit(diam_mm, from = 'mm', to = 'cm')

        if(any(var. %in% 'n'))
            trees_ha <- trees_per_ha_vec(diam_cm, design)
    }

    ## Height-based metrics: treat zeros as missing values and average
    ## repeated height columns row-wise when present.
    if(length(ht_cols) > 0L) {
        ht_mat <- get_numeric_matrix(nfi, ht_cols)
        ht_mat[ht_mat == 0] <- NA_real_

        if(ncol(ht_mat) == 1L) {
            ht_m <- ht_mat[, 1L]
        } else {
            nn <- rowSums(!is.na(ht_mat))
            ht_m <- rowMeans(ht_mat, na.rm = TRUE)
            ht_m[nn == 0L] <- NA_real_
        }

        ht_dm <- conv_unit(ht_m, from = 'm', to = 'dm')
    }

    ## Dispatch each metric, using the fast precomputed vectors when the
    ## relevant raw columns were already resolved above.
    fdn <- function(dbh, var){
        if(var %in% 'd') {
            if(length(diam_cols) > 0L)
                return(diam_mm)
            cols <- resolve_measure_cols(dbh, c('Dn', 'Diamet', 'Diametro'))
            return(apply(dbh[, cols, drop = FALSE], 1,
                         function(x) dbhMetric(x, var)))
        }

        if(var %in% 'ba') {
            if(length(diam_cols) > 0L)
                return(pi * diam_cm^2 * (4 * 1E4)^-1)
            cols <- resolve_measure_cols(dbh, c('Dn', 'Diamet', 'Diametro'))
            return(apply(dbh[, cols, drop = FALSE], 1,
                         function(x) dbhMetric(x, var)))
        }

        if(var %in% 'n') {
            if(length(diam_cols) > 0L)
                return(trees_ha)
            cols <- resolve_measure_cols(dbh, c('Dn', 'Diamet', 'Diametro'))
            return(apply(dbh[, cols, drop = FALSE], 1,
                         function(x) dbhMetric(x, var, design = design)))
        }

        if(var %in% 'h') {
            if(length(ht_cols) > 0L)
                return(ht_dm)
            cols <- resolve_measure_cols(dbh, c('altura', 'Ht'))
            return(apply(dbh[, cols, drop = FALSE], 1,
                         function(x) dbhMetric(x, var, design = design)))
        }
    }

    ## Compute the requested metrics except dominant height.
    metric_list <- vector('list', length(var.))
    if(length(var.) > 0L) {
        for(i in seq_along(var.))
            metric_list[[i]] <- fdn(nfi, var.[i])
    }

    dmt <- data.frame(metric_list, check.names = FALSE,
                      stringsAsFactors = FALSE)
    if(length(var.) > 0L)
        names(dmt) <- var.

    ## Preserve province codes stored as attribute by readNFI().
    if(!is.null(attr(nfi, 'pr.')))
        dmt <- cbind(pr = attr(nfi, 'pr.'), dmt)

    nm_all <- names(nfi)

    ## Match grouping columns case-insensitively while preserving their
    ## original names in the input object.
    match_cols <- function(want, nm_all) {
        out <- nm_all[tolower(nm_all) %in% tolower(want)]
        unique(out)
    }

    id_cols <- match_cols(c('nfi.nr', 'pr'), nm_all)

    flev_fun <- ns_fun("flev")
    if (is.null(flev_fun))
        stop("Could not resolve internal helper 'flev'.", call. = FALSE)

    nms_raw <- flev_fun(nfi, levels)
    nms_raw <- nms_raw[!is.na(nms_raw)]
    nms <- match_cols(nms_raw, nm_all)

    keep_cols <- unique(c(id_cols, nms))
    keep_cols <- keep_cols[keep_cols %in% nm_all]

    ## Bind the grouping columns requested through levels.
    if(length(keep_cols) == 0L) {
        dmt <- data.frame(dmt, check.names = FALSE)
    } else {
        dmt <- data.frame(nfi[, keep_cols, drop = FALSE], dmt,
                          check.names = FALSE)
    }

    ## Compute dominant height within each grouping level.
    if('Hd' %in% var) {
        needed <- c('h', 'd', 'n')
        nd <- paste(needed, collapse = '?,')
        if(!all(needed %in% var))
            stop(paste0('Hd: missing variables: var = c(', nd, '?, ...)'))
        spl <- split(dmt, dmt[, nms], drop = TRUE)
        dmhe <- Map(function(y)
            cbind(y, Hd = tryCatch({
                domheight_fun <- ns_fun("domheight")
                if (is.null(domheight_fun))
                    stop("Could not resolve internal helper 'domheight'.", call. = FALSE)
                domheight_fun(y$'h', y$'d', y$'n')
            },
                                   error = function(e) NA)), spl)
        dmt <- do.call('rbind', dmhe)
        rownames(dmt) <- NULL
    }

    ## Restore attributes, attach unit metadata, and set the output class.
    attr(dmt, 'nfi.nr') <- nfi_nr

    metric_units <- c(
        d  = "mm",
        h  = "dm",
        ba = "m2 tree-1",
        n  = "ha-1",
        Hd = "dm"
    )

    attr(dmt, "units") <- metric_units[intersect(names(dmt), names(metric_units))]

    if (any(var %in% c("n", "Hd"))) {
        design_meta <- list(
            name = if (!is.null(design$name)) design$name else NA_character_,
            class = class(design),
            min_dbh_cm = if (!is.null(design$min_dbh_cm)) design$min_dbh_cm else NA_real_,
            sample_area_m2 = if (!is.null(design$sample_area_m2)) design$sample_area_m2 else NA_real_,
            expansion_factor = if (!is.null(design$sf)) design$sf else NA_real_,
            metadata = if (!is.null(design$metadata)) design$metadata else list(),
            used_for = "n",
            returned_unit = "ha-1"
        )
        attr(dmt, "design_meta") <- design_meta
    }

## attr(dmt, "units") <- units_map[names(dmt)]

    class(dmt) <- append('nfiMetrics', class(dmt))
    return(dmt)

### \code{data.frame} with the matched identifier and grouping
### columns plus the metrics requested in \code{var}. The output
### inherits from \code{'nfiMetrics'} and \code{'data.frame'} and
### stores \code{attr(x, 'nfi.nr')} when available. Inspect
### \code{attr(x, 'units')} for the returned metric units and
### \code{attr(x, 'design_meta')} for the sampling design summary
### attached when \code{'n'} or \code{'Hd'} is requested.
}, ex = function(){
## Minimal reproducible example with a small object that mimics
## readNFI() output
toy_ifn <- structure(
    data.frame(
        esta = c("plot1", "plot1", "plot2"),
        espe = c("sp1", "sp2", "sp1"),
        Dn = c(120, 185, 260),
        altura = c(7.1, 9.4, 13.2),
        stringsAsFactors = FALSE
    ),
    class = c("readNFI", "data.frame"),
    nfi.nr = 4
)

x <- nfiMetrics(
    toy_ifn,
    var = c("d", "h", "ba", "n"),
    levels = c("esta", "espe")
)

x
attr(x, "units")
})
