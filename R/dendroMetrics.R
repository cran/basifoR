dendroMetrics <- structure(function#Summarize dendrometrics
### This function summarizes dendrometric data from the Spanish
### National Forest Inventory (SNF). It primarily accepts a province
### name or number, a local compressed SNF file, or a URL to a
### compressed SNF file hosted by \code{www.miteco.gob.es}. It can
### also process data frames previously returned by
### \code{\link{readNFI}}, \code{\link{nfiMetrics}}, or
### \code{\link{metrics2Vol}}. Dendrometric variables in the output
### are transformed into stand units, see Details section.
                          ##details<< Dendrometric variables are
                          ## summarized according to the levels of
                          ## argument \code{summ.vr}. Summary outputs
                          ## include the categorical columns defined
                          ## by \code{summ.vr} together with the
                          ## quantitative variables available after
                          ## processing with \code{\link{nfiMetrics}}
                          ## and \code{\link{metrics2Vol}}.
                          ##
                          ## These variables may include tree basal
                          ## area \code{ba} (\code{'m2 ha-1'}), mean
                          ## diameter at breast height \code{d}
                          ## (\code{'cm'}), quadratic mean diameter
                          ## \code{dg} (\code{'cm'}), mean tree
                          ## height \code{h} (\code{'m'}), total stand
                          ## density \code{n_tot} (\code{'ha-1'}), and over-bark
                          ## volume \code{v} (\code{'m3 ha-1'}).
                          ##
                          ## When \code{summ.vr = NULL}, the function
                          ## returns tree-level outputs from
                          ## \code{\link{metrics2Vol}} after applying
                          ## the filter defined in \code{cut.dt}.
                          ##
                          ## When \code{summ.vr} is not \code{NULL},
                          ## the function converts supported
                          ## variables to stand units, splits the data
                          ## by the requested grouping variable, and
                          ## computes summaries by group. The tree-level
                          ## expansion factor \code{n} is used internally for
                          ## weighting and summation; its group total is returned
                          ## as \code{n_tot}. Variables \code{d},
                          ## \code{h}, and \code{Hd}, when present,
                          ## are returned as averages weighted by
                          ## \code{n}.
                          ##
                          ## If both \code{ba} and \code{n} are
                          ## available, the function also derives the
                          ## quadratic mean diameter \code{dg}.
                          ##
                          ## Output subsets are extracted using the
                          ## logical expression supplied in
                          ## \code{cut.dt}, see syntax in
                          ## \code{\link{Logic}}.
                          ##
                          ## The function accepts one input object or
                          ## several inputs. When several inputs are
                          ## supplied, each one is processed
                          ## independently and the results are merged
                          ## into a single output data frame.
                          ## Parallel processing is controlled by
                          ## \code{mc.cores}. Values greater than 1
                          ## process several inputs in parallel.
(
nfi, ##<< \code{character}, \code{data.frame}, or \code{list}. A
         ## province name or province number used to locate SNF data;
         ## a local path or URL to a compressed SNF file
         ## (\code{.zip}), including ZIP files hosted by
         ## \code{www.miteco.gob.es}; a data frame such as that
         ## returned by \code{\link{readNFI}},
         ## \code{\link{nfiMetrics}}, or
    ## \code{\link{metrics2Vol}}; or a list of such objects.
    summ.vr = 'Estadillo', ##<< \code{character} or \code{NULL}. Name
                           ##of a Categorical variables in the SNF
                           ##data used to summarize the outputs. If
                           ##\code{NULL} then output from
                           ##\code{\link{metrics2Vol}} is
                           ##returned. Default \code{'Estadillo'}
                           ##processes sample plots.
    metric_levels = NULL, ##<< \code{character} or \code{NULL}. Grouping
                          ## variables used to compute tree-level
                          ## metrics such as \code{Hd} before final
                          ## summarization. When \code{NULL}, metric
                          ## computation follows \code{summ.vr}.
    cut.dt = 'd == d', ##<< \code{character}. Logical condition used
                       ##to subset the output. For grouped summaries, use
                       ##\code{n_tot} to filter total stand density. Default
                       ##\code{'d == d'} avoids subsetting.
    report = FALSE, ##<< \code{logical}. Print a report of the output
                    ##in the current working directory.
   mc.cores = getOption("mc.cores", 1L), ##<< \code{integer}. Number
                                         ## of cores used when several
                                         ## inputs are processed.
    domheight_method = "Hd_strict", ##<< Dominant-height method used when
                                         ## \code{Hd} is recomputed during
                                         ## summary preparation. The default
                                         ## preserves the previous strict
                                         ## dendrometric behavior.
    domheight_registry = dominant_height_method_registry(), ##<< Named
                                         ## dominant-height registry created
                                         ## with \code{dominant_height_method_registry()}.
    ... ##<< Additional arguments passed to \code{\link{readNFI}},
        ##\code{\link{nfiMetrics}}, or \code{\link{metrics2Vol}},
        ##including \code{nfi.nr} when required.

) {
    call0 <- match.call(expand.dots = TRUE)

    finalize_output <- function(out, call) {
        if (is.null(out))
            return(NULL)

        attr(out, "call") <- call
        class(out) <- unique(c("dendroMetrics", class(out)))
        out
    }

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


dendro_one <- function(nfi, summ.vr, metric_levels, cut.dt, report,
                        domheight_method = "Hd_strict",
                        domheight_registry = dominant_height_method_registry(),
                        ...) {
    nfi. <- nfi
    if (is.null(nfi.))
        return(nfi)

    is_precomputed_metrics <- function(x) {
        if (!is.data.frame(x))
            return(FALSE)

        nm <- tolower(names(x))
        has_metric <- any(nm %in% c(
            "d", "h", "hd", "dd", "ba", "n",
            "v", "vcc", "vsc", "iavc", "vle"
        ))

        if (!has_metric)
            return(FALSE)

        un <- attr(x, "units")
        !is.null(un)
    }

    if (!inherits(nfi., "metrics2vol")) {
        if (is_precomputed_metrics(nfi.)) {
            nfi <- nfi.
        } else {
            nfi <- metrics2Vol(
                nfi,
                domheight_method = domheight_method,
                domheight_registry = domheight_registry,
                ...
            )
        }
    }

    design_meta <- attr(nfi, "design_meta")
    volume_meta <- attr(nfi, "volume_meta")
    dominant_height_meta0 <- attr(nfi, "dominant_height_meta")
    dominant_diameter_meta0 <- attr(nfi, "dominant_diameter_meta")
    nfi_nr_attr <- attr(nfi, "nfi.nr")

    names(nfi) <- tolower(names(nfi))

    frm. <- attr(nfi, "units")

    if (!is.null(frm.)) {
        names(frm.) <- tolower(names(frm.))
        keep_units <- !is.na(names(frm.)) & nzchar(names(frm.))
        frm. <- frm.[keep_units]
        frm. <- frm.[!duplicated(names(frm.))]
        frm. <- frm.[names(frm.) %in% names(nfi)]
        attr(nfi, "units") <- frm.
    }

    if (is.null(summ.vr)) {
        nfi <- subset(nfi, eval(parse(text = cut.dt)))

        if (!is.null(frm.))
            attr(nfi, "units") <- frm.[intersect(names(nfi), names(frm.))]
        if (!is.null(design_meta))
            attr(nfi, "design_meta") <- design_meta
        if (!is.null(volume_meta))
            attr(nfi, "volume_meta") <- volume_meta
        if (!is.null(dominant_height_meta0))
            attr(nfi, "dominant_height_meta") <- dominant_height_meta0
        if (!is.null(dominant_diameter_meta0))
            attr(nfi, "dominant_diameter_meta") <- dominant_diameter_meta0
        if (!is.null(nfi_nr_attr))
            attr(nfi, "nfi.nr") <- nfi_nr_attr

        if (report)
            write.csv(nfi, file = "report.csv", row.names = FALSE)

        return(nfi)
    }

    flev_fun <- ns_fun("flev")
    if (is.null(flev_fun))
        stop("Could not resolve internal helper 'flev'.", call. = FALSE)

    summ_cols <- flev_fun(nfi, summ.vr)
    if (!length(summ_cols))
        stop("None of 'summ.vr' were found in 'nfi'.", call. = FALSE)

    metric_cols <- if (is.null(metric_levels)) {
        summ_cols
    } else {
        flev_fun(nfi, metric_levels)
    }
    if (!length(metric_cols))
        stop("None of 'metric_levels' were found in 'nfi'.", call. = FALSE)

    weighted_mean_vars <- intersect(c("d", "h", "hd", "dd"), names(nfi))
    sum_vars <- intersect(c("ba", "n", "v", "vcc", "vsc", "iavc", "vle"),
                          names(nfi))

    if (length(unique(c(weighted_mean_vars, sum_vars))) && !"n" %in% names(nfi)) {
        stop(
            "Summarization requires column 'n' (trees/ha). ",
            "Provide raw data or processed input that includes 'n'.",
            call. = FALSE
        )
    }

    recompute_hd <- "hd" %in% names(nfi) &&
        all(c("d", "h", "n") %in% names(nfi))
    recompute_dd <- "dd" %in% names(nfi) &&
        all(c("d", "n") %in% names(nfi))

    dominant_height_meta <- NULL
    dominant_diameter_meta <- NULL

    if (recompute_hd || recompute_dd) {
        method_info <- tryCatch(
            resolve_dominant_height_method(
                method = domheight_method,
                registry = domheight_registry
            ),
            error = function(e) e
        )

        if (inherits(method_info, "error"))
            stop(conditionMessage(method_info), call. = FALSE)

        if (recompute_hd) {
            domheight_fun <- method_info$fun
            ok_hd <- !is.na(nfi$d) & !is.na(nfi$h) & !is.na(nfi$n) &
                is.finite(nfi$d) & is.finite(nfi$h) & is.finite(nfi$n) &
                nfi$n > 0
            hd_new <- rep(NA_real_, nrow(nfi))

            if (any(ok_hd)) {
                grp_hd <- interaction(
                    nfi[ok_hd, metric_cols, drop = FALSE],
                    drop = TRUE,
                    lex.order = TRUE
                )
                idx_hd <- split(which(ok_hd), grp_hd, drop = TRUE)

                for (idx in idx_hd) {
                    hd_new[idx] <- tryCatch(
                        domheight_fun(h = nfi$h[idx], d = nfi$d[idx], n = nfi$n[idx]),
                        error = function(e) NA_real_
                    )
                }
            }
            nfi$hd <- hd_new
            dominant_height_meta <- method_info$meta
        }

        if (recompute_dd) {
            domdiameter_fun <- method_info$diameter_fun
            if (!is.function(domdiameter_fun)) {
                stop(
                    "Dominant method '", method_info$method,
                    "' does not define a paired dominant-diameter function.",
                    call. = FALSE
                )
            }

            ok_dd <- is.finite(nfi$d) & is.finite(nfi$n) &
                nfi$d > 0 & nfi$n > 0
            dd_new <- rep(NA_real_, nrow(nfi))

            if (any(ok_dd)) {
                grp_dd <- interaction(
                    nfi[ok_dd, metric_cols, drop = FALSE],
                    drop = TRUE,
                    lex.order = TRUE
                )
                idx_dd <- split(which(ok_dd), grp_dd, drop = TRUE)

                for (idx in idx_dd) {
                    dd_new[idx] <- tryCatch(
                        domdiameter_fun(d = nfi$d[idx], n = nfi$n[idx]),
                        error = function(e) NA_real_
                    )
                }
            }
            nfi$dd <- dd_new
            dominant_diameter_meta <- method_info$meta
            dominant_diameter_meta$output <- "Dd"
            dominant_diameter_meta$unit <- method_info$definition$diameter_unit
            dominant_diameter_meta$equation <- method_info$definition$diameter_equation
            dominant_diameter_meta$fun_name <- method_info$definition$diameter_fun_name
        }
    }

    mean_target_units <- c(d = "cm", h = "m", hd = "m", dd = "cm")
    if (length(weighted_mean_vars)) {
        conv_units_fun <- ns_fun("conv_units")
        if (is.null(conv_units_fun))
            stop("Could not resolve internal helper 'conv_units'.", call. = FALSE)

        nfi <- conv_units_fun(
            nfi,
            var = weighted_mean_vars,
            un = unname(mean_target_units[weighted_mean_vars])
        )
    }

    msp <- split(nfi, nfi[summ_cols], drop = TRUE)
    msp <- Filter("nrow", msp)

    ## The completeness columns are provenance output, like *_source and
    ## *_status: returned only with track_provenance = TRUE, or when the
    ## input already carries provenance columns from metrics2Vol().
    with_na_share <- isTRUE(list(...)[["track_provenance"]]) ||
        any(grepl("_(source|status)$", names(nfi)))

    fsum <- function(dt) {
        ## Completeness of each volume total: the share of the group's
        ## expansion factor (trees ha-1) whose tree volume is missing. The
        ## volume total sums only the trees with a value.
        vol_na <- if (with_na_share) {
            intersect(c("v", "vcc", "vsc", "iavc", "vle"), sum_vars)
        } else {
            character(0)
        }
        na_share <- vapply(vol_na, function(vv) {
            ok_n <- !is.na(dt[["n"]])
            tot <- sum(dt[["n"]][ok_n])
            if (!is.finite(tot) || tot <= 0)
                return(NA_real_)
            sum(dt[["n"]][ok_n & is.na(dt[[vv]])]) / tot
        }, numeric(1))
        names(na_share) <- if (length(vol_na)) paste0(vol_na, "_na_share") else character(0)

        scale_vars <- unique(c(setdiff(weighted_mean_vars, "n"),
                               setdiff(sum_vars, "n")))

        if (length(scale_vars))
            dt[, scale_vars] <- dt[, scale_vars, drop = FALSE] * dt[, "n"]

        summ_names <- unique(c(intersect("n", names(dt)),
                               weighted_mean_vars,
                               sum_vars))

        ## if (length(summ_names)) {
        ##     summ <- colSums(dt[, summ_names, drop = FALSE], na.rm = TRUE)
        ## } else {
        ##     summ <- numeric(0)
        ## }
        if (length(summ_names)) {
            sum_or_na <- function(x) {
                if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
            }
            summ <- vapply(dt[, summ_names, drop = FALSE], sum_or_na, numeric(1))
        } else {
            summ <- numeric(0)
        }
        
        keep_avg <- intersect(weighted_mean_vars, names(summ))
        if (length(keep_avg) && "n" %in% names(summ) &&
            is.finite(summ["n"]) && summ["n"] > 0)
            summ[keep_avg] <- summ[keep_avg] / summ["n"]

        if (all(c("ba", "n") %in% names(summ)) &&
            is.finite(summ["n"]) && summ["n"] > 0)
            summ["dg"] <- sqrt((4E4 * summ["ba"] / summ["n"]) / pi)

        ## At tree level, n is the per-record expansion factor. After
        ## aggregation, expose its group total under the unambiguous name n_tot.
        if ("n" %in% names(summ))
            names(summ)[names(summ) == "n"] <- "n_tot"

        summ <- c(summ, na_share)
        summ <- summ[order(names(summ))]
        summ <- sapply(summ, function(x) round(x, 3))
        summ <- t(as.matrix(summ))

        non_metric_cols <- names(dt)[
            !names(dt) %in% unique(c(weighted_mean_vars, sum_vars))
        ]

        is_group_invariant <- function(x) {
            x <- x[!is.na(x)]
            if (!length(x))
                return(TRUE)
            length(unique(x)) == 1L
        }

        invariant_cols <- non_metric_cols[
            vapply(dt[non_metric_cols], is_group_invariant, logical(1))
        ]

        id_cols <- intersect(invariant_cols, c("nfi.nr", "pr", "provincia"))
        keep_cols <- unique(c(id_cols, intersect(non_metric_cols, summ_cols)))
        fcs <- dt[1, keep_cols, drop = FALSE]

        cbind(fcs, summ)
    }

    bind_rows_fill_local <- function(a, b) {
        if (is.null(a))
            return(data.frame(b, check.names = FALSE))
        if (is.null(b))
            return(data.frame(a, check.names = FALSE))

        a <- data.frame(a, check.names = FALSE)
        b <- data.frame(b, check.names = FALSE)

        cols <- union(names(a), names(b))

        missing_a <- setdiff(cols, names(a))
        missing_b <- setdiff(cols, names(b))

        if (length(missing_a))
            a[missing_a] <- NA
        if (length(missing_b))
            b[missing_b] <- NA

        rbind(a[, cols, drop = FALSE], b[, cols, drop = FALSE])
    }

    resm <- lapply(msp, fsum)
    resm <- Reduce(bind_rows_fill_local, resm, init = NULL)
    resm <- data.frame(resm, check.names = FALSE)

    resm <- subset(resm, eval(parse(text = cut.dt)))
    rownames(resm) <- NULL

    first <- unique(c("nfi.nr", "pr", "provincia", summ_cols))
    i <- match(tolower(first), tolower(names(resm)))
    i <- i[!is.na(i)]
    resm <- resm[, c(i, setdiff(seq_along(resm), i)), drop = FALSE]

    if (report)
        write.csv(resm, file = "report.csv", row.names = FALSE)

## metrics2Vol() already returns volume outputs in m3
## so dendroMetrics() should only aggregate and relabel them.
vol_vars <- intersect(c("v", "vcc", "vsc", "iavc", "vle"), names(resm))
    
    units_out <- c(
        d = "cm",
        h = "m",
        hd = "m",
        dd = "cm",
        dg = "cm",
        ba = "m2 ha-1",
        n_tot = "ha-1",
        v = "m3 ha-1",
        vcc = "m3 ha-1",
        vsc = "m3 ha-1",
        iavc = "m3 ha-1",
        vle = "m3 ha-1"
    )
    na_share_cols <- grep("_na_share$", names(resm), value = TRUE)
    units_out[na_share_cols] <- "1"
    attr(resm, "units") <- units_out[intersect(names(resm), names(units_out))]
    if (!is.null(design_meta))
        attr(resm, "design_meta") <- design_meta
    if (!is.null(volume_meta))
        attr(resm, "volume_meta") <- volume_meta
    if (exists("dominant_height_meta", inherits = FALSE) &&
        !is.null(dominant_height_meta)) {
        attr(resm, "dominant_height_meta") <- dominant_height_meta
    } else if (!is.null(dominant_height_meta0)) {
        attr(resm, "dominant_height_meta") <- dominant_height_meta0
    }
    if (exists("dominant_diameter_meta", inherits = FALSE) &&
        !is.null(dominant_diameter_meta)) {
        attr(resm, "dominant_diameter_meta") <- dominant_diameter_meta
    } else if (!is.null(dominant_diameter_meta0)) {
        attr(resm, "dominant_diameter_meta") <- dominant_diameter_meta0
    }
    if (!is.null(nfi_nr_attr))
        attr(resm, "nfi.nr") <- nfi_nr_attr

    resm
}

    dots0 <- list(...)

    nfi.nr <- dots0[["nfi.nr"]]

    n_inputs <- max(
        if (is.data.frame(nfi)) 1L else length(nfi),
        length(nfi.nr),
        1L
    )

    recycle_arg <- function(x, n, arg) {
        if (is.null(x))
            return(rep(list(NULL), n))
        if (is.data.frame(x))
            return(rep(list(x), n))
        if (n == 1L)
            return(list(x))
        if (is.list(x)) {
            if (length(x) == 1L)
                return(rep(x, n))
            if (length(x) != n)
                stop("'", arg, "' must have length 1 or length ", n, ".")
            return(x)
        }
        if (length(x) == 1L)
            return(rep(as.list(x), n))
        if (length(x) != n)
            stop("'", arg, "' must have length 1 or length ", n, ".")
        as.list(x)
    }

    nfi_list <- recycle_arg(nfi, n_inputs, "nfi")
    nfi.nr_list <- recycle_arg(nfi.nr, n_inputs, "nfi.nr")

    dot_names <- setdiff(names(dots0), "nfi.nr")
    dot_lists <- lapply(dot_names, function(arg) {
        recycle_arg(dots0[[arg]], n_inputs, arg)
    })
    names(dot_lists) <- dot_names

    describe_job_input <- function(x) {
        if (is.null(x))
            return("NULL")

        if (is.atomic(x) && !is.list(x))
            return(paste(as.character(x), collapse = ", "))

        if (is.data.frame(x)) {
            cls <- paste(class(x), collapse = "/")
            nr <- tryCatch(nrow(x), error = function(e) NA_integer_)
            nc <- tryCatch(ncol(x), error = function(e) NA_integer_)
            return(paste0("<", cls, ":", nr, "x", nc, ">"))
        }

        cls <- paste(class(x), collapse = "/")
        len <- tryCatch(length(x), error = function(e) NA_integer_)
        paste0("<", cls, ":length=", len, ">")
    }

    jobs <- lapply(seq_len(n_inputs), function(i) {
        dots_i <- lapply(dot_lists, function(x) x[[i]])
        names(dots_i) <- dot_names
        if (!is.null(nfi.nr_list[[i]]))
            dots_i[["nfi.nr"]] <- nfi.nr_list[[i]]

        list(
            nfi = nfi_list[[i]],
            input_label = paste0(
                "nfi=", describe_job_input(nfi_list[[i]]),
                ", nfi.nr=", describe_job_input(nfi.nr_list[[i]])
            ),
            dots = dots_i
        )
    })


run_job <- function(job) {
    tryCatch(
        do.call(
            dendro_one,
            c(
                list(
                    nfi = job$nfi,
                    summ.vr = summ.vr,
                    metric_levels = metric_levels,
                    cut.dt = cut.dt,
                    report = FALSE,
                    domheight_method = domheight_method,
                    domheight_registry = domheight_registry
                ),
                job$dots
            )
        ),
        error = function(e) {
            structure(
                list(
                    message = conditionMessage(e),
                    input_label = job$input_label,
                    nfi = job$nfi,
                    nfi.nr = job$dots[["nfi.nr"]]
                ),
                class = "dendroMetrics_error"
            )
        }
    )
}

    if (length(jobs) == 1L) {
        out <- do.call(
            dendro_one,
            c(
                list(
                    nfi = jobs[[1]]$nfi,
                    summ.vr = summ.vr,
                    metric_levels = metric_levels,
                    cut.dt = cut.dt,
                    report = report,
                    domheight_method = domheight_method,
                    domheight_registry = domheight_registry
                ),
                jobs[[1]]$dots
            )
        )
        return(finalize_output(out, call0))
    }

    mc.cores <- as.integer(mc.cores)
    if (is.na(mc.cores) || mc.cores < 1L)
        mc.cores <- 1L

    is_materialized_input <- function(x) {
        inherits(x, c("readNFI", "nfiMetrics", "metrics2vol"))
    }

    readnfi_args <- function(dots) {
        if (!length(dots))
            return(list())

        nms <- names(dots)
        keep_named <- !is.na(nms) & nzchar(nms)
        dots <- dots[keep_named]
        nms <- names(dots)

        rf <- tryCatch(formals(readNFI), error = function(e) NULL)
        if (is.null(rf)) {
            keep <- nms %in% "nfi.nr"
            return(dots[keep])
        }

        allowed <- setdiff(names(rf), "nfi")
        dots[nms %in% allowed]
    }

    materialize_job_input <- function(job) {
        if (is.null(job$nfi) || is_materialized_input(job$nfi))
            return(job)

        read_args <- readnfi_args(job$dots)
        job$nfi <- do.call(readNFI, c(list(nfi = job$nfi), read_args))
        job
    }

    use_parallel <- length(jobs) > 1L && mc.cores > 1L

    if (use_parallel) {
        jobs <- lapply(jobs, function(job) {
            tryCatch(
                materialize_job_input(job),
                error = function(e) {
                    structure(
                        list(
                            message = conditionMessage(e),
                            input_label = job$input_label,
                            nfi = job$nfi,
                            nfi.nr = job$dots[["nfi.nr"]]
                        ),
                        class = "dendroMetrics_error"
                    )
                }
            )
        })

        preload_errs <- vapply(jobs, inherits, logical(1), what = "dendroMetrics_error")
        if (any(preload_errs)) {
            msg <- vapply(jobs[preload_errs], function(x) {
                label <- if (is.null(x$input_label)) "<input>" else x$input_label
                paste0(
                    "dendroMetrics failed while preloading ",
                    label,
                    ": ",
                    x$message
                )
            }, character(1))

            stop(paste(msg, collapse = "\n"), call. = FALSE)
        }
    }

    if (!use_parallel) {

        res_list <- lapply(jobs, run_job)

    } else if (.Platform$OS.type == "windows") {

        cl <- parallel::makeCluster(mc.cores)
        on.exit(parallel::stopCluster(cl), add = TRUE)

        parallel::clusterEvalQ(cl, {
            suppressPackageStartupMessages(library(basifoR))
            NULL
        })

        parallel::clusterExport(
            cl = cl,
            varlist = c("jobs", "run_job", "dendro_one", "summ.vr",
                        "metric_levels", "cut.dt", "domheight_method",
                        "domheight_registry"),
            envir = environment()
        )

        res_list <- parallel::parLapply(cl = cl, X = jobs, fun = run_job)

    } else {

        res_list <- parallel::mclapply(
            X = jobs,
            FUN = run_job,
            mc.cores = mc.cores
        )
    }

errs <- vapply(res_list, inherits, logical(1), what = "dendroMetrics_error")

if (any(errs)) {
    msg <- vapply(res_list[errs], function(x) {
        label <- if (is.null(x$input_label)) "<input>" else x$input_label
        paste0(
            "dendroMetrics failed for ",
            label,
            ": ",
            x$message
        )
    }, character(1))

    stop(paste(msg, collapse = "\n"), call. = FALSE)
}

    res_list <- Filter(Negate(is.null), res_list)

    if (!length(res_list))
        return(NULL)

    bind_rows_fill <- function(a, b) {
        if (is.null(a))
            return(b)
        if (is.null(b))
            return(a)

        a <- data.frame(a, check.names = FALSE)
        b <- data.frame(b, check.names = FALSE)

        cols <- union(names(a), names(b))

        missing_a <- setdiff(cols, names(a))
        missing_b <- setdiff(cols, names(b))

        if (length(missing_a))
            a[missing_a] <- NA
        if (length(missing_b))
            b[missing_b] <- NA

        rbind(a[, cols, drop = FALSE], b[, cols, drop = FALSE])
    }

    collect_units <- function(x) {
        units_list <- lapply(x, function(y) attr(y, "units"))
        units_list <- Filter(Negate(is.null), units_list)

        if (!length(units_list))
            return(NULL)

        out_units <- do.call(c, units_list)
        out_units[!duplicated(names(out_units))]
    }

    collect_attr <- function(x, attr_name) {
        vals <- lapply(x, function(y) attr(y, attr_name))
        vals <- Filter(Negate(is.null), vals)

        if (!length(vals))
            return(NULL)
        if (length(vals) == 1L)
            return(vals[[1L]])

        same <- vapply(vals[-1L], function(z) identical(z, vals[[1L]]), logical(1))
        if (all(same))
            return(vals[[1L]])

        vals
    }

    out <- Reduce(bind_rows_fill, res_list)
    out <- data.frame(out, check.names = FALSE)
    rownames(out) <- NULL

    out_units <- collect_units(res_list)
    if (!is.null(out_units))
        attr(out, "units") <- out_units[names(out_units) %in% names(out)]

    out_domheight_meta <- collect_attr(res_list, "dominant_height_meta")
    if (!is.null(out_domheight_meta))
        attr(out, "dominant_height_meta") <- out_domheight_meta

    out_design_meta <- collect_attr(res_list, "design_meta")
    if (!is.null(out_design_meta))
        attr(out, "design_meta") <- out_design_meta

    out_volume_meta <- collect_attr(res_list, "volume_meta")
    if (!is.null(out_volume_meta))
        attr(out, "volume_meta") <- out_volume_meta

    out_nfi_nr <- collect_attr(res_list, "nfi.nr")
    if (!is.null(out_nfi_nr))
        attr(out, "nfi.nr") <- out_nfi_nr

    if (report)
        write.csv(out, file = "report.csv", row.names = FALSE)

    finalize_output(out, call0)
### \code{data.frame}. With \code{summ.vr = NULL}, tree-level output keeps
### the expansion-factor column \code{n}. With grouped summaries, total stand
### density is returned as \code{n_tot}. See Details.
}, ex = function(){

## Minimal precomputed metrics object with units
toy_metrics <- structure(
    data.frame(
        Estadillo = c("plot1", "plot1", "plot2"),
        Especie   = c("sp1", "sp2", "sp1"),
        d         = c(120, 185, 260),          # mm
        h         = c(7.1, 9.4, 13.2),         # m
        ba        = c(0.0113, 0.0269, 0.0531), # m2 tree-1
        n         = c(127.32, 31.83, 14.15),
        stringsAsFactors = FALSE
    ),
    class = c("nfiMetrics", "data.frame"),
    units = c(
        d  = "mm",
        h  = "m",
        ba = "m2 tree-1",
        n  = "ha-1"
    ),
    nfi.nr = 4
)

## Summarize by plot
dendromet_toy <- dendroMetrics(toy_metrics, summ.vr = "Estadillo")

## Display output structure
str(dendromet_toy)

## The tree-level expansion factor n is summed and returned as n_tot
names(dendromet_toy)
attr(dendromet_toy, "units")

## Return tree-level processed data
tree_toy <- dendroMetrics(toy_metrics, summ.vr = NULL, cut.dt = "h > 8")
head(tree_toy)

## Alternatively, download data from 'www.miteco.gob.es'
## Specify province name/number and nfi number to compute dendrometrics.
    
})


update.dendroMetrics <- function #Update a dendroMetrics result
##title<< Update a stored dendroMetrics call
##description<< Re-run \code{\link{dendroMetrics}} from the call stored
## in a previous \code{"dendroMetrics"} result while replacing one or
## more named arguments in \code{...}. This method supports reproducible
## re-evaluation of the original workflow and can either return the
## updated result or the reconstructed call.
### This S3 method requires an object created by the updatable
### \code{\link{dendroMetrics}} definition, which stores the original
### call in \code{attr(x, "call")}. All arguments supplied in
### \code{...} must be named.
(
    object, ##<< A \code{"dendroMetrics"} object created by the
            ## updatable \code{\link{dendroMetrics}} definition.
    ..., ##<< Named arguments used to replace entries in the stored
         ## call before evaluation.
    evaluate = TRUE ##<< \code{logical}. If \code{TRUE}, evaluate the
                    ## updated call and return the resulting object.
                    ## If \code{FALSE}, return the reconstructed call
                    ## without evaluation.
) {
    if (!inherits(object, "dendroMetrics"))
        stop(
            "'object' must inherit from 'dendroMetrics'. ",
            "Call dendroMetrics(...) first and then use update(result, ...).",
            call. = FALSE
        )

    call0 <- attr(object, "call")

    if (is.null(call0))
        stop(
            "No stored call found in 'object'. ",
            "Recreate the result with the updatable dendroMetrics() definition ",
            "and then call update(result, ...).",
            call. = FALSE
        )

    extras <- as.list(substitute(list(...)))[-1L]

    if (length(extras)) {
        extra_names <- names(extras)
        has_name <- !is.na(extra_names) & nzchar(extra_names)

        if (any(!has_name))
            stop("All arguments in '...' must be named.", call. = FALSE)

        call_list <- as.list(call0)
        for (i in seq_along(extras))
            call_list[[extra_names[[i]]]] <- extras[[i]]
        call0 <- as.call(call_list)
    }

    if (isTRUE(evaluate))
        eval(call0, envir = parent.frame())
    else
        call0
    ##value<< A new \code{"dendroMetrics"} object when
    ## \code{evaluate = TRUE}; otherwise the updated call.
}


update.list <- function
##title<< Guard raw list inputs in update
##description<< Catch attempts to call \code{update}
## on raw input lists intended for \code{\link{dendroMetrics}} and
## return a workflow-specific error message. When the input does not
## look like a \code{dendroMetrics} workflow, this method falls back to
## \code{update.default}.
### This helper makes \code{update(list("toledo", "madrid"), ...)}
### fail with a message that points users to
### \code{dendroMetrics(...)} followed by \code{update(result, ...)}.
(
    object, ##<< A \code{list} passed to \code{update()}.
    ..., ##<< Additional arguments passed to \code{update()}.
    evaluate = TRUE ##<< \code{logical}. Passed through to
                    ## \code{update.default} when no
                    ## \code{dendroMetrics}-specific guard is triggered.
) {
    extras <- as.list(substitute(list(...)))[-1L]
    extra_names <- names(extras)
    likely_dendro_args <- c(
        "nfi", "nfi.nr", "summ.vr", "cut.dt", "report", "var",
        "parametro", "keep.var", "keep.legacy", "method_registry",
        "track_provenance", "cub.met"
    )

    looks_like_dendro_input <-
        length(object) > 0L &&
        all(vapply(object, function(x) is.character(x) && length(x) == 1L, logical(1)))

    asks_for_dendro_update <-
        length(extras) > 0L &&
        any(!is.na(extra_names) & nzchar(extra_names) & extra_names %in% likely_dendro_args)

    if (looks_like_dendro_input || asks_for_dendro_update) {
        stop(
            "update() cannot start from raw inputs such as list('toledo', 'madrid'). ",
            "First create a result with dendroMetrics(...), then call update(result, ...).\n",
            "Example:\n",
            "  x <- dendroMetrics(list('toledo', 'madrid'), nfi.nr = c(4, 4))\n",
            "  y <- update(x, cut.dt = 'h > 12')",
            call. = FALSE
        )
    }

    getS3method("update", "default")(object, ..., evaluate = evaluate)
    ##value<< Either an error with a \code{dendroMetrics}-specific
    ## message or the result of \code{update.default()}.
}
