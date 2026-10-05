metrics2Vol <- structure(function(
                                  ### Computes one or more tree-level
### volume variables from Spanish National Forest Inventory data. The
### function standardizes the input, selects the appropriate volume
### model for each tree, applies equation-based methods defined in a
### method registry, converts results to cubic metres, and optionally
### keeps legacy outputs (from previous versions of the package) and
### provenance information.
    nfi,
    ### Input accepted by \code{nfiMetrics()}, or a precomputed \code{"nfiMetrics"}
    ### object with tree-level metrics.
    cub.met = "freq",
    ### Cubication selector used when several coefficient rows match.
    parametro = c("VCC"),
    ### One or more volume outputs to compute.
    keep.var = TRUE,
    ### Keep auxiliary coefficient columns when available.
    keep.legacy = FALSE,
    ### Also return the legacy volume estimate for backward compatibility.
    legacy_fallback = TRUE,
    ### \code{logical}. When the official equation cannot be used for a tree
    ### (no coefficients for its species, parameter, province or cycle, or the
    ### equation cannot be evaluated), use the legacy equation for the same
    ### province and species if it has coefficients, labelled
    ### \code{"fallback_legacy"} in the provenance columns. Set to
    ### \code{FALSE} to use official equations only and return \code{NA}
    ### instead. Coefficients of another species are never used.
    method_registry = snfi_volume_method_registry(),
    ### Registry that maps each requested output to descriptive metadata,
    ### its equation function, output column name, units, and fallback rule.
    track_provenance = FALSE,
    ### Add per-row provenance columns and audit metadata, including the
    ### method label, equation description, reference, and output description.
    ...
    ### Passed to \code{nfiMetrics()} when \code{nfi} is not already an
    ### \code{"nfiMetrics"} object.
) {
    ##title<< Compute tree-level volume variables from NFI metrics
    ##details<<
    ##details<< Computes requested volume outputs from standardized NFI
    ##details<< tree metrics using registry-based methods and optional
    ##details<< fallback to legacy estimates. With
    ##details<< \code{track_provenance = TRUE}, the \code{volume_meta}
    ##details<< attribute also records each method's label, equation
    ##details<< description, reference, units, scale, and output description.
    ##value<<
    ##value<< A data.frame with the requested volume outputs added. When
    ##value<< \code{track_provenance = TRUE}, attribute \code{volume_meta}
    ##value<< contains computational and descriptive method metadata.
    ##seealso<< nfiMetrics, snfi_volume_method_registry,
    ##seealso<< default_snfi_volume_equations
    # Registry-based dispatcher for volume equations.

    # Return early for NULL input.
    nfi. <- nfi
    if (is.null(nfi.))
        return(nfi)

    # Build dendrometric metrics when the input is not already
    # standardized.
    if (!inherits(nfi., "nfiMetrics"))
        nfi <- nfiMetrics(nfi, ...)

    # Resolve the inventory edition from the object attribute first and
    # from a column only as a fallback.
    nfi_nr <- attr(nfi, "nfi.nr")

    if (is.null(nfi_nr) || length(nfi_nr) != 1L || is.na(nfi_nr)) {
        nm0 <- names(nfi)
        nm  <- tolower(nm0)
        i_nfi <- match("nfi.nr", nm)

        if (!is.na(i_nfi)) {
            vals <- unique(stats::na.omit(nfi[[nm0[i_nfi]]]))
            if (length(vals) == 1L)
                nfi_nr <- vals[1L]
        }
    }

    if (is.null(nfi_nr) || length(nfi_nr) != 1L || is.na(nfi_nr))
        stop("Could not determine 'nfi.nr' from attribute or column.",
             call. = FALSE)

    nfi_orig <- nfi

    nm0 <- names(nfi)
    nm <- tolower(nm0)

    # Match the first available column name among accepted aliases.
    pick_col <- function(candidates, required = TRUE) {
        ii <- match(tolower(candidates), nm)
        ii <- ii[!is.na(ii)]
        if (!length(ii)) {
            if (required)
                stop("Missing required column: ",
                     paste(candidates, collapse = " / "),
                     call. = FALSE)
            return(NULL)
        }
        nm0[ii[1L]]
    }

    fc_match <- function(dt, cl.) {
        nt <- paste(cl., collapse = "|")
        ii <- grep(nt, names(dt), ignore.case = TRUE)
        sort(names(dt)[ii], decreasing = TRUE)
    }

    ## Identify the core columns needed by the volume equations.
    col_spec <- pick_col(c("Especie", "especie", "codigo_especie", "spec"))
    col_pr   <- pick_col(c("pr", "codigo_provincia"))
    col_d    <- pick_col(c("d"))
    col_h    <- pick_col(c("h"), required = FALSE)
    col_dnm  <- pick_col(c("D.n.m.", "dnm", "d_nm"), required = FALSE)

    ## Validate and standardize measurement-unit metadata before
    ## dispatching any equation.
    get_units_map <- function(x) {
        un <- attr(x, "units")

        if (is.null(un))
            return(setNames(character(0), character(0)))

        if (is.null(names(un)) || anyNA(names(un)) || any(names(un) == ""))
            stop("'attr(nfi, \"units\")' must be a named vector whose names match variable names and whose values are unit strings.",
                 call. = FALSE)

        un[!duplicated(names(un))]
    }

    require_units_for <- function(x, cols, where = "nfi") {
        cols <- cols[!is.null(cols)]
        cols <- cols[!is.na(cols)]
        cols <- cols[nzchar(cols)]

        if (!length(cols))
            return(invisible(NULL))

        un <- get_units_map(x)
        miss <- setdiff(cols, names(un))

        if (length(miss))
            stop("Missing unit metadata in attr(", where, ', "units") for: ',
                 paste(miss, collapse = ", "),
                 call. = FALSE)

        invisible(un)
    }

    if (inherits(nfi, "nfiMetrics"))
        require_units_for(nfi, c(col_d, col_h, col_dnm))

    if (!is.null(col_d))
        nfi <- conv_units(nfi, var = col_d, un = "mm")
    if (!is.null(col_h))
        nfi <- conv_units(nfi, var = col_h, un = "m")
    if (!is.null(col_dnm))
        nfi <- conv_units(nfi, var = col_dnm, un = "mm")

    ## Validate requested outputs against the method registry.
    parametro <- unique(toupper(parametro))
    valid_param <- unique(toupper(names(method_registry)))
    bad_param <- setdiff(parametro, valid_param)
    if (length(bad_param))
        stop("Unknown parametro value(s): ",
             paste(bad_param, collapse = ", "),
             call. = FALSE)

    ## Preallocate all requested output columns.
    out <- nfi_orig
    keep_param <- unique(c(if (keep.legacy) "V", parametro))
    requested_outputs <- unique(vapply(
        keep_param,
        function(x) method_registry[[x]]$output %||% tolower(x),
        character(1)
    ))
    for (nm_out in requested_outputs)
        out[[nm_out]] <- NA_real_

    if (track_provenance) {
        for (nm_out in requested_outputs) {
            out[[paste0(nm_out, "_source")]] <- NA_character_
            out[[paste0(nm_out, "_status")]] <- NA_character_
            out[[paste0(nm_out, "_raw_unit")]] <- NA_character_
            out[[paste0(nm_out, "_scale")]] <- NA_real_
            out[[paste0(nm_out, "_model")]] <- NA_character_
        }
    }

    warn_msg <- character(0)

    ## Compute the legacy volume once so modern methods can fall back to
    ## it when needed.
    compute_legacy_v <- function(nfi_input, cub.met = "freq", nfi.nr = NULL) {
        if (!exists("metrics2Vol_legacy", mode = "function", inherits = TRUE))
            return(rep(NA_real_, nrow(nfi_input)))

        leg <- nfi_input
        leg$.rowid_legacy <- seq_len(nrow(leg))
        if (!is.null(nfi.nr))
            attr(leg, "nfi.nr") <- nfi.nr

        ## The legacy helper evaluates the parEqVcc equations with d in mm and
        ## h in m. Make the unit metadata explicit before entering the
        ## compatibility path.
        leg_units <- attr(leg, "units")
        if (is.null(leg_units))
            leg_units <- c(d = "mm", h = "m")
        if (is.null(names(leg_units)))
            stop("Legacy metrics2Vol requires named unit metadata.",
                 call. = FALSE)
        if ("d" %in% names(leg) && !"d" %in% names(leg_units))
            leg_units["d"] <- "mm"
        if ("h" %in% names(leg) && !"h" %in% names(leg_units))
            leg_units["h"] <- "m"
        attr(leg, "units") <- leg_units

        old <- tryCatch(
            metrics2Vol_legacy(leg, cub.met = cub.met, keep.var = FALSE),
            error = function(e) {
                stop("Legacy metrics2Vol failed: ",
                     conditionMessage(e),
                     call. = FALSE)
            }
        )

        if (is.null(old) || !"v" %in% names(old))
            return(rep(NA_real_, nrow(leg)))

        id_col <- names(old)[tolower(names(old)) == ".rowid_legacy"]
        if (!length(id_col))
            stop("Legacy metrics2Vol did not preserve '.rowid_legacy'.",
                 call. = FALSE)

        out_v <- rep(NA_real_, nrow(leg))
        idx <- match(seq_len(nrow(leg)), old[[id_col[1L]]])
        ok <- !is.na(idx)
        out_v[ok] <- suppressWarnings(as.numeric(old[["v"]][idx[ok]]))
        out_v
    }

    ## Decide whether legacy estimates can be computed from the input.
    can_compute_legacy <- all(c("pr", "d", "h") %in% tolower(names(nfi_orig))) &&
        any(grepl("spec|espec", names(nfi_orig), ignore.case = TRUE))

    legacy_fallback <- isTRUE(legacy_fallback)
    legacy_requested <- keep.legacy || "V" %in% parametro
    legacy_for_fallback <- legacy_fallback &&
        any(setdiff(parametro, "V") %in% names(method_registry))
    need_legacy <- legacy_requested || legacy_for_fallback

    legacy_v_m3 <- rep(NA_real_, nrow(out))
    if (need_legacy && can_compute_legacy) {
        legacy_v_m3 <- if (legacy_requested) {
            compute_legacy_v(
                nfi_input = nfi_orig,
                cub.met = cub.met,
                nfi.nr = nfi_nr
            )
        } else {
            ## Computed only to back the fallback. It fails when no tree has
            ## legacy coefficients; those trees then stay NA, and the
            ## per-species warning from match_coef_rows() already explains it.
            tryCatch(
                suppressWarnings(compute_legacy_v(
                    nfi_input = nfi_orig,
                    cub.met = cub.met,
                    nfi.nr = nfi_nr
                )),
                error = function(e) rep(NA_real_, nrow(out))
            )
        }
    } else if (legacy_requested) {
        warn_msg <- c(
            warn_msg,
            "Legacy method not available: missing species, pr, d and/or h."
        )
    }

    if (keep.legacy || "V" %in% parametro) {
        nm_out_v <- method_registry[["V"]]$output
        out[[nm_out_v]] <- legacy_v_m3

        if (track_provenance) {
            out[[paste0(nm_out_v, "_source")]] <- ifelse(
                is.na(legacy_v_m3), "missing", "legacy"
            )
            out[[paste0(nm_out_v, "_status")]] <- ifelse(
                is.na(legacy_v_m3), "legacy_unavailable", "ok"
            )
            out[[paste0(nm_out_v, "_raw_unit")]] <- method_registry[["V"]]$raw_unit %||% "m3"
            out[[paste0(nm_out_v, "_scale")]] <- method_registry[["V"]]$scale_to_m3 %||% 1
            out[[paste0(nm_out_v, "_model")]] <- NA_character_
        }
    }

    ## Load and normalize the coefficient table used by registry-based
    ## methods.
    can_use_new <- !is.null(col_pr) && !is.null(col_spec)

    if (can_use_new) {
        coef_tab <- if (exists("SNFI43_all_volume_coefficients",
                               inherits = TRUE)) {
            SNFI43_all_volume_coefficients
        } else {
            NULL
        }

        coef_names <- if (is.null(coef_tab)) character(0) else names(coef_tab)
        coef_names_lc <- tolower(coef_names)

        coef_col <- function(candidates) {
            ii <- match(tolower(candidates), coef_names_lc)
            ii <- ii[!is.na(ii)]
            if (!length(ii))
                return(NULL)
            coef_names[ii[1L]]
        }

        coef_col_nfi   <- coef_col(c("nfi.nr"))
        coef_col_pr    <- coef_col(c("pr", "codigo_provincia"))
        coef_col_param <- coef_col(c("Parametro", "parametro"))
        coef_col_specn <- coef_col(c("Especie", "especie", "codigo_especie"))
        coef_col_fc    <- coef_col(c("F.c.", "fc"))
        coef_col_model <- coef_col(c("Modelo", "modelo"))

        if (!is.null(coef_col_param))
            coef_tab[[coef_col_param]] <- toupper(as.character(
                coef_tab[[coef_col_param]]
            ))
        coef_param_chr <- if (!is.null(coef_col_param)) {
            coef_tab[[coef_col_param]]
        } else {
            NULL
        }

        num1 <- function(x) suppressWarnings(as.numeric(as.character(x)))

        ## Convert relevant inputs and coefficient keys to numeric values.
        pr_num  <- num1(nfi_orig[[col_pr]])
        sp_num  <- num1(nfi_orig[[col_spec]])
        d_num   <- if (!is.null(col_d))   num1(nfi[[col_d]])   else rep(NA_real_, nrow(out))
        h_num   <- if (!is.null(col_h))   num1(nfi[[col_h]])   else rep(NA_real_, nrow(out))
        dnm_num <- if (!is.null(col_dnm)) num1(nfi[[col_dnm]]) else rep(NA_real_, nrow(out))

        coef_pr_num <- if (!is.null(coef_col_pr)) num1(coef_tab[[coef_col_pr]]) else NULL
        coef_sp_num <- if (!is.null(coef_col_specn)) num1(coef_tab[[coef_col_specn]]) else NULL

        ## Cache row matches to avoid repeated filtering of the same
        ## coefficient subset.
        pars_cache <- new.env(parent = emptyenv())

        match_coef_rows <- function(pr, especie, param = NULL,
                                    cub.met = "freq") {
            key <- paste(
                num1(nfi_nr),
                num1(pr),
                num1(especie),
                toupper(param %||% ""),
                as.character(cub.met),
                sep = "\r"
            )

            hit <- get0(key, envir = pars_cache,
                        inherits = FALSE, ifnotfound = NULL)
            if (!is.null(hit))
                return(hit)

            if (is.null(coef_tab) || is.null(coef_col_nfi) ||
                is.null(coef_col_pr)) {
                assign(key, data.frame(), envir = pars_cache)
                return(data.frame())
            }

            pr1 <- num1(pr)
            sp1 <- num1(especie)

            ii <- coef_tab[[coef_col_nfi]] == nfi_nr
            if (!is.na(pr1))
                ii <- ii & (coef_pr_num == pr1)

            x <- coef_tab[ii, , drop = FALSE]
            if (!nrow(x)) {
                assign(key, x, envir = pars_cache)
                return(x)
            }

            param_x <- if (!is.null(coef_col_param)) coef_param_chr[ii] else NULL

            ## Never borrow coefficients from another species or another
            ## parameter: an unmatched species or parameter returns an empty
            ## table tagged with the reason, and the caller returns NA.
            no_match <- function(reason) {
                warning(
                    "No ", toupper(param %||% "volume"), " coefficients for ",
                    if (is.na(sp1)) "a tree with missing species code" else
                        paste0("species ", sp1),
                    " in province ", pr1, " (IFN", num1(nfi_nr), "); ",
                    if (legacy_fallback)
                        "using the legacy equation for the same species where it has coefficients, otherwise NA."
                    else
                        "returning NA (legacy_fallback = FALSE).",
                    call. = FALSE
                )
                y <- x[0L, , drop = FALSE]
                attr(y, "no_match") <- reason
                assign(key, y, envir = pars_cache)
                y
            }

            if (!is.null(coef_col_specn)) {
                if (is.na(sp1))
                    return(no_match("species"))
                jj <- !is.na(coef_sp_num[ii]) & coef_sp_num[ii] == sp1
                if (!any(jj))
                    return(no_match("species"))
                x <- x[jj, , drop = FALSE]
                if (!is.null(param_x))
                    param_x <- param_x[jj]
            }

            if (!is.null(param) && !is.null(coef_col_param)) {
                kk <- !is.na(param_x) & param_x == toupper(param)
                if (!any(kk))
                    return(no_match("parameter"))
                x <- x[kk, , drop = FALSE]
            }

            if (nrow(x) > 1L && !is.null(coef_col_fc) &&
                !identical(cub.met, "freq")) {
                fc_x <- as.character(x[[coef_col_fc]])
                y <- x[fc_x == as.character(cub.met), , drop = FALSE]
                if (nrow(y))
                    x <- y
            }

            assign(key, x, envir = pars_cache)
            x
        }

        ## Resolve the parameter row for one method and one tree.
        get_method_pars <- function(param, ctx, resolved) {
            def <- method_registry[[param]]

            if (is.function(def$get_pars)) {
                p <- def$get_pars(
                    ctx = ctx,
                    resolved = resolved,
                    nfi = nfi_orig,
                    cub.met = cub.met
                )
                if (!is.null(p) && nrow(as.data.frame(p)) > 0)
                    return(as.data.frame(p, stringsAsFactors = FALSE))
            }

            if (!is.null(def$pars)) {
                p <- def$pars
                if (!is.data.frame(p))
                    p <- as.data.frame(p, stringsAsFactors = FALSE)

                if (!nrow(p))
                    return(NULL)

                nms <- tolower(names(p))
                y <- p

                col_pr   <- names(p)[match("pr", nms)]
                col_spec <- names(p)[match("especie", nms)]
                col_nfi  <- names(p)[match("nfi.nr", nms)]

                if (!is.na(col_pr))
                    y <- y[suppressWarnings(as.numeric(as.character(
                        y[[col_pr]]
                    ))) == suppressWarnings(as.numeric(as.character(
                        ctx$pr
                    ))), , drop = FALSE]

                if (!is.na(col_spec))
                    y <- y[suppressWarnings(as.numeric(as.character(
                        y[[col_spec]]
                    ))) == suppressWarnings(as.numeric(as.character(
                        ctx$especie
                    ))), , drop = FALSE]

                if (!is.na(col_nfi))
                    y <- y[y[[col_nfi]] == nfi_nr, , drop = FALSE]

                if (nrow(y) > 0)
                    return(y[1L, , drop = FALSE])

                return(NULL)
            }

            p <- match_coef_rows(
                pr = ctx$pr,
                especie = ctx$especie,
                param = param,
                cub.met = cub.met
            )

            if (!nrow(p)) {
                ## Keep the tagged empty table so eval_method() returns NA
                ## instead of the legacy fallback.
                if (!is.null(attr(p, "no_match")))
                    return(p)
                return(NULL)
            }

            p <- p[1L, , drop = FALSE]
            if (!is.null(coef_col_model) && !"Modelo" %in% names(p))
                p$Modelo <- p[[coef_col_model]]

            p
        }

        ## Resolve the equation function for one registry entry.
        get_method_fun <- function(def) {
            if (is.function(def$fun))
                return(def$fun)

            fn <- def$fun_name %||% NULL
            if (is.null(fn))
                return(NULL)

            get0(fn, mode = "function", inherits = TRUE)
        }

        ## Evaluate one registry method and return both value and
        ## provenance.
        eval_method <- function(param, ctx, resolved) {
            def <- method_registry[[param]]

            ## All fallbacks go through this switch, so that
            ## legacy_fallback = FALSE yields official values or NA only.
            fallback_value <- function(pars) {
                if (!legacy_fallback)
                    return(NA_real_)
                def$fallback(ctx, pars, resolved)
            }

            mk <- function(value = NA_real_,
                           source = NA_character_,
                           status = NA_character_,
                           raw_unit = def$raw_unit %||% NA_character_,
                           scale = def$scale_to_m3 %||% (1 / 1000),
                           model = NA_character_) {
                list(
                    value = value,
                    source = source,
                    status = status,
                    raw_unit = raw_unit,
                    scale = scale,
                    model = model
                )
            }

            fun <- get_method_fun(def)
            if (is.null(fun)) {
                fb <- fallback_value(NULL)
                return(mk(
                    value = fb,
                    source = if (!is.na(fb)) "fallback_legacy" else "missing",
                    status = "no_function"
                ))
            }

            pars <- get_method_pars(param, ctx, resolved)
            if (is.null(pars)) {
                fb <- fallback_value(NULL)
                return(mk(
                    value = fb,
                    source = if (!is.na(fb)) "fallback_legacy" else "missing",
                    status = "no_parameters"
                ))
            }

            if (!nrow(pars)) {
                ## No official row for this species or parameter. The legacy
                ## value, if any, comes from the same province and species.
                reason <- attr(pars, "no_match") %||% "parameters"
                fb <- fallback_value(NULL)
                return(mk(
                    value = fb,
                    source = if (!is.na(fb)) "fallback_legacy" else "missing",
                    status = paste0("no_", reason, "_coefficients")
                ))
            }

            pars <- pars[1L, , drop = FALSE]
            if (!is.null(coef_col_model) && !"Modelo" %in% names(pars))
                pars$Modelo <- pars[[coef_col_model]]

            model_val <- if ("Modelo" %in% names(pars)) {
                as.character(pars$Modelo[1L])
            } else {
                NA_character_
            }

            args <- def$build_args(ctx, pars, resolved)
            if (is.null(args)) {
                fb <- fallback_value(pars)
                return(mk(
                    value = fb,
                    source = if (!is.na(fb)) "fallback_legacy" else "missing",
                    status = "no_arguments",
                    model = model_val
                ))
            }

            err_msg <- NULL
            val_raw <- tryCatch(
                do.call(fun, args),
                error = function(e) {
                    err_msg <<- conditionMessage(e)
                    NA_real_
                }
            )

            val_raw <- suppressWarnings(as.numeric(val_raw)[1L])
            if (!length(val_raw) || is.na(val_raw)) {
                fb <- fallback_value(pars)
                return(mk(
                    value = fb,
                    source = if (!is.na(fb)) "fallback_legacy" else "missing",
                    status = if (is.null(err_msg)) "returned_na" else paste0("error: ", err_msg),
                    model = model_val
                ))
            }

            scale_to_m3 <- def$scale_to_m3 %||% (1 / 1000)

            mk(
                value = val_raw * scale_to_m3,
                source = "equation",
                status = "ok",
                model = model_val
            )
        }

        ## Evaluate methods row by row. `VCC` is computed first because
        ## later outputs may reuse it.
        param_order <- unique(c("VCC", setdiff(parametro, c("V", "VCC"))))
        param_order <- intersect(param_order, setdiff(names(method_registry), "V"))

        for (i in seq_len(nrow(out))) {
            ctx <- list(
                pr = pr_num[i],
                especie = sp_num[i],
                d_mm = d_num[i],
                h_m = h_num[i],
                dnm_mm = dnm_num[i]
            )

            resolved <- list(
                legacy_v_m3 = legacy_v_m3[i],
                vcc_m3 = NA_real_
            )

            if ("VCC" %in% param_order) {
                vcc_res <- eval_method("VCC", ctx, resolved)
                resolved$vcc_m3 <- vcc_res$value
                if ("VCC" %in% parametro) {
                    nm_out <- method_registry[["VCC"]]$output
                    out[[nm_out]][i] <- vcc_res$value

                    if (track_provenance) {
                        out[[paste0(nm_out, "_source")]][i] <- vcc_res$source
                        out[[paste0(nm_out, "_status")]][i] <- vcc_res$status
                        out[[paste0(nm_out, "_raw_unit")]][i] <- vcc_res$raw_unit
                        out[[paste0(nm_out, "_scale")]][i] <- vcc_res$scale
                        out[[paste0(nm_out, "_model")]][i] <- vcc_res$model
                    }
                }
            }

            rest <- setdiff(param_order, "VCC")
            for (param in rest) {
                res <- eval_method(param, ctx, resolved)
                nm_out <- method_registry[[param]]$output
                out[[nm_out]][i] <- res$value

                if (track_provenance) {
                    out[[paste0(nm_out, "_source")]][i] <- res$source
                    out[[paste0(nm_out, "_status")]][i] <- res$status
                    out[[paste0(nm_out, "_raw_unit")]][i] <- res$raw_unit
                    out[[paste0(nm_out, "_scale")]][i] <- res$scale
                    out[[paste0(nm_out, "_model")]][i] <- res$model
                }
            }
        }
    } else {
        warn_msg <- c(
            warn_msg,
            "SNFI43 coefficients not used: missing matching variables or coefficient table not available."
        )
    }

    ## Drop auxiliary coefficient columns when requested.
    if (!keep.var) {
        drop_cols <- intersect(
            c("par1", "par2", "par3", "fc", "parametro",
              "codigo_provincia", "nombre_provincia", "codigo_especie",
              "f.c.", "a", "b", "c", "p", "q", "r", "r2", "par_esp"),
            names(out)
        )
        if (length(drop_cols))
            out <- out[, !names(out) %in% drop_cols, drop = FALSE]
    }

    ## Keep only the requested computed outputs.
    keep_out <- unique(vapply(
        keep_param,
        function(x) method_registry[[x]]$output %||% tolower(x),
        character(1)
    ))

    vol_cols_all <- unique(vapply(
        method_registry,
        function(x) x$output %||% NA_character_,
        character(1)
    ))
    vol_cols_all <- vol_cols_all[!is.na(vol_cols_all)]

    drop_vol <- intersect(tolower(names(out)), tolower(vol_cols_all))
    drop_vol <- setdiff(drop_vol, tolower(keep_out))
    if (length(drop_vol))
        out <- out[, !tolower(names(out)) %in% drop_vol, drop = FALSE]

    ## Reorder identifying columns to the front of the output.
    rownames(out) <- NULL
    n <- names(out)
    first <- c("nfi.nr", "pr", "estadillo", "especie")
    i <- match(first, tolower(n))
    i <- i[!is.na(i)]
    out <- out[, c(n[i], n[-i]), drop = FALSE]

    ## Rebuild units from surviving input columns plus returned volume
    ## outputs. `nfiMetrics` stores units as a named vector:
    ## names = variable names, values = unit strings.
    units_orig <- get_units_map(nfi_orig)
    units_keep <- units_orig[names(units_orig) %in% names(out)]

    ## Add units for the computed outputs that are returned.
    vol_units <- vapply(
        keep_out,
        function(out_nm) {
            hit <- which(vapply(
                method_registry,
                function(z) identical(z$output %||% NA_character_, out_nm),
                logical(1)
            ))[1L]

            if (is.na(hit))
                return("m3 tree-1")

            method_registry[[hit]]$unit %||% "m3 tree-1"
        },
        character(1)
    )
    names(vol_units) <- keep_out

    ## Computed outputs override any original units with the same name.
    units_out <- c(units_keep, vol_units)
    units_out <- units_out[!duplicated(names(units_out), fromLast = TRUE)]

    ## Keep units only for columns present in the final output.
    units_out <- units_out[intersect(names(out), names(units_out))]

    attr(out, "units") <- units_out

    design_meta <- attr(nfi_orig, "design_meta")
    if (!is.null(design_meta))
        attr(out, "design_meta") <- design_meta

    if (track_provenance) {
        attr(out, "volume_meta") <- list(
            input_units = get_units_map(nfi_orig),
            normalized_input_units = c(d = "mm", h = "m", dnm = "mm"),
            returned_units = vol_units,
            methods = lapply(
                keep_param,
                function(param) {
                    def <- method_registry[[param]]
                    list(
                        param = param,
                        output = def$output %||% tolower(param),
                        label = def$label %||% param,
                        equation = def$equation %||% NA_character_,
                        reference = def$reference %||% NA_character_,
                        description = def$description %||% NA_character_,
                        raw_unit = def$raw_unit %||% NA_character_,
                        returned_unit = def$unit %||% "m3 tree-1",
                        scale_to_m3 = def$scale_to_m3 %||% (1 / 1000)
                    )
                }
            )
        )
        names(attr(out, "volume_meta")$methods) <- keep_param
    }

    attr(out, "nfi.nr") <- nfi_nr
    class(out) <- append("metrics2vol", class(out))

    if (length(warn_msg))
        warning(paste(unique(warn_msg), collapse = "\n"), call. = FALSE)

    out
}, ex = function() {
    x <- structure(
        data.frame(
            nfi.nr = 4,
            pr = 28,
            especie = 21,
            d = c(180, 260),
            h = c(9.5, 14.2),
            n = c(31.83, 14.15)
        ),
        class = c("nfiMetrics", "data.frame"),
        units = c(d = "mm", h = "m", n = "ha-1"),
        nfi.nr = 4
    )

    demo_registry <- snfi_volume_method_registry(list(
        VCC = list(
            output = "vcc_demo",
            fun = function(dbh_mm, h_m, pars) {
                pars$k[1] * dbh_mm^2 * h_m
            },
            raw_unit = "m3 tree-1",
            unit = "m3 tree-1",
            scale_to_m3 = 1,
            build_args = function(ctx, pars, resolved) {
                list(dbh_mm = ctx$d_mm, h_m = ctx$h_m, pars = pars)
            },
            fallback = function(ctx, pars, resolved) NA_real_,
            pars = data.frame(
                nfi.nr = 4,
                pr = 28,
                especie = 21,
                k = 1e-7
            )
        )
    ))

    y <- metrics2Vol(
        x,
        parametro = "VCC",
        method_registry = demo_registry,
        keep.var = FALSE
    )

    y[, c("nfi.nr", "pr", "especie", "vcc_demo")]

    ## Real SNFI workflows may require local Access/mdbtools support or
    ## previously cached data, so they are intentionally not run here.
    ## z <- nfiMetrics(28, nfi.nr = 4, dir = tools::R_user_dir("basifoR", "cache"))
    ## metrics2Vol(z, parametro = c("VCC", "VSC"))
})
