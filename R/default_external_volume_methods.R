default_external_volume_methods <- structure(function
##title<< Return bundled external volume methods
##description<< Return the built-in external volume-method specifications used as the starting registry for \code{\link{externalMetrics2Vol}} and \code{\link{external_volume_method_registry}}.
() {
    list(
        V = new_volume_method(
            output = "v",
            unit = "m3",
            raw_unit = "m3",
            scale_to_m3 = 1,
            build_args = function(ctx, pars, resolved) list(),
            fallback = function(ctx, pars, resolved) null_or(resolved$preexisting_v_m3, NA_real_),
            #fallback = function(ctx, pars, resolved) resolved$preexisting_v_m3 %||% NA_real_,
            match_by = character(0),
            required_inputs = "v"
        ),
        VCC = new_volume_method(
            output = "vcc",
            unit = "m3",
            raw_unit = "m3",
            scale_to_m3 = 1,
            build_args = function(ctx, pars, resolved) {
                list(dbh_mm = ctx$d_mm, h_m = ctx$h_m, pars = pars)
            },
            fallback = function(ctx, pars, resolved) NA_real_,
            match_by = c("species", "region", "equation_set"),
            required_inputs = c("d", "h")
        ),
        VSC = new_volume_method(
            output = "vsc",
            unit = "m3",
            raw_unit = "m3",
            scale_to_m3 = 1,
            build_args = function(ctx, pars, resolved) {
                list(vcc_m3 = resolved$vcc_m3, pars = pars)
            },
            fallback = function(ctx, pars, resolved) NA_real_,
            match_by = c("species", "region", "equation_set"),
            required_inputs = "vcc"
        )
    )
    ##details<<
    ##details<< The returned list contains the default method definitions for
    ##details<< total volume \code{"V"}, merchantable volume over bark
    ##details<< \code{"VCC"}, and merchantable volume under bark
    ##details<< \code{"VSC"}. Each entry is created with
    ##details<< \code{\link{new_volume_method}} and declares the output
    ##details<< column name, expected units, required inputs, matching
    ##details<< keys, and fallback behaviour used by the external volume
    ##details<< workflow.
    ##details<<
    ##details<< \code{"V"} passes through an already available total-volume
    ##details<< field. \code{"VCC"} computes merchantable volume over bark
    ##details<< from diameter and height, and \code{"VSC"} computes
    ##details<< merchantable volume under bark from the resolved
    ##details<< \code{vcc} value.
    ##value<< A named list of default external volume-method specifications.
    ##value<< The list currently contains entries named \code{"V"},
    ##value<< \code{"VCC"}, and \code{"VSC"}, each stored as the named
    ##value<< list returned by \code{\link{new_volume_method}}.
}, ex = function() {
    methods <- default_external_volume_methods()
    names(methods)
    methods$V$output
    methods$VCC$required_inputs
})
