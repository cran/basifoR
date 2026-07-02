default_snfi_volume_equations <- structure(function(
### Return the default SNFI volume-equation registry. Creates the
### default set of method definitions used by \code{metrics2Vol()} to
### compute tree-level volume variables. Each registry entry includes
### descriptive metadata together with the target column name,
### equation function, unit conversion, argument builder, and fallback
### rule.
                                          ) {
    ##title<< Default SNFI volume-equation methods
    ##value<< A named list with the default methods \code{V}, \code{VCC}, and \code{VSC}.
    ##value<< Each entry includes \code{label}, \code{equation}, \code{reference}, and
    ##value<< \code{description} fields for documentation, in addition to the
    ##value<< computational fields used by \code{metrics2Vol()}.
    list(
        V = list(
            output = "v",
            label = "Legacy SNFI over-bark volume estimate",
            fun_name = NULL,
            unit = "m3 tree-1",
            raw_unit = "m3 tree-1",
            scale_to_m3 = 1,
            equation = "Passthrough of the legacy basifoR SNFI over-bark volume estimate.",
            reference = "Second Spanish National Forest Inventory over-bark volume equations (legacy basifoR implementation).",
            description = "Legacy over-bark volume per tree (column 'v').",
            build_args = function(ctx, pars, resolved) list(),
            fallback = function(ctx, pars, resolved) resolved$legacy_v_m3 %||% NA_real_
        ),
        VCC = list(
            output = "vcc",
            label = "Over-bark stem volume (volumen con corteza)",
            fun_name = "get_snfi_vcc",
            unit = "m3 tree-1",
            raw_unit = "dm3 tree-1",
            scale_to_m3 = 1 / 1000,
            equation = paste(
                "Over-bark stem volume from the official SNFI species equation",
                "evaluated at diameter (mm) and total height (m); raw result in",
                "dm3 tree-1 is scaled to m3 tree-1."
            ),
            reference = "Official SNFI over-bark volume equations (volumen con corteza, VCC).",
            description = "Over-bark stem volume per tree (SNFI 'VCC' -> column 'vcc').",
            build_args = function(ctx, pars, resolved) {
                list(dbh_mm = ctx$d_mm, h_t = ctx$h_m, pars = pars)
            },
            fallback = function(ctx, pars, resolved) resolved$legacy_v_m3 %||% NA_real_
        ),
        VSC = list(
            output = "vsc",
            label = "Under-bark stem volume (volumen sin corteza)",
            fun_name = "get_snfi_vsc",
            unit = "m3 tree-1",
            raw_unit = "dm3 tree-1",
            scale_to_m3 = 1 / 1000,
            equation = paste(
                "Under-bark stem volume derived from the over-bark volume (VCC)",
                "via the official SNFI bark relation; raw result in dm3 tree-1 is",
                "scaled to m3 tree-1."
            ),
            reference = "Official SNFI under-bark volume relation (volumen sin corteza, VSC).",
            description = "Under-bark stem volume per tree (SNFI 'VSC' -> column 'vsc').",
            build_args = function(ctx, pars, resolved) {
                vcc_m3 <- resolved$vcc_m3
                if (is.null(vcc_m3) || is.na(vcc_m3))
                    vcc_m3 <- resolved$legacy_v_m3
                if (is.null(vcc_m3) || is.na(vcc_m3))
                    return(NULL)
                list(vcc = vcc_m3 * 1000, pars = pars)
            },
            fallback = function(ctx, pars, resolved) NA_real_
        )
    )
}, ex = function() {
    methods <- default_snfi_volume_equations()
    names(methods)
    methods$VCC$output
    methods$VCC$unit
})
