snfi_volume_method_registry <- structure(function(
    # Build the active SNFI volume-method registry
    ### Assemble the registry of SNFI volume methods used by \code{metrics2Vol()}. The function starts from the package defaults, optionally merges a user-supplied registry, and then applies session-level overrides from \code{options(basifoR.snfi_volume_methods = ...)}.
    equations = get0("snfi_volume_equations",
                     inherits = TRUE,
                     ifnotfound = NULL)
    ##<< Optional named list of SNFI method definitions to merge with the defaults.
    ##<< Each top-level name should identify a requested output such as \code{"V"},
    ##<< \code{"VCC"}, or \code{"VSC"}. Each element may define a complete method
    ##<< or a partial override of an existing one.
) {
    ##title<< Assemble the active SNFI volume-method registry
    ##description<< Return the final registry of SNFI volume methods after merging package defaults, optional user definitions, and session-level option overrides.
    ##details<<
    ##details<< This helper prepares the registry consumed by \code{metrics2Vol()}.
    ##details<< The returned object tells the dispatcher which outputs are available,
    ##details<< which equation function to call, how to build arguments for that
    ##details<< function, and how to convert raw outputs to cubic metres.
    ##details<<
    ##details<< The merge order is:
    ##details<< \enumerate{
    ##details<<   \item \code{default_snfi_volume_equations()}.
    ##details<<   \item \code{equations}, if supplied directly or found as
    ##details<<   \code{snfi_volume_equations} in the calling environment.
    ##details<<   \item \code{getOption("basifoR.snfi_volume_methods")}, when set.
    ##details<< }
    ##details<< Later sources override earlier ones through \code{utils::modifyList()},
    ##details<< so you can replace only selected fields without rebuilding the whole
    ##details<< registry.
    ##details<<
    ##details<< Each registry entry is a named list that typically contains fields such
    ##details<< as \code{output}, \code{fun_name}, \code{unit}, \code{raw_unit},
    ##details<< \code{scale_to_m3}, \code{build_args}, and \code{fallback}. The
    ##details<< function checks only that the supplied registry is a named list;
    ##details<< downstream functions validate and use the individual fields.
    ##value<< A named list of SNFI volume-method definitions.
    ##value<< Names usually correspond to requested parameters such as \code{"V"},
    ##value<< \code{"VCC"}, and \code{"VSC"}.
    ##value<< Each element describes one computation pathway and commonly includes:
    ##value<< \describe{
    ##value<<   \item{\code{output}}{Name of the output column returned by the method.}
    ##value<<   \item{\code{fun_name}}{Name of the equation helper called at run time, or \code{NULL} for direct passthrough methods.}
    ##value<<   \item{\code{unit}}{Returned unit after scaling, usually \code{"m3 tree-1"}.}
    ##value<<   \item{\code{raw_unit}}{Unit returned by the underlying equation before scaling.}
    ##value<<   \item{\code{scale_to_m3}}{Multiplier applied to raw outputs to express them in cubic metres.}
    ##value<<   \item{\code{build_args}}{Function that builds the argument list for the equation helper.}
    ##value<<   \item{\code{fallback}}{Function used when coefficients are missing or a method cannot compute the requested output.}
    ##value<< }

    defaults <- default_snfi_volume_equations()

    if (is.null(equations))
        equations <- defaults

    if (!is.list(equations) || is.null(names(equations)))
        stop("'snfi_volume_equations' must be a named list.", call. = FALSE)

    extra <- getOption("basifoR.snfi_volume_methods")
    if (!is.null(extra)) {
        if (!is.list(extra) || is.null(names(extra)))
            stop("Option 'basifoR.snfi_volume_methods' must be a named list.",
                 call. = FALSE)
        equations <- utils::modifyList(equations, extra)
    }

    utils::modifyList(defaults, equations)
}, ex = function(){
c(
    "reg <- snfi_volume_method_registry()",
    "names(reg)",
    "reg$VCC$output",
    "custom <- list(VCC = list(output = 'vcc_m3'))",
    "reg2 <- snfi_volume_method_registry(custom)",
    "reg2$VCC$output"
)
})
