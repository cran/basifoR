external_volume_method_registry <- function
##title<< Build the active registry of external volume methods
##description<< Return the registry of external volume methods used by
##description<< \code{externalMetrics2Vol()} and related external-inventory workflows.
##description<< The function starts from the package defaults, optionally uses a
##description<< user-supplied registry, then applies named overrides stored in
##description<< option \code{"basifoR.external_volume_methods"}.
(
    methods = get0("external_volume_methods", inherits = TRUE, ifnotfound = NULL)
    ##<< Optional named \code{list} of method definitions.
    ##<< Each element should follow the structure returned by
    ##<< \code{new_volume_method()} and should usually be named with the
    ##<< requested output code (for example \code{"V"}, \code{"VCC"}, or
    ##<< \code{"VSC"}).
    ##<<
    ##<< When \code{NULL}, the function first looks for an object named
    ##<< \code{external_volume_methods} in the calling environment and, if it
    ##<< does not exist, falls back to the package defaults.
) {
    ##details<<
    ##details<< The returned registry is built in three steps.
    ##details<<
    ##details<< First, \code{default_external_volume_methods()} provides the base
    ##details<< registry shipped with the package.
    ##details<<
    ##details<< Second, \code{methods} replaces or extends those defaults. This
    ##details<< allows project-specific registries to redefine existing method
    ##details<< codes or add new ones.
    ##details<<
    ##details<< Third, if option \code{"basifoR.external_volume_methods"} is set,
    ##details<< its named entries are merged on top of \code{methods}. This gives
    ##details<< session-level overrides the highest precedence.
    ##details<<
    ##details<< Both \code{methods} and the option value must be named lists.
    ##details<< Otherwise the function stops with an error.
    ##details<<
    ##details<< The function only assembles and validates the registry. It does not
    ##details<< evaluate volume equations by itself.
    ##value<< A named \code{list} of external volume-method definitions. The list
    ##value<< always includes the package defaults, updated by any entries supplied
    ##value<< in \code{methods} and then by any entries found in option
    ##value<< \code{"basifoR.external_volume_methods"}. Each element is expected
    ##value<< to be compatible with \code{new_volume_method()}.
    ##examples<< reg <- external_volume_method_registry()
    ##examples<< names(reg)
    ##examples<<
    ##examples<< custom <- list(
    ##examples<<   V = new_volume_method(
    ##examples<<     output = "v",
    ##examples<<     unit = "m3",
    ##examples<<     raw_unit = "m3",
    ##examples<<     scale_to_m3 = 1,
    ##examples<<     build_args = function(ctx, pars, resolved) list(),
    ##examples<<     fallback = function(ctx, pars, resolved) 0,
    ##examples<<     match_by = character(0),
    ##examples<<     required_inputs = "v"
    ##examples<<   )
    ##examples<< )
    ##examples<< reg2 <- external_volume_method_registry(custom)
    ##examples<< reg2$V$output

    defaults <- default_external_volume_methods()

    if (is.null(methods))
        methods <- defaults

    if (!is.list(methods) || is.null(names(methods)))
        stop("'external_volume_methods' must be a named list.", call. = FALSE)

    extra <- getOption("basifoR.external_volume_methods")
    if (!is.null(extra)) {
        if (!is.list(extra) || is.null(names(extra)))
            stop("Option 'basifoR.external_volume_methods' must be a named list.",
                 call. = FALSE)
        methods <- utils::modifyList(methods, extra)
    }

    utils::modifyList(defaults, methods)
}
