new_volume_method <- structure(function
### Creates a compact method specification for the external volume registry. A method definition tells 
### \code{externalMetrics2Vol} which output to compute, where to find or resolve parameters, how to 
### build the argument list for the equation, how to scale the raw result to cubic metres, and which 
### fallback to return when a direct computation is not possible.
##title<< Define one external volume-computation method
##description<< Create a method specification for registry-based volume calculations in external inventory workflows.
(
    output, ##<< \code{character(1)}. Name of the output column produced by the method, for example \code{"v"}, \code{"vcc"}, or \code{"vsc"}.
    fun = NULL, ##<< Optional \code{function}. Direct function used to compute the raw output value.
    fun_name = NULL, ##<< Optional \code{character(1)}. Name of a function to resolve at run time when \code{fun} is not supplied.
    unit = "m3", ##<< \code{character(1)}. Unit reported for the returned value after scaling.
    raw_unit = unit, ##<< \code{character(1)}. Unit produced by the raw method before applying \code{scale_to_m3}.
    scale_to_m3 = 1, ##<< \code{numeric(1)}. Multiplicative factor used to convert the raw result to cubic metres.
    build_args = function(ctx, pars, resolved) list(), ##<< \code{function}. Builds the argument list passed to the equation function. It receives the current row context, the selected parameter row, and already resolved outputs.
    fallback = function(ctx, pars, resolved) null_or(resolved$preexisting_v_m3, NA_real_),
    ##<< \code{function}. Returns a fallback value when the method cannot compute a raw result, for example because the function, parameters, or arguments are missing.
    match_by = character(0), ##<< \code{character}. Column names used to match candidate parameter rows against the current row context.
    get_pars = NULL, ##<< Optional \code{function}. Custom resolver that returns the parameter rows to use for the current tree or observation.
    pars = NULL, ##<< Optional embedded parameter table stored inside the method definition.
    filter_pars = NULL, ##<< Optional \code{function}. Additional filter applied after parameter matching and before row selection.
    required_inputs = NULL ##<< Optional \code{character}. Standardized inputs that must be available before the method can run, for example \code{c("d", "h")}. 
) {
    method <- list(
        output = output,
        fun = fun,
        fun_name = fun_name,
        unit = unit,
        raw_unit = raw_unit,
        scale_to_m3 = scale_to_m3,
        build_args = build_args,
        fallback = fallback,
        match_by = match_by,
        get_pars = get_pars,
        pars = pars,
        filter_pars = filter_pars,
        required_inputs = required_inputs
    )

    method
    ##details<<
    ##details<< Use this constructor to define one method entry for a registry consumed by \code{externalMetrics2Vol}. The returned object does not execute any equation by itself. It only stores the metadata and helper functions needed to evaluate one output during a later workflow.
    ##details<<
    ##details<< A method can provide the computing function directly through \code{fun} or indirectly through \code{fun_name}. Parameter rows may come from the embedded \code{pars} table, from the workflow-level \code{parameter_table}, or from a custom resolver supplied in \code{get_pars}. When more than one candidate row is available, \code{match_by} and \code{filter_pars} help narrow the choice before the workflow selects a final row.
    ##details<<
    ##details<< The workflow interprets \code{build_args(ctx, pars, resolved)} as the bridge between standardized inputs and the target equation. The raw value returned by the equation is tagged with \code{raw_unit} and multiplied by \code{scale_to_m3} so that downstream outputs remain comparable in cubic metres.
    ##details<<
    ##details<< The \code{fallback} function should return a scalar numeric value, typically \code{NA_real_} or a pre-existing volume estimate already present in the data. Define \code{required_inputs} with the standardized variable names expected by the method so the workflow can resolve prerequisites before evaluation.
    ##value<< A named \code{list} describing one external volume method.
    ##value<< The list contains the components \code{output}, \code{fun},
    ##value<< \code{fun_name}, \code{unit}, \code{raw_unit},
    ##value<< \code{scale_to_m3}, \code{build_args}, \code{fallback},
    ##value<< \code{match_by}, \code{get_pars}, \code{pars},
    ##value<< \code{filter_pars}, and \code{required_inputs}.
},
ex = function() {
    ## Minimal standalone example
    vm <- new_volume_method(
        output = "vcc",
        fun = function(dbh_cm, h_m, pars) dbh_cm * h_m * pars$k[1],
        unit = "m3",
        raw_unit = "cm3",
        scale_to_m3 = 1 / 1e6,
        build_args = function(ctx, pars, resolved) {
            list(dbh_cm = ctx$d_cm, h_m = ctx$h_m, pars = pars)
        },
        fallback = function(ctx, pars, resolved) NA_real_,
        match_by = "species",
        pars = data.frame(species = "sp1", k = 2500),
        required_inputs = c("d", "h")
    )

    names(vm)
    vm$output
    vm$build_args(
        ctx = list(d_cm = 20, h_m = 12),
        pars = vm$pars,
        resolved = list()
    )
})
