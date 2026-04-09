\name{new_volume_method}
\alias{new_volume_method}
\title{Define one external volume-computation method}
\description{Creates a compact method specification for the external volume registry. A method definition tells 
Create a method specification for registry-based volume calculations in external inventory workflows.
\code{externalMetrics2Vol} which \code{output} to compute, where to find or resolve parameters, how to 
Create a method specification for registry-based volume calculations in external inventory workflows.
build the argument list for the equation, how to scale the raw result to cubic metres, and which 
Create a method specification for registry-based volume calculations in external inventory workflows.
\code{fallback} to return when a direct computation is not possible.
Create a method specification for registry-based volume calculations in external inventory workflows.}
\usage{new_volume_method(output, 
    fun = NULL, fun_name = NULL, 
    unit = "m3", raw_unit = unit, 
    scale_to_m3 = 1, 
    build_args = function(ctx, 
        pars, resolved) list(), 
    fallback = function(ctx, 
        pars, resolved) null_or(resolved$preexisting_v_m3, 
        NA_real_), match_by = character(0), 
    get_pars = NULL, 
    pars = NULL, filter_pars = NULL, 
    required_inputs = NULL)}
\arguments{
  \item{output}{\code{character(1)}. Name of the \code{output} column produced by the method, for example \code{"v"}, \code{"vcc"}, or \code{"vsc"}.}
  \item{fun}{Optional \code{function}. Direct function used to compute the raw \code{output} value.}
  \item{fun_name}{Optional \code{character(1)}. Name of a function to resolve at run time when \code{fun} is not supplied.}
  \item{unit}{\code{character(1)}. Unit reported for the returned value after scaling.}
  \item{raw_unit}{\code{character(1)}. Unit produced by the raw method before applying \code{scale_to_m3}.}
  \item{scale_to_m3}{\code{numeric(1)}. Multiplicative factor used to convert the raw result to cubic metres.}
  \item{build_args}{\code{function}. Builds the argument list passed to the equation function. It receives the current row context, the selected parameter row, and already resolved outputs.}
  \item{fallback}{fallback }
  \item{match_by}{\code{character}. Column names used to match candidate parameter rows against the current row context.}
  \item{get_pars}{Optional \code{function}. Custom resolver that returns the parameter rows to use for the current tree or observation.}
  \item{pars}{Optional embedded parameter table stored inside the method definition.}
  \item{filter_pars}{Optional \code{function}. Additional filter applied after parameter matching and before row selection.}
  \item{required_inputs}{Optional \code{character}. Standardized inputs that must be available before the method can run, for example \code{c("d", "h")}.}
}
\details{The \code{fallback} function should return a scalar numeric value, typically \code{NA_real_} or a pre-existing volume estimate already present in the data. Define \code{required_inputs} with the standardized variable names expected by the method so the workflow can resolve prerequisites before evaluation.}
\value{A named \code{list} describing one external volume method.

The list contains the components \code{output}, \code{fun},

\code{fun_name}, \code{unit}, \code{raw_unit},

\code{scale_to_m3}, \code{build_args}, \code{fallback},

\code{match_by}, \code{get_pars}, \code{pars},

\code{filter_pars}, and \code{required_inputs}.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
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
}
