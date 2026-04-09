\name{snfi_volume_method_registry}
\alias{snfi_volume_method_registry}
\title{Assemble the active SNFI volume-method registry}
\description{Return the final registry of SNFI volume methods after merging package defaults, optional user definitions, and session-level option overrides.}
\usage{snfi_volume_method_registry(equations = get0("snfi_volume_equations", 
    inherits = TRUE, 
    ifnotfound = NULL))}
\arguments{
  \item{equations}{equations }
}
\details{Each registry entry is a named list that typically contains fields such

as \code{output}, \code{fun_name}, \code{unit}, \code{raw_unit},

\code{scale_to_m3}, \code{build_args}, and \code{fallback}. The

function checks only that the supplied registry is a named list;

downstream functions validate and use the individual fields.}
\value{A named list of SNFI volume-method definitions.

Names usually correspond to requested parameters such as \code{"V"},

\code{"VCC"}, and \code{"VSC"}.

Each element describes one computation pathway and commonly includes:

\describe{

\item{\code{output}}{Name of the output column returned by the method.}

\item{\code{fun_name}}{Name of the equation helper called at run time, or \code{NULL} for direct passthrough methods.}

\item{\code{unit}}{Returned unit after scaling, usually \code{"m3 tree-1"}.}

\item{\code{raw_unit}}{Unit returned by the underlying equation before scaling.}

\item{\code{scale_to_m3}}{Multiplier applied to raw outputs to express them in cubic metres.}

\item{\code{build_args}}{Function that builds the argument list for the equation helper.}

\item{\code{fallback}}{Function used when coefficients are missing or a method cannot compute the requested output.}

}}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
c(
    "reg <- snfi_volume_method_registry()",
    "names(reg)",
    "reg$VCC$output",
    "custom <- list(VCC = list(output = 'vcc_m3'))",
    "reg2 <- snfi_volume_method_registry(custom)",
    "reg2$VCC$output"
)
}
