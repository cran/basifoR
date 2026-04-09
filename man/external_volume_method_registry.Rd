\name{external_volume_method_registry}
\alias{external_volume_method_registry}
\title{Build the active registry of external volume methods}
\description{Return the registry of external volume \code{methods} used by
\code{externalMetrics2Vol()} and related external-inventory workflows.
The function starts from the package defaults, optionally uses a
user-supplied registry, then applies named overrides stored in
option \code{"basifoR.external_volume_methods"}.}
\usage{external_volume_method_registry(methods = get0("external_volume_methods", 
    inherits = TRUE, 
    ifnotfound = NULL))}
\arguments{
  \item{methods}{methods }
}
\details{The function only assembles and validates the registry. It does not

evaluate volume equations by itself.}
\value{A named \code{list} of external volume-method definitions. The list

always includes the package defaults, updated by any entries supplied

in \code{methods} and then by any entries found in option

\code{"basifoR.external_volume_methods"}. Each element is expected

to be compatible with \code{new_volume_method()}.
examples<< reg <- external_volume_method_registry()
examples<< names(reg)
examples<<
examples<< custom <- list(
examples<<   V = new_volume_method(
examples<<     output = "v",
examples<<     unit = "m3",
examples<<     raw_unit = "m3",
examples<<     scale_to_m3 = 1,
examples<<     build_args = function(ctx, pars, resolved) list(),
examples<<     fallback = function(ctx, pars, resolved) 0,
examples<<     match_by = character(0),
examples<<     required_inputs = "v"
examples<<   )
examples<< )
examples<< reg2 <- external_volume_method_registry(custom)
examples<< reg2$V$output}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}





