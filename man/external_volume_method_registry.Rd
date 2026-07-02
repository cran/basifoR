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
  \item{methods}{Optional named \code{list} of method definitions.
Each element should follow the structure returned by
\code{new_volume_method()} and should usually be named with the
requested output code (for example \code{"V"}, \code{"VCC"}, or
\code{"VSC"}).

When \code{NULL}, the function first looks for an object named
\code{external_volume_methods} in the calling environment and, if it
does not exist, falls back to the package defaults.}
}
\details{The function only assembles and validates the registry. It does not

evaluate volume equations by itself.}
\value{A named \code{list} of external volume-method definitions. The list

always includes the package defaults, updated by any entries supplied

in \code{methods} and then by any entries found in option

\code{"basifoR.external_volume_methods"}. Each element is expected

to be compatible with \code{new_volume_method()}.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
reg <- external_volume_method_registry()
names(reg)

custom <- list(
    V = new_volume_method(
        output = "v",
        unit = "m3",
        raw_unit = "m3",
        scale_to_m3 = 1,
        build_args = function(ctx, pars, resolved) list(),
        fallback = function(ctx, pars, resolved) 0,
        match_by = character(0),
        required_inputs = "v"
    )
)

reg2 <- external_volume_method_registry(custom)
reg2$V$output
}
