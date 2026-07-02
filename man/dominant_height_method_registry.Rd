\name{dominant_height_method_registry}
\alias{dominant_height_method_registry}
\title{Build the active dominant-height method registry}
\description{Return the registry of dominant-height \code{methods} used by \code{nfiMetrics()} and related metric workflows.}
\usage{dominant_height_method_registry(methods = get0("dominant_height_methods", 
    inherits = TRUE, 
    ifnotfound = NULL))}
\arguments{
  \item{methods}{Optional named \code{list} of method definitions. Each element should follow the structure returned by \code{new_dominant_height_method()}.}
}
\details{The returned registry is built in three steps. First,

\code{default_dominant_height_methods()} provides the bundled

definitions. Second, \code{methods} replaces or extends those

defaults. Third, named entries in option

\code{"basifoR.dominant_height_methods"} override both. This

mirrors the volume-method registry pattern and lets users inspect

or replace the dominant-height method without editing package code.}
\value{A named \code{list} of dominant-height method definitions.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
reg <- dominant_height_method_registry()
names(reg)
reg$Hd$equation

custom <- list(
    Hd = new_dominant_height_method(
        fun_name = "domheight_strict",
        equation = "Hd = sum(h_i * n_i) / sum(n_i)",
        selection_rule = "Use the largest trees until n reaches 100 trees ha-1.",
        fallback = "Return NA if the threshold is not reached."
    )
)
dominant_height_method_registry(custom)$Hd$fallback
}
