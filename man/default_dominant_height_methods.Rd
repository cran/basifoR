\name{default_dominant_height_methods}
\alias{default_dominant_height_methods}
\title{Default dominant-height methods}
\description{Return the bundled dominant-height method specifications used by \code{nfiMetrics()} and external metric workflows.}
\usage{default_dominant_height_methods()}
\details{The default registry contains the backward-compatible method

\code{"Hd"} plus stricter explicit alternatives. The default

\code{"Hd"} mirrors the previous internal \code{domheight()}

behaviour, so existing analyses keep the same numerical convention

while the equation and fallback rule become visible.}
\value{A named \code{list} of \code{"dominant_height_method"} objects.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
methods <- default_dominant_height_methods()
names(methods)
methods$Hd$equation
}
