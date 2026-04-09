\name{default_external_volume_methods}
\alias{default_external_volume_methods}
\title{Return bundled external volume methods}
\description{Return the built-in external volume-method specifications used as the starting registry for \code{\link{externalMetrics2Vol}} and \code{\link{external_volume_method_registry}}.}
\usage{default_external_volume_methods()}
\details{\code{"V"} passes through an already available total-volume

field. \code{"VCC"} computes merchantable volume over bark

from diameter and height, and \code{"VSC"} computes

merchantable volume under bark from the resolved

\code{vcc} value.}
\value{A named list of default external volume-method specifications.

The list currently contains entries named \code{"V"},

\code{"VCC"}, and \code{"VSC"}, each stored as the named

list returned by \code{\link{new_volume_method}}.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
methods <- default_external_volume_methods()
names(methods)
methods$V$output
methods$VCC$required_inputs
}
