\name{print.concentric_design}
\alias{print.concentric_design}
\title{Print a concentric plot design}
\description{Display the main components of a \code{"concentric_design"} object:
Display the main components of a \code{"concentric_design"} object.
The printed summary reports the design name together with subplot radii, minimum DBH thresholds, and expansion factors for each tally tier.
design name, subplot radii, minimum diameters, and expansion factors.
Display the main components of a \code{"concentric_design"} object.
The printed summary reports the design name together with subplot radii, minimum DBH thresholds, and expansion factors for each tally tier.}
\usage{\method{print}{concentric_design}(x, 
    ...)}
\arguments{
  \item{x}{Object of class \code{"concentric_design"}, typically
<< created by \code{\link{new_concentric_design}}.}
  \item{\dots}{Further arguments passed to methods. Currently unused.}
}
\details{The function does not modify the design and does not currently use additional arguments passed through \code{...}.}
\value{The input \code{"concentric_design"} object, returned invisibly.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
dsg <- new_concentric_design(
    radii_m = c(5, 10, 15),
    min_dbh_cm = c(7.5, 22.5, 42.5),
    name = "Example concentric design"
)

print.concentric_design(dsg)
invisible(NULL)
}
