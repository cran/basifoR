\name{print.inventory_design}
\alias{print.inventory_design}
\title{Print a generic inventory plot design}
\description{Display the main stored components of an \code{"inventory_design"}
Print a compact summary of an \code{"inventory_design"} object.
The method reports the stored design name, minimum DBH thresholds,
sampled areas, and trees-per-hectare expansion factors.
object in a compact human-readable summary.
Print a compact summary of an \code{"inventory_design"} object.
The method reports the stored design name, minimum DBH thresholds,
sampled areas, and trees-per-hectare expansion factors.}
\usage{\method{print}{inventory_design}(x, 
    ...)}
\arguments{
  \item{x}{Object of class \code{"inventory_design"}.}
  \item{\dots}{Further arguments passed to methods. Currently unused.}
}
\details{The method does not modify the object.}
\value{The input \code{"inventory_design"} object, returned invisibly.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
dsg <- new_inventory_design(
    sample_area_m2 = c(400, 100),
    min_dbh_cm = c(20, 0),
    name = "Nested square design"
)

print(dsg)
}
