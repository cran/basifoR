\name{dbhMetric}
\alias{dbhMetric}
\title{Compute diameter, basal area, tree density, or height from tree measurements}
\description{Convert raw tree diameter or height inputs into the compact metric formats used across basifoR workflows.}
\usage{dbhMetric(dbh, met = "d", 
    design = snfi_design())}
\arguments{
  \item{dbh}{\code{numeric}. Diameter at breast height in \code{mm}, or tree height in \code{m} when \code{met = "h"}. Non-numeric inputs are coerced, zeros are treated as missing, and vectors are averaged after that replacement.}
  \item{met}{\code{character(1)}. Metric to compute: mean diameter at breast height (\code{"d"}), basal area (\code{"ba"}), trees per hectare (\code{"n"}), or height (\code{"h"}).}
  \item{design}{Object inheriting from \code{"inventory_design"}, used only when \code{met = "n"} to derive the trees-per-hectare expansion factor. The default is the Spanish National Forest Inventory concentric subplot \code{design}.}
}
\details{The sampling \code{design} affects only \code{met = "n"}. For \code{"d"}, \code{"ba"}, and \code{"h"}, the returned value does not depend on \code{design}.}
\value{A single \code{numeric} value. Returns mean diameter in \code{mm} for \code{met = "d"}, basal area in \code{m^2} per tree for \code{met = "ba"}, trees per hectare for \code{met = "n"}, and height in \code{dm} for \code{met = "h"}. Returns \code{NA_real_} when all supplied values are missing or become missing after zero replacement.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
dbhMetric(300, "d")
dbhMetric(300, "ba")
dbhMetric(18, "h")

dsg <- new_concentric_design(
    radii_m = c(4, 8, 12),
    min_dbh_cm = c(5, 15, 30),
    name = "3-subplot design"
)

dbhMetric(130, "n", design = dsg)
}
