\name{trees_per_ha.concentric_design}
\alias{trees_per_ha.concentric_design}
\title{Compute trees per hectare for concentric subplot designs
Compute trees per hectare for concentric subplot designs}
\description{Match a tree diameter to the appropriate subplot of a
Return the trees-per-hectare expansion factor for a tree measured under a concentric subplot \code{design}.
\code{"concentric_design"} object and return the corresponding
Return the trees-per-hectare expansion factor for a tree measured under a concentric subplot \code{design}.
trees-per-hectare expansion factor.
Return the trees-per-hectare expansion factor for a tree measured under a concentric subplot \code{design}.}
\usage{\method{trees_per_ha}{concentric_design}(design, 
    dbh_cm)}
\arguments{
  \item{design}{Object of class \code{"concentric_design"} created by helpers such as \code{\link{new_concentric_design}} or \code{\link{snfi_design}}.}
  \item{dbh_cm}{\code{numeric}. Diameter at breast height in \code{cm}. When several values are supplied, the method uses their mean after removing missing values.}
}
\details{It returns \code{NA_real_} when all supplied diameters are

missing or when the resulting diameter is smaller than the

smallest subplot threshold.}
\value{A single \code{numeric} expansion factor in trees per hectare

taken from \code{design$sf}, or \code{NA_real_} when the tree

does not belong to any subplot tier.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
trees_per_ha(snfi_design(), 13)
}
