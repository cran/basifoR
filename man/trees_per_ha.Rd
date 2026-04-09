\name{trees_per_ha}
\alias{trees_per_ha}
\title{Compute trees-per-hectare expansion factors from inventory designs}
\description{Dispatch on the sampling-design class to return the expansion factor,
Return the expansion factor, expressed as trees per hectare, associated with a tree of a given diameter at breast height under a supported inventory \code{design}.
expressed as trees per hectare, associated with a tree of a given
Return the expansion factor, expressed as trees per hectare, associated with a tree of a given diameter at breast height under a supported inventory \code{design}.
diameter at breast height.
Return the expansion factor, expressed as trees per hectare, associated with a tree of a given diameter at breast height under a supported inventory \code{design}.}
\usage{trees_per_ha(design, 
    dbh_cm)}
\arguments{
  \item{design}{Sampling \code{design} object. Supported methods currently include \code{"inventory_design"} and \code{"concentric_design"}.}
  \item{dbh_cm}{\code{numeric}. Diameter at breast height in \code{cm}. When several values are supplied, methods first coerce them to numeric and then use their mean after removing missing values.}
}
\details{Current methods return \code{NA_real_} when diameter is

missing after preprocessing or falls below the smallest

supported threshold in the supplied \code{design}.}
\value{A single \code{numeric} value giving the trees-per-hectare

expansion factor for the supplied diameter and \code{design}, or

\code{NA_real_} when no valid factor can be assigned.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
dsg <- new_inventory_design(sample_area_m2 = c(100, 400),
                            min_dbh_cm = c(0, 20))
trees_per_ha(dsg, 13)
}
