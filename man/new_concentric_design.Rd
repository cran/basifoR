\name{new_concentric_design}
\alias{new_concentric_design}
\title{Construct a concentric subplot design}
\description{Define a concentric inventory design from subplot radii and minimum
diameter thresholds.

The constructor orders subplot tiers by minimum DBH, converts radii
to sampled area in square metres, computes trees-per-hectare
expansion factors, and returns an object of class
\code{"concentric_design"} inheriting from
\code{"inventory_design"}.}
\usage{new_concentric_design(radii_m, 
    min_dbh_cm, name = "custom", 
    metadata = NULL)}
\arguments{
  \item{radii_m}{\code{numeric}. Subplot radii in metres, one value per
<< tally tier.}
  \item{min_dbh_cm}{\code{numeric}. Minimum diameter at breast height in
<< \code{cm} required for a tree to be tallied in each
<< subplot. Must have the same length as
<< \code{radii_m}. The function sorts these thresholds in
<< ascending order and reorders the paired radii
<< accordingly.}
  \item{name}{\code{character(1)}. Human-readable design \code{name}
<< stored in the returned object.}
  \item{metadata}{Optional \code{list}. Additional design \code{metadata}
<< merged with default entries for
<< \code{shape = "circular"} and the ordered
<< \code{radii_m}.}
}
\details{The function delegates to \code{\link{new_inventory_design}}

and then appends the ordered \code{radii_m} vector plus the

class \code{"concentric_design"}. The \code{metadata}

argument is merged with default entries for circular plot

shape and ordered radii using

\code{utils::modifyList()}.}
\value{An object of class

\code{c("concentric_design", "inventory_design")}, returned

as a named list with components \code{name},

\code{min_dbh_cm}, \code{sample_area_m2}, \code{sf},

\code{metadata}, and \code{radii_m}. The

\code{sample_area_m2} component stores subplot areas in

square metres, \code{sf} stores the corresponding

trees-per-hectare expansion factors, and

\code{metadata} contains the merged design \code{metadata}.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
dsg <- new_concentric_design(
    radii_m = c(5, 10, 15, 25),
    min_dbh_cm = c(7.5, 12.5, 22.5, 42.5),
    name = "SNFI"
)
dsg
}
