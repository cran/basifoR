\name{new_inventory_design}
\alias{new_inventory_design}
\title{Create a generic inventory sampling design}
\description{Construct a generic \code{"inventory_design"} object from sampled areas and minimum DBH thresholds.
The function computes trees-per-hectare expansion factors for each tally tier and can represent circular,
square, strip, ring, or custom layouts through \code{metadata}.}
\usage{new_inventory_design(sample_area_m2, 
    min_dbh_cm = 0, name = "custom", 
    metadata = NULL)}
\arguments{
  \item{sample_area_m2}{\code{numeric}. Positive sampled area, in square
metres, for each tally tier. Supply one value
per diameter threshold.}
  \item{min_dbh_cm}{\code{numeric}. Minimum diameter at breast height,
in \code{cm}, required for a tree to enter each
tally tier. Must have the same length as
\code{sample_area_m2}. The function sorts these
thresholds in ascending order and reorders the
paired sampled areas accordingly.}
  \item{name}{\code{character(1)}. Human-readable label stored
in the returned design object.}
  \item{metadata}{Optional \code{list}. Extra design \code{metadata} stored
unchanged, for example plot shape, subplot radii,
side length, strip dimensions, or field notes.}
}
\details{The object is shape-agnostic. Use \code{metadata} to record

how the sampled area was obtained, for example from

circular radii, square side lengths, strip dimensions, or

protocol notes. The constructor stores \code{metadata} as

supplied and does not validate or reorder its contents.}
\value{An object of class \code{"inventory_design"}, returned as a

named list with components \code{name},

\code{min_dbh_cm}, \code{sample_area_m2}, \code{sf}, and

\code{metadata}. \code{sf} gives the trees-per-hectare

expansion factor for each tally tier.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
dsg <- new_inventory_design(
    sample_area_m2 = c(400, 100),
    min_dbh_cm = c(20, 0),
    name = "Nested square design",
    metadata = list(shape = "square", side_m = c(20, 10))
)

dsg$min_dbh_cm
dsg$sf
}
