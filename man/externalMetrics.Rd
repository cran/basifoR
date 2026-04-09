\name{externalMetrics}
\alias{externalMetrics}
\title{Compute tree-level metrics from external inventory data}
\description{Standardize external tree measurements for external inventory workflows and return requested tree-level metrics in basifoR units.}
\usage{externalMetrics(x, var = c("d", 
    "h", "ba", "n", "Hd"), 
    levels = NULL, design, 
    colmap = NULL, d_unit = c("mm", 
        "cm")[1], h_unit = c("m", 
        "dm", "cm")[1], 
    keep_cols = NULL, 
    domheight_fun = NULL)}
\arguments{
  \item{x}{Input data.frame with one row per tree or stem.}
  \item{var}{Supported values are \code{"d"}, \code{"h"}, \code{"ba"},
Requested metrics to return.
\code{"n"}, and \code{"Hd"}. Requesting \code{"Hd"} also
Requested metrics to return.
requires \code{"d"}, \code{"h"}, and \code{"n"}.
Requested metrics to return.}
  \item{levels}{Grouping columns to keep in the output.}
  \item{design}{Inventory \code{design} used to compute expansion factors for \code{n}.}
  \item{colmap}{Named list of candidate raw column names for diameter and height.
Matching is case-insensitive and also accepts numeric suffixes such
as \code{diameter_1} or \code{height.2}.}
  \item{d_unit}{Unit of raw diameter columns in \code{colmap$d}.}
  \item{h_unit}{Unit of raw height columns in \code{colmap$h}.}
  \item{keep_cols}{Additional source columns to carry into the result.}
  \item{domheight_fun}{Function used when \code{"Hd"} is requested.}
}
\details{The function first resolves measurement columns from \code{colmap}. Exact matches are preferred, then case-insensitive matches with optional numeric suffixes are considered. When several repeated measurement columns are found for the same variable, row-wise non-missing means are used.

Zero values in resolved diameter or height columns are treated as missing before aggregation. Returned units are standardized to millimetres for \code{d}, decimetres for \code{h} and \code{Hd}, square metres per tree for \code{ba}, and trees per hectare for \code{n}.

When \code{var} includes \code{"n"}, the function uses \code{design} to obtain expansion factors. Fixed-area designs call \code{trees_per_ha()}, while concentric designs choose the proper factor from the \code{design} thresholds. When \code{var} includes \code{"Hd"}, dominant height is computed within each resolved group defined by \code{levels} and \code{keep_cols}.}
\value{A data.frame containing the requested tree-level metrics, optionally preceded by resolved grouping columns.

The returned object inherits from classes \code{"externalMetrics"} and \code{"nfiMetrics"}. Unit metadata are stored in \code{attr(out, "units")}. When \code{var} includes \code{"n"} or \code{"Hd"}, the result also stores sampling \code{design} metadata in \code{attr(out, "design_meta")}.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
sq_0.1ha <- new_inventory_design(
    sample_area_m2 = 1000,
    min_dbh_cm = 7.5,
    name = "Square 0.1-ha plot",
    metadata = list(shape = "square", side_m = sqrt(1000))
)

x <- data.frame(
    plot = c("P1", "P1", "P2"),
    species = c("sp1", "sp1", "sp2"),
    diameter_mm = c(120, 185, 260),
    height_m = c(7.1, 9.4, 13.2),
    stringsAsFactors = FALSE
)

externalMetrics(
    x = x,
    var = c("d", "h", "ba", "n"),
    levels = c("plot", "species"),
    design = sq_0.1ha,
    colmap = list(d = "diameter_mm", h = "height_m"),
    d_unit = "mm",
    h_unit = "m"
)
}
