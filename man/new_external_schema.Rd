\name{new_external_schema}
\alias{new_external_schema}
\title{Define a schema for external inventory workflows}
\description{Create a reusable \code{"external_schema"} object that records how
Create a reusable schema with column aliases, \code{units}, default grouping settings, retained columns, and optional \code{defaults} for the external basifoR workflow.
source columns in an external inventory correspond to the standardized
Create a reusable schema with column aliases, \code{units}, default grouping settings, retained columns, and optional \code{defaults} for the external basifoR workflow.
field names used by the basifoR external workflow. The schema can also
Create a reusable schema with column aliases, \code{units}, default grouping settings, retained columns, and optional \code{defaults} for the external basifoR workflow.
store declared measurement \code{units}, default grouping \code{levels}, columns to
Create a reusable schema with column aliases, \code{units}, default grouping settings, retained columns, and optional \code{defaults} for the external basifoR workflow.
preserve during processing, and auxiliary \code{defaults} that wrapper
Create a reusable schema with column aliases, \code{units}, default grouping settings, retained columns, and optional \code{defaults} for the external basifoR workflow.
functions may reuse across repeated calls.
Create a reusable schema with column aliases, \code{units}, default grouping settings, retained columns, and optional \code{defaults} for the external basifoR workflow.}
\usage{new_external_schema(colmap, 
    units = list(), levels = NULL, 
    keep_cols = NULL, 
    defaults = list())}
\arguments{
  \item{colmap}{Named \code{list} mapping standardized variables to one or more candidate source column names in the input data.}
  \item{units}{Named \code{list} of declared \code{units} for standardized variables, usually entries such as \code{list(d = "mm", h = "m")}.}
  \item{levels}{Optional \code{character} vector of default grouping variables for downstream summaries.}
  \item{keep_cols}{Optional \code{character} vector naming source columns that should be retained in downstream outputs.}
  \item{defaults}{Optional named \code{list} of auxiliary \code{defaults} or metadata that wrappers may reuse.}
}
\details{The returned object is lightweight. It validates the basic

structure of the inputs, assigns class

\code{c("external_schema", "list")}, and leaves semantic

interpretation to downstream helpers such as

\code{externalMetrics()}, \code{externalMetrics2Vol()}, or

\code{external_dendroMetrics()}.}
\value{An object of class \code{"external_schema"} containing normalized \code{colmap}, \code{units}, \code{levels}, \code{keep_cols}, and \code{defaults} components, ready to pass to external workflow wrappers.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
sch <- new_external_schema(
    colmap = list(
        plot = c("plot_id", "plot"),
        species = c("species_code", "sp"),
        d = c("dbh_mm", "diameter_mm"),
        h = c("height_m", "h")
    ),
    units = list(d = "mm", h = "m"),
    levels = "plot",
    keep_cols = c("plot_id", "species_code"),
    defaults = list(selector = "priority")
)

class(sch)
sch$colmap$d
sch$units
sch$levels
}
