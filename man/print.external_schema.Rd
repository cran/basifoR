\name{print.external_schema}
\alias{print.external_schema}
\title{Print an external schema summary}
\description{Display the column mappings, declared units, and default grouping levels stored in an \code{"external_schema"} object.}
\usage{\method{print}{external_schema}(x, 
    ...)}
\arguments{
  \item{x}{An \code{"external_schema"} object created by \code{\link{new_external_schema}}.}
  \item{\dots}{Additional arguments accepted for S3 compatibility and ignored by this method.}
}

\value{The input \code{"external_schema"} object, returned invisibly.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
sch <- new_external_schema(
    colmap = list(plot = "plot_id", d = "dbh_mm", h = "height_m"),
    units = list(d = "mm", h = "m"),
    levels = "plot_id"
)

print.external_schema(sch)
}
