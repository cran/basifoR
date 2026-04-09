\name{snfi_design}
\alias{snfi_design}
\title{Return the default SNFI concentric subplot design}
\description{Return the predefined concentric sampling design used by \code{basifoR} for Spanish National Forest Inventory workflows.}
\usage{snfi_design()}
\details{Use this function when you want the default SNFI design object explicitly, for example before calling \code{\link{trees_per_ha}} or when inspecting the design used by SNFI-oriented workflows.}
\value{An object of class \code{"concentric_design"} that also inherits from \code{"inventory_design"}. It stores the default SNFI subplot radii, minimum DBH thresholds, sampled areas, expansion factors, design name, and metadata.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
snfi_design()
}
