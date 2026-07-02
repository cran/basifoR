\name{asNFI_spatial_sf}
\alias{asNFI_spatial_sf}
\title{asNFI spatial sf}
\description{Reconstruct an sf object from tabular NFI data and its geometry sidecar.}
\usage{asNFI_spatial_sf(x, registry = getNFIgeometry_spatial(x), 
    na.action = c("keep", 
        "drop", "error"), 
    validate = TRUE)}
\arguments{
  \item{x}{Tabular NFI object to convert to an \code{sf} object.}
  \item{registry}{Plot-geometry \code{registry} used to match one geometry to each row.}
  \item{na.action}{Action for rows whose plot key has no matching geometry: retain, remove, or raise an error.}
  \item{validate}{Logical; whether to \code{validate} \code{registry} keys and geometry consistency before reconstruction.}
}



\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}





