\name{filter_gadm_province_spatial}
\alias{filter_gadm_province_spatial}
\title{filter gadm province spatial}
\description{Filter a Spanish GADM layer using a \code{strict} province identity resolver.}
\usage{filter_gadm_province_spatial(adm, 
    prov, province_table = NULL, 
    level = attr(adm, 
        "gadm_level", 
        exact = TRUE), 
    strict = TRUE)}
\arguments{
  \item{adm}{An \code{sf} object returned by \code{gadm_spatial()} or a
user-provided GADM-like boundary layer.}
  \item{prov}{Province code or name. Numeric codes are matched directly.
Non-numeric names are resolved against a clean GADM-oriented
province-name lookup and, only as a last resort, against
basifoR's \code{procods} code resolver.}
  \item{province_table}{Optional province lookup table used only by
the fallback basifoR code resolver. It is not
used as a list of GADM aliases.}
  \item{level}{GADM \code{level} of
\code{adm}. Level 2 returns one province. Higher
levels return features whose \code{NAME_2}
belongs to the province, for example
municipalities inside a province.}
  \item{strict}{\code{logical}. If \code{TRUE}, level-2 matches must
return one province. This prevents false matches from
short aliases such as \code{"ZA"}, which could match
both Zamora and Zaragoza.}
}
\details{This function deliberately does not use all aliases stored in
\code{procods} for GADM filtering. The \code{procods} table can
contain operational curation and province redirections used by
basifoR/MITECO workflows. Boundary filtering needs geographic
province identity. Therefore \code{procods} is used, at most,
to resolve the input to a numeric province code; the final GADM
match uses exact normalized matching against \code{NAME_2} and
a clean province-name lookup. This avoids false matches such as
Zamora also selecting Zaragoza through a short alias.}
\value{A filtered \code{sf} object. For GADM \code{level} 2, the object should
contain one province feature. For higher levels, the object may
contain several subprovince features. Attributes store the
resolved province code, GADM name, and aliases tested.
Exact normalized matching only. Do not use grepl() with short aliases.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}



\seealso{gadm_spatial, readNFI_spatial}
\examples{
if (interactive()) {
    adm <- gadm_spatial("ESP", level = 2, path = tempdir())
    zamora <- filter_gadm_province_spatial(adm, 49)
    nrow(zamora)
}
}
