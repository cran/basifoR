\name{dendroMetrics}
\alias{dendroMetrics}
\title{Summarize dendrometrics}
\description{This function can summarize dendrometric data of the Spanish
National Forest Inventory (SNFI). It can also control most other
functions of the package. Dendrometric variables in the outputs are
transformed into stand units, see the Details section.}
\usage{dendroMetrics(nfi, summ.vr = "Estadillo", 
    cut.dt = "d == d", 
    report = FALSE, ...)}
\arguments{
  \item{nfi}{\code{character} or \code{data.frame}.  URL/path to a
compressed SNFI file (.zip) having data of either
.dbf or .mdb file extensions; or data frame such as that
produced by \code{\link{nfiMetrics}}; or data frame such
as that produced by \code{\link{readNFI}}.}
  \item{summ.vr}{\code{character} or \code{NULL}. Name
of a Categorical variables in the SNFI
data used to summarize the outputs. If
\code{NULL} then output from
\code{\link{metrics2Vol}} is
returned. Default \code{'Estadillo'}
processes sample plots.}
  \item{cut.dt}{\code{character}. Logical condition used
to subset the output. Default \code{'d == d'}
avoids subsetting.}
  \item{report}{\code{logical}. Print a \code{report} of the output
in the current working directory.}
  \item{\dots}{Additional arguments in \code{\link{metrics2Vol}} or
\code{\link{nfiMetrics}} or \code{\link{readNFI}}.}
}
\details{Dendrometric variables are
summarized according to the levels of
the argument \code{summ.vr}. The summary
outputs include the categorical columns
formulated in \code{summ.vr} and the
variables defined using
arguments/defaults in
\code{\link{nfiMetrics}}. These
variables include the tree basal area
\code{ba} (\code{'m2 ha-1'}), the
average diameter at breast height
\code{d} (\code{'cm'}), the quadratic
mean diameter \code{dg} (\code{'cm'}),
the average tree height \code{h}
(\code{'m'}), the number of trees by
hectare \code{n} ('dimensionless'), and
the over bark volume \code{v} (\code{'m3
ha-1'}). Subsets of the output summary
are extracted using logical expressions
in argument \code{'cut.dt'}, see syntax
in \code{\link{Logic}}.}
\value{\code{data.frame}. Depending on \code{summ.vr = NULL}, an output from
\code{\link{metrics2Vol}}, or a summary of the variables, see
Details section.}

\author{Wilson Lara <wilarhen@gmail.com>, Cristobal Ordonez <a_cristo@pvs.uva.es>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
## SNFI Data from the province of Madrid
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
rmad <- readNFI(madridNFI)[1:100,]
mmad <- nfiMetrics(rmad)
vmad <- metrics2Vol(mmad)
dmad <- dendroMetrics(vmad, cut.dt = 'h > 8')
head(dmad)
## see metric units
attr(dmad,'units')

## Retrieval of SNFI data in 'www.miteco.gob.es' and computation
## of the corresponding dendrometric summary:

\donttest{
path <- '/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip'
url2 <- httr::modify_url("https://www.miteco.gob.es", path = path)
dmad <- dendroMetrics(url2, cut.dt = 'h >= 11')
head(dmad)
attr(dmad, 'units')
}

}
