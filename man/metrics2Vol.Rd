\name{metrics2Vol}
\alias{metrics2Vol}
\title{Tree volumes in NFI data}
\description{This function computes over bark volumes (\code{'m3'}) processing
tree metrics from databases of the SNFI data and using volume
equations established in 2nd NFI, see Details section. To derive
dendrometric summaries use \code{\link{dendroMetrics}}.}
\usage{metrics2Vol(nfi, cub.met = "freq", 
    keep.var = FALSE, 
    ...)}
\arguments{
  \item{nfi}{\code{character} or \code{data.frame}.  URL/path to a
compressed file of the NFI (.zip) having data of either
.dbf or .mdb file extensions; or data frame such as that
produced by \code{\link{nfiMetrics}}; or data frame such
as that produced by \code{\link{readNFI}}.}
  \item{cub.met}{\code{character}. Cubication
method. Default \code{'freq'} implements the
equation form that most frequently match the
data, see \code{details} section. Additional
methods have not yet been implemented.}
  \item{keep.var}{\code{logical}. Keep the variables used to
compute the volumes. Default \code{FALSE}.}
  \item{\dots}{Depending on \code{'nfi'}, additional arguments in
\code{\link{metrics2Vol}} or \code{\link{nfiMetrics}} or
\code{\link{readNFI}}.}
}
\details{The quations from the second SNFI
used have the forms \code{'v ~ par1 + par2
* (d^2) * h'}, and \code{'v ~ par1 *
(d^par2) * (h^par3)'}. These equations
estimate over bark volumes in \code{'dcm3'}
but the function transform the units so the
output volumes have \code{'m3'} units. The
function assign equation forms and
parameters depending on several variables,
including the provincial unit, the tree
species, the tree diameter, and the tree
height. Consequently, objects from
\code{\link{nfiMetrics}} must have these
variables.}
\value{\code{data.frame}. Depending on \code{keep.var}, short or expanded
data set.  Short data sets contain the volumes \code{v}
(\code{'m3'}) plus the metrics defined in
\code{\link{nfiMetrics}}. The expanded data contains additional
columns with the variables used to compute the volumes.}

\author{Wilson Lara <wilarhen@gmail.com>, Cristobal Ordonez <a_cristo@pvs.uva.es>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
rmad <- readNFI(madridNFI)[1:10,]
vmad <- metrics2Vol(rmad)
head(vmad)
## see metric units
attr(vmad,'units')
}
