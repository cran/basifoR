\name{dbhMetric}
\alias{dbhMetric}
\title{DBH metrics}
\description{This function can format tree diameters at breast height and tree
heights according to the sampling design of the Spanish National
Forest Inventory (SNFI). The function is used by other routines of
\code{basifoR} to derive tree metrics, see Details
section. Implementation of this function using data sets of the
SNFI can be burdensome. Use \code{\link{dendroMetrics}} instead to
recursively derive tree metrics.}
\usage{dbhMetric(dbh, met = "d")}
\arguments{
  \item{dbh}{\code{numeric}. Either diameters at breast height
(\code{mm}) or tree heights (\code{m}). Vectors are
averaged. Zero values are formatted to \code{NA}.}
  \item{met}{\code{character}. Any of five metrics: mean diameter
at breast height (\code{'d'}), basal area
(\code{'ba'}), number of trees (\code{'n'}), or tree
height (\code{'h'}). Default \code{'d'}.}
}
\details{Replicates of tree diameter
\code{'d'} are averaged. The tree heights
\code{'h'} are formatted from \code{mm} to
\code{dm} for further evaluation of volume
equations. The basal areas are computed
transforming the diameters from \code{mm} to
\code{cm} and using the formula: \code{ba (m2
tree-1 ha-1) = pi * d(cm)^2 * (4 *
1E4)^-1}. The number of trees per hectare
\code{'n'} are calculated considering the
sample design of the NFI: each plot consists
of four concentric subplots with radii
\code{5, 10, 15,} and \code{25 m}. The
minimum diameters recorded in the subplots
are \code{7.5, 12.5, 22.5,} and \code{42.5
cm} respectively. Considering these, any of
four estimates is printed: \code{127.32,
31.83, 14.15}, or \code{5.09}.}
\value{\code{numeric}. A tree metric: mean diameter (\code{mm}), tree basal area
(\code{m2 tree-1}), number of trees (dimensionless), or
tree height (\code{dm}).}

\author{Wilson Lara <wilarhen@gmail.com>, Cristobal Ordonez <a_cristo@pvs.uva.es>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
dbh <- dbhMetric(c(10.7, 11.5), 'h')# average tree height (dm) 

}
