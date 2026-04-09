\name{get_snfi_vle}
\alias{get_snfi_vle}
\title{SNFI coarse woody volume}
\description{Compute SNFI VLE from tree diameter or VCC and one coefficient row.}
\usage{get_snfi_vle(dbh_mm = NULL, 
    vcc = NULL, pars)}
\arguments{
  \item{dbh_mm}{Tree diameter at breast height in millimetres, when required by the model.}
  \item{vcc}{Merchantable volume over bark in dm3, when required by the model.}
  \item{pars}{One-row \code{data.frame} of SNFI coefficients. The function
<< assumes exactly one row with a \code{Modelo} field and the
<< coefficients required by that model.}
}
\details{This function evaluates the SNFI equation for coarse woody

volume (VLE) and returns the result in dm3.

It supports models 10 and 12.}
\value{Numeric VLE in dm3. Returns \code{NA_real_} when \code{pars}

is empty, when required inputs are missing, or when the model

is not recognised. Missing required inputs also trigger a

printed message.
print("Model not recognized for VLE.")}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}





