\name{get_snfi_vsc}
\alias{get_snfi_vsc}
\title{SNFI merchantable volume under bark}
\description{Compute SNFI VSC from VCC and one coefficient row.}
\usage{get_snfi_vsc(vcc, pars)}
\arguments{
  \item{vcc}{Merchantable volume over bark in dm3.}
  \item{pars}{One-row \code{data.frame} of SNFI coefficients. The function
<< assumes exactly one row with a \code{Modelo} field and the
<< coefficients required by that model.}
}
\details{This function evaluates the SNFI equation for merchantable

volume under bark (VSC) and returns the result in dm3.

It currently supports model 7.}
\value{Numeric VSC in dm3. Returns \code{NA_real_} when \code{pars}

is empty or when the model is not recognised.
print("Model not recognized for VSC.")}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}





