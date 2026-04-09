# considering parameter using the same name as in the documentation

# documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/documentador_sig_tcm30-536622.pdf
# see anexo 19, page 66

# dbh_mm is the tree dbh in mm
# dnm_mm is the mean plot dbh in mm
# h_t is the tree height in m


# VCC: Volumen maderable con corteza, en dm3

get_snfi_vcc <- function # SNFI merchantable volume over bark
### Compute SNFI VCC from tree diameter, height, and one coefficient row.
(dbh_mm, ##<< Tree diameter at breast height in millimetres.
 h_t, ##<< Tree height in metres.
 pars ##<< One-row \code{data.frame} of SNFI coefficients. The function
      ##<< assumes exactly one row with a \code{Modelo} field and the
      ##<< coefficients required by that model.
) {
  ##details<<
  ##details<< This function evaluates the SNFI equation for merchantable
  ##details<< volume over bark (VCC) and returns the result in dm3.
  ##details<< It currently supports models 1 and 11 from the SNFI
  ##details<< coefficient table.
  ##value<< Numeric VCC in dm3. Returns \code{NA_real_} when \code{pars}
  ##value<< is empty or when the model is not recognised.

  # check if pars available
  if(nrow(pars) == 0){
    return(NA_real_)
  }

  if(pars$Modelo == 1){
    # model 1
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    vcc <- a + b*dbh_mm^2*h_t
  } else if(pars$Modelo == 11){
    # model 11
    p <- as.numeric(pars$p)
    q <- as.numeric(pars$q)
    r <- as.numeric(pars$r)
    vcc <- p * dbh_mm^q * h_t^r
  } else {
    ## print("Model not recognized for VCC.")
    vcc <- NA_real_
  }

  return(vcc)
}


# VSC: Volumen maderable sin corteza, en dm3

get_snfi_vsc <- function # SNFI merchantable volume under bark
### Compute SNFI VSC from VCC and one coefficient row.
(vcc, ##<< Merchantable volume over bark in dm3.
 pars ##<< One-row \code{data.frame} of SNFI coefficients. The function
      ##<< assumes exactly one row with a \code{Modelo} field and the
      ##<< coefficients required by that model.
) {
  ##details<<
  ##details<< This function evaluates the SNFI equation for merchantable
  ##details<< volume under bark (VSC) and returns the result in dm3.
  ##details<< It currently supports model 7.
  ##value<< Numeric VSC in dm3. Returns \code{NA_real_} when \code{pars}
  ##value<< is empty or when the model is not recognised.

  # check if pars available
  if(nrow(pars) == 0){
    return(NA_real_)
  }

  if(pars$Modelo == 7){
    # model 7
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    c <- as.numeric(pars$c)
    vsc <- a + b*vcc + c*vcc^2
  } else {
    ## print("Model not recognized for VSC.")
    vsc <- NA_real_
  }

  return(vsc)
}


# IAVC: Incremento anual de volumen con corteza, en dm3

get_snfi_iavc <- function # SNFI annual increment of volume over bark
### Compute SNFI IAVC using the variables required by the selected model.
(dbh_mm = NULL, ##<< Tree diameter at breast height in millimetres, when required by the model.
 dnm_mm = NULL, ##<< Mean plot diameter in millimetres, when required by the model.
 h_t = NULL, ##<< Tree height in metres, when required by the model.
 vcc = NULL, ##<< Merchantable volume over bark in dm3, when required by the model.
 pars ##<< One-row \code{data.frame} of SNFI coefficients. The function
      ##<< assumes exactly one row with a \code{Modelo} field and the
      ##<< coefficients required by that model.
) {
  ##details<<
  ##details<< This function evaluates the SNFI equation for annual increment
  ##details<< of volume over bark (IAVC) and returns the result in dm3.
  ##details<< It supports models 8, 13, 14, 16, 17, 18, 19, 20, 21, and 25.
  ##details<< The required inputs depend on the selected model.
  ##value<< Numeric IAVC in dm3. Returns \code{NA_real_} when \code{pars}
  ##value<< is empty, when required inputs are missing, or when the model
  ##value<< is not recognised. Missing required inputs also trigger a
  ##value<< printed message.

  # check if pars available
  if(nrow(pars) == 0){
    return(NA_real_)
  }

  if(pars$Modelo == 8){
    # check vcc is provided
    if(is.null(vcc)){
      print("vcc must be provided for model 8.")
      return(NA_real_)
    }
    # model 8
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    c <- as.numeric(pars$c)
    iavc <- a + b*vcc + c*vcc^2
  } else if(pars$Modelo == 13){
    # check dbh_mm and dnm_mm are provided
    if(is.null(dbh_mm) || is.null(dnm_mm)){
      print("dbh_mm and dnm_mm must be provided for model 13.")
      return(NA_real_)
    }
    # model 13
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    iavc <- a + b*(dbh_mm + dnm_mm)
  } else if(pars$Modelo == 14){
    # check dbh_mm is provided
    if(is.null(dbh_mm)){
      print("dbh_mm must be provided for model 14.")
      return(NA_real_)
    }
    # model 14
    p <- as.numeric(pars$p)
    q <- as.numeric(pars$q)
    iavc <- p*dbh_mm^q
  } else if(pars$Modelo == 16){
    # check dbh_mm is provided
    if(is.null(dbh_mm)){
      print("dbh_mm must be provided for model 16.")
      return(NA_real_)
    }
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    # model 16
    iavc <- a + b*dbh_mm^2
  } else if(pars$Modelo == 17){
    # check dbh_mm is provided
    if(is.null(dbh_mm)){
      print("dbh_mm must be provided for model 17.")
      return(NA_real_)
    }
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    c <- as.numeric(pars$c)
    # model 17
    iavc <- a + b*dbh_mm + c*dbh_mm^2
  } else if(pars$Modelo == 18){
    # check dbh_mm is provided
    if(is.null(dbh_mm)){
      print("dbh_mm must be provided for model 18.")
      return(NA_real_)
    }
    # model 18 (IFN3)
    p <- as.numeric(pars$p)
    q <- as.numeric(pars$q)
    iavc <- p * exp(q*dbh_mm)
  } else if(pars$Modelo == 19){
    # check dbh_mm is provided
    if(is.null(dbh_mm)){
      print("dbh_mm must be provided for model 19.")
      return(NA_real_)
    }
    # model 19
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    c <- as.numeric(pars$c)
    d <- as.numeric(pars$d)
    iavc <- a + b*dbh_mm + c*dbh_mm^2 + d*dbh_mm^3
  } else if(pars$Modelo == 20){
    # check dbh_mm is provided
    if(is.null(dbh_mm)){
      print("dbh_mm must be provided for model 20.")
      return(NA_real_)
    }
    # model 20
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    d <- as.numeric(pars$d)
    iavc <- a + b*dbh_mm + d*dbh_mm^3
  } else if(pars$Modelo == 21){
    # check dbh_mm is provided
    if(is.null(dbh_mm)){
      print("dbh_mm must be provided for model 21.")
      return(NA_real_)
    }
    # model 21
    c <- as.numeric(pars$c)
    d <- as.numeric(pars$d)
    iavc <- c*dbh_mm^2 + d*dbh_mm^3
  } else if(pars$Modelo == 25){
    # check dbh_mm and h_t are provided
    if(is.null(dbh_mm) || is.null(h_t)){
      print("dbh_mm and h_t must be provided for model 25.")
      return(NA_real_)
    }
    # model 25
    p <- as.numeric(pars$p)
    q <- as.numeric(pars$q)
    r <- as.numeric(pars$r)
    iavc <- p*dbh_mm^q*h_t^r
  } else {
    ## print("Model not recognized for IAVC.")
    iavc <- NA_real_
  }

  # model 15 (IFN3)
  # iavc <- a + b*(cd - cdm)

  return(iavc)
}


# VLE: Volumen de leñas gruesas, en dm3

get_snfi_vle <- function # SNFI coarse woody volume
### Compute SNFI VLE from tree diameter or VCC and one coefficient row.
(dbh_mm = NULL, ##<< Tree diameter at breast height in millimetres, when required by the model.
 vcc = NULL, ##<< Merchantable volume over bark in dm3, when required by the model.
 pars ##<< One-row \code{data.frame} of SNFI coefficients. The function
      ##<< assumes exactly one row with a \code{Modelo} field and the
      ##<< coefficients required by that model.
) {
  ##details<<
  ##details<< This function evaluates the SNFI equation for coarse woody
  ##details<< volume (VLE) and returns the result in dm3.
  ##details<< It supports models 10 and 12.
  ##value<< Numeric VLE in dm3. Returns \code{NA_real_} when \code{pars}
  ##value<< is empty, when required inputs are missing, or when the model
  ##value<< is not recognised. Missing required inputs also trigger a
  ##value<< printed message.

  # check if pars available
  if(nrow(pars) == 0){
    return(NA_real_)
  }

  if(pars$Modelo == 10){
    # check vcc is provided
    if(is.null(vcc)){
      print("vcc must be provided for model 10.")
      return(NA_real_)
    }
    # model 10
    a <- as.numeric(pars$a)
    b <- as.numeric(pars$b)
    c <- as.numeric(pars$c)
    vle <- a + b*vcc + c*vcc^2
  } else if(pars$Modelo == 12){
    # check dbh_mm is provided
    if(is.null(dbh_mm)){
      print("dbh_mm must be provided for model 12.")
      return(NA_real_)
    }
    # model 12
    p <- as.numeric(pars$p)
    q <- as.numeric(pars$q)
    vle <- p*dbh_mm^q
  } else {
    ## print("Model not recognized for VLE.")
    vle <- NA_real_
  }

  return(vle)
}
