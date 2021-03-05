dbhMetric <- structure(function#DBH metrics
###This function can format tree diameters at breast height and tree
###heights according to the sampling design of the Spanish National
###Forest Inventory (SNFI). The function is used by other routines of
###\code{basifoR} to derive tree metrics, see Details
###section. Implementation of this function using data sets of the
###SNFI can be burdensome. Use \code{\link{dendroMetrics}} instead to
###recursively derive tree metrics.
                       ##details<< Replicates of tree diameter
                       ##\code{'d'} are averaged. The tree heights
                       ##\code{'h'} are formatted from \code{mm} to
                       ##\code{dm} for further evaluation of volume
                       ##equations. The basal areas are computed
                       ##transforming the diameters from \code{mm} to
                       ##\code{cm} and using the formula: \code{ba (m2
                       ##tree-1 ha-1) = pi * d(cm)^2 * (4 *
                       ##1E4)^-1}. The number of trees per hectare
                       ##\code{'n'} are calculated considering the
                       ##sample design of the NFI: each plot consists
                       ##of four concentric subplots with radii
                       ##\code{5, 10, 15,} and \code{25 m}. The
                       ##minimum diameters recorded in the subplots
                       ##are \code{7.5, 12.5, 22.5,} and \code{42.5
                       ##cm} respectively. Considering these, any of
                       ##four estimates is printed: \code{127.32,
                       ##31.83, 14.15}, or \code{5.09}.
(
    dbh,  ##<<\code{numeric}. Either diameters at breast height
          ##(\code{mm}) or tree heights (\code{m}). Vectors are
          ##averaged. Zero values are formatted to \code{NA}.
    met = 'd' ##<<\code{character}. Any of five metrics: mean diameter
              ##at breast height (\code{'d'}), basal area
              ##(\code{'ba'}), number of trees (\code{'n'}), or tree
              ##height (\code{'h'}). Default \code{'d'}.
    
) {
    
    fn <- function(x){
        br <- c(7.5, 12.5, 22.5, 42.5)#cm
        cp <- c(5, 10, 15, 25)
        sf <- (10^4)/(pi * cp^2)
        if (x <  br[1])             y <- NA
        if (x >= br[1] & x < br[2]) y <- sf[1]
        if (x >= br[2] & x < br[3]) y <- sf[2]
        if (x >= br[3] & x < br[4]) y <- sf[3]
        if (x >= br[4])             y <- sf[4]
        return(y)}
    
    if(!is.numeric(dbh))
        dbh <- as.numeric(as.character(dbh))
            dbh[dbh == 0] <- NA
        if (length(dbh) > 1)
            dbh <- mean(dbh, na.rm = TRUE)
            if(all(is.na(dbh))) y = NA
            if(!is.na(dbh)){
                if(met%in%'d')
                    y <- dbh
                if(met%in%c('ba','n')){
                    dbh <- conv_unit(
                        dbh, from = 'mm', to = 'cm')
                    y <- dbh^2}
                if(met%in%'ba')
                    y <- pi * y * (4 * 1E4)^-1
                if(met%in%'n')
                    y <- fn(x = dbh)
                if(met%in%'h')
                    y <- conv_unit(
                        dbh, from = 'm', to = 'dm')
            }
            
            return(y)
### \code{numeric}. A tree metric: mean diameter (\code{mm}), tree basal area
### (\code{m2 tree-1}), number of trees (dimensionless), or
### tree height (\code{dm}).
}, ex = function(){
    dbh <- dbhMetric(c(10.7, 11.5), 'h')# average tree height (dm) 
    
})
