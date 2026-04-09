dbhMetric <- structure(function
##title<< Compute diameter, basal area, tree density, or height from tree measurements
##description<< Convert raw tree diameter or height inputs into the compact metric formats used across basifoR workflows.
##details<<
##details<< \code{dbhMetric} is a lightweight formatter used by the package to derive a single tree-level metric from diameter-at-breast-height or height inputs.
##details<<
##details<< The function first coerces non-numeric inputs with \code{as.numeric(as.character())}, replaces zeros with \code{NA}, and, when several values are supplied, reduces them to their mean after removing missing values. If every supplied value is missing, the function returns \code{NA_real_}.
##details<<
##details<< Unit handling depends on \code{met}. For \code{"d"}, the function returns the mean diameter in \code{mm}. For \code{"ba"}, it converts diameter from \code{mm} to \code{cm} and returns basal area in \code{m^2} per tree. For \code{"n"}, it also converts diameter to \code{cm} and then uses \code{design} through \code{\link{trees_per_ha}} to obtain the trees-per-hectare expansion factor. For \code{"h"}, it treats the input as height in \code{m} and returns height in \code{dm}.
##details<<
##details<< The sampling design affects only \code{met = "n"}. For \code{"d"}, \code{"ba"}, and \code{"h"}, the returned value does not depend on \code{design}.
(dbh, ##<< \code{numeric}. Diameter at breast height in \code{mm}, or tree height in \code{m} when \code{met = "h"}. Non-numeric inputs are coerced, zeros are treated as missing, and vectors are averaged after that replacement.
 met = "d", ##<< \code{character(1)}. Metric to compute: mean diameter at breast height (\code{"d"}), basal area (\code{"ba"}), trees per hectare (\code{"n"}), or height (\code{"h"}).
 design = snfi_design() ##<< Object inheriting from \code{"inventory_design"}, used only when \code{met = "n"} to derive the trees-per-hectare expansion factor. The default is the Spanish National Forest Inventory concentric subplot design.
) {

    if (!is.numeric(dbh))
        dbh <- as.numeric(as.character(dbh))

    dbh[dbh == 0] <- NA_real_

    if (length(dbh) > 1)
        dbh <- mean(dbh, na.rm = TRUE)

    if (all(is.na(dbh)))
        return(NA_real_)

    if (!met %in% c("d", "ba", "n", "h"))
        stop("'met' must be one of 'd', 'ba', 'n', or 'h'.")

    if (met %in% "d")
        return(dbh)

    if (met %in% c("ba", "n"))
        dbh <- conv_unit(dbh, from = "mm", to = "cm")

    if (met %in% "ba")
        return(pi * dbh^2 * (4 * 1E4)^-1)

    if (met %in% "n")
        return(trees_per_ha(design = design, dbh_cm = dbh))

    if (met %in% "h")
        return(conv_unit(dbh, from = "m", to = "dm"))

    ##value<< A single \code{numeric} value. Returns mean diameter in \code{mm} for \code{met = "d"}, basal area in \code{m^2} per tree for \code{met = "ba"}, trees per hectare for \code{met = "n"}, and height in \code{dm} for \code{met = "h"}. Returns \code{NA_real_} when all supplied values are missing or become missing after zero replacement.

}, ex = function() {
    dbhMetric(300, "d")
    dbhMetric(300, "ba")
    dbhMetric(18, "h")

    dsg <- new_concentric_design(
        radii_m = c(4, 8, 12),
        min_dbh_cm = c(5, 15, 30),
        name = "3-subplot design"
    )

    dbhMetric(130, "n", design = dsg)
})
