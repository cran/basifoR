trees_per_ha <- structure(function
### Dispatch on the sampling-design class to return the expansion factor,
### expressed as trees per hectare, associated with a tree of a given
### diameter at breast height.
(
    design, ##<< Sampling design object. Supported methods currently include \code{"inventory_design"} and \code{"concentric_design"}.
    dbh_cm  ##<< \code{numeric}. Diameter at breast height in \code{cm}. When several values are supplied, methods first coerce them to numeric and then use their mean after removing missing values.
) {
    ##title<< Compute trees-per-hectare expansion factors from inventory designs
    ##description<< Return the expansion factor, expressed as trees per hectare, associated with a tree of a given diameter at breast height under a supported inventory design.
    ##details<<
    ##details<< \code{trees_per_ha()} is an S3 generic. Methods use the
    ##details<< class of \code{design} to decide how diameter thresholds and
    ##details<< expansion factors are matched.
    ##details<<
    ##details<< Current methods return \code{NA_real_} when diameter is
    ##details<< missing after preprocessing or falls below the smallest
    ##details<< supported threshold in the supplied design.
    ##value<< A single \code{numeric} value giving the trees-per-hectare
    ##value<< expansion factor for the supplied diameter and design, or
    ##value<< \code{NA_real_} when no valid factor can be assigned.

    UseMethod("trees_per_ha")

}, ex = function() {
    dsg <- new_inventory_design(sample_area_m2 = c(100, 400),
                                min_dbh_cm = c(0, 20))
    trees_per_ha(dsg, 13)
})


trees_per_ha.inventory_design <- structure(function #Compute trees per hectare for generic inventory designs
### Match a tree diameter to the tally tier of a generic
### \code{"inventory_design"} object and return the corresponding
### trees-per-hectare expansion factor.
(
    design, ##<< Object of class \code{"inventory_design"} created by helpers such as \code{\link{new_inventory_design}}.
    dbh_cm  ##<< \code{numeric}. Diameter at breast height in \code{cm}. When several values are supplied, the method uses their mean after removing missing values.
) {
    ##title<< Compute trees per hectare for generic inventory designs
    ##description<< Return the trees-per-hectare expansion factor for a tree measured under a generic inventory design.
    ##details<<
    ##details<< The method coerces \code{dbh_cm} to numeric, averages it when
    ##details<< several values are supplied, and then locates the matching
    ##details<< diameter tier with \code{findInterval()} over
    ##details<< \code{design$min_dbh_cm}.
    ##details<<
    ##details<< It returns \code{NA_real_} when all supplied diameters are
    ##details<< missing or when the resulting diameter is smaller than the
    ##details<< minimum threshold supported by the design.
    ##value<< A single \code{numeric} expansion factor in trees per hectare
    ##value<< taken from \code{design$sf}, or \code{NA_real_} when the tree
    ##value<< does not belong to any tally tier.

    dbh_cm <- as.numeric(dbh_cm)

    if (length(dbh_cm) > 1)
        dbh_cm <- mean(dbh_cm, na.rm = TRUE)

    if (all(is.na(dbh_cm)))
        return(NA_real_)

    if (is.na(dbh_cm) || dbh_cm < design$min_dbh_cm[1])
        return(NA_real_)

    idx <- findInterval(dbh_cm, design$min_dbh_cm)
    design$sf[idx]

}, ex = function() {
    dsg <- new_inventory_design(sample_area_m2 = c(100, 400),
                                min_dbh_cm = c(0, 20),
                                name = "Example design")
    trees_per_ha(dsg, 13)
})


trees_per_ha.concentric_design <- structure(function #Compute trees per hectare for concentric subplot designs
### Match a tree diameter to the appropriate subplot of a
### \code{"concentric_design"} object and return the corresponding
### trees-per-hectare expansion factor.
(
    design, ##<< Object of class \code{"concentric_design"} created by helpers such as \code{\link{new_concentric_design}} or \code{\link{snfi_design}}.
    dbh_cm  ##<< \code{numeric}. Diameter at breast height in \code{cm}. When several values are supplied, the method uses their mean after removing missing values.
) {
    ##title<< Compute trees per hectare for concentric subplot designs
    ##description<< Return the trees-per-hectare expansion factor for a tree measured under a concentric subplot design.
    ##details<<
    ##details<< The method coerces \code{dbh_cm} to numeric, averages it when
    ##details<< several values are supplied, and then uses
    ##details<< \code{findInterval()} over \code{design$min_dbh_cm} to select
    ##details<< the subplot-specific expansion factor stored in
    ##details<< \code{design$sf}.
    ##details<<
    ##details<< It returns \code{NA_real_} when all supplied diameters are
    ##details<< missing or when the resulting diameter is smaller than the
    ##details<< smallest subplot threshold.
    ##value<< A single \code{numeric} expansion factor in trees per hectare
    ##value<< taken from \code{design$sf}, or \code{NA_real_} when the tree
    ##value<< does not belong to any subplot tier.

    dbh_cm <- as.numeric(dbh_cm)

    if (length(dbh_cm) > 1)
        dbh_cm <- mean(dbh_cm, na.rm = TRUE)

    if (all(is.na(dbh_cm)))
        return(NA_real_)

    if (is.na(dbh_cm) || dbh_cm < design$min_dbh_cm[1])
        return(NA_real_)

    idx <- findInterval(dbh_cm, design$min_dbh_cm)
    design$sf[idx]

}, ex = function() {
    trees_per_ha(snfi_design(), 13)
})
