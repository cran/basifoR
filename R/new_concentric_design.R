new_concentric_design <- structure(function#Construct a concentric subplot design
### Define a concentric inventory design from subplot radii and minimum
### diameter thresholds.
###
### The constructor orders subplot tiers by minimum DBH, converts radii
### to sampled area in square metres, computes trees-per-hectare
### expansion factors, and returns an object of class
### \code{"concentric_design"} inheriting from
### \code{"inventory_design"}.
(radii_m,       ##<< \code{numeric}. Subplot radii in metres, one value per
                ##<< tally tier.
 min_dbh_cm,    ##<< \code{numeric}. Minimum diameter at breast height in
                ##<< \code{cm} required for a tree to be tallied in each
                ##<< subplot. Must have the same length as
                ##<< \code{radii_m}. The function sorts these thresholds in
                ##<< ascending order and reorders the paired radii
                ##<< accordingly.
 name = "custom",##<< \code{character(1)}. Human-readable design name
                ##<< stored in the returned object.
 metadata = NULL ##<< Optional \code{list}. Additional design metadata
                ##<< merged with default entries for
                ##<< \code{shape = "circular"} and the ordered
                ##<< \code{radii_m}.
) {
    ##details<<
    ##details<< The constructor validates that \code{radii_m} and
    ##details<< \code{min_dbh_cm} are numeric, non-empty, and of equal
    ##details<< length.
    ##details<<
    ##details<< Internally, the function orders subplot tiers by
    ##details<< \code{min_dbh_cm}. The returned \code{radii_m},
    ##details<< \code{sample_area_m2}, \code{min_dbh_cm}, and
    ##details<< \code{sf} vectors follow that sorted order.
    ##details<< \code{sample_area_m2} is computed as
    ##details<< \code{pi * radii_m^2}, and \code{sf} as
    ##details<< \code{10000 / sample_area_m2}.
    ##details<<
    ##details<< The function delegates to \code{\link{new_inventory_design}}
    ##details<< and then appends the ordered \code{radii_m} vector plus the
    ##details<< class \code{"concentric_design"}. The \code{metadata}
    ##details<< argument is merged with default entries for circular plot
    ##details<< shape and ordered radii using
    ##details<< \code{utils::modifyList()}.
    ##value<< An object of class
    ##value<< \code{c("concentric_design", "inventory_design")}, returned
    ##value<< as a named list with components \code{name},
    ##value<< \code{min_dbh_cm}, \code{sample_area_m2}, \code{sf},
    ##value<< \code{metadata}, and \code{radii_m}. The
    ##value<< \code{sample_area_m2} component stores subplot areas in
    ##value<< square metres, \code{sf} stores the corresponding
    ##value<< trees-per-hectare expansion factors, and
    ##value<< \code{metadata} contains the merged design metadata.

    if (!is.numeric(radii_m) || !is.numeric(min_dbh_cm))
        stop("'radii_m' and 'min_dbh_cm' must be numeric.")

    if (length(radii_m) != length(min_dbh_cm))
        stop("'radii_m' and 'min_dbh_cm' must have the same length.")

    if (length(radii_m) == 0)
        stop("Design vectors cannot be empty.")

    o <- order(min_dbh_cm)

    if (is.null(metadata))
        metadata <- list()

    dsg <- new_inventory_design(
        sample_area_m2 = pi * radii_m[o]^2,
        min_dbh_cm = min_dbh_cm[o],
        name = name,
        metadata = utils::modifyList(
            list(shape = "circular", radii_m = radii_m[o]),
            metadata
        )
    )

    dsg$radii_m <- radii_m[o]
    class(dsg) <- c("concentric_design", "inventory_design")
    dsg

}, ex = function() {
    dsg <- new_concentric_design(
        radii_m = c(5, 10, 15, 25),
        min_dbh_cm = c(7.5, 12.5, 22.5, 42.5),
        name = "SNFI"
    )
    dsg
})
