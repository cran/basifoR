new_inventory_design <- structure(function
### Build a generic inventory design from tally areas and minimum DBH
### thresholds. The function stores each tally tier, computes its
### trees-per-hectare expansion factor, and returns an object of class
### \code{"inventory_design"} for use in inventory workflows.
(sample_area_m2, ##<< \code{numeric}. Positive sampled area, in square
                 ##<< metres, for each tally tier. Supply one value
                 ##<< per diameter threshold.
 min_dbh_cm = 0, ##<< \code{numeric}. Minimum diameter at breast height,
                 ##<< in \code{cm}, required for a tree to enter each
                 ##<< tally tier. Must have the same length as
                 ##<< \code{sample_area_m2}. The function sorts these
                 ##<< thresholds in ascending order and reorders the
                 ##<< paired sampled areas accordingly.
 name = "custom", ##<< \code{character(1)}. Human-readable label stored
                  ##<< in the returned design object.
 metadata = NULL ##<< Optional \code{list}. Extra design metadata stored
                 ##<< unchanged, for example plot shape, subplot radii,
                 ##<< side length, strip dimensions, or field notes.
) {
    ##title<< Create a generic inventory sampling design
    ##description<< Construct a generic \code{"inventory_design"} object from sampled areas and minimum DBH thresholds.
    ##description<< The function computes trees-per-hectare expansion factors for each tally tier and can represent circular,
    ##description<< square, strip, ring, or custom layouts through \code{metadata}.
    ##details<<
    ##details<< The constructor validates that \code{sample_area_m2} and
    ##details<< \code{min_dbh_cm} are numeric, non-empty, and of equal
    ##details<< length. It also requires all sampled areas to be strictly
    ##details<< positive.
    ##details<<
    ##details<< Internally, the function orders tiers by
    ##details<< \code{min_dbh_cm}. The returned \code{min_dbh_cm},
    ##details<< \code{sample_area_m2}, and \code{sf} vectors follow that
    ##details<< sorted order, where \code{sf = 10000 / sample_area_m2}.
    ##details<<
    ##details<< The object is shape-agnostic. Use \code{metadata} to record
    ##details<< how the sampled area was obtained, for example from
    ##details<< circular radii, square side lengths, strip dimensions, or
    ##details<< protocol notes. The constructor stores \code{metadata} as
    ##details<< supplied and does not validate or reorder its contents.

    if (!is.numeric(sample_area_m2) || !is.numeric(min_dbh_cm))
        stop("'sample_area_m2' and 'min_dbh_cm' must be numeric.")

    if (length(sample_area_m2) != length(min_dbh_cm))
        stop("'sample_area_m2' and 'min_dbh_cm' must have the same length.")

    if (length(sample_area_m2) == 0)
        stop("Design vectors cannot be empty.")

    if (any(sample_area_m2 <= 0))
        stop("'sample_area_m2' must contain positive values.")

    o <- order(min_dbh_cm)

    if (is.null(metadata))
        metadata <- list()

    structure(
        list(
            name = name,
            min_dbh_cm = min_dbh_cm[o],
            sample_area_m2 = sample_area_m2[o],
            sf = 1e4 / sample_area_m2[o],
            metadata = metadata
        ),
        class = "inventory_design"
    )
    ##value<< An object of class \code{"inventory_design"}, returned as a
    ##value<< named list with components \code{name},
    ##value<< \code{min_dbh_cm}, \code{sample_area_m2}, \code{sf}, and
    ##value<< \code{metadata}. \code{sf} gives the trees-per-hectare
    ##value<< expansion factor for each tally tier.

}, ex = function() {
    dsg <- new_inventory_design(
        sample_area_m2 = c(400, 100),
        min_dbh_cm = c(20, 0),
        name = "Nested square design",
        metadata = list(shape = "square", side_m = c(20, 10))
    )

    dsg$min_dbh_cm
    dsg$sf
})
