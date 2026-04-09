print.inventory_design <- structure(function
### Display the main stored components of an \code{"inventory_design"}
### object in a compact human-readable summary.
(
    x,   ##<< Object of class \code{"inventory_design"}.
    ...  ##<< Further arguments passed to methods. Currently unused.
) {
    ##title<< Print a generic inventory plot design
    ##description<< Print a compact summary of an \code{"inventory_design"} object.
    ##description<< The method reports the stored design name, minimum DBH thresholds,
    ##description<< sampled areas, and trees-per-hectare expansion factors.
    ##details<<
    ##details<< This S3 method is intended for quick inspection of generic sampling
    ##details<< designs created with \code{\link{new_inventory_design}}.
    ##details<< It prints the design name, the minimum DBH thresholds that define
    ##details<< each tally tier, the corresponding sampled areas, and the stored
    ##details<< expansion factors. Arguments passed through \code{...} are currently
    ##details<< ignored.
    ##details<<
    ##details<< The method does not modify the object.
    ##value<< The input \code{"inventory_design"} object, returned invisibly.

    cat("Inventory design:", x$name, "\n")
    cat("Minimum DBH (cm):", paste(x$min_dbh_cm, collapse = ", "), "\n")
    cat("Sample area (m2):", paste(x$sample_area_m2, collapse = ", "), "\n")
    cat("Expansion factors:", paste(round(x$sf, 2), collapse = ", "), "\n")
    invisible(x)

}, ex = function() {
    dsg <- new_inventory_design(
        sample_area_m2 = c(400, 100),
        min_dbh_cm = c(20, 0),
        name = "Nested square design"
    )

    print(dsg)
})
