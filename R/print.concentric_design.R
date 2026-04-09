print.concentric_design <- structure(function
### Display the main components of a \code{"concentric_design"} object:
### design name, subplot radii, minimum diameters, and expansion factors.
(x,   ##<< Object of class \code{"concentric_design"}, typically
      ##<< created by \code{\link{new_concentric_design}}.
 ...  ##<< Further arguments passed to methods. Currently unused.
) {
    ##title<< Print a concentric plot design
    ##description<< Display the main components of a \code{"concentric_design"} object.
    ##description<< The printed summary reports the design name together with subplot radii, minimum DBH thresholds, and expansion factors for each tally tier.
    ##details<< This S3-style print method provides a quick inspection of concentric sampling designs used in forest inventories.
    ##details<< It prints the stored values in a concise, human-readable form and returns the input object invisibly.
    ##details<<
    ##details<< The function does not modify the design and does not currently use additional arguments passed through \code{...}.
    ##value<< The input \code{"concentric_design"} object, returned invisibly.

    cat("Concentric plot design:", x$name, "\n")
    cat("Radii (m):", paste(x$radii_m, collapse = ", "), "\n")
    cat("Minimum DBH (cm):", paste(x$min_dbh_cm, collapse = ", "), "\n")
    cat("Expansion factors:", paste(round(x$sf, 2), collapse = ", "), "\n")
    invisible(x)

}, ex = function() {
    dsg <- new_concentric_design(
        radii_m = c(5, 10, 15),
        min_dbh_cm = c(7.5, 22.5, 42.5),
        name = "Example concentric design"
    )

    print.concentric_design(dsg)
    invisible(NULL)
})
