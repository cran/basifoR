snfi_design <- structure(function
##title<< Return the default SNFI concentric subplot design
##description<< Return the predefined concentric sampling design used by \code{basifoR} for Spanish National Forest Inventory workflows.
##details<< This helper has no arguments because it always returns the standard SNFI tree-plot design bundled in the package.
##details<<
##details<< The design comprises four concentric circular subplots with radii of 5, 10, 15, and 25 \code{m}. Trees enter each subplot tier at minimum diameter-at-breast-height thresholds of 7.5, 12.5, 22.5, and 42.5 \code{cm}, respectively.
##details<<
##details<< Internally, the function delegates construction to \code{\link{new_concentric_design}}, which derives subplot areas and expansion factors from the supplied radii and thresholds.
##details<<
##details<< Use this function when you want the default SNFI design object explicitly, for example before calling \code{\link{trees_per_ha}} or when inspecting the design used by SNFI-oriented workflows.
##value<< An object of class \code{"concentric_design"} that also inherits from \code{"inventory_design"}. It stores the default SNFI subplot radii, minimum DBH thresholds, sampled areas, expansion factors, design name, and metadata.
() {
    new_concentric_design(
        radii_m = c(5, 10, 15, 25),
        min_dbh_cm = c(7.5, 12.5, 22.5, 42.5),
        name = "SNFI"
    )
}, ex = function() {
    snfi_design()
})
