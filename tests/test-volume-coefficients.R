## Volume coefficient selection, legacy fallback and group completeness.
## Runs offline on small synthetic tree tables.

library(basifoR)

mk_trees <- function(especie, pr) {
    x <- data.frame(
        pr = pr, nfi.nr = 4, Especie = especie, Estadillo = "P1",
        Dn1 = c(210, 300), Dn2 = c(208, 302), Ht = c(15, 18)
    )
    class(x) <- c("readNFI", class(x))
    attr(x, "nfi.nr") <- 4
    x
}

vol <- function(x, ...) {
    warns <- character(0)
    out <- withCallingHandlers(
        metrics2Vol(nfiMetrics(x, var = c("d", "h", "ba", "n")),
                    track_provenance = TRUE, ...),
        warning = function(w) {
            warns <<- c(warns, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )
    list(out = out, warnings = warns)
}

cylinder <- function(x) pi / 4 * (rowMeans(x[, c("Dn1", "Dn2")]) / 1000)^2 * x$Ht

## 1. A species with neither official nor legacy coefficients returns NA with
##    a warning naming the species and the province. It must not borrow the
##    coefficients of another species.
r <- vol(mk_trees(9999, 28), parametro = "VCC")
stopifnot(
    all(is.na(r$out$vcc)),
    all(r$out$vcc_source == "missing"),
    all(r$out$vcc_status == "no_species_coefficients"),
    any(grepl("species 9999 in province 28", r$warnings, fixed = TRUE))
)

## 2. A listed species is unchanged. Reference values are the 0.7.9 results.
r <- vol(mk_trees(21, 28), parametro = "VCC")
stopifnot(
    isTRUE(all.equal(r$out$vcc, c(0.2514536, 0.5561345), tolerance = 1e-6)),
    all(r$out$vcc_source == "equation"),
    all(r$out$vcc_status == "ok"),
    length(r$warnings) == 0L
)

## 3. A species with no official row in its province but with legacy
##    coefficients falls back to the legacy equation of the same species,
##    labelled as a fallback. With legacy_fallback = FALSE it returns NA.
x <- mk_trees(26, 2)
r <- vol(x, parametro = "VCC")
stopifnot(
    all(!is.na(r$out$vcc)),
    all(r$out$vcc_source == "fallback_legacy"),
    all(r$out$vcc_status == "no_species_coefficients"),
    any(grepl("species 26 in province 2", r$warnings, fixed = TRUE))
)
r0 <- vol(x, parametro = "VCC", legacy_fallback = FALSE)
stopifnot(
    all(is.na(r0$out$vcc)),
    all(r0$out$vcc_source == "missing")
)

## 4. When no coefficients exist for the province and cycle, VCC falls back
##    to the legacy volume and is labelled as a fallback, never ok.
r <- vol(mk_trees(21, 4), parametro = "VCC", keep.legacy = TRUE)
stopifnot(
    all(!is.na(r$out$vcc)),
    isTRUE(all.equal(r$out$vcc, r$out$v)),
    all(r$out$vcc_source == "fallback_legacy"),
    all(r$out$vcc_status != "ok")
)

## 5. Legacy volumes use height in m: they stay below the enclosing cylinder
##    and close to the official equation for the same tree.
x <- mk_trees(21, 28)
r <- vol(x, parametro = c("VCC", "V"))
stopifnot(
    all(r$out$v < cylinder(x)),
    all(abs(r$out$v / r$out$vcc - 1) < 0.25)
)

## 6. With track_provenance = TRUE, grouped summaries report the share of
##    expansion factor whose volume is missing. The default summary keeps the
##    0.7.9 column set, with no *_na_share column.
x <- rbind(mk_trees(21, 28), mk_trees(9999, 28))
s <- suppressWarnings(inventoryMetrics(x))
stopifnot(!any(grepl("_na_share$", names(s))))
s <- suppressWarnings(inventoryMetrics(x, track_provenance = TRUE))
stopifnot(
    "vcc_na_share" %in% names(s),
    isTRUE(all.equal(s$vcc_na_share, 0.5))
)

## 7. Summaries without any volume variable still work and add no share column.
m <- nfiMetrics(mk_trees(21, 28), var = c("d", "h", "ba", "n"))
s <- dendroMetrics(m, summ.vr = "Estadillo")
stopifnot(!any(grepl("_na_share$", names(s))))

## 7b. The legacy form class is chosen per province and species. In Toledo
##     (province 45) Quercus suber (46) has only form class 41, which is not
##     the most frequent form class of a mixed dataset. Before the fix its
##     legacy volume was dropped when it shared a dataset with other species.
x <- rbind(mk_trees(46, 45), mk_trees(45, 45), mk_trees(45, 45))
x$Estadillo <- "P1"
r <- vol(x, parametro = c("VCC", "V"))
stopifnot(all(!is.na(r$out$v)))
r46 <- vol(mk_trees(46, 45), parametro = "V")
stopifnot(isTRUE(all.equal(r$out$v[r$out$Especie == 46], r46$out$v)))

## 8. External path: a source volume column is used only when the schema maps
##    v explicitly. Real French NFI rows (ARBRE.csv, rows 72878 to 72905 of the
##    2024 export) where IGN's own volume V is filled and many heights are
##    missing. Before 0.8.0, trees without height got IGN's V copied into v.
arbre <- structure(list(IDP = c(1900004L, 1900004L, 1900004L, 1900004L,
1900004L, 1900004L, 1900004L, 1900004L, 1900004L, 1900004L, 1900004L,
1900004L, 1900004L, 1900024L, 1900024L, 1900024L, 1900024L, 1900024L,
1900024L, 1900024L, 1900024L, 1900024L, 1900024L, 1900024L, 1900024L,
1900024L, 1900024L, 1900024L), ESPAR = c("04", "04", "04", "04",
"02", "04", "04", "04", "02", "04", "04", "04", "04", "64", "64",
"64", "64", "64", "64", "62", "22M", "64", "64", "52", "64",
"64", "64", "64"), C13 = c(0.641, 0.413, 0.887, 0.455, 0.898,
0.236, 0.601, 0.602, 0.649, 0.838, 0.855, 0.937, 1.291, 1.343,
1.033, 1.235, 1.49, 0.656, 0.785, 0.249, 1.423, 1.409, 1.267,
0.715, 0.984, 0.774, 1.612, 0.545), HTOT = c(NA, NA, 22.3, NA,
18.7, 7.8, 19.2, NA, 17.8, NA, 21.5, NA, 23.5, NA, NA, NA, 35.4,
20.9, NA, 6.5, 23.8, NA, NA, NA, 26.9, NA, 32.8, NA), V = c(0.27538928,
0.09344062, 0.45856896, 0.12015819, 0.47965467, 0.010180898,
0.2366347, 0.2375659, 0.27143562, 0.43874496, 0.45979017, 0.5212414,
1.092826, 2.0255249, 1.0154282, 1.6386406, 2.61333, 0.31024498,
0.48402473, 0.011802327, 1.5857633, 2.2811675, 1.7490538, 0.2532727,
0.892171, 0.4656963, 2.8083766, 0.18552487)), class = "data.frame",
row.names = 72878:72905)
arbre$D13_cm <- arbre$C13 * 100 / pi

## Design, schema and volume method as in the paper's French example.
design <- new_concentric_design(radii_m = c(6, 9, 15),
                                min_dbh_cm = c(7.5, 22.5, 37.5),
                                name = "French NFI")
schema <- new_external_schema(
    colmap = list(plot = "IDP", species = "ESPAR", d = "D13_cm", h = "HTOT"),
    units = list(d = "cm", h = "m"))
pars_v <- data.frame(a = 0.00008, b = 1.85, c = 0.92)
ext_methods <- external_volume_method_registry(list(
    V = new_volume_method(
        output = "v", unit = "m3", pars = pars_v,
        fun = function(dbh_mm, h_m, pars)
            pars$a * (dbh_mm / 10)^pars$b * h_m^pars$c,
        build_args = function(ctx, pars, resolved) {
            if (is.na(ctx$d_mm) || is.na(ctx$h_m)) NULL else
                list(dbh_mm = ctx$d_mm, h_m = ctx$h_m, pars = pars)
        })))

## Input that already carries standardized metrics and keeps V. This is the
## case where 0.7.9 copied IGN's V into v for the trees without height.
m <- externalMetrics(arbre, var = c("d", "h", "ba", "n"), design = design,
                     colmap = list(d = "D13_cm", h = "HTOT"), d_unit = "cm",
                     levels = "IDP", keep_cols = c("ESPAR", "V"))
r <- suppressWarnings(externalMetrics2Vol(m, parametro = "V",
                                          method_registry = ext_methods,
                                          track_provenance = TRUE))
no_h <- is.na(m$h)
stopifnot(
    "V" %in% names(m),
    any(no_h),
    all(is.na(r$v[no_h])),
    all(r$v_source[no_h] == "missing"),
    all(!is.na(r$v[!no_h])),
    all(r$v_source[!no_h] == "equation")
)

## The same through the paper's route.
tv <- suppressWarnings(external_dendroMetrics(
    arbre, summ.vr = NULL, var = c("d", "h", "ba", "n", "v"),
    design = design, schema = schema, method_registry = ext_methods,
    track_provenance = TRUE))
stopifnot(all(is.na(tv$v[is.na(tv$h)])))

## Without a volume method and without a v mapping, v is NA everywhere.
tv0 <- suppressWarnings(external_dendroMetrics(
    arbre, summ.vr = NULL, var = c("d", "h", "ba", "n", "v"),
    design = design, schema = schema, track_provenance = TRUE))
stopifnot(all(is.na(tv0$v)))

## When the schema maps v explicitly, the source column is used.
schema_v <- new_external_schema(
    colmap = list(plot = "IDP", species = "ESPAR", d = "D13_cm", h = "HTOT",
                  v = "V"),
    units = list(d = "cm", h = "m", v = "m3"))
tvm <- suppressWarnings(external_dendroMetrics(
    arbre, summ.vr = NULL, var = c("d", "h", "ba", "n", "v"),
    design = design, schema = schema_v, track_provenance = TRUE))
stopifnot(isTRUE(all.equal(sort(tvm$v), sort(arbre$V))))

cat("volume coefficient tests passed\n")
