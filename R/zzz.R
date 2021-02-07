## alpha element_blank element_line element_rect element_text unit

.onLoad <- function(libname = find.package("r4vstyle"), pkgname = "r4vstyle") {


  # CRAN Note avoidance
  if (getRversion() >= "2.15.1")
    utils::globalVariables(
      # used to remove note when doing devtools::check(document = FALSE, args = c('--as-cran'))
      c("alpha", "element_blank", "element_line", "element_rect", "element_text", "unit",
        "censor", "is.formula", "is.sec_axis", "is.waive manual_pal"
      )
    )


}
