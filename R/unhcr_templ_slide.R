#' @name unhcr_templ_slide
#' @rdname unhcr_templ_slide
#' @title UNHCR HTML slide
#'
#' Loads additional style and template file
#'
#' @references https://raw.githubusercontent.com/dr-harper/example-rmd-templates/master/R/my_html_format.R
#'
#' @param ... additional arguments provided to \code{moon_reader}
#' @return An R Markdown output format.
#'
#' @export unhcr_templ_slide


unhcr_templ_slide <- function(...) {

  # confirm deps ----
  if (!requireNamespace("xaringan", quietly = TRUE)) {
    stop("Package xaringan must be installed to use custom slide styles")
  }

  # locations of resource files in the package
  pkg_resource = function(...) {
    system.file(..., package = "unhcRstyle")
  }

  # create file paths to package assets ----
  css    <- pkg_resource("rmarkdown/resources/slide-styles-test.css")

  # call the base html_document function ----
  xaringan::moon_reader(
    seal = FALSE,
    css = css,
    ...
  )

}
