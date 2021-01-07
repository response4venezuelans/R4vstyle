# locations of resource files in the package
pkg_resource <- function(...) {
  system.file(..., package = "unhcrstyle")
}

#' @name using
#' @rdname using
#' @title Load required packages for the notebook
#'
#'
#' @param  ... list of packages required
#'
#' @examples
#' \dontrun{
#' using('tidyverse','gganimate','gghighlight','ggpubr', 'dplyr', 'tidyr')
#' }
#'
#' @export using

using <- function(...) {
  libs <- unlist(list(...))
  req <- unlist(lapply(libs,require,character.only = TRUE))
  need <- libs[req == FALSE]
  if (length(need) > 0) {
    install.packages(need, repos = "http://cran.rstudio.com")
    lapply(need,require,character.only = TRUE)
  }
}

#' @name left_align
#' @rdname left_align
#' @title Left align text for title in ggplot2 object
#' @description Left align text
#'
#'
#' @param  plot_name name of the plot
#' @param  pieces elements like title, subtitle, caption
#'
#' @return ggplot2 object with aligned title
#'
#' @author https://github.com/bbc/bbplot/blob/master/R/finalise_plot.R
#'
#' @examples
#' \dontrun{
#' ggpubr::ggarrange(left_align(plot1, c("subtitle", "title", "caption")), ncol = 1, nrow = 1)
#' }
#'
#' @export left_align

left_align <- function(plot_name, pieces){
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  return(grob)
}

#' @name format_si
#' @rdname format_si
#' @title Format axis label
#' @description Helper function to format a vector of strings using
#'   SI prefix notation
#'
#'
#'
#' Format a vector of numeric values according
#' to the International System of Units.
#' http://en.wikipedia.org/wiki/SI_prefix
#'
#' Based on code by Ben Tupper
#' https://stat.ethz.ch/pipermail/r-help/2012-January/299804.html
#' Args:
#'
#'
#' @param \dots List of integer or numeric
#'   ...: Args passed to format()
#'
#' @return Formatted number.
#'
#' @author Someone
#'
#' @examples
#' format_si()
#'
#' @export format_si
#'
## a little help function to better format numbers
format_si <- function(...) {
  function(x) {
    limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                1e-9,  1e-6,  1e-3,  1e0,   1e3,
                1e6,   1e9,   1e12,  1e15,  1e18,
                1e21,  1e24)
    prefix <- c("y",   "z",   "a",   "f",   "p",
                "n",   "",   "m",   " ",   "k",
                "M",   "G",   "T",   "P",   "E",
                "Z",   "Y")

    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)

    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i == 0, which(limits == 1e0), i)

    paste(format(round(x/limits[i], 1),
                 trim = TRUE, scientific = FALSE, ...),
          prefix[i])
  }
}



#' @name template_resources
#' @rdname template_resources
#' @title change template ressources if required
#' @description   modified version of tufte:::template_resources that allows
#' us to specify the package where the resource can be found.
#' rather than being hardwired to "tufte"
#'
#' @export template_resources

template_resources = function(name, package, ...) {
  system.file('rmarkdown', 'templates', name, 'resources', ..., package = package)
}

gsub_fixed = function(...) gsub(..., fixed = TRUE)

pandoc2.0 = function() rmarkdown::pandoc_available('2.0')

generate_id2 <- function() {
  f1 <- file.path(tempdir(), "solution_idx")

  id <- ifelse(file.exists(f1), readLines(f1), "1")
  id_new <- as.character(as.integer(id) + 1)
  writeLines(text = id_new, con = f1)

  return(id)
}

#' @name toggle_script
#' @rdname toggle_script
#' @title javascript to be included for toggling solution visiblity
#' @description    There must be a more elegant way of including this,
#'  but it'll do for now.
#'
#' @export toggle_script
#'
toggle_script <- function() {

  return(
    paste("<script>
function toggle_visibility(id1, id2) {
var e = document.getElementById(id1);
var f = document.getElementById(id2);

e.style.display = ((e.style.display!='none') ? 'none' : 'block');

if(f.classList.contains('fa-plus-square')) {
    f.classList.add('fa-minus-square')
    f.classList.remove('fa-plus-square')
} else {
    f.classList.add('fa-plus-square')
    f.classList.remove('fa-minus-square')
}

}
</script>",
'<script>
var prevScrollpos = window.pageYOffset;
window.onscroll = function() {
    if ($(window).width() < 768) {
        var currentScrollPos = window.pageYOffset;
        if (prevScrollpos > currentScrollPos) {
            document.getElementById("navbar").style.top = "0";
        } else {
            document.getElementById("navbar").style.top = "-50px";
        }
        prevScrollpos = currentScrollPos;
    }
}
</script>',
#'<script>$("#mySidenav").BootSideMenu({pushBody:false, width:"25%"});</script>',
#'<script src="https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js"></script>',
'<!-- Latest compiled and minified CSS -->
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossorigin="anonymous">

        <!-- Optional theme
        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css" integrity="sha384-rHyoN1iRsVXV4nD0JutlnGaslCJuC7uwjduW9SVrLvRYooPp2bWYgmgJQIXwl/Sp" crossorigin="anonymous"> -->

            <!-- Latest compiled and minified JavaScript -->
            <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" integrity="sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa" crossorigin="anonymous"></script>',
sep = "\n")
  )
}
