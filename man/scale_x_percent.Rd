% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unhcr_ggplot_util.R
\name{scale_x_percent}
\alias{scale_x_percent}
\title{Helper functions to stylize axis labels}
\usage{
scale_x_percent(
  name = waiver(),
  breaks = waiver(),
  minor_breaks = waiver(),
  guide = waiver(),
  n.breaks = NULL,
  labels,
  limits = NULL,
  expand = c(0.01, 0),
  oob = censor,
  na.value = NA_real_,
  trans = "identity",
  position = "bottom",
  sec.axis = waiver(),
  accuracy = 1,
  scale = 100,
  prefix = "",
  suffix = "\%",
  big.mark = " ",
  decimal.mark = ".",
  trim = TRUE,
  ...
)
}
\arguments{
\item{name}{The name of the scale. Used as axis or legend title. If
`waiver()`, the default, the name of the scale is taken from the first
mapping used for that aesthetic. If `NULL`, the legend title will be
omitted.}

\item{breaks}{One of:
- `NULL` for no breaks
- `waiver()` for the default breaks computed by the
  transformation object
- A numeric vector of positions
- A function that takes the limits as input and returns breaks
  as output}

\item{minor_breaks}{One of:
- `NULL` for no minor breaks
- `waiver()` for the default breaks (one minor break between
  each major break)
- A numeric vector of positions
- A function that given the limits returns a vector of minor breaks.}

\item{guide}{guide    A function used to create a guide or its name. See [guides()] for more information.}

\item{n.breaks}{An integer guiding the number of major breaks. The algorithm may choose a
slightly different number to ensure nice break labels. Will only have an effect if
`breaks = waiver()`. Use NULL to use the default number of breaks given by the transformation.}

\item{labels}{Specifying overrides the default format (i.e. you really don't
want to do that). `NULL` means no labels.}

\item{limits}{A numeric vector of length two providing limits of the scale.
Use `NA` to refer to the existing minimum or maximum.}

\item{expand}{same as in ggplot2}

\item{oob}{Function that handles limits outside of the scale limits
(out of bounds). The default replaces out of bounds values with NA.}

\item{na.value}{If `na.translate = TRUE`, what value aesthetic
value should missing be displayed as? Does not apply to position scales
where `NA` is always placed at the far right.}

\item{trans}{Either the name of a transformation object, or the
object itself. Built-in transformations include "asn", "atanh",
"boxcox", "exp", "identity", "log", "log10", "log1p", "log2",
"logit", "probability", "probit", "reciprocal", "reverse" and "sqrt".}

\item{position}{The position of the axis. "left" or "right" for vertical
scales, "top" or "bottom" for horizontal scales}

\item{sec.axis}{specify a secondary axis}
}
\description{
Directly from hrbrtheme
 Change axis labels to percent or stylize the thousand separator
X & Y scales with opinionated pre-sets for percent & comma label formats

The `_comma` ones set comma format for axis text and `expand=c(0,0)` (you need to set limits).

The `_percent` ones set precent format for axis text and `expand=c(0,0)` (you need to set limits).
}
