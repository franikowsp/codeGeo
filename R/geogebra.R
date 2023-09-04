#' GeoGebra HTML Widget
#'
#' Provides the GeoGebra interface for a ggb file.
#'
#' @import htmlwidgets
#'
#' @export
geogebra <- function(ggb, width = NULL, height = NULL, elementId = NULL) {
  # forward options using x
  input <- list(
    ggb = ggb
  )

  # create widget
  htmlwidgets::createWidget(
    name = "geogebra",
    input,
    width = width,
    height = height,
    package = "codeGeo",
    elementId = elementId
  )
}

#' Shiny bindings for geogebra
#'
#' Output and render functions for using mywidget within Shiny
#' applications and interactive Rmd documents.
#'
#' @param output_id output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a mywidget
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name geogebra-shiny
#'
#' @export
geogebraOutput <- function(output_id, width = "100%", height = "100%") {
  htmlwidgets::shinyWidgetOutput(output_id, "geogebra", width, height, package = "codeGeo")
}

#' @rdname geogebra-shiny
#' @export
renderGeogebra <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, geogebraOutput, env, quoted = TRUE)
}
