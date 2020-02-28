#if(getRversion() >= "3.6.0") utils::globalVariables("annoResults")
#' @name cpvaWeb
#' @title Visualization of the chromatographic peaks
#' @description  The cpvaWeb was used to build interactive web pages with Rshiny, and visualization of the chromatographic peaks.
#' @param x annotatePeaks objects
#' @param y XChromatograms object
#' @param assignGE Defines if result should be assigned to env. (TRUE) or not (FALSE).
#' @return interactive web pages
#' @examples
#' \dontrun{
#' cpvaWeb(anno, dat_chroma)
#' }
#' @keywords annotatePeaks
#' @export

cpvaWeb <- function(x,y, assignGE = FALSE){

  require(shiny)
  require(shinyjs)
  require(shinythemes)
  require(plotly)
  require(shinycssloaders)
  #require(ggthemr)
  #require(shinylogs)
  require(signal)
  require(data.table)
  require(tidyverse)
  require(stringr)
  require(DT)
  #require(xcms)
  #require(CAMERA)
  require(ggplot2)
  require(reshape2)
  require(peakFinder)
  require(ptw)

  if(assignGE) {
    warning("x, y missing")
    x = NULL
    y = NULL
  } else {
  x$dat_chroma <- y
  assign("annoResults_mscpva_000000001", x, envir = .GlobalEnv)
  }
   cpvaApp <- function() {
             appDir <- system.file("shiny_mscpva", package = "CPVA")
             if (appDir == "") {
                stop("Could not find myapp. Try re-installing `CPVA`.", call. = FALSE)
             }
             shiny::runApp(appDir, display.mode = "normal")
            }
   cpvaApp()
}
