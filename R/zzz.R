#' @importFrom magrittr %>%
#' @importFrom graphics layout
#' @importFrom methods new
#' @importFrom stats loess
#' @importFrom stats median
#' @importFrom stats predict
#' @importFrom stats sd
#' @importFrom utils capture.output
#' @importFrom utils combn
#' @importFrom utils head
#' @importFrom utils object.size
#' @importFrom utils read.csv
#' @importFrom utils tail
#' @importFrom utils write.table
.onAttach <- function(...) {
  packageStartupMessage("\nThis is 'CPVA version 0.0.1' ")
  # statTarget::statTargetGUI()
}
