#' @name cpvaOT
#' @title cpvaOT
#' @description  cpvaOutputTable
#' @param x Annotatepeaks objects
#' @param filename base file name to save report, .tsv file will be appended to this name for the report
#' @param type "filterPeaks" or "all"
#' @return A .tsv file
#' @keywords cpvaOutputTable
#' @examples
#' \dontrun{
#' cpvaOT(x,"cpvaOutput")
#' }
#' @export
cpvaOT <- function(x, filename, type = "filterPeaks") {

  file =  paste(filename,"_",type,"_", Sys.Date(), ".tsv", sep="")

  if(type == "filterPeaks"){
  cot <- x$dataOut$filterMatrix
  cot1 <- cot[,1:11]
  cot2 <- as.matrix(cot[,12:dim(cot)[2]])
  meanc <- apply(cot2,1, mean, na.rm = TRUE)
  sdc <- apply(cot2,1, sd, na.rm = TRUE)
  cotnew <- data.frame(cot1,meanArea=meanc,sdArea=sdc,cot2)
  write.table(cotnew, file,  quote = FALSE, sep = "\t",col.names = NA)
  }

  if(type == "all"){
    cot <- x$datainput
    cot1 <- cot[,1:11]
    cot2 <- cot[,12:dim(cot)[2]]
    meanc <- apply(cot2,1, mean)
    sdc <- apply(cot2,1, sd)
    cotnew <- data.frame(cot1,meanArea=meanc,sdArea=sdc,cot2)
    write.table(cotnew, file,  quote = FALSE, sep = "\t", col.names = NA)
  }

  #
  cat(file, "was ouputted.")
}
