#' @name annotatePeaks
#' @title Annotation of the chromatographic peaks
#' @description AnnotatePeaks provides annotation of the chromatographic peaks for the
#' non-targeted metabolomics.
#' @param dat_xcms XCMSnExp object
#' @param dat_chroma XChromatograms object
#' @param ppmTol Mass tolerance refers to the fraction of mass in parts-per-million (ppm) between the measured mass and theoretical mass  (default values: 5).The contaminant peaks are annotated based on the mass error (in ppm) between candidate peaks and contaminants in the open-source contaminants databases.
#' @param polarity The 'positive' or 'negative' polarity for LC-MS data
#' @param MCQindex This MCQ threshold selects high confidence chromatograms from low confidence chromatograms. If a chromatogram has a MCQ index lower than the threshold (default values: 0.5), the program will annotate it as low confidence and will remove the chromatogram from the peak table.
#' @param excAddcutsForm Peaks with specified adduct formation (e.g. [M+Na]+,[M+K]+) entered in the text box will be removed from the peak table.
#' @param assignGE Defines if result should be assigned to env. (TRUE) or not (FALSE).
#' @return annotatePeaks objects
#' @examples
#' \dontrun{
#' anno <- annotatePeaks(dat_xcms, dat_chroma)
#' }
#' @keywords annotatePeaks
#' @export
annotatePeaks <- function(dat_xcms,dat_chroma,
                          ppmTol = 5,
                          polarity = "positive",
                          MCQindex=0.5,
                          excAddcutsForm=c("[M+Na]+","[M+K]+"),
                          assignGE = FALSE
                          ){


  cat(paste(format(object.size(dat_chroma), units = "MB"),"\n",sep = ""))

  ppmCutoff <- as.numeric(ppmTol)
  polaCutoff <- as.character(polarity)
  MCQindex <- as.numeric(MCQindex)

   if(is.null(dat_xcms) | is.null(dat_chroma) ) {

    outputAnno <- NULL
  }  else {

    TuneChromatograms <- shinyTuneChromatograms(dat_chroma,MCQvalue = MCQindex)

    cat("!!")
    TuneXCMSnExp <- shinyTuneXCMSnExp(dat_xcms, ppm = ppmCutoff, polarity = polaCutoff)

    # ionType
    ptable <- TuneXCMSnExp[[1]]

    # missing peaks
    tc_temp <- as.data.frame(TuneChromatograms$peakScore)
    rownames(tc_temp) <- tc_temp$index
    ind_temp <- merge(as.data.frame(ptable[,1:2]),
                      tc_temp,by="row.names",all.x=TRUE)
    ind_temp$index <- ind_temp$Row.names
    ind_temp_id <- which(is.na(ind_temp$jugx))
    if(length(ind_temp_id) == 0) {

      table = ptable
      ionA <- TuneXCMSnExp[[2]]
      ionA <- ionA@annoPeaks
      conta <-  TuneXCMSnExp[[3]]

    } else {

      table <- ptable[-ind_temp_id,]

      #
      ionA <- TuneXCMSnExp[[2]]
      ionA <- ionA@annoPeaks[-ind_temp_id,]
      conta <-  TuneXCMSnExp[[3]][-ind_temp_id,]
    }



    # isotope
    isoExt <- function(x) {stringr::str_sub(x,stringr::str_locate_all(x,"M")[[1]][1]-1)}
    iso <- do.call(rbind,lapply(ionA$isotopes,isoExt))
    isoPie <- rbind(as.data.frame(table(iso),stringsAsFactors=FALSE),
                    c("unknow",sum(is.na(iso))),stringsAsFactors=FALSE)

    # adduct
    adduNo <- stringr::str_split(ionA$adduct," ")
    filt <- function(x){
      Id <- c()
      if(length(x) == 0) Id <- 0
      if(length(x) == 1) Id <- 1

      if(length(x) == 2) Id <- 2

      if(length(x) > 2) Id <- "Multiple.adduction"
      if(x[1] == "") Id <- 0

      return(Id)
    }
    adduExt <- do.call(rbind,lapply(adduNo,filt))
    ma_id <- grep("Multiple.adduction",adduExt)
    ionA$adduct[ma_id] <- "Multiple.adduction"

    adduExt2 <- function(x) {stringr::str_sub(x,end = stringr::str_locate_all(x," ")[[1]][1]-1)}
    addu <- do.call(rbind,lapply(ionA$adduct,adduExt2))
    addu[ma_id] <- "Multiple.hits"
    adducPie <- rbind(as.data.frame(table(addu),stringsAsFactors=FALSE),
                      c("unknow",sum(is.na(iso))),stringsAsFactors=FALSE)


    #conta

    conta[grep("///",conta,fixed = TRUE)] <- "Multiple.Hits"
    conta[conta == ""] <- "null"
    contaPie <- as.data.frame(table(conta))

    # chroma
    meric <- as.data.frame(TuneChromatograms$mericValue)
    #percent.rank <- function(x) trunc(rank(x))/length(x)
    #pr <- as.data.frame(t(do.call(rbind,lapply(meric,percent.rank))),stringsAsFactors=F)
    index <- paste((rep(1:dim(dat_chroma)[1],each = dim(dat_chroma)[2])),sep = "")
    #index <- rep(TuneChromatograms$peakScore$index, each = dim(dat_chroma)[2])
    mericRank_temp <- as.data.frame(
      apply(cbind(meric[,13:15],meric[,9:10]),2,as.numeric),stringsAsFactors=FALSE)
    mericRank <- cbind(index,mericRank_temp)


    combines <- cbind(table,ionA, conta, tc_temp)
    chromaTest <- combines$jugx

    combines$fold[chromaTest] <- "High-Confidence"
    combines$fold[!chromaTest] <- "Low-Confidence"
    combines$fold[combines$fold == -1] <- NA
    combines[,3] <-  combines$isotopes
    combines[,4] <- combines$adduct
    colnames(combines)[2:4] <- c("Chromatogram","isotopes","Addcut ion")
    combines <- combines[!is.na(combines$Chromatogram),]

    filterP <- filterPeaks(combines,adduremov = excAddcutsForm)
    #print(isoPie)
    outputAnno <- list(isoPie = isoPie,
                             adducPie = adducPie,
                             contaPie = contaPie,
                             datainput = combines,
                             dataOut = filterP,
                             mericR = mericRank,
                             dat_chroma = NULL
                       )
  }

  if(assignGE) assign("annoResults_mscpva_000000001", outputAnno, envir = .GlobalEnv)

  return(outputAnno)
}
