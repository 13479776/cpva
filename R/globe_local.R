#' @name filterPeaks
#' @title filterPeaks
#' @description  filterPeaks
#' @param x a object of filterPeaks
#' @param isoremov xx
#' @param adduremov xx
#' @param contaremov xx
#' @param chromaremov xx
#' @examples
#' \dontrun{
#' }
#' @keywords peaks, peakTarget, readPKparam
#' @export
filterPeaks <- function(x,isoremov =TRUE,
                        adduremov = c("[M+Na]+","[M+K]+"),
                        contaremov = TRUE,
                        chromaremov = TRUE) {

  if(isoremov) {isoremovID <- grep("[M+",x$isotopes,fixed = TRUE)} else {isoremovID = NULL}

  ssplit <- function(x,y) {
    te <- unlist(strsplit(x," ")) %in% y
    te2 <- ifelse(te %in% "TRUE", 1,0)
    if(sum(te2) > 0) {return(TRUE)}else (return(FALSE))
  }
  adduremovID <- unlist(lapply(x$adduct,ssplit, y = adduremov))
  adduremovID <- grep("TRUE",adduremovID)

  if(contaremov) {contaremoID <- which(!x$conta %in% "null")} else {contaremoID = NULL}
  if(chromaremov) {chromaremoID <- which(x$jugx %in% c("FALSE"))} else {chromaremoID = NULL}
  sum <- c(isoremov = length(isoremovID),
           adduremov = length(adduremovID),
           contaremov = length(contaremoID),
           chromaremov = length(chromaremoID))

  Filerid <- sort(unique(c(isoremovID,adduremovID,contaremoID,chromaremoID)))
  temp <- x[-Filerid,]

  index <- rep(NA,dim(x)[1])
  index[Filerid] <- "Removed"
  index[-Filerid] <- "Confidented"
  print(sum)
  return(list(index = index, filterMatrix = temp[,1:(dim(temp)[2]-8)], sum= sum))
}
### rectplot
#' @name rectplot
#' @title rectplot
#' @description  rectplot
#' @param x a object of rectplot
#' @param y a object of rectplot
#' @param size xx
#' @examples
#' \dontrun{
#' chrs_raw <- chromaList()
#' }
#' @keywords peaks, peakTarget, readPKparam
#' @export
rectplot <- function(x,y,size){

  y[y == "Confidented"] <- "#7FFFD4"
  y[y == "Removed"] <- "#F4A460"

  mzdiff <- x$mzmax -x$mzmin
  x$mzmin <- x$mzmin - size
  x$mzmax <- x$mzmax + size

  test.dat <- data.frame(ID= seq(1,dim(x)[1]),xmin=x$rtmin, ymin=x$mzmin, xmax=x$rtmax, ymax=x$mzmax, col=y)
  #ggthemr::ggthemr('lilac')
  ggp.test <- ggplot2::ggplot(data=test.dat, ggplot2::aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
                                        #fill = y,
                                        group = test.dat$ID
  )) +
    ggplot2::geom_rect(fill = test.dat$col) +
    ggplot2::labs(
      x="Retention time (min)",
      y = "Mass to Charge Ratio") +

    ggplot2::theme(
      legend.position = "none",
      axis.title.x = ggplot2::element_text(color="blue", size=10, face="bold"),
      axis.title.y = ggplot2::element_text(color="#993333", size=10, face="bold")
    )
  ply.test <- plotly::plotly_build(ggp.test)
  # replace the text hover
  x$conta[x$conta=="null"] <- NA
  x$jugx[x$jugx == "TRUE"] <- "Pass"
  x$jugx[x$jugx == "FALSE"] <- "Fail"
  mytext <- paste(
    "Isotopes: ",x$isotopes,"\n",
    "Adducts: ",x$adduct,"\n",
    "Contaminants: ",x$conta,"\n",
    "Chromatograms: ",x$hit, "--",x$jugx ,sep = "")

  mol <- ply.test$x$data
  if(length(mol) != length(mytext)) stop("the length of mytext shoule be agree to mol")
  textId <- c()
  for(i in 1:length(mol)) {
    textId[i] <- mol[[i]]$text
  }
  textId <- as.numeric(do.call(rbind,str_extract_all(textId,"[0-9]+")))
  mytextSort <- mytext[textId]
  for(i in 1:length(mol)) {
    mol[[i]]$text  <- mytextSort[i]
  }
  ply.test$x$data <- mol
  return(ply.test)
}


##
#' @name peakRank
#' @title peakRank
#' @description  peakRank
#' @param x a object of peakRank
#' @param s xx
#' @param plotmargin xx
#' @examples
#' \dontrun{
#' chrs_raw <- chromaList()
#' }
#' @keywords peaks, peakTarget, readPKparam
#' @export

peakRank <- function(x,s,plotmargin = m){
  #x <- mericRank
  pdat <- x[x$index == s,]
  ppdat <- reshape2::melt(pdat,id.vars = "index",value.name = "value")
  ppdat$value <- round(ppdat$value,2)


  #dat <- as.data.frame(id); colnames(dat) <- "data"
  ggplot2::theme_set(theme_bw())
  p <- ggplot2::ggplot(ppdat, aes(x=ppdat$variable,y=ppdat$value)) +

    ggplot2::geom_segment(aes(y = 0,
                     x = ppdat$variable,
                     yend = ppdat$value,
                     xend = ppdat$variable),
                 linetype="dot",
                 size = 0.6,
                 alpha = 0.5,
                 color = "grey") +

    ggplot2::geom_point(stat='identity', aes(col= ppdat$value
    ), size=3.5,alpha = 0.8)  +

    #geom_text(color="white", size=2,label=ppdat$value) +
    #labs(title="Precent Rank of Performance",
    #     subtitle="") +
    ggplot2::xlab(label = "")+
    ggplot2::ylab("Values")+
    #ylim(0, 1) +
    ggplot2::scale_color_gradient(low="red",name  ="")+
    #scale_colour_manual(values="black") +
    #theme(aspect.ratio = 1) +
    #guides(fill = guide_legend(title = NULL))+
    ggplot2::coord_flip()
  #p2=ggplotly(p,height = 300,width = 600)
  mytext=paste(ppdat$sampleName, "\n" , ppdat$variable, ": ", ppdat$value,  sep="")
  pp=plotly::plotly_build(p)
  m <- plotmargin
       plotly::style(pp, text= mytext, hoverinfo = "text") %>%
         plotly::layout(autosize = F, 
                        width = 600, height = 300, 
                        margin = m)
  #traces = c(1, 2, 3))
  #ggplotly(p)
  #return(pp)
}


#pieplot
#' @name pieplot_conta
#' @title pieplot_conta
#' @description  pieplot_conta
#' @param contaPie a object of pieplot_conta
#' @examples
#' \dontrun{
#' chrs_raw <- chromaList()
#' }
#' @keywords peaks, peakTarget, readPKparam
#' @export
pieplot_conta <- function(contaPie) {
  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
  isoPie <- contaPie
  p <- plotly::plot_ly(isoPie, labels = ~isoPie[,1], values = ~isoPie[,2], type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste('', isoPie[,1],isoPie[,2] ),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE) %>%
    plotly::layout(title = "Contaminants Distribution",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  p
}

#' @name pieplot_addu
#' @title pieplot_addu
#' @description  pieplot_addu
#' @param adducPie a object of pieplot_addu
#' @examples
#' \dontrun{
#' chrs_raw <- chromaList()
#' }
#' @keywords peaks, peakTarget, readPKparam
#' @export
pieplot_addu <- function(adducPie) {
  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
  isoPie <- adducPie
  p <- plotly::plot_ly(isoPie, labels = ~isoPie[,1], values = ~isoPie[,2], type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste('', isoPie[,1],isoPie[,2] ),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE) %>%
    plotly::layout(title = "Adducts Distribution",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  p
}

#' @name pieplot_iso
#' @title pieplot_iso
#' @description  pieplot_iso
#' @param isoPie a object of pieplot_iso
#' @examples
#' \dontrun{
#' chrs_raw <- chromaList()
#' }
#' @keywords peaks, peakTarget, readPKparam
#' @export
pieplot_iso <- function(isoPie) {
  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')

  p <- plotly::plot_ly(isoPie, labels = ~isoPie[,1], values = ~isoPie[,2], type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste('', isoPie[,1],isoPie[,2] ),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE) %>%
    plotly::layout(title = "Isotopes Distribution",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  p
}

#function
#' @name shinyTuneXCMSnExp
#' @title shinyTuneXCMSnExp
#' @description  shinyTuneXCMSnExp provides to annotate the peaks
#' @param XCMSnExp XCMSnExp object
#' @param ppm ppm tollerence
#' @param polarity xx
#' @examples
#' \dontrun{

#' }
#' @keywords peaks, peakTarget, readPKparam
#' @export
shinyTuneXCMSnExp <- function(XCMSnExp,ppm,polarity){
  od <- XCMSnExp
  table <- readXCMSnExp(od)
  #cat("test_od")

  pTar <- xcms2peakTarget(od)
  #cat("test_pTar")

  pTar@peakTable <- table
  ionA <- ionAnnotation(pTar,polarity=polarity)
  #
  #pathdir <- getwd()
  contaminantIP <- system.file("exdata", package = "CPVA")
  cPath <- paste0(contaminantIP,"/contaminantIons_List.csv",sep="")
  contaBase <- read.csv(cPath)
  #load("/Users/Hees/Documents/software/R/contaminationIon/iona.rds")
  compoundBase <- contaBase

  conta <- queryConta(pTar@peakTable$mzmed,
                      compoundBase = compoundBase,
                      ppm = ppm,
                      polarity = polarity,
                      reportName = "CompoundID_or_species")

  return(list(table,ionA,conta))
}

#' @name shinyTuneChromatograms
#' @title shinyTuneChromatograms
#' @description  shinyTuneChromatograms
#' @param Chromatograms a object of pieplot_iso
#' @param MCQvalue xx
#' @examples
#' \dontrun{
#' chrs_raw <- chromaList()
#' }
#' @keywords peaks, peakTarget, readPKparam
#' @export
shinyTuneChromatograms <- function(Chromatograms,MCQvalue) {

  meric <- chromaProfileMeric(Chromatograms)


  meric[is.na(meric)] <- 0L

  sdpeaks <- meric$MCQ >= MCQvalue
  testPred <- rep_len(NA,dim(meric)[1])
  testPred[sdpeaks] <- "H"; testPred[!sdpeaks] <- "L"


  # output
  new <- peakFinder::peakInter(Chromatograms,as.factor(testPred),"H")
  new$index <- rownames(Chromatograms@featureDefinitions)
  return(list(mericValue = meric,preictValue = testPred, peakScore = new))

}

#' @name merictable
#' @title merictable
#' @description  merictable
#' @param x a object of merictable
#' @examples
#' \dontrun{
#' chrs_raw <- chromaList()
#' }
#' @keywords peaks, peakTarget, readPKparam
#' @export
merictable <- function(x){

  metaID <- x

  peakfitObj <- list()
  #for(j in ){
  cat("No. of peaks = ")

  for(j in 1:length(metaID)){
    #for(j in 22){
    fID <- metaID[[j]]
    #fID <- metaID[j]
    prid <- seq(1,length(metaID),1)
    if(j %in% prid) cat(j,"...", sep = "")
    peakfitObj[j] <- list(lapply(fID,peakFit))
  }
  nameID <- c()
  for(k in 1:length(metaID)){
    nameID_tem <- metaID[[k]][[1]]
    nameID[k] <- nameID_tem@area[1]
  }
  names(peakfitObj) <- paste(nameID)
  #result
  #peakfitObj

  ## ID
  #xyy <- do.call(cbind,peakfitObj)
  meric <- do.call(rbind,do.call(cbind,peakfitObj))
  name <- do.call(rbind,lapply(peakfitObj,names))
  name_temp <- rownames(name)
  colnames(name) <- c(1:dim(name)[2]);
  rownames(name) <- c(1:dim(name)[1]);
  name_2 <-reshape2::melt(name)
  name_temp2 <- rep(name_temp,dim(name)[2])
  name_2 <- cbind(name_2,name_temp2)
  colnames(name_2) <- c("ID","index","value","metabo")
  name_2 <- as.data.frame(dplyr::arrange(name_2,ID),stringsAsFactors=F)
  finaltable <- as.data.frame(cbind(ID = paste("F",1:dim(name_2)[1],sep = ""),
                                    metabo = as.character(name_2$metabo),
                                    sampleName = as.character(name_2$value),
                                    meric),stringsAsFactors=FALSE)
  return(finaltable)
  # end
}


####
#functions
####
peakObj <- setClass("peakObj",slots =
                      c(time = "numeric",
                        sig = "data.frame",
                        area = "character")
)


#' @name peakgroup
#' @title peakgroup
#' @description  peakgroup
#' @param x a object of peakgroup
#' @examples
#' \dontrun{
#' chrs_raw <- chromaList()
#' }
#' @keywords peaks, peakTarget, readPKparam
#' @export
peakgroup <- function(x){
  if(!class(x) == "data.frame") stop("data.frame")
  time <- x$rtime
  sig <- x$intensity

  peak.sig <- as.data.frame(cbind(sig,sig))
  area <- as.character(x$name)
  peakgroup <- peakObj(
    time = time,
    sig = peak.sig,
    area = area)
  return(peakgroup)
}

#####
peakFit <- function(fID) {
  #xx <- fID[[2]]
  fID@time <- fID@time[!is.na(fID@sig)[,1]]
  fID@sig <-   fID@sig[!is.na(fID@sig)[,1],]


  if(dim(fID@sig)[1] < 6) {
    maxBoundaryIntensity <- NA
    peakModality <- NA
    peakJaggedness <- NA
    peakMaxIntensity <- NA
    peakFWHM <- NA
    peakFWHM2base <- NA
    peakSymmetry <- NA
    MCQ <- 0
  } else {

    #fit

    fID@sig[,1] <- signal::sgolayfilt(fID@sig[,1], 3, 5)
    #fID@sig[,1] <- signal::filtfilt(butter(3,0.1),fID@sig[,1])
    fID@sig[,2] <- fID@sig[,1]
    fID@time <- fID@time[!is.na(fID@sig)[,1]]
    fID@sig <-   fID@sig[!is.na(fID@sig)[,1],]

    MCQ <- ptw::coda(fID@sig[,1],window = 3)


    maxBoundaryIntensity <- CalculateMaxBoundaryIntensity(fID)[[1]]
    peakModality <- CalculateModality(fID)$peak.modality
    peakJaggedness <- CalculatePeakJaggedness(fID)$peak.jaggedness
    peakMaxIntensity <- CalculatePeakMaxIntensity(fID)[[1]]
    peakFWHM <-  CalculateFWHM(fID)$peak.fwhm[[1]]
    peakFWHM2base <- CalculateFWHM(fID)$r.fwhm2base[[1]]
    #PeakSymmetry <- CalculatePeakSymmetry(fID)$peak.symmetry
    peakSymmetry <- peak.Symmetry(fID)
    #gaussFitScore <- gaussFit.Score(fID, snthreshFS = snthreshFS)



    #Peakpoints
    rt <- fID@time; drt <- rt[!is.na(rt)]
    peakFullwidth <- max(drt) - min(drt)
    int <- fID@sig$sig
    peakSkewness <- timeDate::skewness(int,method = c("moment"))
    peakKurtosis <- timeDate::kurtosis(int,method = c("moment"))
  }


  datp <- fID@sig$sig
  if(length(datp) - sum(is.na(datp))  < 6) {
    peakpoints <- "Low"
    peakFullwidth = NA
    peakSkewness = NA
    peakKurtosis = NA
    maxBoundaryIntensity= NA
    peakModality= NA
    peakJaggedness= NA
    peakMaxIntensity= NA
    peakFWHM = NA
    peakFWHM2base = NA
    peakSymmetry= NA
    #gaussFitScore = NA
    MCQ = 0
  } else {peakpoints <- "Medium"}

  peakFit <-c(peakpointScore = peakpoints,
              peakFullwidth = peakFullwidth,
              peakSkewness = peakSkewness,
              peakKurtosis = peakKurtosis,
              maxBoundaryIntensity= maxBoundaryIntensity,
              peakModality= peakModality,
              PeakJaggedness= peakJaggedness,
              peakMaxIntensity= peakMaxIntensity,
              peakFWHM = peakFWHM,
              peakFWHM2base = peakFWHM2base,
              peakSymmetry= peakSymmetry,
              #gaussFitScore = gaussFitScore,
              MCQ= MCQ
  )
  return(peakFit)
}

peak.Symmetry <- function(x) {

  dat <- x@sig$sig
  datRt <- x@time

  missInt <- is.na(dat)
  datInt <- dat[!missInt]
  datRt <- datRt[!missInt]
  missRT <- is.na(datRt)
  datInt <- datInt[!missRT]
  datRt <- datRt[!missRT]

  maxIntID <- grep(max(datInt),datInt)

  datlow <- datInt[1]
  dathow <- datInt[length(datInt)]

  #
  maxIntRT <- datRt[datInt == max(datInt)]
  leftrt <- datRt[1:maxIntID]
  leftdat <- datInt[1:maxIntID]
  rightrt <- datRt[-c(1:maxIntID)]
  rightdat <- datInt[-c(1:maxIntID)]


  if(length(leftdat) > 4 & length(rightdat) > 4) {

    
    if(datlow > dathow ) {

      leftlow <- min(leftdat)
      leftrt2 <- leftrt
      leftdat2 <-  leftdat
      rightdat2 <- rightdat[rightdat > leftlow]
      rightrt2 <- rightrt[rightdat > leftlow]
    } else {
      rightlow <- min(rightdat)
      leftdat2 <- leftdat[leftdat > rightlow]
      leftrt2 <- leftdat[leftdat > rightlow]
      rightrt2 <- rightrt
      rightdat2 <- rightdat
    }
    #if(min(leftdat) <= int10 & min(rightdat) <= int10 & length(leftdat) >=4 & length(rightdat) >= 4){

    # 10% height of max intensity 取最低点-最高10%
    int10 <- (max(leftdat2) -  min(leftdat2))*0.1 + min(leftdat2)

    # 整体数据的loess
    leftfit <- suppressWarnings(loess(leftrt ~ leftdat,span = 0.5))
    rightfit <- suppressWarnings(loess(rightrt ~ rightdat,span = 0.5))
    leftrtV <- suppressWarnings(predict(leftfit,int10))
    rightrtV <- suppressWarnings(predict(rightfit,int10))

    PeakSymmetry <- abs((rightrtV[[1]] - maxIntRT)/(maxIntRT - leftrtV[[1]]))[1]

  } else {
    PeakSymmetry <- NA
  }
  return(PeakSymmetry)
}

#' @name readXCMSnExp
#' @title readXCMSnExp
#' @description  readXCMSnExp
#' @param XCMSnExp a object of peakgroup
#' @examples
#' \dontrun{
#' chrs_raw <- chromaList()
#' }
#' @keywords peaks, peakTarget, readPKparam
#' @export
readXCMSnExp <- function(XCMSnExp) {

  MSdat <- XCMSnExp
  groupval2 <- xcms::featureValues(MSdat, method = c("medret"), value = "into", intensity = "into", filled = TRUE)
  #
  pks <- xcms::chromPeaks(MSdat)

  mat <- do.call(rbind, lapply(xcms::featureDefinitions(MSdat)$peakidx, function(z) {
    pks_current <- pks[z, , drop = FALSE]
    c(pks_current[, c("rt")][[1]]/60L,
      range(pks_current[, c("rtmin", "rtmax")]/60L),
      pks_current[, c("mz")][[1]],
      range(pks_current[, c("mzmin", "mzmax")]),
      pks_current[, c("sn")][[1]]
    )
  }))
  colnames(mat) <- c("rtmed","rtmin", "rtmax","mzmed", "mzmin", "mzmax","sn")
  mat <- as.data.frame(mat,stringsAsFactors= FALSE)

  IDrt <- round((mat[,1]*60),0)
  IDmz <- round((mat[,4]),0)
  peaksID <- paste("M",IDmz, "T",IDrt,sep="")

  #peaksID <- xcms::groupnames(MSdat2)
  #rm(MSdat2)
  stat <- matrix(-1L,nrow = length(peaksID), ncol = 3)
  colnames(stat) <- c("fold","tstat","pvalue")
  tsv <- cbind(name = peaksID, stat, round(mat,4), groupval2, stringsAsFactors =FALSE)
  return(tsv)
}

#' @name xcms2peakTarget
#' @title xcms2peakTarget
#' @description  xcms2peakTarget
#' @param XCMSnExp a object of peakgroup
#' @examples
#' \dontrun{
#' chrs_raw <- chromaList()
#' }
#' @keywords peaks, peakTarget, readPKparam
#' @export
xcms2peakTarget <- function(XCMSnExp) {

  #XCMSnExp

  path <- system.file("parameters",package = "CPVA")
  param <- list.files(path,".pk.param.xls",recursive=FALSE,full.names=TRUE)
  inhouseParam <- peakFinder::readPKparam(param)

  para <- XCMSnExp@.processHistory
  # Two step
  # 1
  len <- length(para)
  if(len >= 2) {
  para_1 <- para[[1]]@param
  para_2 <- para[[2]]@param
  if(class(para_1) == "CentWaveParam") {

    inhouseParam$ppm <-  as.numeric(para_1@ppm)
    inhouseParam$mzdiff <- as.numeric(para_1@mzdiff)
    inhouseParam$noise <- as.numeric(para_1@noise)
    inhouseParam$peakwidth <- as.numeric(para_1@peakwidth)
   }
  if(class(para_2) == "PeakDensityParam") {
    inhouseParam$adjustRtime.binSize <- as.numeric(para_2@binSize)
    inhouseParam$group.minfrac <- as.numeric(para_2@minFraction)
    inhouseParam$group.minsamp <- as.numeric(para_2@minSamples)
    inhouseParam$group.bw <- as.numeric(para_2@bw)
   }
  } else {
    stop("missing the CentWaveParam or PeakDensityParam")
    }

  pT <- new("peakTarget")
  pT@inputPara <- inhouseParam
  pT@peakChroma <- XCMSnExp
  return(pT)
}




#functions
queryConta <- function(x,compoundBase, ppm, polarity,reportName) {

  diffmass <- lapply(x, ppmc, to = compoundBase$Monoion) # Monoion --- mass
  qureyC <- lapply(diffmass, qureyFun, compoundBase = compoundBase, ppm = ppm, polarity = polarity)
  qureyC <- lapply(qureyC,trans,reportName = reportName)
  qureyC <- do.call(rbind,qureyC)
  return(qureyC)
}

ppmc <- function(from, to) {
  (from - to)*10^6/to
}

qureyFun <- function(x, compoundBase, ppm, polarity) {
  IDq <- abs(x) <= ppm & compoundBase[,"polarity"] == polarity
  match <- compoundBase[IDq,]
  return(match)
}

trans <- function(x,reportName) {
  back <- paste(x[,reportName],collapse = "///")
  return(back)
}


#' @name grectPlot
#' @title grectPlot
#' @description  grectPlot
#' @param XCMSnExp a object of peakgroup
#' @examples
#' \dontrun{
#' x
#' }
#' @keywords peaks, peakTarget, readPKparam
#' @export
grectPlot <- function(x,y,size) {

  y[y == "Confidented"] <- "#7FFFD4"
  y[y == "Removed"] <- "#F4A460"

  mzdiff <- x$mzmax -x$mzmin
  x$mzmin <- x$mzmin - size
  x$mzmax <- x$mzmax + size

  test.dat <- data.frame(ID= seq(1,dim(x)[1]),xmin=x$rtmin, ymin=x$mzmin, xmax=x$rtmax, ymax=x$mzmax, col=y)
  #ggthemr('greyscale',layout = "scientific")
  ggp.test <- ggplot(data=test.dat, aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
                                        #fill = test.dat$col,
                                        show.legend=TRUE,
                                        group = test.dat$ID
  )) +

    ggplot2::geom_rect(fill = test.dat$col) +
    ggplot2::labs(
      x="Retention time (min)",
      y = "Mass to Charge Ratio") +

    theme(
      #legend.position = "right",
      #legend.text = element_text(color="azure4", size = 14, face = "bold"),
      axis.text.x = ggplot2::element_text(size=16,face="bold"),
      axis.text.y = ggplot2::element_text( size=16,face="bold"),
      axis.title.x = ggplot2::element_text(color="blue", size=20, face="bold"),
      axis.title.y = ggplot2::element_text(color="#993333", size=20, face="bold")
    )
}



#create the databases
#' @name chromaProfileMeric
#' @title chromaProfileMeric
#' @description  chromaProfileMeric
#' @param xy a object of chromaProfileMeric
#' @examples
#' \dontrun{
#' chrs_raw <- chromaList()
#' }
#' @keywords peaks, peakTarget, readPKparam
#' @export
chromaProfileMeric <- function(xy){

  mydb <- DBI::dbConnect(RSQLite::SQLite(), "")

  cat("\nConnected peaks: ")
  for(i in 1:dim(xy)[1]) {
    cat(i,"...", sep = "")
    xydat <- xy[i,]
    feature <- chromaList_cpva(xydat)

    #xydat <- xy[i,]
    #feature <- chromaList(xy,i)
    DBI::dbWriteTable(mydb,paste("x",i,sep = "_"),feature, overwrite =TRUE)
    #gc()
    #rm(xydat)
  }
  #dbDisconnect(mydb)


  #readdb <- dbConnect(RSQLite::SQLite(), "")
  cat("\n\nprocessed peaks: ")

  peakfitObj <- list()
  nameID <- c()
  for(j in 1:length(DBI::dbListTables(mydb))){

    #prid <- seq(1,length(dbListTables(mydb)),1)
    #if(j %in% prid) cat(j,"...", sep = "")
    cat(j,"...", sep = "")

    feature <- DBI::dbReadTable(mydb,paste("x",j,sep = "_"))
    metaID <- lapply(split(feature,feature$sampleID),peakgroup)
    peakfitObj[[j]] <- lapply(metaID,peakFit)
    nameID[j] <- metaID[[1]]@area[1]
  }
  names(peakfitObj) <- paste(nameID)

  # delete db
  DBI::dbDisconnect(mydb)
  ## ID
  #xyy <- do.call(cbind,peakfitObj)
  meric <- do.call(rbind,do.call(cbind,peakfitObj))
  name <- do.call(rbind,lapply(peakfitObj,names))
  name_temp <- rownames(name)
  colnames(name) <- c(1:dim(name)[2]);
  rownames(name) <- c(1:dim(name)[1]);
  name_2 <-reshape2::melt(name)
  name_temp2 <- rep(name_temp,dim(name)[2])
  name_2 <- cbind(name_2,name_temp2)
  colnames(name_2) <- c("ID","index","value","metabo")
  name_2 <- as.data.frame(dplyr::arrange(name_2,ID),stringsAsFactors=F)
  finaltable <- as.data.frame(cbind(ID = paste("F",1:dim(name_2)[1],sep = ""),
                                    metabo = as.character(name_2$metabo),
                                    sampleName = as.character(name_2$value),
                                    meric),stringsAsFactors=FALSE)
  return(finaltable)
}



#' @name chromaList_cpva
#' @title chromaList_cpva
#' @description  chromaList_cpva
#' @param x a object of featureChroma
#' @examples
#' \dontrun{
#' chrs_raw <- chromaList()
#' }
#' @keywords peaks, peakTarget, readPKparam
#' @export

chromaList_cpva <- function(x){

    #pk <- x[n,]

     #transXchroma <- function(x){

      Sname <- x@phenoData@data$sampleNames
      IDrt <- round(median(c(x@featureData@data[1,3],x@featureData@data[1,4])),0)
      IDmz <- round(median(c(x@featureData@data[1,1],x@featureData@data[1,2])),0)
      mzID <- paste("M",IDmz, "T",IDrt,sep="")
      pxd <- x@.Data

      pxdf <- function(x) data.frame(cbind(x@rtime,x@intensity))
      rtin <- do.call(rbind,lapply(pxd,pxdf))

      pxdname <- function(x) {length(x@rtime)}
      pxdname <- do.call(rbind,lapply(pxd,pxdname))
      filenam <- rep.int(Sname,as.vector(pxdname))

      rtintO <- cbind(data.frame(
        rtin, filenam, mzID
      ))

      colnames(rtintO) <- c("rtime","intensity","sampleID","name")
      return(rtintO)
    #}
    #pkout <- transXchroma(x)
    #return(pkout)
  }



## merics Calculation
### revised from TargetedMSQC package

CalculatePeakMaxIntensity <- function(peak, ...) {

  # error and warning handling ---------------------------------------

  #   input errors
  error.input.format = simpleError("CalculatePeakMaxIntensity: input peak should be non-empty and of class peakObj")

  #if (is.na(peak)) stop(error.input.format)

  # function body  ---------------------------------------

  # max peak intensity is calculated
  r.max.intensity <- sapply(peak@sig,max)

  # return output
  return(r.max.intensity)
}


CalculateMaxBoundaryIntensity <- function(peak, ...) {

  # error and warning handling ---------------------------------------

  #   input errors
  error.input.format <- simpleError("CalculateMaxBoundaryIntensity: input peak should be non-empty and of class peakObj")

  #if (is.na(peak)) stop(error.input.format)

  # function body  ---------------------------------------

  #   max intensity at peak boundary is calculated
  r.max.boundary.intensity <- sapply(peak@sig,function(x) max(head(x,1),tail(x,1)))

  # return output
  return(r.max.boundary.intensity)
}


CalculatePeakElutionShift <- function(peak,...) {

  # error and warning handling ---------------------------------------

  #   input errors
  error.input.format <- simpleError("CalculatePeakElutionShift: input peak should be non-empty and of class peakObj")

  #if (is.na(peak)) stop(error.input.format)


  # function body  ---------------------------------------

  #   elution shift is calculated for each pair of transitions in the peak group using the CalculateElutionShift function

  r.shift <- data.frame(shift = mapply(CalculateElutionShift,t(combn(peak@sig,2))[,1],t(combn(peak@sig,2))[,2],data.frame(peak@time)),
                        ion = combn(colnames(peak@sig),2)[1,],
                        ion2 = combn(colnames(peak@sig),2)[2,]) %>%
    spread(key = ion, value = shift)
  rownames(r.shift) <- r.shift$ion2
  r.shift <- r.shift %>% select(-ion2)
  r.shift <- cbind(rbind(r.shift,rep(NA,ncol(r.shift))),rep(NA,ncol(r.shift) + 1))
  colnames(r.shift)[ncol(r.shift)] <- setdiff(colnames(peak@sig),colnames(r.shift))
  rownames(r.shift)[nrow(r.shift)] <- setdiff(colnames(peak@sig),rownames(r.shift))

  # the rows and columns are ordered according to the order of the transitions in the peak object
  r.shift <- r.shift[names(peak@area),names(peak@area)]

  # shift for each transition vs the peak groups is determined by the difference between time at max for each transition and the median of time at max for all the transitions in the peak group.
  max.intensity.times <- peak@time[unlist(sapply(peak@sig,function(sig) min(which(sig == max(sig)))))]
  diag(r.shift) <- round(abs(max.intensity.times - median(max.intensity.times))/(tail(peak@time,1) - head(peak@time,1)),digits = 4)

  # the NA values in this matrix correspond to transition pairs that are ordered differently. NAs fpr (tr1,tr2) transitions pairs are replaced by the peak elution shift calculated for (tr2,tr1) pairs
  ind_na <- which(is.na(r.shift), TRUE)
  if (nrow(ind_na) == 1)
    r.shift[ind_na] <- r.shift[ind_na[1,2],ind_na[1,1]]
  else
    r.shift[ind_na] <- r.shift[ind_na[,2:1]]

  # format output
  r.shift <- as.matrix(r.shift)
  shift <- list(r.shift = r.shift)

  # return output
  return(shift)
}


CalculateJaggedness <- function(sig, flatness.factor = 0.05, ...) {

  # error and warning handling ---------------------------------------

  #   input errors: if the input vectors are na or too short
  error.input.format <- simpleError("CalculateJaggedness: input sig should be a numeric vector with at least 3 elements")

  if (length(sig) < 3) stop(error.input.format)

  error.flatness.format <- simpleError("CalculateJaggedness: flatness.factor should be a numeric value between 0 and 1")

  if (flatness.factor < 0 || flatness.factor > 1) stop(error.flatness.format)


  # function body  ---------------------------------------

  #   changes in the sign of differential of the peak is used to quantify the jaggedness of the peak
  diff.sig <- diff(sig)

  #   the near-flat ranges of peak, where the differential is less than flatness.factor x peak max are assumed to be flat in measurements of jaggedness
  diff.sig[which(abs(diff.sig) < flatness.factor*max(abs(sig)))] = 0

  #   jaggeddness is calculated
  jaggedness <- (sum(abs(diff(sign(diff.sig))) > 1) - 1)/(length(diff.sig) - 1)

  #   if jaggedness is negative return zero
  jaggedness <- max(0,jaggedness)

  # return output
  return(round(jaggedness,digits = 4))
}

CalculatePeakJaggedness <- function(peak, flatness.factor = 0.05, ...) {

  # error and warning handling ---------------------------------------

  #   input errors
  error.input.format <- simpleError("CalculatePeakJaggedness: input peak should be non-empty and of class peakObj")

  #if (is.na(peak)) stop(error.input.format)

  # function body  ---------------------------------------

  #   jaggeddness is calculated using the CalculateJaggedness function
  r.jaggedness <- mapply(CalculateJaggedness,peak@sig,flatness.factor = flatness.factor)
  peak.jaggedness <- round(mean(r.jaggedness),digits = 4)

  # format output
  jaggedness <- list(r.jaggedness = r.jaggedness, peak.jaggedness = peak.jaggedness)

  # return output
  return(jaggedness)
}

CalculatePeakShapeSimilarity <- function(peak,...) {

  # error and warning handling ---------------------------------------

  #   input errors
  error.input.format <- simpleError("CalculatePeakShapeSimilarity: input peak group should be non-empty")

  #if (is.na(peak)) stop(error.input.format)


  # function body  ---------------------------------------

  #   pearson correlation coefficient between sig and the ref peak
  r.similarity <- psych::corr.test(peak@sig,method = "pearson",adjust = "holm", alpha = .05,ci = TRUE)$r

  # NA values are imputed to 0. they are a result of all zero signals.
  r.similarity[is.na(r.similarity)] <- 0

  # with the exception of the diagonal NA values which must be imputed to 1.
  diag(r.similarity) <- 1
  peak.similarity <- round(mean(r.similarity),digits = 4)

  # return output
  similarity <- list(r.similarity = r.similarity, peak.similarity = peak.similarity)
  return(similarity)
}


CalculateFWHM <- function(peak, ...) {

  # error and warning handling ---------------------------------------

  #   input errors
  error.input.format <- simpleError("CalculateFWHM: input peak should be non-empty and of class peakObj")

  #if (is.na(peak)) stop(error.input.format)

  # function body  ---------------------------------------

  time <- peak@time
  sig <- peak@sig

  calc.fwhm <- function(sig,time) {

    # find the peak max
    peakmax <- max(sig)

    # determine the first timepoint that crosses the half line
    left.index <- c(which(sig - peakmax/2 > 0)[1] - 1,which(sig - peakmax/2 > 0)[1])
    right.index <- c(tail(which(sig - peakmax/2 > 0),1),tail(which(sig - peakmax/2 > 0),1) + 1)

    # if the leftmost left.index  is 0, which can happen if the peak value on the boundary is high, or if it's NA, which can happen if sig is all zeros, assign the peak boundary value to them:
    if (left.index[1] == 0 || is.na(left.index[1])) {

      t.left <- time[1]

    } else {

      # use linear interpolation to find the timepoint at half max.
      t.left <- (time[left.index[2]] - time[left.index[1]])/(sig[left.index[2]] - sig[left.index[1]])*(peakmax/2 - sig[left.index[1]]) + time[left.index[1]]
    }

    # if the rightmost right.index  is greater than length of time, which can happen if the peak value on the boundary is high, or if it's NA, which can happen if sig is all zeros, assign the peak boundary value to them:

    if (right.index[2] > length(time) || is.na(right.index[2])) {

      t.right <- tail(time,1)}

    else {
      t.right <- (time[right.index[2]] - time[right.index[1]])/(sig[right.index[2]] - sig[right.index[1]])*(peakmax/2 - sig[right.index[1]]) + time[right.index[1]]
    }

    # if t.left or t.right returns nothing, which can happen if the peak value on the boundary is high assign the peak boundary value to them:
    if (length(t.left) == 0) t.left <- time[1]
    if (length(t.right) == 0) t.right <- tail(time,1)

    # fwhm is the difference in time between the two timepoints that are crossed by the half max line
    fwhm <- t.right - t.left
    return(fwhm)
  }

  # calculate fwhm for each transition
  r.fwhm <- mapply(calc.fwhm,sig,data.frame(time))

  peak.fwhm <- round(mean(r.fwhm),digits = 4)

  # calculate fwhm to base ratio for each transition
  r.fwhm2base <- r.fwhm/(tail(time,1) - time[1])

  peak.fwhm2base <- round(mean(r.fwhm2base),digits = 4)

  # format output
  fwhm <- list(r.fwhm = r.fwhm, peak.fwhm = peak.fwhm, r.fwhm2base = r.fwhm2base, peak.fwhm2base = peak.fwhm2base)

  # return output
  return(fwhm)
}

CalculateModality <- function(peak, flatness.factor = 0.05, ...) {

  # error and warning handling ---------------------------------------

  #   input errors
  error.input.format <- simpleError("CalculateModality: input peak should be non-empty and of class peakObj")

  #if (is.na(peak)) stop(error.input.format)

  # function body  ---------------------------------------

  time <- peak@time
  sig <- peak@sig

  calc.modality <- function(sig,time) {

    # find the differential of the peak
    diff.sig <- diff(sig)

    # any differences that are below the flatnessfactor of the maximum peak height are flattened.
    diff.sig[which(abs(diff.sig) < flatness.factor*max(abs(sig)))] <- 0

    # find the first and last timepoint where the differential changes sign
    first.fall <- head(which(diff.sig < 0),1)
    last.rise <- tail(which(diff.sig > 0),1)

    if (length(first.fall) == 0) first.fall <- length(time) + 1
    if (length(last.rise) == 0) last.rise <- -1

    # if first fall is after last rise, peak cannot be bi or multi-modal, so max.dip is set to 0. Otherwise it is set to the largest fall or rise between the first fall and last rise

    max.dip <- 0

    if (!is.na(first.fall) & !is.na(last.rise) & first.fall < last.rise) {
      max.dip <- max(abs(diff.sig[first.fall:last.rise]))
    }

    # The output is the maximum dip normalized by the peak height
    if (max(sig) == 0) {
      modality <- 0
    } else {
      modality <- max.dip/max(sig)
    }

    return(modality)
  }

  # calculate modality for each transition
  r.modality <- mapply(calc.modality,sig,data.frame(time))
  peak.modality <- round(mean(r.modality),digits = 4)

  # format output
  modality <- list(r.modality = r.modality, peak.modality = peak.modality)

  # return output
  return(modality)
}

CalculateTransitionSum <- function(peak,endogenous.label = "light", standard.label = "heavy", ...) {

  # error and warning handling ---------------------------------------

  #   input errors
  error.input.format <- simpleError("CalculateTransitionSum: input peak should be non-empty and of class peakObj")

  #if (is.na(peak)) stop(error.input.format)


  # function body  ---------------------------------------

  # separate columns of light and heavy isotopes

  endogenous.cols <- grepl(endogenous.label,colnames(peak@sig))
  standard.cols <- grepl(standard.label,colnames(peak@sig))

  # calculate the signal for the sum transition peak

  sig <- data.frame(rowSums(peak@sig[,endogenous.cols, drop = FALSE]), rowSums(peak@sig[,standard.cols, drop = FALSE]))

  colnames(sig) <- paste0("sum.0.",c(endogenous.label,standard.label))
  # calculate the area for the sum transition peak

  area <- c(sum(peak@area[endogenous.cols]),sum(peak@area[standard.cols]))
  names(area) <- paste0("sum.0.",c(endogenous.label,standard.label))

  # create the peak object

  peak.sum <- peakObj(
    time = peak@time,
    sig = sig,
    area = area
  )

  return(peak.sum)
}

ApplyPeakBoundary <- function(peak, boundary,...) {

  # error and warning handling ---------------------------------------

  #   input errors: if the input peak is empty
  error.input.format <- simpleError("ApplyPeakBoundary: input peak should be non-empty and of class peakObj")

  #if (is.na(peak)) stop(error.input.format)

  #   input errors: if the boundary is not a numeric vector of length 2
  error.boundary.format <- simpleError("ApplyPeakBoundary: boudary should be a numeric vector of length 2")

  if (length(boundary) != 2) stop(error.boundary.format)

  #   if the peak boundaries are NA
  warning.na.boundary <- simpleWarning("ApplyPeakBoundary: the peak boundaries are NA")

  if (sum(is.na(boundary)) > 0) {
    warnings(warning.na.boundary)

    # return the original peak as input
    return(peak)
  }

  # function body  ---------------------------------------

  # determine the timepoints and the signal within the boundaries
  time <- peak@time[which(peak@time > boundary[1] & peak@time < boundary[2])]

  sig <- peak@sig %>%
    slice(which(peak@time > boundary[1] & peak@time < boundary[2]))

  # calculate peak area using the trapozoidal approximation
  area <- sapply(sig,function(x) pracma::trapz(time,x))

  # create a new peak with the time and sig data points within the boundaries
  peak <- tryCatch({
    peakObj(time = time, sig = sig, area = area)
  }, error = function(e) {
    return(NA)
  }
  )

  return(peak)
}



#' @name ionAnnotation
#' @title ionAnnotation
#' @description  ionAnnotation
#' @param object xx
#' @param polarity xx
#' @param annotation xx
#' @param isotopeRemove xx
#' @return ionAnnotation results
#' @examples
#' \dontrun{
#' chrs_raw <- chromaList()
#' }
#' @keywords annotatePeaks
#' @export
ionAnnotation <- function(object,
                          polarity="positive",
                          annotation=TRUE,
                          isotopeRemove=F) {
  if(!class(object) == "peakTarget") stop("the peakTarget object required!")
  MSdat <- object@peakChroma
  targetPara <- object@inputPara
  if(is.null(MSdat)) stop("peakChroma of peakTarget object is required")
  # annotation
  if(annotation){
    cat("\n\nPeaks annotation: \n")
    annoMSdat <- suppressMessages(methods::as(MSdat,"xcmsSet"))
    annoMSdat <-CAMERA::xsAnnotate(annoMSdat, polarity = polarity)
    suprr = capture.output(annoMSdat <-CAMERA:: groupFWHM(annoMSdat))
    suprr <- capture.output(annoMSdat <- CAMERA::findIsotopes(
      annoMSdat,
      ppm = targetPara$ppm,
      mzabs=abs(targetPara$mzdiff)
    ))
    suprr <- capture.output(annoMSdat <- CAMERA::findAdducts(annoMSdat,
                                                             polarity=polarity))
    peakTableAnnoMSdat <- CAMERA::getPeaklist(annoMSdat)
    iso_addu_pcg <- peakTableAnnoMSdat[,c("isotopes","adduct","pcgroup")]
    # conta_seeker

    ionAnno <- iso_addu_pcg

    tsv <- object@peakTable
    #
    if(isotopeRemove){
      iso1 <- grep("[M+",peakTableAnnoMSdat$isotopes,fixed = TRUE)
      iso <- iso1
      tsv <- tsv[- iso1, ]
    } else {
      iso = grep("[M+",peakTableAnnoMSdat$isotopes,fixed = TRUE)
      iso1 <- NULL
      tsv = tsv }

  } else {
    iso1 <- NULL
    iso <- NULL
  }
  object@peakTable <- tsv
  object@annoPeaks <- ionAnno
  object@filterPeaks <- iso1
  return(object)
}

