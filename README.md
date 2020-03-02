# CPVA


Source Code: [CPVA package](https://raw.githubusercontent.com/13479776/Picture/master/CPVA_0.0.1.tar.gz)

Demo data: [Data](https://raw.githubusercontent.com/13479776/Picture/master/ExampleDataset_CPVApos.zip)


#### How to install the local CPVA package?

Dependent on R (>= 3.6.2)

If you did not install the R software yet,you can download R >= 3.6.2 from https://www.r-project.org

Firstly, please install the dependent packages using the following code:

    install.packages(c("RSQLite","shiny","shinyjs", "shinythemes", "plotly","DT","tidyverse","ggplot2","reshape2","stringr","signal",   "data.table","ptw", "timeDate", "dplyr","shinycssloaders", "methods","magrittr"))     
    
    if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages(c("BiocManager","devtools"))
    
    BiocManager::install(c("MSnbase","xcms","CAMERA"))
    devtools::install_github("13479776/peakFinder")
    
The CPVA package can be directly installed from GitHub:

    devtools::install_github("13479776/cpva")

The CPVA package can be downloaded and installed manually. The local R package: [CPVA package](https://raw.githubusercontent.com/13479776/Picture/master/CPVA_0.0.1.tar.gz)

    install.packages(file_path,repos=NULL,type="source") # please replace the file_path with the real path of CPVA package; eg:install.packages("F:/CPVA.0.0.1.tar.gz",repos=NULL,type="source")
    

*Rcpp 1.0.2 version was recommended, if you got the warning message that "mzR has been built against a different Rcpp version (1.0.2) than is installed on your system".


    
 #### How to use the CPVA?
1. prepare and upload your LC-MS data

   Before the XCMS analysis, raw MS1 data files should be converted into open data format (e.g, mzXML, mzML, cdf). The ProteoWizard's msconvert utility was recommended as the converter.

> peak picking analysis (Example script)
     
     # the faahKO for MS data
     if (!requireNamespace("faahKO", quietly = TRUE))
     BiocManager::install("faahKO")
     library(faahKO)
     
     library(xcms)
     if(packageVersion("xcms") < "3.6.0") stop("Update the 'xcms' version please")
     fls <- dir(system.file("cdf", package = "faahKO"), recursive = TRUE,
       full.names = TRUE) ## LC-MS data
     ## Reading the samples 
     ## Only MS1 Levels 
     raw_data <- readMSData(fls,mode = "onDisk", msLevel. = 1L) 
     ## Perform the chromatographic peak detection using the cenWave method.
     cwp <- CentWaveParam(ppm = 25, noise = 10000) 
     res <- findChromPeaks(raw_data, param = cwp)
     ## Performing the chromatographic peak grouping. Assigning all samples to the same sample group.
     fdp <- PeakDensityParam(sampleGroups = rep(1, length(fileNames(res))))
     
     XCMSnExpFile <- groupChromPeaks(res, fdp)
    ## Extract chromatogram of peaks. 
    ## The 'features' defining a subset of features for which chromatograms should be returned. Highly recommended ways to reduce the file size of chromatograms object.
    ## chromatogramsFile <- featureChromatograms(XCMSnExpFile, features = c(1:100),expandRt = 10)
    chromatogramsFile <- featureChromatograms(XCMSnExpFile, expandRt = 10)

    ################################
    #   Output for the online CPVA 
    ################################

    # save peak list file (XCMSnExp object)
    saveRDS(XCMSnExpFile, "XCMSnExpFile.rds")
    # save chromatogram file (Chromatograms object)
    saveRDS(chromatogramsFile,"chromatogramsFile.rds")

2. local CPVA analysis (Example script)

       library(CPVA)
       # Annotation of peaks
       anno <- annotatePeaks(XCMSnExpFile,chromatogramsFile)
       # The Shiny app was activated for visualization of the reports
       cpvaWeb(anno,chromatogramsFile)
   
