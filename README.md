# CPVA

For details see https://stattarget.github.io/docs/

Souece Code: [CPVA package](https://github.com/13479776/cpva/raw/master/ExampleDataset_CPVApos.zip)

Demo data: [Data](https://github.com/13479776/cpva/raw/master/ExampleDataset_CPVApos.zip)

Contaminants Databases: [DB](https://github.com/13479776/cpva/raw/master/1-s2.0-S0003267008007605-mmc1.xls)

#### How to install CPVA package?
Firstly, please install the dependent packages using the following code:

    install.packages(c("RSQLite","shiny","shinyjs", "shinythemes", "plotly","DT","tidyverse","ggplot2","reshape2","stringr","signal",   "data.table","ptw", "timeDate", "dplyr","shinycssloaders", "methods","magrittr"))     
    
    if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages(c("BiocManager","devtools"))
    
    BiocManager::install(c("MSnbase","xcms","CAMERA"),version = "3.10")
    
    devtools::install_github("13479776/peakFinder")
    
Then install CPVA package:

    install.packages(file_path,repos=NULL,type="source") # please replace the file_path with the real path of CPVA package; eg:install.packages("F:/CPVA.0.0.1.tar.gz",repos=NULL,type="source")
    
    
 #### How to use the CPVA?
1. prepare and upload your LC-MS data

   Before the XCMS analysis, raw MS1 data files should be converted into open data format (e.g, mzXML, mzML, cdf). The ProteoWizard's msconvert utility was recommended as the converter.

> peak picking analysis (Example script)
     
     library(faahKO)
     library(xcms)
     if(packageVersion("xcms") < "3.6.0") stop("You need to update 'xcms'")
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

    #########################
    #   Output for CPVA 
    #########################

    # save peak list file (XCMSnExp object)
    saveRDS(XCMSnExpFile, "XCMSnExpFile.rds")
    # save chromatogram file (Chromatograms object)
    saveRDS(chromatogramsFile,"chromatogramsFile.rds")

2. CPVA analysis (Example script)
   
