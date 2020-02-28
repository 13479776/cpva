---
 title: "Parameterized Report for Shiny"
 output: 
   html_document: 
     highlight: tango
     theme: readable
     toc: yes
     toc_depth: 6
---

##### **What is CPVA?**


CPVA is a interactive web tool for visualization and annotation of chromatographic peaks in untargeted metabolomics data. CPVA unfolds the hidden characteristics of chromatographic peaks of LC-MS data through annotation of adducts, isotopes and contaminants, and visualization of peak shape metrics. The purpose of CPVA is to provide a free, user-friendly tool for to reduce the potential noises and contaminants encountered in chromatographic feature lists generated from untargeted metabolomics data. 

Several R packages are used internally, including shiny, ggplot2, xcms, CAMERA, DT, plotly, etc. To submit an issue/bug report/source code, or to request a feature, please feel free to contact Dr. Hemi Luan (Email: luanhm@sustech.edu.cn)

**** 
##### **How to use the CPVA?**



CPVA require the extracted peak list (XCMSnExp object) and chromatograpic file (Chromatograms object) as the input data, that can be obtained by using XCMS package (>= 3.6.0). The step-by-step guides to generate the input files was shown. 

*The max memory size of a single input file does not exceed* `50M !`

**** 

###### `1. prepare and upload your LC-MS data`


1) Before the XCMS analysis, raw MS1 data files should be converted into open data format (e.g, mzXML, mzML, cdf). The [ProteoWizard's](http://proteowizard.sourceforge.net) msconvert utility was recommended as the converter.

2) XCMS analysis (Example script): 


    library(faahKO)
    library(xcms)
    
    if(packageVersion("xcms") < "3.6.0") stop("You need to update 'xcms'")
    
    ## LC-MS data
    fls <- dir(system.file("cdf", package = "faahKO"), recursive = TRUE,
           full.names = TRUE)

    ## Reading the samples 
    ## Only MS1 Levels 
    raw_data <- readMSData(fls,mode = "onDisk", msLevel. = 1L)
    
    ## Perform the chromatographic peak detection using the cenWave method.
    cwp <- CentWaveParam(ppm = 25, noise = 10000)
    res <- findChromPeaks(raw_data, param = cwp)
    
    ## Performing the chromatographic peak grouping. Assigning all samples to
    ## the same sample group.
    fdp <- PeakDensityParam(sampleGroups = rep(1, length(fileNames(res))))
    XCMSnExpFile <- groupChromPeaks(res, fdp)
    
    ## Extract chromatogram of peaks. 
    ## The 'features' defining a subset of features for which chromatograms should be returned. Highly recommended ways to reduce the file size of chromatograms object.
    ## chromatogramsFile <- featureChromatograms(XCMSnExpFile, features = c(1:100),expandRt = 10)
    chromatogramsFile <- featureChromatograms(XCMSnExpFile, expandRt = 10)
    
    #########################
        Output for CPVA 
    #########################
    
    # save peak list file (XCMSnExp object)
    saveRDS(XCMSnExpFile, "XCMSnExpFile.rds")
    # save chromatogram file (Chromatograms object)
    saveRDS(chromatogramsFile,"chromatogramsFile.rds")
    
Click [here (ExampleDataset_CPVA.zip)](https://raw.githubusercontent.com/13479776/cpva/master/ExampleDataset_CPVApos.zip) to download the analysis result for demo dataset.  

**** 


###### `2. Upload your XCMSnExp object and Chromatograms object` (Online version only)


  `The max memory size of a single input file does not exceed 50M !`
  

  ![Upload your XCMSnExp object and Chromatograms object](upload2.JPG)


****   

###### `3. Parameters setting`

**Mass tolerance**: The peaks were putatively identified based on an accurate mass matching against an open-source contaminats databases ([Click to download the contaminats databases](https://github.com/13479776/cpva/blob/master/1-s2.0-S0003267008007605-mmc1.xls?raw=true)) (Keller et al, 2008). The putatively identifed contaminats will be removed automatically. Mass tolerance refer to the fraction of mass in parts-per-million (ppm) between the measured mass and theoretical mass.


**Mass Chromatographic Quality (MCQ) Index**: This MCQ threshold selects high confidence chromatograms from low confidence chromatograms. If an chromatogram has a MCQ index lower than the threshold, the program will annotate it as Low confidence and will remove the chromatogram from the peak lists.


**Adduct ion removal**: Adducts are generally observed in ESI-MS data. The protonated molecular ion (M) was accompanied by kinds of adduct ions, such as [M + Na]+, [M + K]+, [M + NH4]+, etc. Adducts with assigned adduct ions will be removed.


**Polarity**: 'positive' or 'negative' polarity for LC-MS data 

**** 
###### `4.SUBMIT`


Click the 'SUBMIT' button.

**** 

###### `4.Report Download`


Click the 'Result.Download' button at the HomeGuide page. A tab-separated file (as a TSV file) can be downloaded. The tab-separated file showing the remained peaks of shape metrics, isotopes, adduct ions, mass to charge ratio (m/z), retention time, as well as the integrated peak intensities (peak area). 


Summary of the rule-based filtering algorithm

1. Isotopes: The isotope peaks are denoted as [M + 1], [M + 2], . . . ions. [M + 1] is the first isotopic peak for the monoisotopic peak [M]. All isotopic peaks will be removed automatically.

2. Adducts: Adducts are generally observed in ESI-MS data. The protonated molecular ion (M) was accompanied by kinds of adduct ionic peaks, such as [M + Na]+, [M + K]+, [M + NH4]+, etc. Selected adducts will be removed.

3. Contaminants: Contaminants were frequently observed in MS background. Common MS contaminants including 782 species were collected as in-house contaminants database. The matched peaks will be excluded accroding Mass tolerance (PPM).

4. Chromatograms: Confidence levels (High (H) / Low (L)) were evaluated by using the mass chromatograms quality (MCQ) index. If an chromatogram has a MCQ index lower than the threshold, the program will remove the chromatogram and will annotate it as Low confidence. This MCQ threshold selects high confidence chromatograms from low confidence chromatograms.

**** 

##### **Chromatographic peak shape metrics**

**peakSymmetry**: The peakSymmetry was known as the peak asymmetry factor. If value > 1 : tailing, else value < 1 : fronting. Click for more details: http://lcresources.com/resources/TSWiz/hs170.htm

**peakJaggedness**: The peakJaggedness refers to the fraction of time points across a peak where the signal changes direction. The jaggedness scores closer to 0 indicate a smooth peak, whereas 1.0 indicate a noisy peak. 

**peakModality**: The peakModality indicates the largest dip in the peak, being denoted by the ratio of the depth of largest dip to peak height. 

**peakFWHM2base**: The peakFWHM2base denotes full-width at half-max (FWHM) to peak base width ratio, which indicate peak shoulders or poor chromatography.


**MCQ**: The mass chromatographic quality (MCQ) is the similarity index between the original mass chromatograms and their smoothed and mean-subtracted versions (Zhang W, et al, 2014).

**** 

##### **References**

 
 
 Luan H., Ji F., Chen Y., Cai Z. (2018) statTarget: A streamlined tool for signal drift correction and interpretations of quantitative mass spectrometry-based omics data. Analytica Chimica Acta. dio: https://doi.org/10.1016/j.aca.2018.08.002
 
 Smith, C. A.; Want, E. J.; O'Maille, G.; Abagyan, R.; Siuzdak, G., XCMS: processing mass spectrometry data for metabolite profiling using nonlinear peak alignment, matching, and identification. Anal Chem 2006, 78(3), 779-87.
 
 Zhang W; Zhao X. P. Quality evaluation of extracted ion chromatograms and chromatographic peaks in liquid chromatography/mass spectrometry-based metabolomics data. BMC Bioinformatics. 2014, 15(Suppl 11): S5.
 
 Bernd O. Keller, Jie Sui, et al. Interferences and contaminants encountered in modern mass spectrometry, Analytica Chimica Acta, 2008, 627:71-81.
 
 
