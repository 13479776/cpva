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
    
    BiocManager::install("xcms",version = "3.10")
    BiocManager::install("CAMERA",version = "3.10")
    
    devtools::install_github("cttobin/ggthemr")
    
