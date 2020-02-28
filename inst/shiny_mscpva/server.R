
options(shiny.maxRequestSize=500*1024^2)
options(shiny.trace=FALSE)


shinyServer(function(input, output,session){

  session$onSessionEnded(stopApp)
  session$allowReconnect(TRUE)

  ###



  output$texthome0 <- NULL
  output$texthome1 <- NULL
  output$texthome2 <- NULL
  output$rectHG <- NULL
  output$statHG <- NULL


  Cutoff_repalce <-  reactive({
                                    #hide
                                    hide("imagehome")
                                    hide("imagehome1")
                                    #show("downloadData")


                                    output$texthome0 <- renderText({
                                      "CPVA Reports"
                                    })
                                    output$texthome1 <- renderText({
                                      "Global Overview of Chromatographic Peaks"
                                    })
                                    output$texthome2 <- renderText({
                                      "Green and chocolate colors represent satisfying and noise-like peaks, respectively."
                                    })

                                    #v$Cutoff_repalce <- readRDS("exampleData.rds")
                                    Cutoff_repalce <- annoResults_mscpva_000000001
                                    #print(v$Cutoff_repalce)
                                       #finnaldata <- exam
                                       #v$Cutoff_repalce <- exam


                                  })



  #show or hide button
  shinyjs::enable("downloadData")
  shinyjs::enable("downloadDataPDF")
  shinyjs::enable("tableX")
  shinyjs::show("homeanno")
  shinyjs::show("homeanno1")
  shinyjs::show("homeanno2")
  shinyjs::show("homeanno3")
  shinyjs::show("homeanno4")



    #show or hide button
    shinyjs::disable("downloadData")
    shinyjs::disable("downloadDataPDF")
    shinyjs::disable("tableX")
    hide("homeanno")
    hide("homeanno1")
    hide("homeanno2")
    hide("homeanno3")
    hide("homeanno4")



    
    
    ##########
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("peakTable-", Sys.Date(), ".tsv", sep="")
      },
      content = function(file) {
        write.table(Cutoff_repalce()$dataOut$filterMatrix, file,  quote = FALSE, sep = "\t", col.names = NA)
      }
    )
    
    
    output$tableX <- downloadHandler(
      filename = function() {
        paste("peakTableALL-", Sys.Date(), ".tsv", sep="")
      },
      content = function(file) {
        write.table(Cutoff_repalce()$datainput, file,  quote = FALSE, sep = "\t", col.names = NA)
      }
    )
    
    
    
    #####
    
    output$downloadDataPDF <- downloadHandler(
      filename ="peakMap.pdf",
      content = function(file) {
        pdf(file, width=10, height=6.3)
        print(grectPlot(Cutoff_repalce()$datainput,Cutoff_repalce()$dataOut$index,size = 3))
        dev.off()
      })
    
    
    
    
    
  #############No data ############
  ####
  ###
  # DT table plot
  output$tx <- renderText({
    ""
  })
  output$txd <- renderText({
    "The quality of extracted ion chromatographic peak is critical for LC-MS based quantitative metabolomics.
    The 'bad' chromatographic peak are difficult to being integrated reproducibly,
    resulting inaccurate peak quantitation. CPVA visualizes the extracted ion chromatographic peaks (Left side of figure below),
    and provides Multi-Metrics for evaluation of quality of chromatographic peaks (Right side of figure below)."
  })


  output$txfg <- renderText({
    ""
  })
  output$txdfg <- renderText({
    "Annotation of adducts, isotopes and contaminants."
  })

  output$txtHG <- renderText({
    ""
  })



  output$markdown <- renderUI({
    #HTML(markdown::markdownToHTML(knit('RMarkdownFile.Rmd', quiet = TRUE)))
    withMathJax(includeMarkdown("RMarkdownFile.md"))
  })
  output$markdownNews <- renderUI({
    #HTML(markdown::markdownToHTML(knit('RMarkdownFile.Rmd', quiet = TRUE)))
    withMathJax(includeMarkdown("RMarkdownFileNews.md"))
  })

  output$x1 <- DT::renderDataTable(

    if (is.null(Cutoff_repalce())) {
      return()
      } else {
    Cutoff_repalce()$datainput[,1:10]},
    server = FALSE,
    selection = "single",
    options = list(pageLength=10))

  # highlight selected rows in the scatterplot
  output$x2 = renderPlotly({


    if (!is.null(dev.list())) graphics.off()
    gc()
    gc()
    pdf(NULL)
    if (is.null(Cutoff_repalce())) {return()} else {
    s = input$x1_rows_selected
    if(is.null(s)) s <- 1
    #par(mar = c(2, 4, 1, 1))
    m <- list(
      l = 150, #left mar
      r = 200,
      b = 90, #
      t = 100,
      pad = 4
    )
    suppressWarnings(peakRank(Cutoff_repalce()$mericR,s,m))}
  })
  
  output$x3 = suppressMessages(renderPlotly({
    #dev.off()

    if (!is.null(dev.list())) graphics.off()
    gc()
    gc()
    pdf(NULL)
    if (is.null(Cutoff_repalce())) {return()} else {
    s = input$x1_rows_selected
    if(is.null(s)) s <- 1
    #par(mar=c(5,7,4,2)+0.1)
    par(mar = c(80, 0, 20, 80))
    #ggthemr_reset()
    ggplotly(peakFinder:::plotChroma(Cutoff_repalce()$dat_chroma,
                                                      s,expandRT = 10,facet_wrap = F,
                                                      leg.pos = "none"),height = 300,width = 350)}
    #graphics.off()
  })
  )
  output$rectHG <- renderPlotly({
    if (is.null(Cutoff_repalce())) {return()} else {
      suppressWarnings(rectplot(Cutoff_repalce()$datainput,Cutoff_repalce()$dataOut$index,size = 3)) }
  })

  output$statHG <- renderText({
    if (is.null(Cutoff_repalce())) {return()} else {
    paste("Total of ", Cutoff_repalce()$dataOut$sum[[1]], " isotopes, ",
          Cutoff_repalce()$dataOut$sum[[2]], " adducts, ",
          Cutoff_repalce()$dataOut$sum[[3]], " contaminants, " ,
          Cutoff_repalce()$dataOut$sum[[4]], " low quality of chromatograms were found and removed. Total of ",
          dim(Cutoff_repalce()$dataOut$filterMatrix)[1], " peaks were acceptable."
          ,sep = ""
          )
    }

  })





  output$cgibarplot <- renderPlotly({
    if (is.null(Cutoff_repalce())) {return()} else {
    pieplot_iso(Cutoff_repalce()$isoPie)}
    #pieplot_iso(isoPie)
  })
  output$featurebarplot <- renderPlotly({
    if (is.null(Cutoff_repalce())) {return()} else {
    #innerfeaturebarplot(Cutoff_repalce()$h.feature)
    pieplot_addu(Cutoff_repalce()$adducPie)}
  })
  output$featurecgibarplot <- renderPlotly({
    if (is.null(Cutoff_repalce())) {return()} else {
    #innerfeaturecgibarplot(Cutoff_repalce()$h.feature.cgi)
    #datacgi <- print(Cutoff_repalce()$contaPie)
    pieplot_conta(Cutoff_repalce()$contaPie)
      }
  })






}
)
