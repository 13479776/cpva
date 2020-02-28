

options(shiny.trace=FALSE)
options(timeout = 300)
#options(connectionObserver = )

shinyUI(navbarPage(

  title = div(
    div(
      #id = "img-id",
      #img(src = "title.png",height = 100, width = 90)
    ),
    "CPVA: a web-based metabolomic tool for Chromatographic Peak Visualization and Annotation"
  ),


    #footer = "msCPVA 1.0",
                   #header = "xxxx",
  # start
  #numericInput(inputId="start", label=NULL, value=0),
  tags$head(tags$style(type="text/css", "body {padding-top: 0px;}")),
  tags$head(tags$style(".rightAlign{float:right;}")),
  tags$head(tags$style("#test{height:40vh !important;}")),
  #tags$head(tags$style("#heatmap{height:80vh !important;}")),
  tags$head(tags$style(".title-panel {background: black}")),

  tags$head(tags$style("#cgibarplot{height:40vh !important;}")),
  tags$head(tags$style("#featurebarplot{height:40vh !important;}")),
  tags$head(tags$style("#featurecgibarplot{height:40vh !important;}")),

  # busy indicator - background
  tags$head(tags$style(type="text/css", "
                       #loadmessage {
                       position: fixed;
                       top: 0px;
                       left: 0px;
                       width: 100%;
                       padding: 5px 0px 5px 0px;
                       text-align: center;
                       font-weight: bold;
                       font-size: 100%;
                       color: #F4A460;
                       background-color: #262626;
                       z-index: 105;
                       }
                       ")),

  tags$head(tags$style(type="text/css", "#img-id{
  position: fixed;
  right: 20px;
  top: 500px;
  }")),

  # titile backgroud
  tags$style(type = 'text/css', '.navbar { background-color: #2F4F4F;
                           font-family: Arial;
             font-size: 13px;
             color: #F4A460; }',

             '.navbar-dropdown { background-color: #262626;
             font-family: Arial;
             font-size: 13px;
             color: #FF0000; }',

             '.navbar-default .navbar-brand {
             color: #F4A460;
             }'

  ),


  ##



  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div(img(src="busy2.gif",width="30",height="30"), "Calculation in process... Please be patient (The speed depends on the network, data size etc.)",id="loadmessage")),

  tags$head(tags$style(type='text/css',
                       '#statHG {background-color: rgba(255,255,0,0.40); color: green;}')),

    mainPanel(
      tabsetPanel(type = "pills",
        tabPanel("Home Guide",
                 align = "center",
                 #div(dataTableOutput("table"), style = "font-size:75%"),
                 br(),
                 div(p("What is CPVA?"),style = "font-size:15px;font-weight: bold"),
                 p("CPVA is a interactive web tool for visualization and annotation of chromatographic peaks in untargeted metabolomics data. CPVA unfolds the hidden characteristics of chromatographic peaks of LC-MS data through annotation of adducts, isotopes and contaminants, and visualization of peak shape metrics. The CPVA is a free, user-friendly tool to reduce the potential noises and contaminants encountered in chromatographic feature lists generated from untargeted metabolomics data, resulting in a decrease of the number of non-reliable chromatographic peaks, and subsequently improving the data quality.
                 "),
                 #div(id = 'imagehome', img(src = "introduce.png",height = 380, width = 850)),
                 div(id = 'imagehome1',p("The CPVA service is powered by R shiny package, and is free and open to all users with no login requirement. CPVA can be readily accessed by all popular web browsers including Safari, Google Chrome and Internet Explorer 10, and so on. We would really appreciate if you can submit bug reports and feature requests to Dr. Hemi Luan at luanhm@sustech.edu.cn.")),

                 #div(p("Global Overview of Chromatographic Peaks"),style = "font-size:15px"),
                 #div(p("Global overview of the peak found along the retention time axis.
                 #  Green and chocolate colors represent satisfying and noise-like peaks, respectively."),style = "font-size:13px;color: #CD5C5C"),
                 fluidRow(
                   #column(12,align="left", plotOutput('imagehome')),
                   #column(12,align="left", textOutput('imagehome1')),

                   column(12,align="left", div(textOutput('texthome0'),style = "font-size:15px;font-weight: bold")),

                   column(12,align="center", div(textOutput('texthome1'),style = "font-size:15px")),
                   column(12,align="center", div(textOutput('texthome2'),style = "font-size:13px; color: #CD5C5C")),

                   #div(p("Global overview of the peak found along the retention time axis.
                   #  Green and chocolate colors represent satisfying and noise-like peaks, respectively."),style = "font-size:13px;color: #CD5C5C"),

                   #column(12,align="left", div(textOutput('txtHG'),style = "font-size:15px")),
                   column(12,align="center",
                          fluidRow(
                            column(12,align="center", plotlyOutput('rectHG') %>% withSpinner(type = 6, color="#0dc5c1",color.background = 3)),
                            br(),

                            #column(12,align="center", plotlyOutput('rectHG')),
                            br(),
                            br(),
                            column(12,align="left", div(verbatimTextOutput('statHG'),style = "font-size:15px")),
                            column(12,align="right", downloadButton('downloadData', 'peakTable.Download')),
                            column(12,align="right", downloadButton('downloadDataPDF', 'peakMap_PDF.Download')),
                            br()
                            #column(12,align="right", downloadButton('tableX', 'peakTableALL.Download'))
                            #column(12,align="left", div(textOutput('txtHG'),style = "font-size:15px"))

                          )
                   )
                 ),
                 br(),

                 div(id = 'homeanno', p("*Results Informations*"),style = "font-size:15px"),
                 div(id = 'homeanno1', p("1. Isotopes: The isotope peaks are denoted as [M + 1], [M + 2], . . . ions. [M + 1] is the first isotopic peak for the monoisotopic peak [M]. All isotopic peaks will be removed automatically.")),
                 div(id = 'homeanno2',p("2. Adducts: Adducts are generally observed in ESI-MS data. The protonated molecular ion (M) was accompanied by kinds of adduct ionic peaks, such as [M + Na]+, [M + K]+, [M + NH4]+, etc. Selected adducts will be removed.")),
                 div(id = 'homeanno3',p("3. Contaminants: Contaminants were frequently observed in MS background. Common MS contaminants including 782 species were collected as in-house contaminants database. The matched peaks will be excluded accroding Mass tolerance (PPM).")),
                 div(id = 'homeanno4',p("4. Chromatograms: Confidence levels (High (H) / Low (L)) were evaluated by using the mass chromatograms quality (MCQ) index. If an chromatogram has a MCQ index lower than the threshold, the program will remove the chromatogram and will annotate it as Low confidence. This MCQ threshold selects high confidence chromatograms from low confidence chromatograms."))

                 ),
        tabPanel("Chromatographic Pattern",
                 align = "left",
                 br(),
                 #div(dataTableOutput("table"), style = "font-size:75%")
                 #h1('A Client-side Table'),
                 #body <- dashboardBody(
                 #  fluidRow(
                 #    box(plotlyOutput('x3',height = "200px")),
                 #    box(plotlyOutput('x2',height = "200px")),
                 #    box(DT::dataTableOutput('x1'))
                 #  )
                 #)


                 fluidRow(
                   column(12,align="center", div(textOutput('tx'),style = "font-size:18px")),
                   column(12,align="left", div(textOutput('txd'),style = "font-size:13px")),
                   br(),
                   br(),
                   br(),
                   column(5, plotlyOutput('x3',width = "300px", height = "340px") %>% withSpinner(type = 6, color="#0dc5c1",color.background = 3)),
                   column(7, plotlyOutput('x2',width = "400px", height = "340px")%>% withSpinner(type = 6, color="#0dc5c1",color.background = 3)),

                   column(12, div(DT::dataTableOutput('x1'),style = "font-size:75%"))


                 )

        ),

        tabPanel("Peaks Annotation",
                 align = "center",
                 br(),
                 fluidRow(
                   column(12,align="center", div(textOutput('txfg'),style = "font-size:18px")),
                   column(12,align="left", div(textOutput('txdfg'),style = "font-size:13px")),
                   br(),
                   br(),
                   column(width = 6,
                          plotlyOutput("cgibarplot")
                   ),
                   column(width = 6,
                          plotlyOutput("featurebarplot")
                   ),#column
                   column(width = 12,
                          plotlyOutput("featurecgibarplot")
                   )#column
                 )#fluidRow
        ),
        tabPanel("Help",
                 #align = "left",
                 class = 'rightAlign',
                 column(width = 12,
                          align = "left",
                          br(),
                          #htmlOutput("inc")
                          uiOutput('markdown')
                   #column
                 )#fluidRow
        )
        #tabPanel
        #tabPanel("News",
                 #align = "left",
               #  class = 'leftAlign',
               #  column(width = 12,
                #        align = "left",
                #        br(),
                        #htmlOutput("inc")
                 #       uiOutput('markdownNews')
                        #column
                 )#fluidRow
        )#tabPanel

        )#tabsetPanel
      #)
    #)
  #)
)


