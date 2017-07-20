library(shiny)
library(shinyjs)
library(shinythemes)
library(DT)
library(cequre)
library(survival)
library(survMisc)
#library(survplot)
library(gplots)
library(ggplot2)
library(survminer)
library(gridExtra)
library(cmprsk)
library(dynpred)
library(reshape2)
library(RColorBrewer)

ui <- fluidPage(
  navbarPage("CASAS: Cancer Survival Analysis Suite", 
             
             tabPanel("Standard Survival Analysis",
                      fluidRow(
                        column(2,
                               wellPanel(
                                 h4(strong("Input your file")),
                                 selectInput("file2",label= "Select an example ds or upload your own with 'Load my own'", 
                                             choices = c("Example ds File"="Example2", "Load my own data" = "load_my_own2")),
                                 conditionalPanel("input.file2 == 'load_my_own2'",
                                                  fileInput('file22', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                 conditionalPanel("input.file2 == 'Example2'",
                                                  downloadButton('downloadEx2', 'Download Example data')
                                 )),
                               conditionalPanel("input.cPanels1 == 2",
                               wellPanel(
                                 h4(strong("KM Analysis")),
                                 selectInput("select21", "Select a Variable of Interest as Cohort Group", 
                                             choices=c("AGE", "RACE")),
                                 selectInput("binary",label= "The variable of interest is categorical or continuous", 
                                             choices = c("Categorical Variable" = "categorical", "Continuous Variable"="continuous"), selected= "continuous"),
                                 conditionalPanel("input.binary == 'continuous'",
                                                  radioButtons("cutoff2", "Choose Cutoff Point for Continuous Variable:", list("Optimal Cutoff" = 1,"25th Percentile" = 25,"50th Percentile" = 50, "75th Percentile" = 75), selected = 25)),
                                 selectInput("select22", "Select a Time Variable to Visualize KM Plot", 
                                             choices=c("os")),
                                 selectInput("select23", "Select a Censoring Variable to Visualize KM Plot", 
                                             choices=c("os_censor")),
                                 radioButtons("time", "Time Unit:", list("Years" = 1, "Months" = 2, "Days" = 3), selected = 1),
                                 radioButtons("riskt", "Risk Table:", list("Yes" = TRUE, "No" = FALSE), selected = TRUE),
                                 hr()                                
                               )),
                               conditionalPanel("input.cPanels1 == 3",
                               wellPanel(
                                 h4(strong("Univariate Association Analysis")),
                                 selectInput("select24", "Select a Time Variable for Survival Analysis", 
                                             choices=c("os")),
                                 selectInput("select25", "Select a Censoring Variable for Survival Analysis", 
                                             choices=c("os_censor")),
                                 selectInput("show_vars26", "Select multiple Variables to Generate Univariate Survival Association Table", 
                                             c("AGE", "RACE"), choices=c("AGE", "RACE"), multiple = TRUE),
                                 radioButtons("assum", "Test for Proportional Hazards Assumption:", list("Yes" = 1,"No" = 0), selected = 0),
                                 hr()                                
                               )),
                               
                               conditionalPanel("input.cPanels1 == 3",
                               h4(strong("Downloads")),
                               wellPanel(
                                 textInput("fname22", "Type the file name you would like to save as", value = "survivaltable"),
                                 downloadButton('x1', 'Download Survival Report')
                               )),
                               conditionalPanel("input.cPanels1 == 2",
                               h4(strong("Downloads")),
                               wellPanel(
                                 textInput("fname21", "Type the file name you would like to save as", value = "kmplot"),
                                 downloadButton('downloadKM', 'Download KM Plot')
                               )
                        )),
                        column(10,
                               tabsetPanel(
                               tabPanel("Read Me", htmlOutput("ReadMe2"), value =1),
                               tabPanel("KM Analysis and Plot", htmlOutput("pv21"), plotOutput("kmplot", height= 600, width = 800), value =2),
                               tabPanel("Univariate Survival Association (Cox Model)", htmlOutput("pv22"),
                                         DT::dataTableOutput("out2"), value = 3),
                               id = "cPanels1"
                               )                               #uiOutput("out2")
                               
                        ),
                        column(12,
                               tags$head(tags$style(type="text/css", "
                                                    #loadmessage {
                                                    position: fixed;
                                                    bottom: 0px;
                                                    right: 0px;
                                                    width: 100%;
                                                    padding: 5px 0px 5px 0px;
                                                    text-align: center;
                                                    font-weight: bold;
                                                    font-size: 100%;
                                                    color: #000000;
                                                    background-color: #b8b8b8;
                                                    z-index: 105;
                                                    }
                                                    ")),
                               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                tags$div("Loading...",id="loadmessage"))
                               
                               )
                        )),
             tabPanel("Competing Risk Survival Analysis",
                      fluidRow(
                        column(2,
                               wellPanel(
                                 h4(strong("Input your file")),
                                 selectInput("file3",label= "Select an example ds or upload your own with 'Load my own'", 
                                             choices = c("Example ds File"="Example3", "Load my own data" = "load_my_own3")),
                                 conditionalPanel("input.file3 == 'load_my_own3'",
                                                  fileInput('file32', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                 conditionalPanel("input.file3 == 'Example3'",
                                                  downloadButton('downloadEx3', 'Download Example data')
                                 )),
                               conditionalPanel("input.cPanels2 == 2",
                               wellPanel(
                                 h4(strong("CIF Analysis")),
                                 selectInput("select31", "Select a Variable of Interest as Cohort Group", 
                                             choices=c("AGE", "RACE")),
                                 selectInput("binary2",label= "The variable of interest is categorical or continuous", 
                                             choices = c("Categorical Variable" = "categorical", "Continuous Variable"="continuous")),
                                 conditionalPanel("input.binary2 == 'continuous'",
                                                  radioButtons("cutoff3", "Choose Cutoff Point for Continuous Variable:", list("25th Percentile" = 25,"50th Percentile" = 50, "75th Percentile" = 75), selected = 25)),
                                 selectInput("select32", "Select a Time Variable to Visualize CIF Plot", 
                                             choices=c("os")),
                                 selectInput("select33", "Select a Censoring Variable to Visualize CIF Plot", 
                                             choices=c("os_censor")),
                                 radioButtons("points", "Input Time Points:", list("Yes" = 1, "No" = 0), selected = 0),
                                 textInput("text", label = "Time Points Input", value = "0, 10, 20, 30"),
                                 radioButtons("time2", "Time Unit:", list("Years" = 1, "Months" = 2, "Days" = 3), selected = 2),
                                 radioButtons("event", "Event Code:", list("1" = 1, "2" = 2, "0" = 0), selected = 2),
                                 radioButtons("censor", "Censor Code:", list("1" = 1, "2" = 2, "0" = 0), selected = 0),
                                 hr()                                
                               )),
                               conditionalPanel("input.cPanels2 == 3",
                               wellPanel(
                                 h4(strong("Univariate Association Analysis")),
                                 selectInput("select34", "Select a Time Variable for Competing Risk Survival Analysis", 
                                             choices=c("os")),
                                 selectInput("select35", "Select a Censoring Variable for Competing Risk Survival Analysis", 
                                             choices=c("os_censor")),
                                 selectInput("show_vars36", "Select multiple Variables to Generate Univariate Survival Association Table", 
                                             c("AGE", "RACE"), choices=c("AGE", "RACE"), multiple = TRUE),
                                 radioButtons("event2", "Event Code:", list("1" = 1, "2" = 2, "0" = 0), selected = 2),
                                 radioButtons("censor2", "Censor Code:", list("1" = 1, "2" = 2, "0" = 0), selected = 0),
                                 hr()                                
                               )),
                               conditionalPanel("input.cPanels2 == 3",
                               h4(strong("Downloads")),
                               wellPanel(
                                 textInput("fname32", "Type the file name you would like to save as", value = "crrtable"),
                                 downloadButton('x2', 'Download Competing Risk Report')
                               )),
                               conditionalPanel("input.cPanels2 == 2",
                               h4(strong("Downloads")),
                               wellPanel(
                                 textInput("fname31", "Type the file name you would like to save as", value = "cifplot"),
                                 downloadButton('downloadcif', 'Download CIF Plot')
                               ),
                               wellPanel(
                                 textInput("fname33", "Type the file name you would like to save as", value = "ciftable"),
                                 downloadButton('downloadciftable', 'Download CIF Report')
                               )
                               )
                        ),
                        column(10,
                               tabsetPanel(
                                 tabPanel("Read Me", htmlOutput("ReadMe3"), value = 1),
                                 tabPanel("CIF Analysis and Plot", htmlOutput("pv31"),
                                          plotOutput("cifplot", height= 600, width = 800),
                                          DT::dataTableOutput("ciftable"), value = 2),
                                 tabPanel("Univariate Survival Association (Fine and Gray Model)", htmlOutput("pv32"),
                                          DT::dataTableOutput("out3"), value = 3),
                                 id = "cPanels2")                               #uiOutput("out2")
                               
                               #uiOutput("out2")
                               
                        ),
                        column(12,
                               tags$head(tags$style(type="text/css", "
                                                    #loadmessage {
                                                    position: fixed;
                                                    bottom: 0px;
                                                    right: 0px;
                                                    width: 100%;
                                                    padding: 5px 0px 5px 0px;
                                                    text-align: center;
                                                    font-weight: bold;
                                                    font-size: 100%;
                                                    color: #000000;
                                                    background-color: #b8b8b8;
                                                    z-index: 105;
                                                    }
                                                    ")),
                               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                tags$div("Loading...",id="loadmessage"))
                               
                               )
                        )),
             tabPanel("Landmark Survival Analysis",
                      fluidRow(
                        column(2,
                               wellPanel(
                                 h4(strong("Input your file")),
                                 selectInput("file4",label= "Select an example ds or upload your own with 'Load my own'", 
                                             choices = c("Example ds File"="Example4", "Load my own data" = "load_my_own4")),
                                 conditionalPanel("input.file4 == 'load_my_own4'",
                                                  fileInput('file42', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                 conditionalPanel("input.file4 == 'Example4'",
                                                  downloadButton('downloadEx4', 'Download Example data')
                                 )),
                               wellPanel(
                                 h4(strong("KM Analysis")),
                                 selectInput("select41", "Select a Variable of Interest as Cohort Group", 
                                             choices=c("AGE", "RACE")),
                                 selectInput("binary3",label= "The variable of interest is categorical or continuous", 
                                             choices = c("Categorical Variable" = "categorical", "Continuous Variable"="continuous")),
                                 conditionalPanel("input.binary3 == 'continuous'",
                                                  radioButtons("cutoff4", "Choose Cutoff Point for Continuous Variable:", list("Optimal Cutoff" = 1,"25th Percentile" = 25,"50th Percentile" = 50, "75th Percentile" = 75), selected = 25)),
                                 selectInput("select42", "Select a Time Variable to Visualize KM/CIF Plot", 
                                             choices=c("os")),
                                 selectInput("select43", "Select a Censoring Variable to Visualize KM/CIF Plot", 
                                             choices=c("os_censor")),
                                 selectInput("select44", "Select a Time Dependent Variable to Visualize KM/CIF Plot", 
                                             choices=c("wtime")),
                                       textInput("text2", label = "Input Time Point for Landmark Analysis", value = "200"),
                                 radioButtons("time3", "Time Unit:", list("Years" = 1, "Months" = 2, "Days" = 3), selected = 3),
                                 radioButtons("option", "KM or CIF:", list("KM" = TRUE, "CIF" = FALSE), selected = TRUE),
                                 radioButtons("riskt2", "Risk Table:", list("Yes" = TRUE, "No" = FALSE), selected = TRUE),
                                 hr()                                
                               ),
                               h4(strong("Downloads")),
                               wellPanel(
                                 textInput("fname41", "Type the file name you would like to save as", value = "landmarkplot"),
                                 downloadButton('downloadlandmarkplot', 'Download Landmark Plot')
                               )
                        ),
                        column(10,
                               tabsetPanel(
                                 tabPanel("Read Me", htmlOutput("ReadMe4") ),
                                 tabPanel("Landmark Survival Plot", htmlOutput("pv41"),
                                 plotOutput("landmarkplot", height= 600, width = 800))
                               )                               #uiOutput("out2")

                               #uiOutput("out2")
                               
                        ),
                        column(12,
                               tags$head(tags$style(type="text/css", "
                                                    #loadmessage {
                                                    position: fixed;
                                                    bottom: 0px;
                                                    right: 0px;
                                                    width: 100%;
                                                    padding: 5px 0px 5px 0px;
                                                    text-align: center;
                                                    font-weight: bold;
                                                    font-size: 100%;
                                                    color: #000000;
                                                    background-color: #b8b8b8;
                                                    z-index: 105;
                                                    }
                                                    ")),
                               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                tags$div("Loading...",id="loadmessage"))
                               
                               )
                        )),
             tabPanel("Quantile Survival Analysis",  
                      fluidRow(
                        column(2,
                               wellPanel(
                                 h4(strong("Input your file")),
                                 selectInput("file1",label= "Select an example ds or upload your own with 'Load my own'", 
                                             choices = c("Example ds File"="Example", "Load my own data" = "load_my_own")),
                                 conditionalPanel("input.file1 == 'load_my_own'",
                                                  fileInput('file12', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                 conditionalPanel("input.file1 == 'Example'",
                                                  downloadButton('downloadEx', 'Download Example data')
                                 )),
                               wellPanel(
                                 h4(strong("Quantile Survival Analysis")),
                                 selectInput("select", "Select a Variable of Interest", 
                                             choices=c("AGE", "RACE")),
                                 selectInput("binary4",label= "The variable of interest is categorical or continuous", 
                                             choices = c("Categorical Variable" = "categorical", "Continuous Variable"="continuous")),
                                 conditionalPanel("input.binary4 == 'continuous'",
                                                  radioButtons("cutoff", "Choose Cutoff Point:", list("Optimal Cutoff" = 1,"25th Percentile" = 25,"50th Percentile" = 50, "75th Percentile" = 75), selected = 25),
                                                  radioButtons("exp_ref", "Choose Reference Level:", list("High" = "High","Low" = "Low"), selected = "High")
                                 ),
                                 selectInput("select12", "Select a Time Variable for Quantile Analysis", 
                                             choices=c("os")),
                                 selectInput("select13", "Select a Censoring Variable for Quantile Analysis", 
                                             choices=c("os_censor")),
                                 radioButtons("time4", "Time Unit:", list("Years" = 1, "Months" = 2, "Days" = 3), selected = 1),
                                 actionButton("numb", "Run & generate random numbers"),
                                 h5("Hit Run & Generate random number button above EACH time to display output on main panel."),
                                 hr()                              # Copy the line below to make a select box 
                               ),
                               h4(strong("Downloads")),
                               wellPanel(
                                 textInput("fname11", "Type the file name you would like to save as", value = "QAplot"),
                                 downloadButton('downloadQA', 'Download Quantile Survival Plot')),
                               wellPanel(
                                 textInput("fname12", "Type the file name you would like to save as", value = "forestplot"),
                                 downloadButton('downloadFP', 'Download Forest Plot')),
                               wellPanel(
                                 textInput("fname13", "Type the file name you would like to save as", value = "Data for Grid"),
                                 downloadButton('downloadTB', 'Download Grid Data')
                               )
                        ),
                        column(10,
                               tabsetPanel(
                                 tabPanel("Read Me", htmlOutput("ReadMe1")),
                                 tabPanel("Quantile Survival Plots and Output",                                
                                          htmlOutput("pv15"),
                                          splitLayout(cellWidths = c("60%", "40%"), htmlOutput("pv11"), htmlOutput("pv12")),
                                          splitLayout(cellWidths = c("60%", "40%"), plotOutput("QAplot", height= 600, width = 800),
                                                      plotOutput("forestplot", height= 500, width = 600)),
                                          htmlOutput("pv13"),
                                          DT::dataTableOutput("coxout"),
                                          htmlOutput("pv14"),
                                          DT::dataTableOutput("CIout")
                                 )
                               )                               #uiOutput("out2")
                               #htmlOutput("pv"),
                               #plotOutput("QAplot", height= 600, width = 800),
                               #htmlOutput("pv2"),
                               #plotOutput("forestplot", height= 600, width = 800)
                               #htmlOutput("blurp"))
                               #h5("Descriptive Statistics"),
                               #verbatimTextOutput("out")
                               
                        ),
                        column(12,
                               tags$head(tags$style(type="text/css", "
                                                    #loadmessage {
                                                    position: fixed;
                                                    bottom: 0px;
                                                    right: 0px;
                                                    width: 100%;
                                                    padding: 5px 0px 5px 0px;
                                                    text-align: center;
                                                    font-weight: bold;
                                                    font-size: 100%;
                                                    color: #000000;
                                                    background-color: #b8b8b8;
                                                    z-index: 105;
                                                    }
                                                    ")),
                               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                tags$div("Loading...",id="loadmessage"))
                               
                               )
                        )),
             tabPanel("Optimal Cutoff Point Finder",  
                      fluidRow(
                        column(2,
                               wellPanel(
                                 h4(strong("Input your file")),
                                 selectInput("file5",label= "Select an example ds or upload your own with 'Load my own'", 
                                             choices = c("Example ds File"="Example5", "Load my own data" = "load_my_own5")),
                                 conditionalPanel("input.file5 == 'load_my_own5'",
                                                  fileInput('file51', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                 conditionalPanel("input.file5 == 'Example5'",
                                                  downloadButton('downloadEx5', 'Download Example data')
                                 )),
                               wellPanel(
                                 h4(strong("Choose Variable")),
                                 selectInput("select51", "Select Input", 
                                             choices=c("AGE", "RACE")),
                                 selectInput("select52", "Select a Time Variable", 
                                             choices=c("os")),
                                 selectInput("select53", "Select a Censoring Variable", 
                                             choices=c("os_censor")),
                                 hr()                              # Copy the line below to make a select box 
                               ),
                               h4(strong("Downloads")),
                               wellPanel(
                                 textInput("fname51", "Type the file name you would like to save as", value = "MRplot"),
                                 downloadButton('downloadMR', 'Download Martingale Residual Plot')
                               )
                        ),
                        column(10,
                               tabsetPanel(
                                 tabPanel("Read Me", htmlOutput("ReadMe5")),
                                 tabPanel("Optimal Cutpoint Finder Output)", plotOutput("MRplot", height= 600, width = 800),
                                          htmlOutput("pv51"),
                                          DT::dataTableOutput("Optimal"))
                               )                               #uiOutput("out2")
                               #htmlOutput("pv4"),
                               
                               #htmlOutput("blurp"))
                               #h5("Descriptive Statistics"),
                               #verbatimTextOutput("out")
                               
                        ),
                        column(12,
                               tags$head(tags$style(type="text/css", "
                                                    #loadmessage {
                                                    position: fixed;
                                                    bottom: 0px;
                                                    right: 0px;
                                                    width: 100%;
                                                    padding: 5px 0px 5px 0px;
                                                    text-align: center;
                                                    font-weight: bold;
                                                    font-size: 100%;
                                                    color: #000000;
                                                    background-color: #b8b8b8;
                                                    z-index: 105;
                                                    }
                                                    ")),
                               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                tags$div("Loading...",id="loadmessage"))
                               
                               )
                        )),    
             tabPanel("Grid for Summarized Significance",
                      fluidRow(
                        column(2,
                               wellPanel(
                                 h4(strong("Input your file")),
                                 selectInput("file6",label= "Select an example ds or upload your own with 'Load my own'", 
                                             choices = c("Example ds File"="Example6", "Load my own data" = "load_my_own6")),
                                 conditionalPanel("input.file6 == 'load_my_own6'",
                                                  fileInput('file61', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                 conditionalPanel("input.file6 == 'Example6'",
                                                  downloadButton('downloadEx6', 'Download Example data')
                                 )),
                               wellPanel(
                                 radioButtons("exp_ref2", "Choose Reference Level:", list("High" = "High","Low" = "Low"), selected = "High")
                                 ),
                             h4(strong("Downloads")),
                             wellPanel(
                                 textInput("fname61", "Type the file name you would like to save as", value = "Grid"),
                                 downloadButton('downloadHM', 'Download Grid Plot')
                               )
                        ),
                        column(10,
                              tabsetPanel(
                               tabPanel("Read Me", htmlOutput("ReadMe6")),
                               tabPanel("Summarized Significance Grid", htmlOutput("pv61"),
                                        plotOutput("heatmapplot", height= 800, width = 800))
                        )                               #uiOutput("out2")
                               
                               #htmlOutput("pv3"), 
                               #htmlOutput("title"),
                               #DT::dataTableOutput("SuperPC")
                        ),
                        column(12,
                               tags$head(tags$style(type="text/css", "
                                                    #loadmessage {
                                                    position: fixed;
                                                    bottom: 0px;
                                                    right: 0px;
                                                    width: 100%;
                                                    padding: 5px 0px 5px 0px;
                                                    text-align: center;
                                                    font-weight: bold;
                                                    font-size: 100%;
                                                    color: #000000;
                                                    background-color: #b8b8b8;
                                                    z-index: 105;
                                                    }
                                                    ")),
                               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                tags$div("Loading...",id="loadmessage"))
                               
                               )
                      )),
             tabPanel("Tutorial",
                      tags$iframe(src= "CASAS_Tutorial.pdf", width = 1800, height = 1000)),
             navbarMenu("About Us",
                        tabPanel("How to Cite",
                                 fluidRow(
                                   column(8, offset = 2,
                                          "Rupji M, Zhang X and Kowalski J. CASAS: Cancer Survival Analysis Suite, a web based application [version 1; referees: awaiting peer review]. F1000Research 2017, 6:919 (doi: 10.12688/f1000research.11830.1)",
                                          br(),
                                          br(),
                                          "The National Cancer Institute (NCI) requires that publications acknowledge the Winship Cancer Institute CCSG support, and they are tracking compliance. When using this tool to report results in your publication, please include the following statement in the acknowledgment section of your publication(s):",
                                          br(),
                                          br(),
                                          em("Research reported in this publication was supported in part by the Biostatistics and Bioinformatics Shared Resource of Winship Cancer Institute of Emory University and NIH/NCI under award number P30CA138292. The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institutes of Health.")
                                   ))),
                        tabPanel("Contact Us",
                                 fluidRow(
                                   column(8, offset = 2,
                                          "This tool was prepared by members of the Winship Biostatistics and Bioinformatics Shared Resource (BBISR) of Emory University.",
                                          br(),
                                          a(href="https://bbisr.winship.emory.edu/", "https://bbisr.winship.emory.edu/"),
                                          br(),
                                          br(),
                                          "Authors- Manali Rupji, dual M.S., Xinyan (Abby) Zhang, MPH. & Jeanne Kowalski Ph.D.",
                                          br(),
                                          "Maintainer- Manali Rupji 'manali(dot)rupji(at)emory(dot)edu'")
                                 )),
                        tabPanel("Feedback",
                                 fluidRow(
                                   column(8, offset = 2,
                                          #br(),
                                          "As a Biostatistics and Bioinformatics core, we are actively improving and expanding our NGS analysis services and analysis products. For any questions, comments, or suggestions, please email the developer at manali(dot)rupji(at)emory(dot)edu."
                                   )))
             )
  ))



################################################
################ SERVER.R ######################
################################################


options(shiny.maxRequestSize= 500*1024^2)
options(shiny.sanitize.errors = TRUE)
#options(bitmapType='cairo')

source("QSE.R")
source("forestplot.R")
source("significant.R")
source("boxcolor.R")
source("survplot.R")
source("cutpoint finder.R")
source("optimalcut_plot.R")
source("optimalcut_table.R")
source("CumIncidence.R")
source("factor2ind.R")


server <- function(input, output, session){
  
  data_input <- reactive({
    if(input$file1 == 'Example'){
      d <- read.csv("data/BRCA_for_quantile_survival_analysis.csv", header =T, sep =",")
    }
    else if(input$file1 == 'load_my_own'){
      inFile <- input$file12
      if (is.null(inFile))
        return(NULL)
      else if(grepl(".xlsx", inFile[1])) { d = read.xlsx(as.character(inFile$datapath), colNames = TRUE, rowNames = F, as.is = T) }
      else if(grepl(".csv", inFile[1])) { d = read.csv(as.character(inFile$datapath), header = TRUE, sep = ",", stringsAsFactors = F, as.is = T, fill = T) }
      else if(grepl(".txt", inFile[1])) { d = read.table(as.character(inFile$datapath), header = TRUE, sep = "\t", stringsAsFactors = F, as.is = T, fill = T) }
    }
    else 
      return(NULL)
    # dim(data)
    Dataset <- data.frame(d)
    return(as.data.frame(Dataset))
  })
  
  output$downloadEx <- downloadHandler(
    
    filename <- function() {
      paste('Example ds', Sys.time(),'.csv', sep='')
    },
    content <- function(file) {
      ds <- data_input()#res()$CI_Data2
      write.csv(ds, file, row.names = FALSE)
    }
  )
  
  observe({
    dsnames <- colnames(data_input())
    cb_options <- dsnames
    updateSelectInput(session, "select", label = "Select Variable of Interest",
                      choices = cb_options,
                      selected = tail(cb_options))
    updateSelectInput(session, "select12", label = "Select a Time Variable for Quantile Analysis",
                      choices = cb_options,
                      selected=cb_options[3])
    updateSelectInput(session, "select13", label = "Select a Censoring Variable for Quantile Analysis",
                      choices = cb_options,
                      selected =cb_options[2] )
    dsnames2 <- colnames(data_input2())
    cb_options2 <- dsnames2
    updateSelectInput(session, "select21", label = "Select a Variable of Interest as Cohort Group",
                      choices = c(cb_options2, "All Patients"),
                      selected = cb_options2[5])
    updateSelectInput(session, "select22", label = "Select a Time Variable to Visualize KM Plot",
                      choices = cb_options2,
                      selected=cb_options2[3])
    updateSelectInput(session, "select23", label = "Select a Censoring Variable to Visualize KM Plot",
                      choices = cb_options2,
                      selected =cb_options2[2] )
    updateSelectInput(session, "select24", label = "Select a Time Variable for Survival Analysis",
                      choices = cb_options2,
                      selected=cb_options2[3])
    updateSelectInput(session, "select25", label = "Select a Censoring Variable for Survival Analysis",
                      choices = cb_options2,
                      selected =cb_options2[2] )
    updateSelectInput(session, "show_vars26", label = "Select multiple Variables to Generate Univariate Survival Association Table",
                      choices = cb_options2,
                      selected = cb_options2[c(5,6)])
    dsnames3 <- colnames(data_input3())
    cb_options3 <- dsnames3
    updateSelectInput(session, "select31", label = "Select a Variable of Interest as Cohort Group",
                      choices = c(cb_options3, "All Patients"),
                      selected = cb_options3[length(cb_options3)])
    updateSelectInput(session, "select32", label = "Select a Time Variable to Visualize CIF Plot",
                      choices = cb_options3,
                      selected=cb_options3[1])
    updateSelectInput(session, "select33", label = "Select a Censoring Variable to Visualize CIF Plot",
                      choices = cb_options3,
                      selected =cb_options3[2] )
    updateSelectInput(session, "select34", label = "Select a Time Variable for Competing Risk Survival Analysis",
                      choices = cb_options3,
                      selected=cb_options3[1])
    updateSelectInput(session, "select35", label = "Select a Censoring Variable for Competing Risk Survival Analysis",
                      choices = cb_options3,
                      selected =cb_options3[2] )
    updateSelectInput(session, "show_vars36", label = "Select multiple Variables to Generate Univariate Survival Association Table",
                      choices = cb_options3,
                      selected = cb_options3[c(3,5)])
    dsnames4 <- colnames(data_input4())
    cb_options4 <- dsnames4
    updateSelectInput(session, "select41", label = "Select a Variable of Interest as Cohort Group",
                      choices = cb_options4,
                      selected = cb_options4[3])
    updateSelectInput(session, "select42", label = "Select a Time Variable to Visualize Landmark Plot",
                      choices = cb_options4,
                      selected=cb_options4[1])
    updateSelectInput(session, "select43", label = "Select a Censoring Variable to Visualize landmark Plot",
                      choices = cb_options4,
                      selected =cb_options4[2] )
    updateSelectInput(session, "select44", label = "Select a Time Dependent Variable to Visualize landmark Plot",
                      choices = cb_options4,
                      selected =cb_options4[6] )
    
    dsnames5 <- colnames(data_input5())
    cb_options5 <- dsnames5
    updateSelectInput(session, "select51", label = "Select a Continuous Variable of Interest",
                      choices = cb_options5,
                      selected = cb_options5[5])
    updateSelectInput(session, "select52", label = "Select a Time Variable",
                      choices = cb_options5,
                      selected=cb_options5[3])
    updateSelectInput(session, "select53", label = "Select a Censoring Variable",
                      choices = cb_options5,
                      selected =cb_options5[2] )
    
  })
  
  res <- eventReactive(input$numb, {
    data <- data_input()
    xvar <- data[, input$select]
    time <- as.numeric(data[,input$select12])
    censor <- data[,input$select13]
    dat <- cbind.data.frame(time, censor, xvar)
    dat <- dat[!is.na(dat$time),]
    
    if (class(xvar) %in% c("integer", "numeric") & input$binary4 == "continuous") {
      if (as.numeric(as.integer(input$cutoff == 1))) {
        perc <- optimalcut(dat)
      } else {perc <- as.numeric(as.integer(input$cutoff))}
      QS1 = dat
      #QS1<-read.table(file = "C:/Users/xzhan60/Downloads/bioinformatics research projects/apps/Quantile Analysis/data/BRCA_for_quantile_survival_analysis.csv", header=T, sep=",", stringsAsFactors = F)
      QS1$Group <- ifelse(QS1[, 'xvar'] < quantile(QS1[, 'xvar'], perc/100), "Low", "High")

      res <- QSE(QS1, perc, input$exp_ref)
      return(res)
    } else if (class(xvar) == 'factor' & input$binary4 =='categorical') {
        dat$Group <- dat$xvar
        ref <- levels(factor(dat$Group))[1]
        res <- QSE(dat, percentile = 1, ref)
        return(res)
        
    } else if (class(xvar) == 'character' & input$binary4 =='categorical') {
      
      dat$Group <- as.factor(dat$xvar)
      ref <- levels(factor(dat$Group))[1]
      res <- QSE(dat, percentile = 1, ref)
      return(res)
      }else {return(NULL)}
    })
  
  output$QAplot <- renderPlot({
    
     
      xlabel <- c("Years", "Months", "Days")
    #b <- c(1, 12, 365)
    
      TopLegend = "F"
      color=c(3,2,1)
      upperQ = 1
      KM_xlab= paste0("Survival Time (", xlabel[as.numeric(input$time4)], ")")
      Q_ylab= "Survival Time Diff:"
      Dlabel = "T"
      
      surv = res()$surv
      exp= res()$exp
      fit = res()$fit
      m_survt = res()$m_survt
      cvt.length = res()$cvt.length
      NR = res()$NR
      p.val = res()$p.val
      pos = res()$pos
      char.cvt = res()$char.cvt
      taus = res()$taus
      exp.ref = res()$exp.ref

      op <- par(mfrow = c(1, cvt.length),mar = c(5,4,2,2), oma=c(4,0,0,0))
      
      plot(surv, lty =c(1,1), ylim=c(-0.05,1), xlim=c(-0.05*max(surv$time), 1.05*max(surv$time)),col=color, lwd=2, xlab= KM_xlab, ylab="Survival Probability", xaxt="n")
      axis(1,at=round(pos,2))
      abline(v=m_survt, col = "lightgray", lty=3)
      
      for (n in 1: dim(NR)[1]){text(x=pos,y= 0,labels= NR[n,],cex=0.8,pos=3, offset=(-0.8*n), col=color[n],font=3)}
      
      text(x=0,y=0.01, label="Number at Risk", pos=4, offset=-0.5, cex=0.8, font=4)
      
      if (TopLegend == "F")legend(0,0.1*cvt.length, char.cvt,lty =c(1,1), col=color,bty = "n" ,cex=1,seg.len=2,lwd =2, title=paste("logrank P = ", p.val, sep = ""))
      
      if (TopLegend == "T") legend("topright", char.cvt,lty =c(1,1), col=color,bty = "n" ,cex=1,seg.len=2,lwd =2, title=paste("logrank P = ", p.val, sep = ""))
      
      
      for (k in 2:cvt.length){
        est<-fit$bt[k,]
        va <- fit$va[k,k,]
        va <- sapply(va,function(x) ifelse(x<0,0,x))
        #This assumes that the length of exposure is the n, i.e. there are no missing outcomes
        low <-fit$bt[k,]-qt(0.975,df=c(length(exp)-1))*sqrt(va)
        up <-fit$bt[k,]+qt(0.975,df=c(length(exp)-1))*sqrt(va)
        
        yrange<-max(up)-min(low)
        
        plot(est~taus,xlab="Quantile",xlim=c(-0.03*upperQ,upperQ), ylim=c(min(low,-1)-0.1*yrange,max(up,0)),lwd=2, col="blue",type="l", 
             ylab=paste("Survival Time Diff: ", char.cvt[k], " vs. ", exp.ref, sep= " "),xaxt = "n")
        axis(1,at=round(taus,3), labels = paste("Q", 1:10, sep = ""), las=2)
        
        polygon(c(taus,rev(taus)), c(low, rev(up)),col="grey", density = 20,ylim=c(min(low),max(up)))
        abline(h=0,col="red", lty= 2)
        
        if (Dlabel == "T") {
          text(x=c(-0.005*upperQ,taus),y=min(low,-1)-0.1*yrange,labels=c(char.cvt[k],round(fit$bt[k,]+ fit$bt[1,],1)),cex=0.8,pos=3, offset=0.1, col=color[k],font=3)
          text(x=c(-0.005*upperQ,taus),y=min(low,-1)-0.1*yrange,labels=c(char.cvt[1],round(fit$bt[1,],1)),cex=0.8,pos =1, offset=0.1, col=color[1],font=3) 
          text(x=0.2*upperQ,y=min(low,-1)-0.1*yrange , label="Survival Time at Quantiles", pos=3, offset=0.8, cex=0.8, font=4)
        }
    
      }
      
      #title(sub = Title,line = 1, outer=T,cex.sub =1.1, font.sub = 2)
      par(op)

	})
  
  
  output$forestplot <- renderPlot({

    
    CI_Data = res()$CI_Data
    forestplot(CI_Data)
 

  })
  
  
  output$coxout <- DT::renderDataTable({
    coxout <- round(res()$coxout, 4)
    
    DT::datatable(coxout)
  })
  
  output$CIout <- DT::renderDataTable({
    CIout <- res()$CI_Data3
    CIout <- cbind.data.frame(CIout[, 1], round(CIout[, 2:4], 4), CIout[, 5])
    r <- c("High", "Low")
    
    colnames(CIout) <- c("Quantiles", "Mean Time Difference/Transformed Hazard Ratio(log(1/HR))", "CI Lower Limit", "CI Upper Limit", 
                         paste0("Significance(0: Non-sig, 1:", input$exp_ref, " Better, 2:", r[r != input$exp_ref], " Better, 3:Non-estimable)") )
    
    
    DT::datatable(CIout, options = list(
      lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')),
      pageLength = 20))
  })

  output$pv11 <- renderUI({
    
    if (is.null(res())) {
      return(NULL)
    } else {
    hs1 <- paste("&emsp;")
    hs2 <- paste("Quantile Survival Analysis Plots")
    HTML(paste(h2(strong(hs2)), hs1, sep = '<br/>'))
    }
  })
  
  output$pv12 <- renderUI({
    if (is.null(res())) {
      return(NULL)
    } else {
      
    hs1 <- paste("&emsp;")
    hs2 <- paste("Forest Plot for Survival Time Difference")
    HTML(paste(h2(strong(hs2)), hs1, sep = '<br/>'))
    }
  })
  
  output$pv13 <- renderUI({
    
    if (is.null(res())) {
      return(NULL)
    } else {
      hs1 <- paste("&emsp;")
      hs2 <- paste("Univariate Cox Survival Analysis Table")
      HTML(paste(h2(strong(hs2)), hs1, sep = '<br/>'))
    }
  })
  
  output$pv14 <- renderUI({
    if (is.null(res())) {
      return(NULL)
    } else {
      
      hs1 <- paste("&emsp;")
      hs2 <- paste("Data for Forest Plot")
      HTML(paste(h2(strong(hs2)), hs1, sep = '<br/>'))
    }
  })
  
  output$pv15 <- renderUI({
    if (is.null(res())) {
      return(NULL)
    } else {
      str0 <- paste("&emsp;")
    str9 <- paste("The three plots from left to right are as following:") 
    str3 <- paste("&emsp;               1.            The first plot is KM plot with number at risk table for overall survival and log rank test p-value.")
    str4 <- paste("&emsp;               2.            The second plot is the survival time difference with 95% CI between two dichotomized groups at 10 quantiles as Q1 to Q10 (defined as 10 percentile to 100 percentile by 10 percentile at mean survival time among all patients).")
    str5 <- paste("&emsp;               3.            The third plot the the summary using forest plot for the survival time difference at 10 quantiles. The overall in forest plot corresponds to the transformed HR and 95% CI for overall survival (log(1/HR)). 
                  The transformed HR will be interpret as if 0 is included then it is not significant.")
    str10 <- paste("NOTE1: The first table contains hazard ratio and 95% CI with cox proportional hazard model for the overall survival with the variable of interest in the original scale.")
    str11 <- paste("NOTE2: The second table 'Data for Forest Plot' contains information to generate the forest plot. For item 'overall', it shows the transformed HR and 95% CI. The last column in this table includes information of direction. 
                   It is also needed to generate a summarized grid if you have multiple cancer types or subgroups.")
    str12 <- paste("NOTE3: To generate a summarized grid if you have multiple cancer types or subgroups, you can download the data for grid in the left side panel. The percentile in the output table will be 1 if your variable of interest is categorical.
                    You only need to keep the first two columns and transpose the data. Repeat this for different cancer types or subgroups and combine as a new datafile to generate a grid in the tab: 
                    Grid for summarized significance'.")
    HTML(paste(str0, strong(str9),str0, str3, str4, str5, str0,str0,strong((str10)), str0,str0,strong((str11)), str0,str0, strong((str12)), str0,str0, sep = '<br/>'))
    }
  })
  
  output$downloadQA <- downloadHandler(
    filename <- function() {
      pdf_file <<- as.character(input$fname11)
      paste('QA_', pdf_file, Sys.time(),'.pdf', sep='')
    },
    content <- function(file) {
      pdf(file=paste(pdf_file,".pdf",sep="") , height= 8, width=12)
      xlabel <- c("Years", "Months", "Days")
      
      TopLegend = "F"
      color=c(3,2,1)
      upperQ = 1
      KM_xlab= paste0("Survival Time (", xlabel[as.numeric(input$time4)], ")")
      Q_ylab= "Survival Time Diff:"
      Dlabel = "T"
      
      surv = res()$surv
      exp= res()$exp
      fit = res()$fit
      m_survt = res()$m_survt
      cvt.length = res()$cvt.length
      NR = res()$NR
      p.val = res()$p.val
      pos = res()$pos
      char.cvt = res()$char.cvt
      taus = res()$taus
      exp.ref = res()$exp.ref
      
      op <- par(mfrow = c(1, cvt.length),mar = c(5,4,2,2), oma=c(4,0,0,0))
      
      plot(surv, lty =c(1,1), ylim=c(-0.05,1), xlim=c(-0.05*max(surv$time), 1.05*max(surv$time)),col=color, lwd=2, xlab= KM_xlab, ylab="Survival Probability", xaxt="n")
      axis(1,at=round(pos,2))
      abline(v=m_survt, col = "lightgray", lty=3)
      
      for (n in 1: dim(NR)[1]){text(x=pos,y= 0,labels= NR[n,],cex=0.8,pos=3, offset=(-0.8*n), col=color[n],font=3)}
      
      text(x=0,y=0.01, label="Number at Risk", pos=4, offset=-0.5, cex=0.8, font=4)
      
      if (TopLegend == "F")legend(0,0.1*cvt.length, char.cvt,lty =c(1,1), col=color,bty = "n" ,cex=1,seg.len=2,lwd =2, title=paste("logrank P = ", p.val, sep = ""))
      
      if (TopLegend == "T") legend("topright", char.cvt,lty =c(1,1), col=color,bty = "n" ,cex=1,seg.len=2,lwd =2, title=paste("logrank P = ", p.val, sep = ""))
      
      
      for (k in 2:cvt.length){
        est<-fit$bt[k,]
        va <- fit$va[k,k,]
        va <- sapply(va,function(x) ifelse(x<0,0,x))
        #This assumes that the length of exposure is the n, i.e. there are no missing outcomes
        low <-fit$bt[k,]-qt(0.975,df=c(length(exp)-1))*sqrt(va)
        up <-fit$bt[k,]+qt(0.975,df=c(length(exp)-1))*sqrt(va)
        
        yrange<-max(up)-min(low)
        
        plot(est~taus,xlab="Quantile",xlim=c(-0.03*upperQ,upperQ), ylim=c(min(low,-1)-0.1*yrange,max(up,0)),lwd=2, col="blue",type="l", 
             ylab=paste("Survival Time Diff: ", char.cvt[k], " vs. ", exp.ref, sep= " "),xaxt = "n")
        axis(1,at=round(taus,3), labels = paste("Q", 1:10, sep = ""), las=2)
        
        polygon(c(taus,rev(taus)), c(low, rev(up)),col="grey", density = 20,ylim=c(min(low),max(up)))
        abline(h=0,col="red", lty= 2)
        
        if (Dlabel == "T") {
          text(x=c(-0.005*upperQ,taus),y=min(low,-1)-0.1*yrange,labels=c(char.cvt[k],round(fit$bt[k,]+ fit$bt[1,],1)),cex=0.8,pos=3, offset=0.1, col=color[k],font=3)
          text(x=c(-0.005*upperQ,taus),y=min(low,-1)-0.1*yrange,labels=c(char.cvt[1],round(fit$bt[1,],1)),cex=0.8,pos =1, offset=0.1, col=color[1],font=3) 
          text(x=0.2*upperQ,y=min(low,-1)-0.1*yrange , label="Survival Time at Quantiles", pos=3, offset=0.8, cex=0.8, font=4)
        }
        
      }
      
      #title(sub = Title,line = 1, outer=T,cex.sub =1.1, font.sub = 2)
      par(op)
      
      dev.off()
      file.copy(paste(pdf_file,'.pdf', sep='') ,file, overwrite=TRUE)
    })
  
  output$downloadFP <- downloadHandler(
    filename <- function() {
      pdf_file <<- as.character(input$fname12)
      paste('FP_', pdf_file, Sys.time(),'.pdf', sep='')
    },
    content <- function(file) {
      pdf(file=paste(pdf_file,".pdf",sep="") , height= 12, width=8)
      #plot_fp()
      CI_Data = res()$CI_Data
      forestplot(CI_Data)
      
      dev.off()
      file.copy(paste(pdf_file,'.pdf', sep='') ,file, overwrite=TRUE)
    })
  
  output$downloadTB <- downloadHandler(
    filename <- function() {
      csv_file <<- as.character(input$fname13)
      paste('TB_', csv_file, Sys.time(),'.csv', sep='')
    },
    content <- function(file) {
      CIout <- res()$CI_Data3[, c(1, 5, 2, 3, 4)]
      CIout[,1] <- as.character(c(paste("Q", 1:10, sep = ""), "Overall"))
      CIout[12,1] <- "Percentile"

      CIout[12,2] <- res()$percentile
      
      r <- c("High", "Low")
      
      colnames(CIout) <- c("Quantiles", paste0("Significance(0: Non-sig, 1:", input$exp_ref, " Better, 2:", r[r != input$exp_ref], " Better, 3:Non-estimable)"), 
                           "Mean Time Difference/Transformed Hazard Ratio(log(1/HR))", "CI Lower Limit", "CI Upper Limit")
      write.csv(CIout, file, row.names = FALSE)
      
    })
  
  data_input2 <- reactive({
    if(input$file2 == 'Example2'){
      d2 <- read.csv("data/BRCA_for_quantile_survival_analysis.csv", header =T, sep =",", stringsAsFactors = F)
    }
    else if(input$file2 == 'load_my_own2'){
      inFile <- input$file22
      if (is.null(inFile))
        return(NULL)
      else if(grepl(".xlsx", inFile[1])) { d2 = read.xlsx(as.character(inFile$datapath), colNames = TRUE, rowNames = F, as.is = T) }
      else if(grepl(".csv", inFile[1])) { d2 = read.csv(as.character(inFile$datapath), header = TRUE, sep = ",", stringsAsFactors = F, as.is = T, fill = T) }
      else if(grepl(".txt", inFile[1])) { d2 = read.table(as.character(inFile$datapath), header = TRUE, sep = "\t", stringsAsFactors = F, as.is = T, fill = T) }
    }
    else 
      return(NULL)
    # dim(data)
    Dataset2 <- data.frame(d2)
    return(as.data.frame(Dataset2))
  })
  
  output$downloadEx2 <- downloadHandler(
    
    filename <- function() {
      paste('Example ds data', Sys.time(),'.csv', sep='')
    },
    content <- function(file) {
      ds2 <- data_input2()
      write.csv(ds2, file, row.names = FALSE)
    }
  )
  
  output$kmplot <- renderPlot({
    data2 <- data_input2()
    if(!is.null(data2)){
    for (j in 1:ncol(data2)) {
      if (class(data2[, j]) %in% c("character")) 
        data2[,j] <- as.factor(data2[,j])
      else data2[,j] = data2[, j]
    }
    if (input$select21 == "All Patients") {
      
      time <- data2[,input$select22]
      censor <- data2[,input$select23]
      dat <- cbind.data.frame(time, censor)
      
      fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ 1, data = dat)
      # Drawing curves
      xlabel <- c("Years", "Months", "Days")
      b <- c(1, 12, 365)
      #TF <- c(TRUE, FALSE)
      if (input$riskt == TRUE) {
        res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                          font.x =  14,
                          font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                          pval.coord = c(max(time)*0.9, 0.9), pval.method.coord = c(max(time)*0.9, 0.95),
                          font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select21,
                          color = "#2E9FDF", risk.table = TRUE, risk.table.y.text.col = TRUE)#, fun = function(y) y*100)
        return(res) 
        
      } else {
        res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                          font.x =  14,
                          font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                          pval.coord = c(max(time)*0.9, 0.9), pval.method.coord = c(max(time)*0.9, 0.95),
                          font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select21,
                          color = "#2E9FDF", risk.table = FALSE, risk.table.y.text.col = FALSE)#, fun = function(y) y*100)
        return(res) 
        
      }
    } else {
    xvar <- data2[, input$select21]
    time <- data2[,input$select22]
    censor <- data2[,input$select23]
    dat <- cbind.data.frame(time, censor, xvar)
   
    if (is.null(xvar) | is.null(time) | is.null(censor)) {
      return(NULL)
    } else if (nlevels(xvar) > 30) {
      return(NULL)
    } else if (class(xvar) == "factor") {
      fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ xvar, data = dat)
      # Drawing curves
      xlabel <- c("Years", "Months", "Days")
      b <- c(1, 12, 365)
      #TF <- c(TRUE, FALSE)
      if (input$riskt == TRUE) {
        res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                          font.x =  14,
                          font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                          pval.coord = c(max(time)*0.9, 0.9), pval.method.coord = c(max(time)*0.9, 0.95),
                          font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select21, legend.labs = levels(xvar),
                          color = "#2E9FDF", risk.table = TRUE, risk.table.y.text.col = TRUE)#, fun = function(y) y*100)
        return(res) 
        
      } else {
        res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                          font.x =  14,
                          font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                          pval.coord = c(max(time)*0.9, 0.9), pval.method.coord = c(max(time)*0.9, 0.95),
                          font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select21, legend.labs = levels(xvar),
                          color = "#2E9FDF", risk.table = FALSE, risk.table.y.text.col = FALSE)#, fun = function(y) y*100)
        return(res) 
        
      }
    }else if (class(xvar) %in% c("integer", "numeric") & input$binary == "continuous") {
      if (as.numeric(as.integer(input$cutoff2)) == 1) {
        perc <- optimalcut(dat)
      } else {perc <- as.numeric(as.integer(input$cutoff2))}
      
      dat$Group <- ifelse(dat[, 'xvar'] < quantile(dat[, 'xvar'], perc/100, na.rm= TRUE), "Low", "High")
      Group <- as.factor(dat$Group)
      
      fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ Group, data = dat)
      # Drawing curves
      xlabel <- c("Years", "Months", "Days")
      b <- c(1, 12, 365)
      #TF <- c(TRUE, FALSE)
      if (input$riskt == TRUE) {
        res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                          font.x =  14,
                          font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                          pval.coord = c(max(time)*0.9, 0.9), pval.method.coord = c(max(time)*0.9, 0.95),
                          font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select21, legend.labs = levels(Group),
                          color = "#2E9FDF", risk.table = TRUE, risk.table.y.text.col = TRUE)#, fun = function(y) y*100, type="cairo")
        return(res) 
        
      } else {
        res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                          font.x =  14,
                          font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                          pval.coord = c(max(time)*0.9, 0.9), pval.method.coord = c(max(time)*0.9, 0.95),
                          font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select21, legend.labs = levels(Group),
                          color = "#2E9FDF", risk.table = FALSE, risk.table.y.text.col = FALSE)#, fun = function(y) y*100, type="cairo")
        return(res) 
        
      }
    } else {
      return(NULL)
    }
    }
    }
    
  })
  
  output$out2 <- renderDataTable({
    data2 <- data_input2()
    for (j in 1:ncol(data2)) {
      if (class(data2[, j]) %in% c("character")) 
        data2[,j] <- as.factor(data2[,j])
      else data2[,j] = data2[, j]
    } 
    xvar <- data2[,input$show_vars26, drop = FALSE]
    time <- data2[,input$select24]
    censor <- data2[,input$select25]
    
    options(warn=-1)
    res <- list()
    for (i in 1: ncol(xvar)){
      x <- xvar[, i]
      if (is.null(x) | is.null(time) | is.null(censor)) {
        return(NULL)
      } else if (nlevels(x) > 30) {
        return(NULL)
      } else if (class(x) == "factor" | class(x) %in% c("integer", "numeric")){
        
        if (class(x) %in% c("integer", "numeric")){
          dat <- cbind.data.frame(time, censor, xvar=x)
          if (as.numeric(as.integer(input$cutoff2)) == 1) {
            perc <- optimalcut(dat)
          } else {perc <- as.numeric(as.integer(input$cutoff2))}
          
          dat$Group <- ifelse(dat[, 'xvar'] < quantile(dat[, 'xvar'], perc/100, na.rm= TRUE), "Low", "High")
          Group <- as.factor(dat$Group)
          x <- Group
        }
        
        Variable <- c(colnames(xvar)[i], rep("", (length(levels(x))-1)))
        #x <- C(x, contr.treatment, base=3)
        fit <- coxph(Surv(as.numeric(time), as.numeric(factor(censor))) ~ x)
        temp <- cox.zph(fit)
        assum.p.value <- ifelse(temp$table[3] < 0.001, "<0.001", paste0(round(temp$table[3], 4)))
        assump <- c(assum.p.value, rep("", (length(levels(x))-1)))
        sum <- summary(fit)
        hazard.ratio = round(sum$conf.int[, 1], 2)
        lower95 = round(sum$conf.int[, 3], 2)
        upper95 = round(sum$conf.int[, 4], 2)
        logrank.p.value = ifelse(sum$sctest[3] < 0.001, "<0.001", paste0(round(sum$sctest[3], 4)))
        logrankp <- c(logrank.p.value, rep("", (length(levels(x))-1)))
        type3.p.value = ifelse(sum$coefficients[, 5] < 0.001, "<0.001", paste0(round(sum$coefficients[, 5], 4)))
        counts <- data.frame(table(x))
        counts <- rbind.data.frame(counts[2:nrow(counts),], counts[1, ])
        coxres <- cbind.data.frame(Variable, counts, c(paste0(hazard.ratio, " (", lower95,
                                                              "-", upper95, ")"), ""), c(type3.p.value, ""), logrankp)
        colnames(coxres) <- c("Variable", "Level", "N", "Hazard Ratio (95% CI)", "Type 3 P-value", "Log-rank P-value")
        if (input$assum == 0) {
          res[[i]] <- coxres
        } else if (input$assum == 1) {
          coxres2 <- cbind.data.frame(coxres, assump)
          colnames(coxres2)[7] <- "P-value for Proportional Hazards Assumption"
          res[[i]] <- coxres2
        }
      } else 
        return(NULL)
    }
    
    res_table <- do.call("rbind", res)
  }, rownames = FALSE) #, options = list(dom = 'tip'))
  
  
  output$pv21 <- renderUI({
    hs3 <- paste("&emsp;")
    hs4 <- paste("To Visualize the Kaplan Meier Plot:")
    HTML(paste(h2(strong(hs4)), hs3, sep = '<br/>'))
  })
  
  
  output$pv22 <- renderUI({
    hs3 <- paste("&emsp;")
    hs4 <- paste("Univariate Survival Association Analysis for Multiple Selected Variables")
    HTML(paste(hs3, h2(strong(hs4)), hs3, sep = '<br/>'))
  })
  
  #output$out2 = renderUI ({
  #  tagList(
  #    htmlOutput("pv22"),
  #    dataTableOutput("out2")
  #  )
  #})
  
  output$downloadKM <- downloadHandler(
    filename <- function() {
      pdf_file <<- as.character(input$fname21)
      paste('KM_', pdf_file, Sys.time(),'.pdf', sep='')
    },
    content <- function(file) {
      pdf(file=paste(pdf_file,".pdf",sep="") , height= 8, width=12)
      #plot_fp()
      data2 <- data_input2()
      for (j in 1:ncol(data2)) {
        if (class(data2[, j]) %in% c("character"))  
          data2[,j] <- as.factor(data2[,j])
        else data2[,j] = data2[, j]
      }
      if (input$select21 == "All Patients") {
        time <- data2[,input$select22]
        censor <- data2[,input$select23]
        dat <- cbind.data.frame(time, censor)
        fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ 1, data = dat)
        # Drawing curves
        xlabel <- c("Years", "Months", "Days")
        b <- c(1, 12, 365)
        #TF <- c(TRUE, FALSE)
        if (input$riskt == TRUE) {
          res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                            font.x =  14,
                            font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                            pval.coord = c(max(time)*0.9, 0.9), pval.method.coord = c(max(time)*0.9, 0.95),
                            font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select21,
                            color = "#2E9FDF", risk.table = TRUE, risk.table.y.text.col = TRUE)#, fun = function(y) y*100)
          p <- grid.arrange(res$plot, res$table, nrow = 2, heights=c(0.7,0.3))
          plot(p)
          
        } else {
          res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                            font.x =  14,
                            font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                            pval.coord = c(max(time)*0.9, 0.9), pval.method.coord = c(max(time)*0.9, 0.95),
                            font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select21,
                            color = "#2E9FDF", risk.table = FALSE, risk.table.y.text.col = FALSE)#, fun = function(y) y*100)
          p <- grid.arrange(res$plot, res$table, nrow = 2, heights=c(0.7,0.3))
          plot(p)
          
        }
      } else {
        xvar <- data2[, input$select21]
        time <- data2[,input$select22]
        censor <- data2[,input$select23]
        dat <- cbind.data.frame(time, censor, xvar)
        if (is.null(xvar) | is.null(time) | is.null(censor)) {
          return(NULL)
        } else if (nlevels(xvar) > 30) {
          return(NULL)
        } else if (class(xvar) == "factor") {
          fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ xvar, data = dat)
          # Drawing curves
          xlabel <- c("Years", "Months", "Days")
          b <- c(1, 12, 365)
          #TF <- c(TRUE, FALSE)
          if (input$riskt == TRUE) {
            res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                              font.x =  14,
                              font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                              pval.coord = c(max(time)*0.9, 0.9), pval.method.coord = c(max(time)*0.9, 0.95),
                              font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select21, legend.labs = levels(xvar),
                              color = "#2E9FDF", risk.table = TRUE, risk.table.y.text.col = TRUE)#, fun = function(y) y*100)
            p <- grid.arrange(res$plot, res$table, nrow = 2, heights=c(0.7,0.3))
            plot(p)
            
          } else {
            res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                              font.x =  14,
                              font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                              pval.coord = c(max(time)*0.9, 0.9), pval.method.coord = c(max(time)*0.9, 0.95),
                              font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select21, legend.labs = levels(xvar),
                              color = "#2E9FDF", risk.table = FALSE, risk.table.y.text.col = FALSE)#, fun = function(y) y*100)
            p <- grid.arrange(res$plot, res$table, nrow = 2, heights=c(0.7,0.3))
            plot(p)
            
          }
        }else if (class(xvar) %in% c("integer", "numeric") & input$binary == "continuous") {
          if (as.numeric(as.integer(input$cutoff2)) == 1) {
            perc <- optimalcut(dat)
          } else {perc <- as.numeric(as.integer(input$cutoff2))}
          
          dat$Group <- ifelse(dat[, 'xvar'] < quantile(dat[, 'xvar'], perc/100), "Low", "High")
          Group <- as.factor(dat$Group)
          
          fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ Group, data = dat)
          # Drawing curves
          xlabel <- c("Years", "Months", "Days")
          b <- c(1, 12, 365)
          #TF <- c(TRUE, FALSE)
          if (input$riskt == TRUE) {
            res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                              font.x =  14,
                              font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                              pval.coord = c(max(time)*0.9, 0.9), pval.method.coord = c(max(time)*0.9, 0.95),
                              font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select21, legend.labs = levels(Group),
                              color = "#2E9FDF", risk.table = TRUE, risk.table.y.text.col = TRUE)#, fun = function(y) y*100)
            p <- grid.arrange(res$plot, res$table, nrow = 2, heights=c(0.7,0.3))
            plot(p)
            
          } else {
            res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                              font.x =  14,
                              font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                              pval.coord = c(max(time)*0.9, 0.9), pval.method.coord = c(max(time)*0.9, 0.95),
                              font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select21, legend.labs = levels(Group),
                              color = "#2E9FDF", risk.table = FALSE, risk.table.y.text.col = FALSE)#, fun = function(y) y*100)
            p <- grid.arrange(res$plot, res$table, nrow = 2, heights=c(0.7,0.3))
            plot(p)
            
          }
        } else {
          return(NULL)
        }
      }
      dev.off()
      file.copy(paste(pdf_file,'.pdf', sep='') ,file, overwrite=TRUE)
    })
  
  output$x1 = downloadHandler(
    filename <- function() {
      csv_file <<- as.character(input$fname22)
      paste('ST_', csv_file, Sys.time(),'.csv', sep='')
    },
    content = function(file) {
      data2 <- data_input2()
      for (j in 1:ncol(data2)) {
        if (class(data2[, j]) %in% c("character")) 
          data2[,j] <- as.factor(data2[,j])
        else data2[,j] = data2[, j]
      } 
      xvar <- data2[,input$show_vars26, drop = FALSE]
      time <- data2[,input$select24]
      censor <- data2[,input$select25]
      
      options(warn=-1)
      res <- list()
      for (i in 1: ncol(xvar)){
        x <- xvar[, i]
        if (is.null(x) | is.null(time) | is.null(censor)) {
          return(NULL)
        } else if (nlevels(x) > 30) {
          return(NULL)
        } else if (class(x) == "factor" | class(x) %in% c("integer", "numeric")){
          
          if (class(x) %in% c("integer", "numeric")){
            dat <- cbind.data.frame(time, censor, xvar=x)
            if (as.numeric(as.integer(input$cutoff2)) == 1) {
              perc <- optimalcut(dat)
            } else {perc <- as.numeric(as.integer(input$cutoff2))}
            
            dat$Group <- ifelse(dat[, 'xvar'] < quantile(dat[, 'xvar'], perc/100, na.rm= TRUE), "Low", "High")
            Group <- as.factor(dat$Group)
            x <- Group
          }
          Variable <- c(colnames(xvar)[i], rep("", (length(levels(x))-1)))
          #x <- C(x, contr.treatment, base=3)
          fit <- coxph(Surv(as.numeric(time), as.numeric(factor(censor))) ~ x)
          temp <- cox.zph(fit)
          assum.p.value <- ifelse(temp$table[3] < 0.001, "<0.001", paste0(round(temp$table[3], 4)))
          assump <- c(assum.p.value, rep("", (length(levels(x))-1)))
          sum <- summary(fit)
          hazard.ratio = round(sum$conf.int[, 1], 2)
          lower95 = round(sum$conf.int[, 3], 2)
          upper95 = round(sum$conf.int[, 4], 2)
          logrank.p.value = ifelse(sum$sctest[3] < 0.001, "<0.001", paste0(round(sum$sctest[3], 4)))
          logrankp <- c(logrank.p.value, rep("", (length(levels(x))-1)))
          type3.p.value = ifelse(sum$coefficients[, 5] < 0.001, "<0.001", paste0(round(sum$coefficients[, 5], 4)))
          counts <- data.frame(table(x))
          counts <- rbind.data.frame(counts[2:nrow(counts),], counts[1, ])
          coxres <- cbind.data.frame(Variable, counts, c(paste0(hazard.ratio, " (", lower95,
                                                                "-", upper95, ")"), ""), c(type3.p.value, ""), logrankp)
          colnames(coxres) <- c("Variable", "Level", "N", "Hazard Ratio (95% CI)", "Type 3 P-value", "Log-rank P-value")
          if (input$assum == 0) {
            res[[i]] <- coxres
          } else if (input$assum == 1) {
            coxres2 <- cbind.data.frame(coxres, assump)
            colnames(coxres2)[7] <- "P-value for Proportional Hazards Assumption"
            res[[i]] <- coxres2
          }
        } else 
          return(NULL)
      }
      
      res_table <- do.call("rbind", res)
      
      write.csv(res_table, file, row.names = F)
      
    })

  data_input3 <- reactive({
    if(input$file3 == 'Example3'){
      d3 <- read.csv("data/bmtcrr.csv", header =T, sep =",")
    }
    else if(input$file3 == 'load_my_own3'){
      inFile <- input$file32
      if (is.null(inFile))
        return(NULL)
      else if(grepl(".xlsx", inFile[1])) { d3 = read.xlsx(as.character(inFile$datapath), colNames = TRUE, rowNames = F, as.is = T) }
      else if(grepl(".csv", inFile[1])) { d3 = read.csv(as.character(inFile$datapath), header = TRUE, sep = ",", stringsAsFactors = T, as.is = T, fill = T) }
      else if(grepl(".txt", inFile[1])) { d3 = read.table(as.character(inFile$datapath), header = TRUE, sep = "\t", stringsAsFactors = T, as.is = T, fill = T) }
    }
    else 
      return(NULL)
    # dim(data)
    Dataset3 <- data.frame(d3)
    return(as.data.frame(Dataset3))
  })
  
  output$downloadEx3 <- downloadHandler(
    
    filename <- function() {
      paste('Example ds data', Sys.time(),'.csv', sep='')
    },
    content <- function(file) {
      ds3 <- data_input3()
      write.csv(ds3, file, row.names = FALSE)
    }
  )

  output$cifplot <- renderPlot({
    data3 <- data_input3()
    if (input$select31 == "All Patients") {
      time <- data3[,input$select32]
      censor <- data3[,input$select33]
      dat <- cbind.data.frame(time, censor)
      if (is.null(time) | is.null(censor)) {
        return(NULL)
      } else if (class(time)=="numeric"){
        if (input$points == 0) {
          xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
          fit <- CumIncidence (time, censor, cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
        } else {
          #xlabel <- c("Years", "Months", "Days")[input$time2]
          xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
          fit <- CumIncidence (time, censor, t = as.numeric(as.vector(unlist(strsplit(input$text, ", ")))), cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
        }
        ## Drawing curves
      }else {
        return(NULL)
      }
          } else {
      xvar <- data3[, input$select31]
      time <- data3[,input$select32]
      censor <- data3[,input$select33]
      dat <- cbind.data.frame(time, censor, xvar)
    if (is.null(xvar) | is.null(time) | is.null(censor)) {
      return(NULL)
    } else if (nlevels(xvar) > 30) {
      return(NULL)
    } else if (class(xvar) == "factor" & (class(time)=="numeric"|class(time)=="integer")){
      if (input$points == 0) {
        xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
        fit <- CumIncidence (time, censor, xvar, cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
      } else {
        #xlabel <- c("Years", "Months", "Days")[input$time2]
        xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
        fit <- CumIncidence (time, censor, xvar, t = as.numeric(as.vector(unlist(strsplit(input$text, ", ")))), cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
      }
      ## Drawing curves
    }else if (class(xvar) == "character" & (class(time)=="numeric"|class(time)=="integer")){
      xvar <- as.factor(xvar)
      if (input$points == 0) {
        xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
        fit <- CumIncidence (time, censor, xvar, cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
      } else {
        #xlabel <- c("Years", "Months", "Days")[input$time2]
        xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
        fit <- CumIncidence (time, censor, xvar, t = as.numeric(as.vector(unlist(strsplit(input$text, ", ")))), cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
      }
    } else if (class(xvar) %in% c("integer", "numeric") & input$binary2 == "continuous") {
      perc <- as.numeric(as.integer(input$cutoff3))
      
      dat$Group <- ifelse(dat[, 'xvar'] < quantile(dat[, 'xvar'], perc/100), "Low", "High")
      Group <- as.factor(dat$Group)
      
      if (input$points == 0) {
        xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
        fit <- CumIncidence (as.numeric(time), censor, Group, cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
      } else {
        #xlabel <- c("Years", "Months", "Days")[input$time2]
        xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
        fit <- CumIncidence (as.numeric(time), censor, Group, t = as.numeric(as.vector(unlist(strsplit(input$text, ", ")))), cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
      }

      } 
  }
    
  })
  
  output$ciftable <- renderDataTable({
    data3 <- data_input3()
    if (input$select31 == "All Patients") {
      time <- data3[,input$select32]
      censor <- data3[,input$select33]
      dat <- cbind.data.frame(time, censor)
      if (is.null(time) | is.null(censor)) {
        return(NULL)
      } else if (class(time)=="numeric" |class(time)=="integer"){
        if (input$points == 0) {
          xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
          fit <- CumIncidence (time, censor, cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
        } else {
          #xlabel <- c("Years", "Months", "Days")[input$time2]
          xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
          fit <- CumIncidence (time, censor, t = as.numeric(as.vector(unlist(strsplit(input$text, ", ")))), cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
        }
        ## Drawing curves
      }else {
        return(NULL)
      }
    } else {
      xvar <- data3[, input$select31]
      time <- data3[,input$select32]
      censor <- data3[,input$select33]
      dat <- cbind.data.frame(time, censor, xvar)
      if (is.null(xvar) | is.null(time) | is.null(censor)) {
        return(NULL)
      } else if (nlevels(xvar) > 30) {
        return(NULL)
      } else if (class(xvar) == "factor" & (class(time)=="numeric"|class(time)=="integer")){
        if (input$points == 0) {
          xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
          fit <- CumIncidence (time, censor, xvar, cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
        } else {
          #xlabel <- c("Years", "Months", "Days")[input$time2]
          xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
          fit <- CumIncidence (time, censor, xvar, t = as.numeric(as.vector(unlist(strsplit(input$text, ", ")))), cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
        }
        ## Drawing curves
      } else if(class(xvar) == "character" & (class(time)=="numeric"|class(time)=="integer")){
        xvar <- as.factor(xvar)
        if (input$points == 0) {
          xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
          fit <- CumIncidence (time, censor, xvar, cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
        } else {
          #xlabel <- c("Years", "Months", "Days")[input$time2]
          xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
          fit <- CumIncidence (time, censor, xvar, t = as.numeric(as.vector(unlist(strsplit(input$text, ", ")))), cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
        }
        
      } else if (class(xvar) %in% c("integer", "numeric") & input$binary2 == "continuous") {
        perc <- as.numeric(as.integer(input$cutoff3))
        
        dat$Group <- ifelse(dat[, 'xvar'] < quantile(dat[, 'xvar'], perc/100), "Low", "High")
        Group <- as.factor(dat$Group)
        
        if (input$points == 0) {
          xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
          fit <- CumIncidence (as.numeric(time), censor, Group, cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
        } else {
          #xlabel <- c("Years", "Months", "Days")[input$time2]
          xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
          fit <- CumIncidence (as.numeric(time), censor, Group, t = as.numeric(as.vector(unlist(strsplit(input$text, ", ")))), cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
        }
        
      } else {
        return(NULL)
      }
    }
    z <- qnorm(1-(1-0.95)/2)
    lower <- fit$est ^ exp(-z*fit$se/(fit$est*log(fit$est)))
    lower <- melt(round(lower, 4), id.vars=rownames(lower))
    lower <- lower[order(lower[,1]),]
    upper <- fit$est ^ exp(z*fit$se/(fit$est*log(fit$est)))
    upper <- melt(round(upper, 4), id.vars=rownames(upper))
    upper <- upper[order(upper[,1]),]
    est <- melt(round(fit$est, 4), id.vars=rownames(fit$est))
    est <- est[order(est[,1]),]
    
    ci <- data.frame(paste0(est[, 3], " (", lower[, 3], ", ", upper[, 3], ")"))
    Variable <- as.character(est[, 1])
    table <- cbind.data.frame(Variable, data.frame(est[, 2]), ci, stringsAsFactors=FALSE)
    colnames(table) <- c(input$select31, paste0("Time (", xlabel, ")"), "CIF Estimate (95% CI)")
    vec <- as.vector(table[, 2])
    seq <- 1:nrow(table)
    seq <- seq[-which(vec == unique(vec)[1])]
    table[seq, 1] <- ""
    table
     
  }, rownames = FALSE, options = list(pageLength = 100))
  
  output$out3 <- renderDataTable({
    data3 <- data_input3()
    xvar <- data3[,input$show_vars36, drop = FALSE]
    time <- data3[,input$select34]
    censor <- data3[,input$select35]
    
    res <- list()
    for (i in 1: ncol(xvar)){
      x <- xvar[, i]
      if (is.null(x) | is.null(time) | is.null(censor)) {
        return(NULL)
      } else if (nlevels(x) > 30) {
        return(NULL)
      } else if (class(x) == "factor" & (class(time)=="numeric"|class(time)=="integer")){
        Variable <- c(colnames(xvar)[i], rep("", (length(levels(x))-1)))
        #x <- C(x, contr.treatment, base=3)
        fit <- crr(time, censor, factor2ind(x), failcode = input$event2, cencode = input$censor2)
        sum <- summary(fit)
        hazard.ratio = round(sum$conf.int[, 1], 2)
        lower95 = round(sum$conf.int[, 3], 2)
        upper95 = round(sum$conf.int[, 4], 2)
        gray.p.value = ifelse(cuminc(time, censor, factor2ind(x))$Tests[as.integer(input$event2), "pv"] < 0.001, "<0.001", paste0(round(cuminc(time, censor, factor2ind(x))$Tests[as.integer(input$event2), "pv"], 4)))
        grayp <- c(gray.p.value, rep("", (length(levels(x))-1)))
        type3.p.value = ifelse(sum$coef[, 5] < 0.001, "<0.001", paste0(round(sum$coef[, 5], 4)))
        counts <- data.frame(table(x))
        counts <- rbind.data.frame(counts[2:nrow(counts),], counts[1, ])
        coxres <- cbind.data.frame(Variable, counts, c(paste0(hazard.ratio, " (", lower95,
                                                              "-", upper95, ")"), ""), c(type3.p.value, ""), grayp)
        colnames(coxres) <- c("Variable", "Level", "N", "Hazard Ratio (95% CI)", "Type 3 P-value", "Gray's P-value")
        res[[i]] <- coxres
      }else if(class(x) == "character" & (class(time)=="numeric"|class(time)=="integer")){ 
        x <- as.factor(x)
        Variable <- c(colnames(xvar)[i], rep("", (length(levels(x))-1)))
        #x <- C(x, contr.treatment, base=3)
        fit <- crr(time, censor, factor2ind(x), failcode = input$event2, cencode = input$censor2)
        sum <- summary(fit)
        hazard.ratio = round(sum$conf.int[, 1], 2)
        lower95 = round(sum$conf.int[, 3], 2)
        upper95 = round(sum$conf.int[, 4], 2)
        gray.p.value = ifelse(cuminc(time, censor, factor2ind(x))$Tests[as.integer(input$event2), "pv"] < 0.001, "<0.001", paste0(round(cuminc(time, censor, factor2ind(x))$Tests[as.integer(input$event2), "pv"], 4)))
        grayp <- c(gray.p.value, rep("", (length(levels(x))-1)))
        type3.p.value = ifelse(sum$coef[, 5] < 0.001, "<0.001", paste0(round(sum$coef[, 5], 4)))
        counts <- data.frame(table(x))
        counts <- rbind.data.frame(counts[2:nrow(counts),], counts[1, ])
        coxres <- cbind.data.frame(Variable, counts, c(paste0(hazard.ratio, " (", lower95,
                                                              "-", upper95, ")"), ""), c(type3.p.value, ""), grayp)
        colnames(coxres) <- c("Variable", "Level", "N", "Hazard Ratio (95% CI)", "Type 3 P-value", "Gray's P-value")
        res[[i]] <- coxres
      
      }else if (class(x) %in% c("integer", "numeric") & (class(time)=="numeric"|class(time)=="integer")){
        Variable <- colnames(xvar)[i]
        fit <- crr(time, censor, x, failcode = input$event2, cencode = input$censor2)
        sum <- summary(fit)
        hazard.ratio = round(sum$conf.int[1], 2)
        lower95 = round(sum$conf.int[3], 2)
        upper95 = round(sum$conf.int[4], 2)
        #logrank.p.value = sum$sctest[3]
        type3.p.value = ifelse(sum$coef[5] < 0.001, "<0.001", paste0(round(sum$coef[5], 4)))
        coxres <- cbind.data.frame(Variable, NA, length(!is.na(x)), paste0(hazard.ratio, " (", lower95,
                                                                           "-", upper95, ")"), type3.p.value, "-")
        colnames(coxres) <- c("Variable", "Level", "N", "Hazard Ratio (95% CI)", "Type 3 P-value", "Gray's P-value")
        res[[i]] <- coxres
      } else 
        return(NULL)
    }
    
    res_table <- do.call("rbind", res)
  }, rownames = FALSE) #, options = list(dom = 'tip'))
  
  output$pv31 <- renderUI({
    hs3 <- paste("&emsp;")
    hs4 <- paste("To Visualize the Cumulative Incidence Function Plot:")
    HTML(paste(h2(strong(hs4)), hs3, sep = '<br/>'))
  })
  
  
  output$pv32 <- renderUI({
    hs3 <- paste("&emsp;")
    hs4 <- paste("Univariate Competing Risk Survival Association Analysis for Multiple Selected Variables")
    HTML(paste(hs3, h2(strong(hs4)), hs3, sep = '<br/>'))
  })
  
  output$downloadcif <- downloadHandler(
    filename <- function() {
      pdf_file <<- as.character(input$fname31)
      paste('CIF_', pdf_file, Sys.time(),'.pdf', sep='')
    },
    content <- function(file) {
      pdf(file=paste(pdf_file,".pdf",sep="") , height= 8, width=12)
      #plot_fp()
      data3 <- data_input3()
      if (input$select31 == "All Patients") {
        time <- data3[,input$select32]
        censor <- data3[,input$select33]
        dat <- cbind.data.frame(time, censor)
        if (is.null(time) | is.null(censor)) {
          return(NULL)
        } else if (class(time)=="numeric"){
          if (input$points == 0) {
            xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
            fit <- CumIncidence (time, censor, cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
          } else {
            #xlabel <- c("Years", "Months", "Days")[input$time2]
            xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
            fit <- CumIncidence (time, censor, t = as.numeric(as.vector(unlist(strsplit(input$text, ", ")))), cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
          }
          ## Drawing curves
        }else {
          return(NULL)
        }
      } else {
        xvar <- data3[, input$select31]
        time <- data3[,input$select32]
        censor <- data3[,input$select33]
        dat <- cbind.data.frame(time, censor, xvar)
        if (is.null(xvar) | is.null(time) | is.null(censor)) {
          return(NULL)
        } else if (nlevels(xvar) > 30) {
          return(NULL)
        } else if (class(xvar) == "factor" & (class(time)=="numeric"|class(time)=="integer")){
          if (input$points == 0) {
            xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
            fit <- CumIncidence (time, censor, xvar, cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
          } else {
            #xlabel <- c("Years", "Months", "Days")[input$time2]
            xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
            fit <- CumIncidence (time, censor, xvar, t = as.numeric(as.vector(unlist(strsplit(input$text, ", ")))), cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
          }
          ## Drawing curves
        }else if (class(xvar) == "character" & (class(time)=="numeric"|class(time)=="integer")){
          xvar <- as.factor(xvar)
          if (input$points == 0) {
            xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
            fit <- CumIncidence (time, censor, xvar, cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
          } else {
            #xlabel <- c("Years", "Months", "Days")[input$time2]
            xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
            fit <- CumIncidence (time, censor, xvar, t = as.numeric(as.vector(unlist(strsplit(input$text, ", ")))), cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
          }
        } else if (class(xvar) %in% c("integer", "numeric") & input$binary2 == "continuous") {
          perc <- as.numeric(as.integer(input$cutoff3))
          
          dat$Group <- ifelse(dat[, 'xvar'] < quantile(dat[, 'xvar'], perc/100), "Low", "High")
          Group <- as.factor(dat$Group)
          
          if (input$points == 0) {
            xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
            fit <- CumIncidence (as.numeric(time), censor, Group, cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
          } else {
            #xlabel <- c("Years", "Months", "Days")[input$time2]
            xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
            fit <- CumIncidence (as.numeric(time), censor, Group, t = as.numeric(as.vector(unlist(strsplit(input$text, ", ")))), cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
          }
          
        } 
      }
      dev.off()

      file.copy(paste(pdf_file,'.pdf', sep='') ,file, overwrite=TRUE)
    })
  
  output$downloadciftable = downloadHandler(
    filename <- function() {
      csv_file <<- as.character(input$fname33)
      paste('ciftable_', csv_file, Sys.time(),'.csv', sep='')
    },
    content = function(file) {
      data3 <- data_input3()
      if (input$select31 == "All Patients") {
        time <- data3[,input$select32]
        censor <- data3[,input$select33]
        dat <- cbind.data.frame(time, censor)
        if (is.null(time) | is.null(censor)) {
          return(NULL)
        } else if (class(time)=="numeric" |class(time)=="integer"){
          if (input$points == 0) {
            xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
            fit <- CumIncidence (time, censor, cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
          } else {
            #xlabel <- c("Years", "Months", "Days")[input$time2]
            xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
            fit <- CumIncidence (time, censor, t = as.numeric(as.vector(unlist(strsplit(input$text, ", ")))), cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
          }
          ## Drawing curves
        }else {
          return(NULL)
        }
      } else {
        xvar <- data3[, input$select31]
        time <- data3[,input$select32]
        censor <- data3[,input$select33]
        dat <- cbind.data.frame(time, censor, xvar)
        if (is.null(xvar) | is.null(time) | is.null(censor)) {
          return(NULL)
        } else if (nlevels(xvar) > 30) {
          return(NULL)
        } else if (class(xvar) == "factor" & (class(time)=="numeric"|class(time)=="integer")){
          if (input$points == 0) {
            xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
            fit <- CumIncidence (time, censor, xvar, cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
          } else {
            #xlabel <- c("Years", "Months", "Days")[input$time2]
            xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
            fit <- CumIncidence (time, censor, xvar, t = as.numeric(as.vector(unlist(strsplit(input$text, ", ")))), cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
          }
          ## Drawing curves
        } else if(class(xvar) == "character" & (class(time)=="numeric"|class(time)=="integer")){
          xvar <- as.factor(xvar)
          if (input$points == 0) {
            xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
            fit <- CumIncidence (time, censor, xvar, cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
          } else {
            #xlabel <- c("Years", "Months", "Days")[input$time2]
            xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
            fit <- CumIncidence (time, censor, xvar, t = as.numeric(as.vector(unlist(strsplit(input$text, ", ")))), cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
          }
          
        }else if (class(xvar) %in% c("integer", "numeric") & input$binary2 == "continuous") {
          perc <- as.numeric(as.integer(input$cutoff3))
          
          dat$Group <- ifelse(dat[, 'xvar'] < quantile(dat[, 'xvar'], perc/100), "Low", "High")
          Group <- as.factor(dat$Group)
          
          if (input$points == 0) {
            xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
            fit <- CumIncidence (as.numeric(time), censor, Group, cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
          } else {
            #xlabel <- c("Years", "Months", "Days")[input$time2]
            xlabel <- c("Years", "Months", "Days")[as.numeric(input$time2)]
            fit <- CumIncidence (as.numeric(time), censor, Group, t = as.numeric(as.vector(unlist(strsplit(input$text, ", ")))), cencode = input$censor, xlab=xlabel, event = as.numeric(input$event))
          }
          
        } else {
          return(NULL)
        }
      }
      z <- qnorm(1-(1-0.95)/2)
      lower <- fit$est ^ exp(-z*fit$se/(fit$est*log(fit$est)))
      lower <- melt(round(lower, 4), id.vars=rownames(lower))
      lower <- lower[order(lower[,1]),]
      upper <- fit$est ^ exp(z*fit$se/(fit$est*log(fit$est)))
      upper <- melt(round(upper, 4), id.vars=rownames(upper))
      upper <- upper[order(upper[,1]),]
      est <- melt(round(fit$est, 4), id.vars=rownames(fit$est))
      est <- est[order(est[,1]),]
      
      ci <- data.frame(paste0(est[, 3], " (", lower[, 3], ", ", upper[, 3], ")"))
      Variable <- as.character(est[, 1])
      table <- cbind.data.frame(Variable, data.frame(est[, 2]), ci, stringsAsFactors=FALSE)
      colnames(table) <- c(input$select31, paste0("Time (", xlabel, ")"), "CIF Estimate (95% CI)")
      vec <- as.vector(table[, 2])
      seq <- 1:nrow(table)
      seq <- seq[-which(vec == unique(vec)[1])]
      table[seq, 1] <- ""
      table
      
      write.csv(table, file, row.names = F)
      
    })
  
  output$x2 = downloadHandler(
    filename <- function() {
      csv_file <<- as.character(input$fname32)
      paste('ciftable_', csv_file, Sys.time(),'.csv', sep='')
    },
    content = function(file) {
      data3 <- data_input3()
      xvar <- data3[,input$show_vars36, drop = FALSE]
      time <- data3[,input$select34]
      censor <- data3[,input$select35]
      
      res <- list()
      for (i in 1: ncol(xvar)){
        x <- xvar[, i]
        if (is.null(x) | is.null(time) | is.null(censor)) {
          return(NULL)
        } else if (nlevels(x) > 30) {
          return(NULL)
        } else if (class(x) == "factor" & (class(time)=="numeric"|class(time)=="integer")){
          Variable <- c(colnames(xvar)[i], rep("", (length(levels(x))-1)))
          #x <- C(x, contr.treatment, base=3)
          fit <- crr(time, censor, factor2ind(x), failcode = input$event2, cencode = input$censor2)
          sum <- summary(fit)
          hazard.ratio = round(sum$conf.int[, 1], 2)
          lower95 = round(sum$conf.int[, 3], 2)
          upper95 = round(sum$conf.int[, 4], 2)
          gray.p.value = ifelse(cuminc(time, censor, factor2ind(x))$Tests[as.integer(input$event2), "pv"] < 0.001, "<0.001", paste0(round(cuminc(time, censor, factor2ind(x))$Tests[as.integer(input$event2), "pv"], 4)))
          grayp <- c(gray.p.value, rep("", (length(levels(x))-1)))
          type3.p.value = ifelse(sum$coef[, 5] < 0.001, "<0.001", paste0(round(sum$coef[, 5], 4)))
          counts <- data.frame(table(x))
          counts <- rbind.data.frame(counts[2:nrow(counts),], counts[1, ])
          coxres <- cbind.data.frame(Variable, counts, c(paste0(hazard.ratio, " (", lower95,
                                                                "-", upper95, ")"), ""), c(type3.p.value, ""), grayp)
          colnames(coxres) <- c("Variable", "Level", "N", "Hazard Ratio (95% CI)", "Type 3 P-value", "Gray's P-value")
          res[[i]] <- coxres
        }else if(class(x) == "character" & (class(time)=="numeric"|class(time)=="integer")){ 
          x <- as.factor(x)
          Variable <- c(colnames(xvar)[i], rep("", (length(levels(x))-1)))
          #x <- C(x, contr.treatment, base=3)
          fit <- crr(time, censor, factor2ind(x), failcode = input$event2, cencode = input$censor2)
          sum <- summary(fit)
          hazard.ratio = round(sum$conf.int[, 1], 2)
          lower95 = round(sum$conf.int[, 3], 2)
          upper95 = round(sum$conf.int[, 4], 2)
          gray.p.value = ifelse(cuminc(time, censor, factor2ind(x))$Tests[as.integer(input$event2), "pv"] < 0.001, "<0.001", paste0(round(cuminc(time, censor, factor2ind(x))$Tests[as.integer(input$event2), "pv"], 4)))
          grayp <- c(gray.p.value, rep("", (length(levels(x))-1)))
          type3.p.value = ifelse(sum$coef[, 5] < 0.001, "<0.001", paste0(round(sum$coef[, 5], 4)))
          counts <- data.frame(table(x))
          counts <- rbind.data.frame(counts[2:nrow(counts),], counts[1, ])
          coxres <- cbind.data.frame(Variable, counts, c(paste0(hazard.ratio, " (", lower95,
                                                                "-", upper95, ")"), ""), c(type3.p.value, ""), grayp)
          colnames(coxres) <- c("Variable", "Level", "N", "Hazard Ratio (95% CI)", "Type 3 P-value", "Gray's P-value")
          res[[i]] <- coxres
          
        }else if (class(x) %in% c("integer", "numeric") & (class(time)=="numeric"|class(time)=="integer")){
          Variable <- colnames(xvar)[i]
          fit <- crr(time, censor, x, failcode = input$event2, cencode = input$censor2)
          sum <- summary(fit)
          hazard.ratio = round(sum$conf.int[1], 2)
          lower95 = round(sum$conf.int[3], 2)
          upper95 = round(sum$conf.int[4], 2)
          #logrank.p.value = sum$sctest[3]
          type3.p.value = ifelse(sum$coef[5] < 0.001, "<0.001", paste0(round(sum$coef[5], 4)))
          coxres <- cbind.data.frame(Variable, NA, length(!is.na(x)), paste0(hazard.ratio, " (", lower95,
                                                                             "-", upper95, ")"), type3.p.value, "-")
          colnames(coxres) <- c("Variable", "Level", "N", "Hazard Ratio (95% CI)", "Type 3 P-value", "Gray's P-value")
          res[[i]] <- coxres
        } else 
          return(NULL)
      }
      
      res_table <- do.call("rbind", res)
      
      write.csv(res_table, file, row.names = F)
      
    })

  data_input4 <- reactive({
    if(input$file4 == 'Example4'){
      d4 <- read.csv("data/example_landmark_Stan.csv", header =T, sep =",",stringsAsFactors = F)
    }
    else if(input$file4 == 'load_my_own4'){
      inFile <- input$file42
      if (is.null(inFile))
        return(NULL)
      else if(grepl(".xlsx", inFile[1])) { d4 = read.xlsx(as.character(inFile$datapath), colNames = TRUE, rowNames = F, as.is = T) }
      else if(grepl(".csv", inFile[1])) { d4 = read.csv(as.character(inFile$datapath), header = TRUE, sep = ",", stringsAsFactors = F, as.is = T, fill = T) }
      else if(grepl(".txt", inFile[1])) { d4 = read.table(as.character(inFile$datapath), header = TRUE, sep = "\t", stringsAsFactors = F, as.is = T, fill = T) }
    }
    else 
      return(NULL)
    # dim(data)
    Dataset4 <- data.frame(d4)
    return(as.data.frame(Dataset4))
  })
  
  output$downloadEx4 <- downloadHandler(
    
    filename <- function() {
      paste('Example ds data', Sys.time(),'.csv', sep='')
    },
    content <- function(file) {
      ds4 <- data_input4()
      write.csv(ds4, file, row.names = FALSE)
    }
  )
  
  output$landmarkplot <- renderPlot({
    data4 <- data_input4()
    if(!is.null(data4)){
      
      for (j in 1:ncol(data4)) {
        if (class(data4[, j]) %in% c("character")) 
          data4[,j] <- as.factor(data4[,j])
        else data4[,j] = data4[, j]
      }
      
    xvar <- data4[,input$select41]
    time <- as.numeric(data4[,input$select42])
    censor <- as.numeric(factor(data4[,input$select43]))
    rtime <- as.numeric(data4[,input$select44])
    dat <- cbind.data.frame(time, censor, xvar, rtime)
    dat <- as.data.frame(dat[!is.na(dat$xvar),]) ###
    check_data <<- dat
    
    if (is.null(xvar) | is.null(time) | is.null(censor)) {
      return(NULL)
    } else if (nlevels(xvar) > 30) {
      return(NULL)
    } else if (class(xvar) == "factor" & input$text2 != "") {
      
      #test <- cutLM(dat, outcome=list(time="time", status="censor"),
      #              LM=as.numeric(input$text2), horizon=max(time), covs=list(varying = colnames(dat)[3]))
      
      
      test0 <- check_data[order(check_data$time),] 
      test <- test0[test0$time > as.numeric(input$text2), ]
      
      change_level <- as.character(unique(test[is.na(test$rtime), "xvar"]))
      
      for(j in 1:nrow(test)) {
        check_xvar <- as.character(test$xvar[j])
        test$xvar[j] <- ifelse(is.na(test$rtime[j])|test$rtime[j] > as.numeric(input$text2), change_level, as.character(test$xvar[j]))
      }
      
      fit1 <- survfit(Surv(time, censor) ~ xvar, data = dat)
      fit2 <- survfit(Surv(time, censor) ~ xvar, data = test)

      p1 <- survdiff(Surv(time, censor) ~ xvar, data = dat)
      p2 <- survdiff(Surv(time, censor) ~ xvar, data = test)
      p.val1 <-  1 - pchisq(p1$chisq, length(p1$n) - 1)
      p.val2 <-  1 - pchisq(p2$chisq, length(p2$n) - 1)
      darkcols <- c("blue", "red", "green", "black", "yellow", "orange", "pink", "purple", "grey")
      
      # Drawing curves
      xlabel <- c("Years", "Months", "Days")
      b <- c(0.25, 3, 90)
      pos1<-c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]), as.numeric(input$text2))
      NR1<-nrisk(fit1, times=pos1)
      NR1[, ncol(NR1)] <- rep(NA, nrow(NR1)) 

      pos2<-seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)])
      NR2<-nrisk(fit2, times=pos2)

      if (input$riskt2 == TRUE) {
        #TF <- c(TRUE, FALSE)
        if (input$option == TRUE) {
          op <- par(mar=c(0.5, 0, 0.2, 0), mfrow=c(1,2),
                    oma = c(20, 5, 0.6, 0.2))
          layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
          plot(fit1, col=darkcols[1:length(levels(dat$xvar))], xlim=c(0,as.numeric(input$text2)), axes = F)
          text(0.45*as.numeric(input$text2), 1, paste0("Log-rank p-value: ", round(p.val1, 4)), cex = 1.5)
          axis(side = 1, cex.axis = 1.5, at=c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]),as.numeric(input$text2)))
          axis(side = 2)
          #for (n in 1: dim(NR1)[1]){text(x=pos1,y= 0,labels= NR1[n,],cex=1.5,pos=3, offset=(-0.8*n), col=darkcols[1:length(levels(test$xvar))],font=3)}
          #text(x=0,y=0.01, label="Number at Risk", pos=4, offset=-0.5, cex=1.5, font=4)
          for (n in 1: dim(NR1)[1]) {mtext(text = NR1[n, ], side = 1, line = (6+2*n), outer = F, at = pos1, col=darkcols[1:length(levels(test$xvar))][n])}
          mtext("Number at Risk", side = 1, line = 5, at = 0, outer = T)
          plot(fit2, col=darkcols[1:length(levels(dat$xvar))], xlim=c(as.numeric(input$text2), 1.02*max(time)), axes = F)
          text(0.7*1.02*max(dat$time), 1, paste0("Log-rank p-value: ", round(p.val2, 4)), cex = 1.5)
          axis(side = 1, cex.axis=1.5, at=seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)]))
          for (n in 1: dim(NR2)[1]) {mtext(text = NR2[n, ], side = 1, line = (6+2*n), outer = F, at = pos2, col=darkcols[1:length(levels(test$xvar))][n])}
          title(xlab = xlabel[as.numeric(input$time3)],
                ylab = "Survival Probability",
                outer = TRUE, line = 3, cex.lab= 2)
          legend(0.85*1.02*max(dat$time), 1, legend = levels(factor(test$xvar)), 
                 lty = c(1:1), col=darkcols[1:length(levels(test$xvar))], cex = 1.5)
          abline(v=as.numeric(input$text2), lty = 2)
          par(op)
          
        } else {
          op <- par(mar=c(0.5, 0, 0.2, 0), mfrow=c(1,2),
                    oma = c(20, 5, 0.6, 0.2))
          layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
          plot(fit1, xaxs='i', xlim=c(0,as.numeric(input$text2)), ylim =c(0,1), fun = function(x) {1-x}, col=darkcols[1:length(levels(dat$xvar))], axes = F)
          text(0.45*as.numeric(input$text2), 1, paste0("Log-rank p-value: ", round(p.val1, 4)), cex = 1.5)
          axis(side = 1, cex.axis = 1.5, at=c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]),as.numeric(input$text2)))
          axis(side = 2)
          for (n in 1: dim(NR1)[1]) {mtext(text = NR1[n, ], side = 1, line = (6+2*n), outer = F, at = pos1, col=darkcols[1:length(levels(test$xvar))][n])}
          mtext("Number at Risk", side = 1, line = 5, at = 0, outer = T)
          plot(fit2, xaxs='i', fun = function(x) {1-x}, col=darkcols[1:length(levels(test$xvar))], xlim=c(as.numeric(input$text2), 1.02*max(time)), ylim =c(0,1), axes = F)
          text(0.7*1.02*max(dat$time), 1, paste0("Log-rank p-value: ", round(p.val2, 4)), cex = 1.5)
          axis(side = 1, cex.axis=1.5, at=seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)]))
          for (n in 1: dim(NR2)[1]) {mtext(text = NR2[n, ], side = 1, line = (6+2*n), outer = F, at = pos2, col=darkcols[1:length(levels(test$xvar))][n])}
          title(xlab = xlabel[as.numeric(input$time3)],
                ylab = "Cumulative Incidence Rate",
                outer = TRUE, line = 3, cex.lab= 2)
          legend(0.85*1.02*max(dat$time), 0.1, legend = levels(factor(test$xvar)), 
                 lty = c(1:1), col=darkcols[1:length(levels(test$xvar))], cex = 1.5)
          abline(v=as.numeric(input$text2), lty = 2)
          par(op)
          
        }
        
      } else {
        #TF <- c(TRUE, FALSE)
        if (input$option == TRUE) {
          op <- par(mar=c(0.5, 0, 0.2, 0), mfrow=c(1,2),
                    oma = c(20, 5, 0.6, 0.2))
          layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
          plot(fit1, col=darkcols[1:length(levels(dat$xvar))], xlim=c(0,as.numeric(input$text2)), axes = F)
          text(0.45*as.numeric(input$text2), 1, paste0("Log-rank p-value: ", round(p.val1, 4)), cex = 1.5)
          axis(side = 1, cex.axis = 1.5, at=c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]),as.numeric(input$text2)))
          axis(side = 2)
          #for (n in 1: dim(NR1)[1]){text(x=pos1,y= 0,labels= NR1[n,],cex=1.5,pos=3, offset=(-0.8*n), col=darkcols[1:length(levels(test$xvar))],font=3)}
          #text(x=0,y=0.01, label="Number at Risk", pos=4, offset=-0.5, cex=1.5, font=4)
          plot(fit2, col=darkcols[1:length(levels(dat$xvar))], xlim=c(as.numeric(input$text2), 1.02*max(time)), axes = F)
          text(0.7*1.02*max(dat$time), 1, paste0("Log-rank p-value: ", round(p.val2, 4)), cex = 1.5)
          axis(side = 1, cex.axis=1.5, at=seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)]))
          title(xlab = xlabel[as.numeric(input$time3)],
                ylab = "Survival Probability",
                outer = TRUE, line = 3, cex.lab= 2)
          legend(0.85*1.02*max(dat$time), 1, legend = levels(factor(test$xvar)), 
                 lty = c(1:1), col=darkcols[1:length(levels(test$xvar))], cex = 1.5)
          abline(v=as.numeric(input$text2), lty = 2)
          par(op)
          
        } else {
          op <- par(mar=c(0.5, 0, 0.2, 0), mfrow=c(1,2),
                    oma = c(20, 5, 0.6, 0.2))
          layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
          plot(fit1, xaxs='i', xlim=c(0,as.numeric(input$text2)), ylim =c(0,1), fun = function(x) {1-x}, col=darkcols[1:length(levels(dat$xvar))], axes = F)
          text(0.45*as.numeric(input$text2), 1, paste0("Log-rank p-value: ", round(p.val1, 4)), cex = 1.5)
          axis(side = 1, cex.axis = 1.5, at=c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]),as.numeric(input$text2)))
          axis(side = 2)
          plot(fit2, xaxs='i', fun = function(x) {1-x}, col=darkcols[1:length(levels(test$xvar))], xlim=c(as.numeric(input$text2), 1.02*max(time)), ylim =c(0,1), axes = F)
          text(0.7*1.02*max(dat$time), 1, paste0("Log-rank p-value: ", round(p.val2, 4)), cex = 1.5)
          axis(side = 1, cex.axis=1.5, at=seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)]))
          title(xlab = xlabel[as.numeric(input$time3)],
                ylab = "Cumulative Incidence Rate",
                outer = TRUE, line = 3, cex.lab= 2)
          legend(0.85*1.02*max(dat$time), 0.1, legend = levels(factor(test$xvar)), 
                 lty = c(1:1), col=darkcols[1:length(levels(test$xvar))], cex = 1.5)
          abline(v=as.numeric(input$text2), lty = 2)
          par(op)
          
        }
        
      }

    } else if (class(xvar) %in% c("integer", "numeric")  & input$text2 != "" & input$binary3 == "continuous") {
      if (as.numeric(as.integer(input$cutoff4)) == 1) {
        perc <- optimalcut(dat)
      } else {perc <- as.numeric(as.integer(input$cutoff4))}
      
      dat$Group <- ifelse(dat[, 'xvar'] < quantile(dat[, 'xvar'], perc/100), "Low", "High")
      #Group <- as.factor(dat$Group)

      dat <- dat[, -3]
      
      test <- cutLM(dat, outcome=list(time="time", status="censor"),
                    LM=as.numeric(input$text2), horizon=max(time), covs=list(fixed=colnames(dat)[3]))
      fit1 <- survfit(Surv(time, censor) ~ Group, data = dat)
      fit2 <- survfit(Surv(time, censor) ~ Group, data = test)
      
      p1 <- survdiff(Surv(time, censor) ~ Group, data = dat)
      p2 <- survdiff(Surv(time, censor) ~ Group, data = test)
      p.val1 <-  1 - pchisq(p1$chisq, length(p1$n) - 1)
      p.val2 <-  1 - pchisq(p2$chisq, length(p2$n) - 1)
      darkcols <- c("blue", "red", "green", "black", "yellow", "orange", "pink", "purple", "grey")
      
      # Drawing curves
      xlabel <- c("Years", "Months", "Days")
      b <- c(0.25, 3, 90)
      pos1<-c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]), as.numeric(input$text2))
      NR1<-nrisk(fit1, times=pos1)
      NR1[, ncol(NR1)] <- rep(NA, nrow(NR1))
      
      pos2<-seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)])
      NR2<-nrisk(fit2, times=pos2)
      
      if (input$riskt2 == TRUE) {
        #TF <- c(TRUE, FALSE)
        if (input$option == TRUE) {
          op <- par(mar=c(0.5, 0, 0.2, 0), mfrow=c(1,2),
                    oma = c(20, 5, 0.6, 0.2))
          layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
          plot(fit1, col=darkcols[1:length(levels(factor(dat$Group)))], xlim=c(0,as.numeric(input$text2)), axes = F)
          text(0.45*as.numeric(input$text2), 1, paste0("Log-rank p-value: ", round(p.val1, 4)), cex = 1.5)
          axis(side = 1, cex.axis = 1.5, at=c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]),as.numeric(input$text2)))
          axis(side = 2)
          #for (n in 1: dim(NR1)[1]){text(x=pos1,y= 0,labels= NR1[n,],cex=1.5,pos=3, offset=(-0.8*n), col=darkcols[1:length(levels(test$Group))],font=3)}
          #text(x=0,y=0.01, label="Number at Risk", pos=4, offset=-0.5, cex=1.5, font=4)
          for (n in 1: dim(NR1)[1]) {mtext(text = NR1[n, ], side = 1, line = (6+2*n), outer = F, at = pos1, col=darkcols[1:length(levels(factor(test$Group)))][n])}
          mtext("Number at Risk", side = 1, line = 5, at = 0, outer = T)
          plot(fit2, col=darkcols[1:length(levels(factor(dat$Group)))], xlim=c(as.numeric(input$text2), 1.02*max(time)), axes = F)
          text(0.7*1.02*max(dat$time), 1, paste0("Log-rank p-value: ", round(p.val2, 4)), cex = 1.5)
          axis(side = 1, cex.axis=1.5, at=seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)]))
          for (n in 1: dim(NR2)[1]) {mtext(text = NR2[n, ], side = 1, line = (6+2*n), outer = F, at = pos2, col=darkcols[1:length(levels(factor(test$Group)))][n])}
          title(xlab = xlabel[as.numeric(input$time3)],
                ylab = "Survival Probability",
                outer = TRUE, line = 3, cex.lab= 2)
          legend(0.85*1.02*max(dat$time), 1, legend = levels(factor(test$Group)), 
                 lty = c(1:1), col=darkcols[1:length(levels(factor(test$Group)))], cex = 1.5)
          abline(v=as.numeric(input$text2), lty = 2)
          par(op)
          
        } else {
          op <- par(mar=c(0.5, 0, 0.2, 0), mfrow=c(1,2),
                    oma = c(20, 5, 0.6, 0.2))
          layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
          plot(fit1, xaxs='i', xlim=c(0,as.numeric(input$text2)), ylim =c(0,1), fun = function(x) {1-x}, col=darkcols[1:length(levels(factor(dat$Group)))], axes = F)
          text(0.45*as.numeric(input$text2), 1, paste0("Log-rank p-value: ", round(p.val1, 4)), cex = 1.5)
          axis(side = 1, cex.axis = 1.5, at=c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]),as.numeric(input$text2)))
          axis(side = 2)
          for (n in 1: dim(NR1)[1]) {mtext(text = NR1[n, ], side = 1, line = (6+2*n), outer = F, at = pos1, col=darkcols[1:length(levels(factor(test$Group)))][n])}
          mtext("Number at Risk", side = 1, line = 5, at = 0, outer = T)
          plot(fit2, xaxs='i', fun = function(x) {1-x}, col=darkcols[1:length(levels(factor(test$Group)))], xlim=c(as.numeric(input$text2), 1.02*max(time)), ylim =c(0,1), axes = F)
          text(0.7*1.02*max(dat$time), 1, paste0("Log-rank p-value: ", round(p.val2, 4)), cex = 1.5)
          axis(side = 1, cex.axis=1.5, at=seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)]))
          for (n in 1: dim(NR2)[1]) {mtext(text = NR2[n, ], side = 1, line = (6+2*n), outer = F, at = pos2, col=darkcols[1:length(levels(factor(test$Group)))][n])}
          title(xlab = xlabel[as.numeric(input$time3)],
                ylab = "Cumulative Incidence Rate",
                outer = TRUE, line = 3, cex.lab= 2)
          legend(0.85*1.02*max(dat$time), 0.1, legend = levels(factor(test$Group)), 
                 lty = c(1:1), col=darkcols[1:length(levels(factor(test$Group)))], cex = 1.5)
          abline(v=as.numeric(input$text2), lty = 2)
          par(op)
          
        }
        
      } else {
        #TF <- c(TRUE, FALSE)
        if (input$option == TRUE) {
          op <- par(mar=c(0.5, 0, 0.2, 0), mfrow=c(1,2),
                    oma = c(20, 5, 0.6, 0.2))
          layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
          plot(fit1, col=darkcols[1:length(levels(factor(dat$Group)))], xlim=c(0,as.numeric(input$text2)), axes = F)
          text(0.45*as.numeric(input$text2), 1, paste0("Log-rank p-value: ", round(p.val1, 4)), cex = 1.5)
          axis(side = 1, cex.axis = 1.5, at=c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]),as.numeric(input$text2)))
          axis(side = 2)
          #for (n in 1: dim(NR1)[1]){text(x=pos1,y= 0,labels= NR1[n,],cex=1.5,pos=3, offset=(-0.8*n), col=darkcols[1:length(levels(test$Group))],font=3)}
          #text(x=0,y=0.01, label="Number at Risk", pos=4, offset=-0.5, cex=1.5, font=4)
          plot(fit2, col=darkcols[1:length(levels(factor(dat$Group)))], xlim=c(as.numeric(input$text2), 1.02*max(time)), axes = F)
          text(0.7*1.02*max(dat$time), 1, paste0("Log-rank p-value: ", round(p.val2, 4)), cex = 1.5)
          axis(side = 1, cex.axis=1.5, at=seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)]))
          title(xlab = xlabel[as.numeric(input$time3)],
                ylab = "Survival Probability",
                outer = TRUE, line = 3, cex.lab= 2)
          legend(0.85*1.02*max(dat$time), 1, legend = levels(factor(test$Group)), 
                 lty = c(1:1), col=darkcols[1:length(levels(factor(test$Group)))], cex = 1.5)
          abline(v=as.numeric(input$text2), lty = 2)
          par(op)
          
        } else {
          op <- par(mar=c(0.5, 0, 0.2, 0), mfrow=c(1,2),
                    oma = c(20, 5, 0.6, 0.2))
          layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
          plot(fit1, xaxs='i', xlim=c(0,as.numeric(input$text2)), ylim =c(0,1), fun = function(x) {1-x}, col=darkcols[1:length(levels(factor(dat$Group)))], axes = F)
          text(0.45*as.numeric(input$text2), 1, paste0("Log-rank p-value: ", round(p.val1, 4)), cex = 1.5)
          axis(side = 1, cex.axis = 1.5, at=c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]),as.numeric(input$text2)))
          axis(side = 2)
          plot(fit2, xaxs='i', fun = function(x) {1-x}, col=darkcols[1:length(levels(factor(test$Group)))], xlim=c(as.numeric(input$text2), 1.02*max(time)), ylim =c(0,1), axes = F)
          text(0.7*1.02*max(dat$time), 1, paste0("Log-rank p-value: ", round(p.val2, 4)), cex = 1.5)
          axis(side = 1, cex.axis=1.5, at=seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)]))
          title(xlab = xlabel[as.numeric(input$time3)],
                ylab = "Cumulative Incidence Rate",
                outer = TRUE, line = 3, cex.lab= 2)
          legend(0.85*1.02*max(dat$time), 0.1, legend = levels(factor(test$Group)), 
                 lty = c(1:1), col=darkcols[1:length(levels(factor(test$Group)))], cex = 1.5)
          abline(v=as.numeric(input$text2), lty = 2)
          par(op)
          
        }
        
      }
      
    } else {
      return(NULL)
    }
    }
  })

  output$pv41 <- renderUI({
    hs3 <- paste("&emsp;")
    hs4 <- paste("To Visualize the Landmark Survival Plot:")
    HTML(paste(h2(strong(hs4)), hs3, sep = '<br/>'))
  })
  
  output$downloadlandmarkplot <- downloadHandler(
    filename <- function() {
      pdf_file <<- as.character(input$fname41)
      paste('landmarkplot_', pdf_file, Sys.time(),'.pdf', sep='')
    },
    content <- function(file) {
      pdf(file=paste(pdf_file,".pdf",sep="") , height= 8, width=12)
      #plot_fp()
      data4 <- data_input4()
      
      for (j in 1:ncol(data4)) {
          if (class(data4[, j]) %in% c("character")) 
            data4[,j] <- as.factor(data4[,j])
          else data4[,j] = data4[, j]
        }
        
        xvar <- data4[,input$select41]
        time <- as.numeric(data4[,input$select42])
        censor <- as.numeric(factor(data4[,input$select43]))
        rtime <- as.numeric(data4[,input$select44])
        dat <- cbind.data.frame(time, censor, xvar, rtime)
        dat <- as.data.frame(dat[!is.na(dat$xvar),]) ###
        check_data <<- dat
        
        if (is.null(xvar) | is.null(time) | is.null(censor)) {
          return(NULL)
        } else if (nlevels(xvar) > 30) {
          return(NULL)
        } else if (class(xvar) == "factor" & input$text2 != "") {
          
          #test <- cutLM(dat, outcome=list(time="time", status="censor"),
          #              LM=as.numeric(input$text2), horizon=max(time), covs=list(varying = colnames(dat)[3]))
          
          
          test0 <- check_data[order(check_data$time),] 
          test <- test0[test0$time > as.numeric(input$text2), ]
          
          change_level <- as.character(unique(test[is.na(test$rtime), "xvar"]))
          
          for(j in 1:nrow(test)) {
            check_xvar <- as.character(test$xvar[j])
            test$xvar[j] <- ifelse(is.na(test$rtime[j])|test$rtime[j] > as.numeric(input$text2), change_level, as.character(test$xvar[j]))
          }
          
          fit1 <- survfit(Surv(time, censor) ~ xvar, data = dat)
          fit2 <- survfit(Surv(time, censor) ~ xvar, data = test)
          
          p1 <- survdiff(Surv(time, censor) ~ xvar, data = dat)
          p2 <- survdiff(Surv(time, censor) ~ xvar, data = test)
          p.val1 <-  1 - pchisq(p1$chisq, length(p1$n) - 1)
          p.val2 <-  1 - pchisq(p2$chisq, length(p2$n) - 1)
          darkcols <- c("blue", "red", "green", "black", "yellow", "orange", "pink", "purple", "grey")
          
          
        # Drawing curves
        xlabel <- c("Years", "Months", "Days")
        b <- c(0.25, 3, 90)
        pos1<-c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]), as.numeric(input$text2))
        NR1<-nrisk(fit1, times=pos1)
        NR1[, ncol(NR1)] <- rep(NA, nrow(NR1))
        
        pos2<-seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)])
        NR2<-nrisk(fit2, times=pos2)
        
        if (input$riskt2 == TRUE) {
          #TF <- c(TRUE, FALSE)
          if (input$option == TRUE) {
            op <- par(mar=c(0.5, 0, 0.2, 0), mfrow=c(1,2),
                      oma = c(20, 5, 0.6, 0.2))
            layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
            plot(fit1, col=darkcols[1:length(levels(dat$xvar))], xlim=c(0,as.numeric(input$text2)), axes = F)
            text(0.45*as.numeric(input$text2), 1, paste0("Log-rank p-value: ", round(p.val1, 4)), cex = 1.5)
            axis(side = 1, cex.axis = 1.5, at=c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]),as.numeric(input$text2)))
            axis(side = 2)
            #for (n in 1: dim(NR1)[1]){text(x=pos1,y= 0,labels= NR1[n,],cex=1.5,pos=3, offset=(-0.8*n), col=darkcols[1:length(levels(test$xvar))],font=3)}
            #text(x=0,y=0.01, label="Number at Risk", pos=4, offset=-0.5, cex=1.5, font=4)
            for (n in 1: dim(NR1)[1]) {mtext(text = NR1[n, ], side = 1, line = (6+2*n), outer = F, at = pos1, col=darkcols[1:length(levels(test$xvar))][n])}
            mtext("Number at Risk", side = 1, line = 5, at = 0, outer = T)
            plot(fit2, col=darkcols[1:length(levels(dat$xvar))], xlim=c(as.numeric(input$text2), 1.02*max(time)), axes = F)
            text(0.7*1.02*max(dat$time), 1, paste0("Log-rank p-value: ", round(p.val2, 4)), cex = 1.5)
            axis(side = 1, cex.axis=1.5, at=seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)]))
            for (n in 1: dim(NR2)[1]) {mtext(text = NR2[n, ], side = 1, line = (6+2*n), outer = F, at = pos2, col=darkcols[1:length(levels(test$xvar))][n])}
            title(xlab = xlabel[as.numeric(input$time3)],
                  ylab = "Survival Probability",
                  outer = TRUE, line = 3, cex.lab= 2)
            legend(0.85*1.02*max(dat$time), 1, legend = levels(factor(test$xvar)), 
                   lty = c(1:1), col=darkcols[1:length(levels(test$xvar))], cex = 1.5)
            abline(v=as.numeric(input$text2), lty = 2)
            par(op)
            
          } else {
            op <- par(mar=c(0.5, 0, 0.2, 0), mfrow=c(1,2),
                      oma = c(20, 5, 0.6, 0.2))
            layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
            plot(fit1, xaxs='i', xlim=c(0,as.numeric(input$text2)), ylim =c(0,1), fun = function(x) {1-x}, col=darkcols[1:length(levels(dat$xvar))], axes = F)
            text(0.45*as.numeric(input$text2), 1, paste0("Log-rank p-value: ", round(p.val1, 4)), cex = 1.5)
            axis(side = 1, cex.axis = 1.5, at=c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]),as.numeric(input$text2)))
            axis(side = 2)
            for (n in 1: dim(NR1)[1]) {mtext(text = NR1[n, ], side = 1, line = (6+2*n), outer = F, at = pos1, col=darkcols[1:length(levels(test$xvar))][n])}
            mtext("Number at Risk", side = 1, line = 5, at = 0, outer = T)
            plot(fit2, xaxs='i', fun = function(x) {1-x}, col=darkcols[1:length(levels(test$xvar))], xlim=c(as.numeric(input$text2), 1.02*max(time)), ylim =c(0,1), axes = F)
            text(0.7*1.02*max(dat$time), 1, paste0("Log-rank p-value: ", round(p.val2, 4)), cex = 1.5)
            axis(side = 1, cex.axis=1.5, at=seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)]))
            for (n in 1: dim(NR2)[1]) {mtext(text = NR2[n, ], side = 1, line = (6+2*n), outer = F, at = pos2, col=darkcols[1:length(levels(test$xvar))][n])}
            title(xlab = xlabel[as.numeric(input$time3)],
                  ylab = "Cumulative Incidence Rate",
                  outer = TRUE, line = 3, cex.lab= 2)
            legend(0.85*1.02*max(dat$time), 0.1, legend = levels(factor(test$xvar)), 
                   lty = c(1:1), col=darkcols[1:length(levels(test$xvar))], cex = 1.5)
            abline(v=as.numeric(input$text2), lty = 2)
            par(op)
            
          }
          
        } else {
          #TF <- c(TRUE, FALSE)
          if (input$option == TRUE) {
            op <- par(mar=c(0.5, 0, 0.2, 0), mfrow=c(1,2),
                      oma = c(20, 5, 0.6, 0.2))
            layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
            plot(fit1, col=darkcols[1:length(levels(dat$xvar))], xlim=c(0,as.numeric(input$text2)), axes = F)
            text(0.45*as.numeric(input$text2), 1, paste0("Log-rank p-value: ", round(p.val1, 4)), cex = 1.5)
            axis(side = 1, cex.axis = 1.5, at=c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]),as.numeric(input$text2)))
            axis(side = 2)
            #for (n in 1: dim(NR1)[1]){text(x=pos1,y= 0,labels= NR1[n,],cex=1.5,pos=3, offset=(-0.8*n), col=darkcols[1:length(levels(test$xvar))],font=3)}
            #text(x=0,y=0.01, label="Number at Risk", pos=4, offset=-0.5, cex=1.5, font=4)
            plot(fit2, col=darkcols[1:length(levels(dat$xvar))], xlim=c(as.numeric(input$text2), 1.02*max(time)), axes = F)
            text(0.7*1.02*max(dat$time), 1, paste0("Log-rank p-value: ", round(p.val2, 4)), cex = 1.5)
            axis(side = 1, cex.axis=1.5, at=seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)]))
            title(xlab = xlabel[as.numeric(input$time3)],
                  ylab = "Survival Probability",
                  outer = TRUE, line = 3, cex.lab= 2)
            legend(0.85*1.02*max(dat$time), 1, legend = levels(factor(test$xvar)), 
                   lty = c(1:1), col=darkcols[1:length(levels(test$xvar))], cex = 1.5)
            abline(v=as.numeric(input$text2), lty = 2)
            par(op)
            
          } else {
            op <- par(mar=c(0.5, 0, 0.2, 0), mfrow=c(1,2),
                      oma = c(20, 5, 0.6, 0.2))
            layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
            plot(fit1, xaxs='i', xlim=c(0,as.numeric(input$text2)), ylim =c(0,1), fun = function(x) {1-x}, col=darkcols[1:length(levels(dat$xvar))], axes = F)
            text(0.45*as.numeric(input$text2), 1, paste0("Log-rank p-value: ", round(p.val1, 4)), cex = 1.5)
            axis(side = 1, cex.axis = 1.5, at=c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]),as.numeric(input$text2)))
            axis(side = 2)
            plot(fit2, xaxs='i', fun = function(x) {1-x}, col=darkcols[1:length(levels(test$xvar))], xlim=c(as.numeric(input$text2), 1.02*max(time)), ylim =c(0,1), axes = F)
            text(0.7*1.02*max(dat$time), 1, paste0("Log-rank p-value: ", round(p.val2, 4)), cex = 1.5)
            axis(side = 1, cex.axis=1.5, at=seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)]))
            title(xlab = xlabel[as.numeric(input$time3)],
                  ylab = "Cumulative Incidence Rate",
                  outer = TRUE, line = 3, cex.lab= 2)
            legend(0.85*1.02*max(dat$time), 0.1, legend = levels(factor(test$xvar)), 
                   lty = c(1:1), col=darkcols[1:length(levels(test$xvar))], cex = 1.5)
            abline(v=as.numeric(input$text2), lty = 2)
            par(op)
            
          }
          
        }
        
      } else if (class(xvar) %in% c("integer", "numeric")  & input$text2 != "" & input$binary3 == "continuous") {
        if (as.numeric(as.integer(input$cutoff4)) == 1) {
          perc <- optimalcut(dat)
        } else {perc <- as.numeric(as.integer(input$cutoff4))}
        
        dat$Group <- ifelse(dat[, 'xvar'] < quantile(dat[, 'xvar'], perc/100), "Low", "High")
        #Group <- as.factor(dat$Group)
        
        dat <- dat[, -3]
        
        test <- cutLM(dat, outcome=list(time="time", status="censor"),
                      LM=as.numeric(input$text2), horizon=max(time), covs=list(fixed=colnames(dat)[3]))
        fit1 <- survfit(Surv(time, censor) ~ Group, data = dat)
        fit2 <- survfit(Surv(time, censor) ~ Group, data = test)
        
        p1 <- survdiff(Surv(time, censor) ~ Group, data = dat)
        p2 <- survdiff(Surv(time, censor) ~ Group, data = test)
        p.val1 <-  1 - pchisq(p1$chisq, length(p1$n) - 1)
        p.val2 <-  1 - pchisq(p2$chisq, length(p2$n) - 1)
        darkcols <- c("blue", "red", "green", "black", "yellow", "orange", "pink", "purple", "grey")
        
        # Drawing curves
        xlabel <- c("Years", "Months", "Days")
        b <- c(0.25, 3, 90)
        pos1<-c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]), as.numeric(input$text2))
        NR1<-nrisk(fit1, times=pos1)
        NR1[, ncol(NR1)] <- rep(NA, nrow(NR1))
        
        pos2<-seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)])
        NR2<-nrisk(fit2, times=pos2)
        
        if (input$riskt2 == TRUE) {
          #TF <- c(TRUE, FALSE)
          if (input$option == TRUE) {
            op <- par(mar=c(0.5, 0, 0.2, 0), mfrow=c(1,2),
                      oma = c(20, 5, 0.6, 0.2))
            layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
            plot(fit1, col=darkcols[1:length(levels(factor(dat$Group)))], xlim=c(0,as.numeric(input$text2)), axes = F)
            text(0.45*as.numeric(input$text2), 1, paste0("Log-rank p-value: ", round(p.val1, 4)), cex = 1.5)
            axis(side = 1, cex.axis = 1.5, at=c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]),as.numeric(input$text2)))
            axis(side = 2)
            #for (n in 1: dim(NR1)[1]){text(x=pos1,y= 0,labels= NR1[n,],cex=1.5,pos=3, offset=(-0.8*n), col=darkcols[1:length(levels(test$Group))],font=3)}
            #text(x=0,y=0.01, label="Number at Risk", pos=4, offset=-0.5, cex=1.5, font=4)
            for (n in 1: dim(NR1)[1]) {mtext(text = NR1[n, ], side = 1, line = (6+2*n), outer = F, at = pos1, col=darkcols[1:length(levels(factor(test$Group)))][n])}
            mtext("Number at Risk", side = 1, line = 5, at = 0, outer = T)
            plot(fit2, col=darkcols[1:length(levels(factor(dat$Group)))], xlim=c(as.numeric(input$text2), 1.02*max(time)), axes = F)
            text(0.7*1.02*max(dat$time), 1, paste0("Log-rank p-value: ", round(p.val2, 4)), cex = 1.5)
            axis(side = 1, cex.axis=1.5, at=seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)]))
            for (n in 1: dim(NR2)[1]) {mtext(text = NR2[n, ], side = 1, line = (6+2*n), outer = F, at = pos2, col=darkcols[1:length(levels(factor(test$Group)))][n])}
            title(xlab = xlabel[as.numeric(input$time3)],
                  ylab = "Survival Probability",
                  outer = TRUE, line = 3, cex.lab= 2)
            legend(0.85*1.02*max(dat$time), 1, legend = levels(factor(test$Group)), 
                   lty = c(1:1), col=darkcols[1:length(levels(factor(test$Group)))], cex = 1.5)
            abline(v=as.numeric(input$text2), lty = 2)
            par(op)
            
          } else {
            op <- par(mar=c(0.5, 0, 0.2, 0), mfrow=c(1,2),
                      oma = c(20, 5, 0.6, 0.2))
            layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
            plot(fit1, xaxs='i', xlim=c(0,as.numeric(input$text2)), ylim =c(0,1), fun = function(x) {1-x}, col=darkcols[1:length(levels(factor(dat$Group)))], axes = F)
            text(0.45*as.numeric(input$text2), 1, paste0("Log-rank p-value: ", round(p.val1, 4)), cex = 1.5)
            axis(side = 1, cex.axis = 1.5, at=c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]),as.numeric(input$text2)))
            axis(side = 2)
            for (n in 1: dim(NR1)[1]) {mtext(text = NR1[n, ], side = 1, line = (6+2*n), outer = F, at = pos1, col=darkcols[1:length(levels(factor(test$Group)))][n])}
            mtext("Number at Risk", side = 1, line = 5, at = 0, outer = T)
            plot(fit2, xaxs='i', fun = function(x) {1-x}, col=darkcols[1:length(levels(factor(test$Group)))], xlim=c(as.numeric(input$text2), 1.02*max(time)), ylim =c(0,1), axes = F)
            text(0.7*1.02*max(dat$time), 1, paste0("Log-rank p-value: ", round(p.val2, 4)), cex = 1.5)
            axis(side = 1, cex.axis=1.5, at=seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)]))
            for (n in 1: dim(NR2)[1]) {mtext(text = NR2[n, ], side = 1, line = (6+2*n), outer = F, at = pos2, col=darkcols[1:length(levels(factor(test$Group)))][n])}
            title(xlab = xlabel[as.numeric(input$time3)],
                  ylab = "Cumulative Incidence Rate",
                  outer = TRUE, line = 3, cex.lab= 2)
            legend(0.85*1.02*max(dat$time), 0.1, legend = levels(factor(test$Group)), 
                   lty = c(1:1), col=darkcols[1:length(levels(factor(test$Group)))], cex = 1.5)
            abline(v=as.numeric(input$text2), lty = 2)
            par(op)
            
          }
          
        } else {
          #TF <- c(TRUE, FALSE)
          if (input$option == TRUE) {
            op <- par(mar=c(0.5, 0, 0.2, 0), mfrow=c(1,2),
                      oma = c(20, 5, 0.6, 0.2))
            layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
            plot(fit1, col=darkcols[1:length(levels(factor(dat$Group)))], xlim=c(0,as.numeric(input$text2)), axes = F)
            text(0.45*as.numeric(input$text2), 1, paste0("Log-rank p-value: ", round(p.val1, 4)), cex = 1.5)
            axis(side = 1, cex.axis = 1.5, at=c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]),as.numeric(input$text2)))
            axis(side = 2)
            #for (n in 1: dim(NR1)[1]){text(x=pos1,y= 0,labels= NR1[n,],cex=1.5,pos=3, offset=(-0.8*n), col=darkcols[1:length(levels(test$Group))],font=3)}
            #text(x=0,y=0.01, label="Number at Risk", pos=4, offset=-0.5, cex=1.5, font=4)
            plot(fit2, col=darkcols[1:length(levels(factor(dat$Group)))], xlim=c(as.numeric(input$text2), 1.02*max(time)), axes = F)
            text(0.7*1.02*max(dat$time), 1, paste0("Log-rank p-value: ", round(p.val2, 4)), cex = 1.5)
            axis(side = 1, cex.axis=1.5, at=seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)]))
            title(xlab = xlabel[as.numeric(input$time3)],
                  ylab = "Survival Probability",
                  outer = TRUE, line = 3, cex.lab= 2)
            legend(0.85*1.02*max(dat$time), 1, legend = levels(factor(test$Group)), 
                   lty = c(1:1), col=darkcols[1:length(levels(factor(test$Group)))], cex = 1.5)
            abline(v=as.numeric(input$text2), lty = 2)
            par(op)
            
          } else {
            op <- par(mar=c(0.5, 0, 0.2, 0), mfrow=c(1,2),
                      oma = c(20, 5, 0.6, 0.2))
            layout(matrix(c(1,2,2), 1, 3, byrow = TRUE))
            plot(fit1, xaxs='i', xlim=c(0,as.numeric(input$text2)), ylim =c(0,1), fun = function(x) {1-x}, col=darkcols[1:length(levels(factor(dat$Group)))], axes = F)
            text(0.45*as.numeric(input$text2), 1, paste0("Log-rank p-value: ", round(p.val1, 4)), cex = 1.5)
            axis(side = 1, cex.axis = 1.5, at=c(seq(0,as.numeric(input$text2), b[as.numeric(input$time3)]),as.numeric(input$text2)))
            axis(side = 2)
            plot(fit2, xaxs='i', fun = function(x) {1-x}, col=darkcols[1:length(levels(factor(test$Group)))], xlim=c(as.numeric(input$text2), 1.02*max(time)), ylim =c(0,1), axes = F)
            text(0.7*1.02*max(dat$time), 1, paste0("Log-rank p-value: ", round(p.val2, 4)), cex = 1.5)
            axis(side = 1, cex.axis=1.5, at=seq(as.numeric(input$text2), max(time), b[as.numeric(input$time3)]))
            title(xlab = xlabel[as.numeric(input$time3)],
                  ylab = "Cumulative Incidence Rate",
                  outer = TRUE, line = 3, cex.lab= 2)
            legend(0.85*1.02*max(dat$time), 0.1, legend = levels(factor(test$Group)), 
                   lty = c(1:1), col=darkcols[1:length(levels(factor(test$Group)))], cex = 1.5)
            abline(v=as.numeric(input$text2), lty = 2)
            par(op)
            
          }
          
        }
        
      } else {
        return(NULL)
      }
      dev.off()
      
      file.copy(paste(pdf_file,'.pdf', sep='') ,file, overwrite=TRUE)
    })

    data_input5 <- reactive({
    if(input$file5 == 'Example5'){
      d5 <- read.csv("data/BRCA_for_quantile_survival_analysis.csv", header =T, sep =",")
    }
    else if(input$file5 == 'load_my_own5'){
      inFile <- input$file51
      if (is.null(inFile))
        return(NULL)
      else if(grepl(".xlsx", inFile[1])) { d5 = read.xlsx(as.character(inFile$datapath), colNames = TRUE, rowNames = F, as.is = T) }
      else if(grepl(".csv", inFile[1])) { d5 = read.csv(as.character(inFile$datapath), header = TRUE, sep = ",", stringsAsFactors = F, as.is = T, fill = T) }
      else if(grepl(".txt", inFile[1])) { d5 = read.table(as.character(inFile$datapath), header = TRUE, sep = "\t", stringsAsFactors = F, as.is = T, fill = T) }
    }
    else 
      return(NULL)
    # dim(data)
    Dataset5 <- data.frame(d5)
    return(as.data.frame(Dataset5))
  })
  
  output$downloadEx5 <- downloadHandler(
    
    filename <- function() {
      paste('Example ds cutpoint PC', Sys.time(),'.csv', sep='')
    },
    content <- function(file) {
      ds5 <- data_input5()
      write.csv(ds5, file, row.names = FALSE)
    }
  )
  
  output$MRplot <- renderPlot({
    data5 <- data_input5()
    if(!is.null(data5)){
    xvar <- data5[, input$select51]
    time <- data5[,input$select52]
    censor <- data5[,input$select53]
    
    dat <- cbind.data.frame(time, censor, xvar)
    dat <- as.data.frame(dat[!is.na(dat$time),])

    
    optimalcut_plot(dat, input$select51)
    }else
    return(NULL)
  })
  
  output$Optimal <- DT::renderDataTable({
    data5 <- data_input5()
    if(!is.null(data5)){
    xvar <- data5[, input$select51]
    time <- data5[,input$select52]
    censor <- data5[,input$select53]
    
    dat <- cbind.data.frame(time, censor, xvar)
    dat <- as.data.frame(dat[!is.na(dat$time),])
    
    DT::datatable(optimalcut_table(dat, input$select51), options = list(
      lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')),
      pageLength = 10))
    }
  })
  
  output$pv51 <- renderUI({
    hs1 <- paste("&emsp;")
    hs2 <- paste("Optimal Cut Point Output")
    HTML(paste(h2(strong(hs2)), hs1, sep = '<br/>'))
  })
  
  output$downloadMR <- downloadHandler(
    filename <- function() {
      pdf_file <<- as.character(input$fname51)
      paste('MR_', pdf_file, Sys.time(),'.pdf', sep='')
    },
    content <- function(file) {
      pdf(file=paste(pdf_file,".pdf",sep="") , height= 8, width=12)
      data5 <- data_input5()
      xvar <- data5[, input$select51]
      time <- data5[,input$select52]
      censor <- data5[,input$select53]
      
      dat <- cbind.data.frame(time, censor, xvar)
      dat <- as.data.frame(dat[!is.na(dat$time),])
      
      optimalcut_plot(dat, input$select51)    

      dev.off()
      file.copy(paste(pdf_file,'.pdf', sep='') ,file, overwrite=TRUE)
    })  
  
  data_input6 <- reactive({
    if(input$file6 == 'Example6'){
      d6 <- read.csv("data/heatmap.csv", header =T, sep =",")
    }
    else if(input$file6 == 'load_my_own6'){
      inFile <- input$file61
      if (is.null(inFile))
        return(NULL)
      else if(grepl(".xlsx", inFile[1])) { d6 = read.xlsx(as.character(inFile$datapath), colNames = TRUE, rowNames = F, as.is = T) }
      else if(grepl(".csv", inFile[1])) { d6 = read.csv(as.character(inFile$datapath), header = TRUE, sep = ",", stringsAsFactors = F, as.is = T, fill = T) }
      else if(grepl(".txt", inFile[1])) { d6 = read.table(as.character(inFile$datapath), header = TRUE, sep = "\t", stringsAsFactors = F, as.is = T, fill = T) }
    }
    else 
      return(NULL)
    # dim(data)
    Dataset6 <- data.frame(d6)
    return(as.data.frame(Dataset6))
  })
  
  output$downloadEx6 <- downloadHandler(
    
    filename <- function() {
      paste('Example ds grid PC', Sys.time(),'.csv', sep='')
    },
    content <- function(file) {
      ds6 <- data_input6()
      write.csv(ds6, file, row.names = FALSE)
    }
  )
  
  output$heatmapplot <- renderPlot({
    data6 <- data_input6()
    
    hm_p <- data.matrix(data6[, -1])
    class(hm_p) <- "numeric"
    rownames(hm_p) <- data6[, 1]
    colnames(hm_p) <- colnames(data6[, -1])
    hm_p[, ncol(hm_p)] <- 3
    
    hm_p_values <- cbind(matrix(NA, nrow(hm_p), (ncol(hm_p)-1)), data6[, ncol(data6)])
    colnames(hm_p_values) <- colnames(data6[, -1])
    
    par(oma=c(2,1,1,0), mar=c(0, 0, 0, 2), xpd=TRUE, cex.main=0.9)
    r <- c("High", "Low")
    p <- heatmap.2(hm_p, cellnote = hm_p_values, notecol = "black", Rowv=F, Colv=FALSE, scale="none", dendrogram="none", key = F,
                   trace="none", col = c("grey", "blue", "red", "white"), xlab="", ylab="", margins=c(5,13), main = "")
    legend("topleft", inset=c(0,0.15), cex = 0.8, legend=c("Non-significant", paste0(r[r != input$exp_ref2], " Expression Better Survival"), paste0(input$exp_ref2, " Expression Better Survival"), "Non-estimable"), fill=c("grey", "red", "blue", "white"), title="Color Key")
    
  })
  
  output$downloadHM <- downloadHandler(
    filename <- function() {
      pdf_file <<- as.character(input$fname61)
      paste('HM_', pdf_file, Sys.time(),'.pdf', sep='')
    },
    content <- function(file) {
      pdf(file=paste(pdf_file,".pdf",sep="") , height= 8, width=12)
      data6 <- data_input6()
      
      hm_p <- data.matrix(data6[, -1])
      class(hm_p) <- "numeric"
      rownames(hm_p) <- data6[, 1]
      colnames(hm_p) <- colnames(data6[, -1])
      hm_p[, ncol(hm_p)] <- 3
      
      hm_p_values <- cbind(matrix(NA, nrow(hm_p), (ncol(hm_p)-1)), data6[, ncol(data6)])
      colnames(hm_p_values) <- colnames(data6[, -1])
      
      par(oma=c(2,1,1,0), mar=c(0, 0, 0, 2), xpd=TRUE, cex.main=0.9)
      r <- c("High", "Low")
      p <- heatmap.2(hm_p, cellnote = hm_p_values, notecol = "black", Rowv=F, Colv=FALSE, scale="none", dendrogram="none", key = F,
                     trace="none", col = c("grey", "blue", "red", "white"), xlab="", ylab="", margins=c(5,13), main = "")
      legend("topleft", inset=c(0,0.15), cex = 0.8, legend=c("Non-significant", paste0(r[r != input$exp_ref2], " Expression Better Survival"), paste0(input$exp_ref2, " Expression Better Survival"), "Non-estimable"), fill=c("grey", "red", "blue", "white"), title="Color Key")
      
      dev.off()
      file.copy(paste(pdf_file,'.pdf', sep='') ,file, overwrite=TRUE)
    })
  
  output$pv61 <- renderUI({
    hs1 <- paste("&emsp;")
    hs2 <- paste("Summarized Significance conclusion over all cancer types")
    HTML(paste(h2(strong(hs2)), hs1, sep = '<br/>'))
  })
  
  output$ReadMe2 <- renderUI({
    str0 <- paste("&emsp;")
    str1 <- paste("DATA FORMAT")
    str2 <- paste("&emsp;               1.            Data should be input as a .txt or .xlsx or .csv file. The first row of the data file have information about the variables.")
    str3 <- paste("&emsp;               2.            The remaining lines contain measurements one line per each subject/sample, described in the format below.")
    str4 <- paste("&emsp;               3.            The first column of the file contains the survival time 'time' (column 1), survival status 'status' (column 2) followed by the other variables of interest.")
    str6 <- paste("&emsp;&emsp;  a)   Column_1. This should contain the survival time, for the user's reference.")
    str7 <- paste("&emsp;&emsp;  b)   Column_2. This should contain the Survival status, input as 'censored' vs 'dead'.")
    str8 <- paste("&emsp;&emsp;  c)   Remaining Columns. These columns should contain information with variables of interest, such as age, race, gender and patient id kept as reference." )
    str9 <- paste("NOTE1: In this tab, you are able to carry out standard survival analysis. On the left side panel, you have the options to upload data; select variables for univariate
                   survival association analysis with cox proportional hazard model; meanwhile, select the variables for Kaplan Meier analysis; download the output table and plot accordingly.")
    str10 <- paste("NOTE2: To carry out univariate survival association analysis, it is based on cox proportional hazards model with entering one variable in the model each time. Select the variables of interest to generate the output table. 
                  You can choose the option to be 'Yes' if you want to test the proportional hazard assumption using Schoenfeld residuals test.")
    str11 <- paste("NOTE3: To carry out Kaplan Meier Analysis, the variable of interest can be All Patients or a categorical variable. You also need to specify the time unit corresponding to the data in order for the plot to display correctly.")
    HTML(paste(str0, strong(str9), str0,str0,str0,strong((str10)), str0,str0,str0,strong((str11)), str0, str0,h5(strong(str1)), str0, str2, str3, str4, str6, str7,str8, str0, sep = '<br/>'))
  })
  
  output$ReadMe1 <- renderUI({
    str0 <- paste("&emsp;")
    str1 <- paste("DATA FORMAT")
    str2 <- paste("&emsp;               1.            Data should be input as a .txt or .xlsx or .csv file. The first row of the data file have information about the variables.")
    str3 <- paste("&emsp;               2.            The remaining lines contain measurements one line per each subject/sample, described in the format below.")
    str4 <- paste("&emsp;               3.            The first column of the file contains sample id, the second column includes the survival status 'os_censor' (column 2), survival time 'os_time' (column 3) followed by the other variables of interest.")
    str6 <- paste("&emsp;&emsp;  a)   Column_1. This should contain the Survival status, input as 0 for 'censored' vs 1 for 'dead'.")
    str7 <- paste("&emsp;&emsp;  b)   Column_2. This should contain the survival time, for the user's reference.")
    str8 <- paste("&emsp;&emsp;  c)   Remaining Columns. These columns should contain information with variables of interest, such as age, race, gender and patient id kept as reference." )
    str9 <- paste("NOTE1: In this tab, you are able to carry out quantile survival analysis. The quantile survival analysis in this shiny app is based on the method developed by Huang (2010 & 2016).
                    For each quantile, the survival time difference will estimated with 95% CI. If it included 0, then that quantile is not significant. Otherwise, it is significant in that quantile.")
    str10 <- paste("NOTE2: On the left side panel, you have the options to upload data; select a continuous variable of interest to be dichotomized with optimal cutoff, 25 percentile, 
                  50 percentile, 75 percentile; the option to choose reference level needs to be specified by the user with respect to interest; download the output table and plot accordingly.")
    str11 <- paste("NOTE3: Once you click the Run & Generate a random number button, the app will start to run the quantile survival analysis. You can then check the output plots and tables in the second subtab.")
    HTML(paste(str0, strong(str9), str0,str0,str0,strong((str10)), str0,str0,str0,strong((str11)), str0, str0,h5(strong(str1)), str0, str2, str3, str4, str6, str7,str8, str0, sep = '<br/>'))
  })
  
  output$ReadMe3 <- renderUI({
    str0 <- paste("&emsp;")
    str1 <- paste("DATA FORMAT")
    str2 <- paste("&emsp;               1.            Data should be input as a .txt or .xlsx or .csv file. The first row of the data file have information about the variables.")
    str3 <- paste("&emsp;               2.            The remaining lines contain measurements one line per each subject/sample, described in the format below.")
    str4 <- paste("&emsp;               3.            The first column contains survival time, the second column includes the survival status, followed by the other variables of interest.")
    str6 <- paste("&emsp;&emsp;  a)   Column_1. This should contain the Survival time, for the user's reference.")
    str7 <- paste("&emsp;&emsp;  b)   Column_2. This should contain the survival status, input as 0 for 'censored' vs 1 for 'dead'.")
    str8 <- paste("&emsp;&emsp;  c)   Remaining Columns. These columns should contain information with variables of interest, such as age, race, gender and patient id kept as reference." )
    str9 <- paste("NOTE1: In this tab, you are able to carry out a competing risk survival analysis. The method is based on 
                  Fine and Gray's Model. On the left side panel, you have the options to upload data; select variables for univariate
                   survival association analysis with Fine and Gray model; meanwhile, select the variables for cumulative incidence function analysis; download the output table and plot accordingly.")
    str10 <- paste("NOTE2: For Univariate analysis, choose variables of interest to enter the analysis. Meanwhile, you need to specify event code and censor code.")
    str11 <- paste("NOTE3: To generate a CIF plot and table, choose a categorical variable to compare or All Patient for overall. You can select the correct time unit based on the data.
                   Also, you can choose to input time points of interest or not. Censor code is also need to be specified.")
    HTML(paste(str0, strong(str9), str0,str0,strong((str10)), str0,str0,strong((str11)), str0, str0,h5(strong(str1)), str0, str2, str3, str4, str6, str7,str8, str0, sep = '<br/>'))
  })

  output$ReadMe4 <- renderUI({
    str0 <- paste("&emsp;")
    str1 <- paste("DATA FORMAT")
    str2 <- paste("&emsp;               1.            Data should be input as a .txt or .xlsx or .csv file. The first row of the data file have information about the variables.")
    str3 <- paste("&emsp;               2.            The remaining lines contain measurements one line per each subject/sample, described in the format below.")
    str4 <- paste("&emsp;               3.            The first column of the file contains the survival time 'time' (column 1), survival status 'status' (column 2) followed by the other variables of interest.")
    str6 <- paste("&emsp;&emsp;  a)   Column_1. This should contain the survival time, for the user's reference.")
    str7 <- paste("&emsp;&emsp;  b)   Column_2. This should contain the Survival status, input as 'censored' vs 'dead'.")
    str8 <- paste("&emsp;&emsp;  c)   Remaining Columns. These columns should contain information with variables of interest, such as age, race, gender and patient id kept as reference." )
    str9 <- paste("NOTE1: In this tab, you are able to carry out landmark survival analysis. On the left side panel, you have the options to upload data; select variables for landmark analysis; you also need to specify the value for landmark analysis; download the output plot accordingly.")
    str10 <- paste("NOTE2: The analysis is based on the function from R package 'dynpred' to generate the landmark dataset.")
    str11 <- paste("NOTE3: You also need to specify the time unit corresponding to the data in order for the plot to display correctly. You can choose to get KM curves or CIF curves per research interest.")
    HTML(paste(str0, strong(str9), str0,str0,str0,strong((str10)), str0,str0,str0,strong((str11)), str0, str0,h5(strong(str1)), str0, str2, str3, str4, str6, str7,str8, str0, sep = '<br/>'))
  })
  
  output$ReadMe5 <- renderUI({
    str0 <- paste("&emsp;")
    str1 <- paste("DATA FORMAT")
    str2 <- paste("&emsp;               1.            Data should be input as a .txt or .xlsx or .csv file. The first row of the data file have information about the variables.")
    str3 <- paste("&emsp;               2.            The remaining lines contain measurements one line per each subject/sample, described in the format below.")
    str4 <- paste("&emsp;               3.            The first column of the file contains sample id, the second column includes the survival status 'os_censor' (column 2), survival time 'os_time' (column 3) followed by the other variables of interest.")
    str6 <- paste("&emsp;&emsp;  a)   Column_1. This should contain the Survival status, input as 0 for 'censored' vs 1 for 'dead'.")
    str7 <- paste("&emsp;&emsp;  b)   Column_2. This should contain the survival time, for the user's reference.")
    str8 <- paste("&emsp;&emsp;  c)   Remaining Columns. These columns should contain information with variables of interest, such as age, race, gender and patient id kept as reference." )
    str9 <- paste("NOTE1: In this tab, you are able to search for optimal cutpoint for a continuous variable of interest. The method is based on 
                  Contal and O'Quigley (1999) and has also been implemented in SAS by Mandrekar et al. (2003).")
    str10 <- paste("NOTE2: In the output, a martingale residual plot is included. In the following table, you can find the cutpoint value of the variable and corresponding percentile.")
    HTML(paste(str0, strong(str9), str0,str0,str0,strong((str10)), str0, str0,h5(strong(str1)), str0, str2, str3, str4, str6, str7,str8, str0, sep = '<br/>'))
  })
  
  output$ReadMe6 <- renderUI({
    str0 <- paste("&emsp;")
    str1 <- paste("DATA FORMAT")
    str2 <- paste("&emsp;               1.            Data should be input as a .txt or .xlsx or .csv file. The first row of the data file have information about the variables.")
    str3 <- paste("&emsp;               2.            The remaining lines contain measurements one line per each cancer type or subgroup, described in the format below.")
    str4 <- paste("&emsp;               3.            The data is designed to have 13 columns.")
    str6 <- paste("&emsp;&emsp;  a)   Column_1. The column contains names of different cancer types or subgroups.")
    str7 <- paste("&emsp;&emsp;  b)   Column_13. This should contain the percentile for each cutpoint you used in quantile survival analysis.")
    str8 <- paste("&emsp;&emsp;  c)   Column_2 to Column 12. from 2nd to 12th column, the significance indications are included for 10 quantiles and overall." )
    str9 <- paste("NOTE1: In this tab, you are able to generate a grid if you have multiple cancer or groups of interest to run multiple quantile survival analysis separately.")
    str10 <- paste("NOTE2: On the left side panel, you have the options to upload data; select the reference level corresponding to the reference level chosen in quantile survival analysis;
                     download the output plot accordingly.")
    str11 <- paste("NOTE3: To generate the input data, you can refer to the 'Quantile Survival Analysis' tab for more information.")
    HTML(paste(str0, strong(str9), str0,str0,str0,strong((str10)), str0,str0,str0,strong((str11)), str0, str0,h5(strong(str1)), str0, str2, str3, str4, str6, str7,str8, str0, sep = '<br/>'))
  })
  
  
}

shinyApp(ui= ui, server= server)  
