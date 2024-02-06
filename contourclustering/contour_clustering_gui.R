# General information:
# This script runs as Shiny app and is able to take time-series f0 measurements and perform hierarchical cluster analysis on f0 contours. Measurements can alternatively be taken using the Praat script "time-series_F0.praat".
# The code below is mostly uncommented. Additional mouse-hoover help comments are provided for some settings in the app. Detailed script comments can be found in the no-gui version of this script 'contour_clustering.R', which has essentially the same functionality.
# A theoretical motivation with illustrated examples is given in the accompanying paper: https://doi.org/10.1017/S0025100321000049.
# Usage guidelines are given in the accompanying manual: https://constantijnkaland.github.io/contourclustering/#documentation.
#
# Developed and tested using R/package versions:
# R version 4.1.2 (2021-11-01)
# RStudio 2023.5.0.366
# data.table 1.14.8
# dplyr 1.1.3
# dtwclust 5.5.12
# ggdendro 0.1.23
# ggplot2 3.4.3
# graphics 4.1.2
# Hmisc 5.1.1
# Metrics 0.1.4
# pracma 2.4.2
# proxy 0.4.27
# purrr 1.0.2
# readr 2.1.4
# readtextgrid 0.1.1
# scales 1.2.1
# shiny 1.7.5
# stats 4.1.2
# stringr 1.5.0
# TSdist 3.7.1
# usedist 0.4.0
# utils 4.1.2
# wrassp 1.0.4
# zoo 1.8.12
# 
# Constantijn Kaland, February 2024.
# https://constantijnkaland.github.io/contourclustering/

# install/load required packages automatically ####
packages <- c("data.table", "dplyr", "dtwclust", "ggdendro", "ggplot2", "graphics", 
              "Hmisc", "Metrics", "pracma", "proxy", "purrr", "readr", "readtextgrid", 
              "scales", "shiny", "stats", "stringr", "TSdist", "usedist", "utils", 
              "wrassp", "zoo")

# version list
# library(rstudioapi)
# version$version.string -> vlist
# append(vlist,paste0("RStudio ", versionInfo()$version)) -> vlist
# for (p in sort(packages)){
# append(vlist,paste(p,packageVersion(p))) -> vlist
# }
# writeLines(c("Developed and tested using R/package versions:",
#              vlist,
#              "",
#              paste0("Constantijn Kaland, ", strftime(file.info(getActiveDocumentContext()$path)$mtime, "%B %Y"),"."),
#              "https://constantijnkaland.github.io/contourclustering/"))

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

suppressPackageStartupMessages(invisible(lapply(packages, library, character.only = TRUE)))

options(shiny.maxRequestSize = 20 * 1024 ^ 2, warn = -1, error = NULL)


# ui and server ####

ui <- fluidPage(
  tags$style(".shiny-file-input-progress {display: none}"),
  tags$style(HTML("
    .irs--shiny .irs-bar {
      background: linear-gradient(to bottom, #dedede -50%, #fff 150%);
      border-top: 1px solid #cccccc;
      border-bottom: 1px solid #cccccc;
    }
    .irs--shiny .irs-to, .irs--shiny .irs-from, .irs--shiny .irs-single {
      background-color: rgba(0, 0, 0, 0.1);
      color: #ff2b2b;
    }")),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(width = 5, align = "center", uiOutput("tsfz")),
        column(width = 5, align = "center", uiOutput("cc"))
      ),
      uiOutput("dirdata"),
      fluidRow(
        column(width = 5, align = "center", uiOutput("snd_ext")),
        column(width = 6, align = "center", uiOutput("read_grids"))
      ),
      uiOutput("sepchoice"),
      uiOutput("strfact"),
      uiOutput("filenc"),
      uiOutput("skpnl"),
      textOutput("choose"),
      uiOutput("rem_empty"),
      uiOutput("decl_corr"),
      uiOutput("f0_header"),
      uiOutput("f0_conv"),
      uiOutput("jump_header"),
      uiOutput("jump_margin"),
      uiOutput("spkdiff_header"),
      uiOutput("spkdiff"),
      fluidRow(
        column(
          width = 5,
          align = "left",
          uiOutput("dfcols_header")
        ),
        column(
          width = 5,
          align = "left",
          uiOutput("stdcols_header")
        )
      ),
      fluidRow(
        column(width = 5, align = "left", uiOutput("dfcols")),
        column(width = 5, align = "left", uiOutput("stdcols")),
        column(width = 2, align = "center", uiOutput("confcols"))
      ),
      fluidRow(
        column(width = 6, align = "center", uiOutput("include_dur")),
        column(width = 6, align = "center", uiOutput("include_int"))
      ),
      uiOutput("prep_data"),
      tags$hr(),
      uiOutput("select_tier"),
      fluidRow(
        column(width = 4, align = "left", uiOutput("pmin")),
        column(width = 4, align = "left", uiOutput("pmax")),
        column(width = 4, align = "left", uiOutput("timestep"))
      ),
      fluidRow(
        column(width = 4, align = "left", uiOutput("f0fit")),
        column(width = 4, align = "left", uiOutput("npoints")),
        column(width = 4, align = "left", uiOutput("smooth_bw"))
      ),
      uiOutput("select_int"),
      
      uiOutput("nclust"),
      fluidRow(
        column(width = 6, align = "center", uiOutput("distm")),
        column(width = 6, align = "center", uiOutput("sellink"))
      ),
      fluidRow(
        column(width = 5, align = "center", uiOutput("getdendro")),
        column(width = 4, align = "center", uiOutput("gettab")),
        column(width = 3, align = "center", uiOutput("getplot"))
      ),
      tags$hr(),
      textOutput("suggest"),
      
      uiOutput("subset"),
      uiOutput("dosubset"),
      tags$hr(),
      fluidRow(
        column(
          width = 3,
          align = "left",
          uiOutput("select_sample")
        ),
        column(width = 2, align = "right", uiOutput("n_sample")),
        column(width = 5, align = "left", textOutput("txt_sample")),
        column(width = 2, align = "center", uiOutput("do_sample"))
      ),
      fluidRow(column(
        width = 12, align = "center", uiOutput("tocc")
      )),
      
      fluidRow(
        column(width = 3, align = "center", uiOutput("goevaluate")),
        column(width = 5, align = "center", uiOutput("savecurrent")),
        column(width = 4, align = "center", uiOutput("textgrid"))
      ),
      tags$hr(),
      HTML(
        '<center><a href="https://constantijnkaland.github.io/contourclustering/"><img src="https://constantijnkaland.github.io/contourclustering/logo.png" width="107" height="61"></a></center>'
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "outputs",
        type = "tabs",
        tabPanel(value = "summary", 'Status', span(
          HTML('<br>'), verbatimTextOutput("summary")
        )),
        tabPanel("Sample", span(HTML('<br>'), uiOutput("sample"))),
        tabPanel("Data (long)", span(HTML('<br>'), tableOutput("data_long"))),
        tabPanel("Dendrogram", span(HTML('<br>'), plotOutput("dendro"))),
        tabPanel("Table", span(HTML('<br>'), tableOutput("table"))),
        tabPanel("Plot", span(HTML('<br>'), plotOutput("plot"))),
        tabPanel("Evaluate", span(HTML('<br>'), uiOutput("evaluate"))),
        tabPanel("Data (wide)", span(HTML('<br>'), tableOutput("data_wide"))),
        tabPanel("Manual", span(HTML('<br>'), uiOutput("manual")))
      )
      
    )
  )
)


server <- function(input, output, session) {
  if (dir.exists("www") == F) {
    0 ->> oldwww
    dir.create("www")
  } else {
    1 ->> oldwww
    showNotification("Existing directory 'www' found. Remove this directory before starting this app.",type = "error")
    Sys.sleep(5)
    cat("Existing directory 'www' found. Remove this directory before starting this app.\n")
    stopApp()
  }
  hideTab(inputId = "outputs", target = "Data (long)")
  hideTab(inputId = "outputs", target = "Sample")
  hideTab(inputId = "outputs", target = "Dendrogram")
  hideTab(inputId = "outputs", target = "Table")
  hideTab(inputId = "outputs", target = "Plot")
  hideTab(inputId = "outputs", target = "Evaluate")
  hideTab(inputId = "outputs", target = "Data (wide)")
  
  # outputs ####
  
  output$manual <- renderUI({
    fluidPage(align = "center",
              tags$iframe(style="height:90vh; width:100%", src="https://constantijnkaland.github.io/contourclustering/manual.pdf", loading="lazy"))
  })
  
  output$tsfz <- renderUI({
    tags$div(title = "Acoustic analysis.",
             actionButton("tsfz", "Time-series f0"))
  })
  
  output$cc <- renderUI({
    tags$div(title = "Contour clustering.",
             actionButton("cc", "Contour clustering"))
  })
  
  output$choose <- reactive({
    if (is.null(input$file_input))
    {
      "! Before uploading: select correct file properties above."
    }
    if (is.null(input$file_input) == F)
    {
      inFile <- input$file_input
      if (input$sepchoice == 1) {
        sep = ","
      }
      if (input$sepchoice == 2) {
        sep = "\t"
      }
      if (input$strfact == 1) {
        saf = T
      }
      if (input$strfact == 2) {
        saf = F
      }
      if (input$filenc == 1) {
        fenc = "UTF-8"
      }
      if (input$filenc == 2) {
        fenc = "utf16"
      }
      if (input$skpnl == 1) {
        snl = T
      }
      if (input$skpnl == 2) {
        snl = F
      }
      data <-
        as.data.frame(
          fread(
            inFile$datapath,
            sep = sep,
            header = T,
            stringsAsFactors = saf,
            encoding = fenc,
            blank.lines.skip = snl
          )
        )
      if("dB" %in% colnames(data)){
        int_available <<- 1
      }
      nrow(data) ->> nrowinput
      clnms <- colnames(data)
      clnrs <- c(1:length(clnms))
      colsdf <<- setNames(as.list(as.numeric(clnrs)), c(clnms))
      fwrite(data, file.path("www", "data_long.csv"))
      updateTabsetPanel(session, inputId = "outputs", selected = "summary")
      ifelse(
        input$sepchoice == 1,
        sep <<-
          "column separator = comma, ",
        sep <<- "column separator = tab, "
      )
      ifelse(input$strfact == 1,
             saf <<-
               "strings as factors = T, ",
             saf <<- "strings as factors = F, ")
      ifelse(
        input$filenc == 1,
        enc <<-
          "file-encoding = utf-8, ",
        enc <<- "file-encoding = utf-16, "
      )
      ifelse(input$skpnl == 1,
             skn <<- "skip nulls = T.",
             skn <<- "skip nulls = F.")
      paste0("")
    } else{
      return(NULL)
    }
  })
  
  output$rem_empty <- renderUI({
    if (is.null(input$file_input))
      return(NULL)
    tags$div(title = "Clean missing interval labels and f0 values. Also removes contours with a rate of f0 change beyond the ones in Xu and Sun (2002, Table X; 72 ST/s for rises and 96 ST/s for falls). Only apply cleaning on f0 values in Hertz. ",
             checkboxInput("rem_empty", "clean data (remove NA and f0 errors)", FALSE))
  })
  
  output$decl_corr <- renderUI({
    if (is.null(input$file_input))
      return(NULL)
    tags$div(title = "Avoid flat contours by applying a declination effect ('t Hart et al., 1991, p.129): D = (-11)/(t+1.5). Enables Pearson/auto-correlation distance measures. Only applied to flat contours.",
             checkboxInput("decl_corr", "forced declination", FALSE))
  })
  
  output$f0_header <- renderUI({
    if (is.null(input$file_input))
      return(NULL)
    "f0 scale:"
  })
  
  output$f0_conv <- renderUI({
    if (is.null(input$file_input))
      return(NULL)
    tags$div(
      title = "Default assumption: f0 is measured in Hertz (Hz). Convert to semitones re 50 Hz: f0(ST) = log10((f0(Hz)/50)*39.87 or convert to Equivalent Rectangular Bandwidth (ERB): f0(ERB) = 16.6*log10(1+(f0(Hz)/165.4))" ,
      selectInput("f0_conv", label = NULL,choices = list("Hz (no conversion)" = "Hz", "ST" = "ST", "ERB" = "ERB"),selected = 1,multiple = F, width = "40%")
    )
  })
  
  output$jump_header <- renderUI({
    if (is.null(input$file_input))
      return(NULL)
    "Allowed % change after octave jump correction:"
  })
  
  output$jump_margin <- renderUI({
    if (is.null(input$file_input))
      return(NULL)
    numericInput(
      "jump_margin",
      label = NULL,
      value = 10,
      min = 0,
      max = 100,
      width = '20%'
    )
  })
  
  output$spkdiff_header <- renderUI({
    if (is.null(input$file_input))
      return(NULL)
    "Correct speaker differences:"
  })
  
  output$spkdiff <- renderUI({
    if (is.null(input$file_input))
      return(NULL)
    tags$div(
      title = "Only apply when number of speakers is correctly detected. 1 = none, 2 = f0–mean(f0.spk), 3 = f0–mean(f0.spk) / sd(f0.spk), 4 = f0–min(f0.spk) / max(f0.spk)–min(f0.spk), 5 = f0–median(f0.spk) / 75q(f0.spk)–25q(f0.spk), 6 = log2(f0.spk/media(f0.spk)), 7 = Δf0/Δtime",
      selectInput(
        "spkdiff",
        label = NULL,
        choices = list(
          "1: none" = 1,
          "2: subtract mean" = 2,
          "3: standardise" = 3,
          "4: min-max normalize" = 4,
          "5: robust scaler" = 5,
          "6: octave-median scaled" = 6,
          "7: first derivative (d1)" = 7
        ),
        selected = 1,
        multiple = F,
        width = "50%"
      )
    )
  })
  
  output$dfcols_header <- renderUI({
    if (is.null(input$file_input) |
        all(
          names(colsdf) == c(
            "filename",
            "interval_label",
            "start",
            "end",
            "step",
            "stepnumber",
            "f0",
            "jumpkilleffect"
          )
        )) {
      return(NULL)
    } else{
      "Rename (now in data)"
    }
  })
  
  output$dfcols <- renderUI({
    if (is.null(input$file_input) |
        all(
          names(colsdf) == c(
            "filename",
            "interval_label",
            "start",
            "end",
            "step",
            "stepnumber",
            "f0",
            "jumpkilleffect"
          )
        ))
    return(NULL)
    tags$div(
      title = "These columns are now detected to be in the loaded data (no need to remap if they match the required columns)",
      selectInput(
        "dfcols",
        label = NULL,
        choices = colsdf,
        selected = 1,
        multiple = F,
        width = "80%"
      )
    )
  })
  
  output$stdcols_header <- renderUI({
    if (is.null(input$file_input) |
        all(
          names(colsdf) == c(
            "filename",
            "interval_label",
            "start",
            "end",
            "step",
            "stepnumber",
            "f0",
            "jumpkilleffect"
          )
        ))
    return(NULL)
    "into (required)"
  })
  
  output$stdcols <- renderUI({
    if (is.null(input$file_input) |
        all(
          names(colsdf) == c(
            "filename",
            "interval_label",
            "start",
            "end",
            "step",
            "stepnumber",
            "f0",
            "jumpkilleffect"
          )
        ))
    return(NULL)
    colsstd <<- list(
      "filename" = 1,
      "interval label" = 2,
      "start" = 3,
      "end" = 4,
      "stepnumber" = 5,
      "f0" = 6,
      "jumpkilleffect" = 7
    )
    tags$div(
      title = "Choose new name from required columns (by default \"filename\" is taken for speaker differences)",
      selectInput(
        "stdcols",
        label = NULL,
        choices = colsstd,
        selected = 1,
        multiple = F,
        width = "80%"
      )
    )
  })
  
  output$confcols <- renderUI({
    if (is.null(input$file_input) |
        all(
          names(colsdf) == c(
            "filename",
            "interval_label",
            "start",
            "end",
            "step",
            "stepnumber",
            "f0",
            "jumpkilleffect"
          )
        ))
    return(NULL)
    tags$div(title = "Check this box to confirm renaming.",
             checkboxInput("confcols", "", value = F))
  })
  
  output$include_int <- renderUI({
    if (is.null(input$file_input) | int_available == 0) {
      return(NULL)
    } else {
    tags$div(title = "Include intensity information from datafile.",
                 checkboxInput("include_int", "Include intensity", T))
    }
  })
  
  output$include_dur <- renderUI({
    if (is.null(input$file_input)) {
      return(NULL)
    } else {
      tags$div(title = "Include duration information.",
             checkboxInput("include_dur", "Include duration", T))
    }
  })
  
  output$prep_data <- renderUI({
    if (is.null(input$file_input))
      return(NULL)
    tags$div(title = "Applies cleaning/octave jump range selection/speaker correction to the data. Apply once after uploading data.",
             actionButton("prep_data", "Proceed"))
  })
  
  output$evaldep <- renderUI({
    if (input$evalmethod == 2)
      return(NULL)
    tags$div(
      title = "Value indicating the dependency between measurement points; higher for more dependency. Higher dependency for smaller units of analysis. Recommended values lie between 1 and 5 approximately.",
      numericInput(
        "smoothing",
        "Bending factor:",
        value = evaldep,
        width = '80%'
      )
    )
  })
  
  # observers ####
  
  observeEvent(input$tsfz, {
    output$dirdata <- renderUI({
      tags$div(
        title = paste0(
          "Specify path to the directory that contains sound files and TextGrids, including final ",
          .Platform$file.sep,
          ". Only long formatted TextGrids with UTF-8 or ASCII encoding can be read, no other encodings and no TextGrids saved 'as short text file'."
        ),
        textInput(
          "dir_input",
          "Choose directory that contains sound files and TextGrids:",
          ""
        )
      )
    })
    
    output$snd_ext <- renderUI({
      radioButtons(
        "snd_ext",
        "Select sound file extension:",
        choices = list(".wav" = 1, ".mp3" = 2),
        selected = 1,
        inline = T
      )
    })
    
    output$read_grids <- renderUI({
      tags$div(
        title = "Reads all TextGrids that have corresponding sound file in the specified directory. All tiers from each TextGrid are read, consider using TextGrids with one tier for the unit of analysis to speed up processing.",
      actionButton("read_grids", "Read directory", width = '70%')
      )
    })
    
    output$tsfz <- renderUI({
      return(NULL)
    })
    
    output$cc <- renderUI({
      return(NULL)
    })
  })
  
  observeEvent(input$read_grids, {
    input$dir_input ->> inDir
    if (inDir == ""){
      showNotification("Specify path before reading directory.",type = "error")
      Sys.sleep(5)
    }
    else if (substr(inDir,nchar(inDir),nchar(inDir))!=.Platform$file.sep){
      showNotification(paste0("Include final ",.Platform$file.sep),type = "error")
    } 
    else if (dir.exists(inDir)==F){
      showNotification("Directory does not exist.",type = "error")
    } 
    else if (length(list.files(inDir, pattern = "*.TextGrid", ignore.case = T))==0 | length(list.files(inDir, pattern = "*.wav", ignore.case = T))==0){
      showNotification("No audio and/or TextGrids in directory.",type = "error")
      }else {
    if (input$snd_ext == 1) {
      ".wav" ->> snd_ext
    }
    if (input$snd_ext == 2) {
      ".mp3" ->> snd_ext
    }
    list.files(inDir, pattern = "*.TextGrid", ignore.case = T) -> grids
    as.data.frame(str_split(grids, pattern = "\\.", simplify = T))[, 1] -> grids
    as.data.frame(grids) -> grids
    grids$rm <- 0
    for (r in (1:nrow(grids))) {
      if (file.exists(paste0(inDir, grids$grids[r], snd_ext)) == F) {
        1 -> grids$rm[r]
      } else {
        0 -> grids$rm[r]
      }
    }
    grids[grids$rm == 0, 1] -> grids
    df.g <- c()
    withProgress(message = "Reading directory...", {
      for (g in grids) {
        incProgress(1 / ((length(grids) * 1.2)))
        if (! as.character(guess_encoding(paste0(inDir, g, ".TextGrid"))[1,1]) %in% c("UTF-8","ASCII")){
          showNotification("Cannot read TextGrids. Convert encoding of TextGrids to UTF-8 or ASCII.",type = "error")
          Sys.sleep(5)
          cat("Cannot read TextGrids. Convert encoding of TextGrids to UTF-8 or ASCII.\n")
          cat(paste0("Error occurred with file: ",inDir,g,".TextGrid.\n"))
          stopApp()
        }
        if (substr(read_lines(
          paste0(inDir, g, ".TextGrid"),
          n_max = 1,
          skip = 3
        ), 1, 4) != "xmin") {
          showNotification("Cannot read TextGrids. Choose only TextGrids in long format.",type = "error")
          Sys.sleep(5)
          cat("Cannot read TextGrids. Choose only TextGrids in long format.\n")
          cat(paste0("Error occurred with file: ",inDir,g,".TextGrid.\n"))
          stopApp()
        }
        rbind(df.g, read_textgrid(paste0(inDir, g, ".TextGrid"))) -> df.g
      }
    })
    as.data.frame(str_split(df.g$file, pattern = "\\.", simplify = T))[, 1] -> df.g$name_trim
    df.g ->> df.g
    tiers <- c()
    for (t in unique(df.g$tier_name[df.g$tier_type == "IntervalTier"])) {
      if (length(df.g$file[df.g$tier_name == t]) > 1) {
        append(tiers, t) -> tiers
      }
    }
    setNames(as.list(as.numeric(c(
      1:length(tiers)
    ))), c(tiers)) ->> tiers
    median(df.g$xmax[df.g$tier_name == input$select_tier] - df.g$xmin[df.g$tier_name ==
                                                                        input$select_tier]) ->> mD
    
    output$select_tier <- renderUI({
      selectInput(
        "select_tier",
        "Select the interval tier for analysis:",
        choices = tiers,
        selected = 1,
        multiple = F,
        width = '75%'
      )
    })
    

    
    output$pmin <- renderUI({
      numericInput(
        "pmin",
        "f0 floor (Hz)",
        value = 75,
        min = 0,
        max = 200,
        step = 25
      )
    })
    output$pmax <- renderUI({
      numericInput(
        "pmax",
        "f0 ceiling (Hz)",
        value = 500,
        min = 200,
        max = 800,
        step = 25
      )
    })
    
    output$f0fit <- renderUI({
      tags$div(
        title = "Package 'wrassp':  minimum quality value of f0 fit, defaults to 0.52. More accurate/less f0 candidates > 0.52 > less accurate/more f0 candidates.",
        numericInput(
          "f0fit",
          "f0 fit",
          value = 0.52,
          min = 0,
          max = 1,
          step = 0.1
        )
      )
    })
    
    output$smooth_bw <- renderUI({
      tags$div(
        title = "Smoothing bandwith for kernel smoothing. Snugger < smoother.",
        numericInput(
          "smooth_bw",
          "Smoothing bandwith",
          value = 1,
          min = 0,
          max = 100,
          step = 1
        )
      )
    })
    
    output$timestep <- renderUI({
      numericInput(
        "timestep",
        "Time-step (ms)",
        value = 10,
        min = 1,
        max = 50,
        step = 1
      )
    })
    
    output$npoints <- renderUI({
      tags$div(
        title = "Number of measurement points to represent the smoothed contour. See hints in sidepanel on the maximum number of measurement points.",
        numericInput(
          "npoints",
          "N measurement pts",
          value = 20,
          min = 2,
          max = 100,
          step = 1
        )
      )
    })
    
    output$select_int <- renderUI({
      tags$div(
        title = "Select whether to include time-series intensity measures (dB) in addition to f0. Will affect clustering.",
        checkboxInput("select_int", "Include intensity (dB)",F))
    })
    
    output$select_sample <- renderUI({
      tags$div(
        title = "Select whether to take a number of random samples to check the measurement/smoothing settings or to measure all selected intervals and write the output to a dataframe.",
        selectInput(
          "select_sample",
          label = NULL,
          choices = c("Sample", "Measure"),
          selected = 1,
          multiple = F
        )
      )
    })
    
    output$n_sample <- renderUI({
      tags$div(
        title = "Set the number of random samples to take and plot.",
        numericInput(
          "n_sample",
          label = NULL,
          value = 5,
          min = 1,
          max = length(df.g$file[df.g$tier_name == input$select_tier]),
          step = 1
        )
      )
    })
    
    output$txt_sample <- reactive({
      paste0("random intervals from tier ", input$select_tier)
    })
    
    output$do_sample <- renderUI({
      actionButton("do_sample", "Run")
    })
    }
  })
  
  observeEvent(input$select_sample, {
    if (input$select_sample == "Sample") {
      output$n_sample <- renderUI({
        numericInput(
          "n_sample",
          label = NULL,
          value = 5,
          min = 1,
          max = length(df.g$file[df.g$tier_name == input$select_tier]),
          step = 1
        )
      })
      output$txt_sample <- reactive({
        paste0("random intervals from tier ", input$select_tier)
      })
    }
    if (input$select_sample == "Measure") {
      output$n_sample <- renderUI({
        return(NULL)
      })
      output$txt_sample <- reactive({
        paste0("all intervals from tier ", input$select_tier)
      })
    }
  })
  
  observeEvent(c(input$select_tier, input$timestep), {
    output$suggest <- reactive({
      median(df.g$xmax[df.g$tier_name == input$select_tier] - df.g$xmin[df.g$tier_name ==
                                                                          input$select_tier]) ->> mD
      paste0(
        "With a median interval duration of ",
        round(mD, 2),
        " s (tier = ",
        input$select_tier,
        ") and a time-step of ",
        input$timestep,
        " ms, f0 measurement resolution will be ~",
        floor((1000 / input$timestep) * mD) - 1,
        " points per interval (tracking accuracy). Praat settings default to an f0 floor of 75 Hz with 10 ms timestep. Smoothing accuracy will be determined by setting the measurement points per interval and adjusting the smoothing factor."
      )
    })
  })
  
  observeEvent(input$do_sample, {
    if (exists('rempng')) {
      if (length(rempng) > 0) {
        for (p in rempng) {
          if (file.exists(p)) {
            file.remove(p)
          }
        }
      }
    }
    c() ->> rempng
    df.g[df.g$tier_name == input$select_tier &
           df.g$text != "", ] -> df.gs
    as.data.frame(df.gs, row.names = 1:nrow(df.gs)) ->> df.gs
    ncol(df.g) + 1 ->> colX
    cbind(df.gs,as.data.frame(matrix(nrow = nrow(df.gs),ncol = input$npoints,dimnames = list(c(),rep(paste0("X",1:input$npoints)))))) ->> df.gs
    if (input$select_int == T){
      cbind(df.gs,as.data.frame(matrix(nrow = nrow(df.gs),ncol = input$npoints,dimnames = list(c(),rep(paste0("Y",1:input$npoints)))))) ->> df.gs
    }
    if (input$select_sample == "Sample") {
      sample(1:nrow(df.gs), input$n_sample) -> rows
    }
    if (input$select_sample == "Measure") {
      as.list(c()) ->> pnglist
      1:nrow(df.gs) -> rows
    }
    withProgress(message = "Measuring f0...", {
      for (r in rows) {
        incProgress(1 / (length(rows) * 1.2))
        "" ->> NAmsg
        df.p <- c()
        paste0(inDir, df.gs$name_trim[r], snd_ext) ->> f
        df.gs$xmax[r] ->> Tmax
        df.gs$xmin[r] ->> Tmin
        df.gs$xmax[r] - df.gs$xmin[r] -> di
        input$timestep ->> s
        50 ->> w
        if (di < mD) {
          if (di < 2 * (w / 1000)) {
            floor((di * 1000) / 2) ->> w
          }
        }
        if (di < w / 1000) {
          "Interval too short, writing NAs to output." ->> NAmsg
        }
        if (di >= w / 1000) {
          tsf0() -> listf0
          c() -> df.p
          listf0$pitch -> df.p$pitch
          NA -> df.p$pitch[df.p$pitch==0]
          seq(0,numRecs.AsspDataObj(listf0) - 1) / rate.AsspDataObj(listf0) + attr(listf0, 'startTime') -> df.p$time
          as.data.frame(df.p) -> df.p
          if (is.null(nrow(df.p))) {
            as.list(rep(NA, input$npoints)) -> df.gs[r, c(rep(paste0("X",1:input$npoints)))]
            "Measurements failed, writing NAs to output" ->> NAmsg
          }
          if (is.null(nrow(df.p)) == F) {
            if (sum(is.na(df.p$pitch)) == nrow(df.p)) {
              as.list(rep(NA, input$npoints)) -> df.gs[r, c(rep(paste0("X",1:input$npoints)))]
              "No f0 detected, writing NAs to output." ->> NAmsg
            } else {
              na.approx(df.p$pitch, na.rm = F) -> df.p$pitchInt
              approxExtrap(df.p$time,
                           df.p$pitchInt,
                           xout = df.p$time,
                           method = "constant")$y -> df.p$pitchInt
              as.list(approx(df.p$pitchInt, n = input$npoints)$y) -> df.gs[r, c(rep(paste0("X",1:input$npoints)))]
              ksmooth(
                1:input$npoints,
                df.gs[r, c(rep(paste0("X",1:input$npoints)))],
                kernel = "normal",
                bandwidth = input$smooth_bw,
                n.points = input$npoints
              )$y -> ps
              ps ->> df.gs[r, c(rep(paste0("X",1:input$npoints)))]
              if (input$select_int==T){
                tsdB() -> listdB
                na.approx(listdB$rms, na.rm = F) -> listdB
                as.list(approx(listdB, n = input$npoints)$y) -> df.gs[r, c(rep(paste0("Y",1:input$npoints)))]
              }
            }
            if (input$select_sample == "Sample") {
              if (NAmsg != "") {
                  ggplot(df.p, aes(time)) +
                    scale_y_continuous(limits = c(input$pmin, input$pmax)) +
                    scale_x_continuous(limits = c(Tmin *
                                                    1000, Tmax * 1000)) +
                    labs(
                      title = df.gs$name_trim[r],
                      subtitle = df.gs$text[r],
                      x = "Time (ms)",
                      y = "Pitch (Hz)"
                    ) +
                    annotate(
                      "text",
                      x = (Tmax * 1000) - ((di * 1000) / 2),
                      y = input$pmax - ((input$pmax - input$pmin) / 2),
                      label = NAmsg,
                      colour = "red"
                    ) -> p
              } else {
                min(na.omit(df.p$time)) - ((max(na.omit(
                  df.p$time
                )) - min(na.omit(
                  df.p$time
                ))) / (input$npoints - 1)) + (cumsum(rep(((max(na.omit(
                  df.p$time
                )) - min(na.omit(
                  df.p$time
                ))) / (input$npoints - 1)
                ), input$npoints))) -> ts
                as.data.frame(cbind(ts, ps)) -> df.ps
                  ggplot(df.p, aes(time)) +
                    geom_line(
                      mapping = aes(y = pitch),
                      na.rm = TRUE
                    ) +
                    geom_line(
                      df.ps,
                      mapping = aes(x = ts, y = ps),
                      linetype = "dashed",
                      colour = "red",
                      na.rm = TRUE
                    ) +
                    coord_cartesian(ylim = c(input$pmin, input$pmax)) +
                    labs(
                      title = df.gs$name_trim[r],
                      subtitle = df.gs$text[r],
                      x = "Time (ms)",
                      y = "Pitch (Hz)"
                    ) -> p
              }
              file.path("www", paste0(r, ".png")) -> pf
              suppressMessages(ggsave(pf, p, scale = 0.7))
              append(rempng, pf) ->> rempng
            }
          }
        }
      }
    })
    if (input$select_sample == "Sample") {
      output$sample <- renderUI({
        fluidPage(align = "center",
                  do.call(tagList, lapply(1:length(rempng), function(i) {
                    img(src = as.character(unlist(as.data.frame(
                      str_split(rempng, .Platform$file.sep)
                    )[2, ]))[i],
                    width = "50%")
                  })))
      })
      showTab(inputId = "outputs", target = "Sample")
      isolate(updateTabsetPanel(session, inputId = "outputs", selected = "Sample"))
    }
    if (input$select_sample == "Measure") {
      data.table::melt(
        as.data.table(df.gs[,1:(colX+input$npoints-1)]),
        id.vars = c('name_trim', 'text', 'xmin', 'xmax'),
        measure.vars = colX:(colX + input$npoints - 1),
        value.name = "f0"
      ) -> data_long
      as.numeric(data_long$variable) -> data_long$variable
      cbind(data_long[, c('name_trim', 'text', 'xmin', 'xmax')],
            data_long$variable * ((data_long$xmax - data_long$xmin) / (input$npoints +
                                                                         1)),
            data_long[, c('variable', 'f0')],
            rep(1, nrow(data_long))) -> data_long
      colnames(data_long) <-
        c(
          'filename',
          'interval_label',
          'start',
          'end',
          'step',
          'stepnumber',
          'f0',
          'jumpkilleffect'
        )
      if (input$select_int==T){
        suppressMessages(data.table::melt(df.gs[, c(rep(paste0("Y",1:input$npoints)))], value.name = "dB")$dB -> data_long$dB)
        int_available <<- 1
      }
      arrange(data_long, filename, start, stepnumber) -> data_long
      fwrite(data_long, file.path("www", "data_long.csv"))
      output$tocc <- renderUI({
        tags$div(title = "Contour clustering with this dataset",
                 actionButton("tocc", "Start contour clustering with this dataset"))
      })
      showTab(inputId = "outputs", target = "Data (wide)")
      output$data_wide <- renderTable({
        df.gs
      })
      updateTabsetPanel(session, inputId = "outputs", selected = "Data (wide)")
    }
    
  })
  
  observeEvent(input$tocc, {
    suggest_cln <<- F
    output$dirdata <- renderUI({
      return(NULL)
    })
    output$select_tier <- renderUI({
      return(NULL)
    })
    output$suggest <- renderUI({
      return(NULL)
    })
    output$snd_ext <- renderUI({
      return(NULL)
    })
    output$read_grids <- renderUI({
      return(NULL)
    })
    output$pmin <- renderUI({
      return(NULL)
    })
    output$pmax <- renderUI({
      return(NULL)
    })
    output$timestep <- renderUI({
      return(NULL)
    })
    output$f0fit <- renderUI({
      return(NULL)
    })
    output$npoints <- renderUI({
      return(NULL)
    })
    output$smooth_bw <- renderUI({
      return(NULL)
    })
    output$select_int <- renderUI({
      return(NULL)
    })
    output$select_sample <- renderUI({
      return(NULL)
    })
    output$n_sample <- renderUI({
      return(NULL)
    })
    output$txt_sample <- renderUI({
      return(NULL)
    })
    output$do_sample <- renderUI({
      return(NULL)
    })
    output$tocc <- renderUI({
      return(NULL)
    })
    
    output$rem_empty <- renderUI({
      tags$div(title = "Clean missing interval labels and f0 values. Also removes contours with a rate of f0 change beyond the ones in Xu and Sun (2002, Table X; 72 ST/s for rises and 96 ST/s for falls). Only apply cleaning on f0 values in Hertz. ",
               checkboxInput("rem_empty", "clean data (remove NA and f0 errors)", suggest_cln))
    })
    
    output$decl_corr <- renderUI({
      tags$div(title = "Avoid flat contours by applying a declination effect ('t Hart et al., 1991, p.129): D = (-11)/(t+1.5). Enables Pearson/auto-correlation distance measures. Only applied to flat contours.",
               checkboxInput("decl_corr", "forced declination", FALSE))
    })
    
    output$f0_header <- renderUI({
      if (is.null(input$file_input))
        return(NULL)
      "Choose f0 scale:"
    })
    
    output$f0_conv <- renderUI({
      tags$div(
        title = "Default assumption: f0 is measured in Hertz (Hz). Convert to semitones re 50 Hz: f0(ST) = log10((f0(Hz)/50)*39.87 or convert to Equivalent Rectangular Bandwidth (ERB): f0(ERB) = 16.6*log10(1+(f0(Hz)/165.4))",
        selectInput("f0_conv", label = NULL,choices = list("Hz (no conversion)" = "Hz", "ST" = "ST", "ERB" = "ERB"),selected = 1,multiple = F,width="40%")
      )
    })
    
    output$jump_header <- renderUI({
      "Allowed % change after octave jump correction:"
    })
    
    output$jump_margin <- renderUI({
      numericInput(
        "jump_margin",
        label = NULL,
        value = 10,
        min = 0,
        max = 100,
        width = '20%'
      )
    })
    
    output$spkdiff_header <- renderUI({
      "Correct speaker differences:"
    })
    
    output$spkdiff <- renderUI({
      tags$div(
        title = "Only apply when number of speakers is correctly detected. 1 = none, 2 = f0–mean(f0.spk), 3 = f0–mean(f0.spk) / sd(f0.spk), 4 = f0–min(f0.spk) / max(f0.spk)–min(f0.spk), 5 = f0–median(f0.spk) / 75q(f0.spk)–25q(f0.spk), 6 = log2(f0.spk/media(f0.spk)), 7 = Δf0/Δtime",
        selectInput(
          "spkdiff",
          label = NULL,
          choices = list(
            "1: none" = 1,
            "2: subtract mean" = 2,
            "3: standardise" = 3,
            "4: min-max normalize" = 4,
            "5: robust scaler" = 5,
            "6: octave-median scaled" = 6,
            "7: first derivative (d1)" = 7
          ),
          selected = 1,
          multiple = F,
          width = "50%"
        )
      )
    })
    
    output$dfcols_header <- renderUI({
      if (all(
        names(colsdf) == c(
          "filename",
          "interval_label",
          "start",
          "end",
          "step",
          "stepnumber",
          "f0",
          "jumpkilleffect"
        )
      )) {
        return(NULL)
      } else{
        "Rename (now in data)"
      }
    })
    
    output$dfcols <- renderUI({
      if (all(
        names(colsdf) == c(
          "filename",
          "interval_label",
          "start",
          "end",
          "step",
          "stepnumber",
          "f0",
          "jumpkilleffect"
        )
      ))
      return(NULL)
      tags$div(
        title = "These columns are now detected to be in the loaded data (no need to remap if they match the required columns)",
        selectInput(
          "dfcols",
          label = NULL,
          choices = colsdf,
          selected = 1,
          multiple = F,
          width = "80%"
        )
      )
    })
    
    output$stdcols_header <- renderUI({
      if (all(
        names(colsdf) == c(
          "filename",
          "interval_label",
          "start",
          "end",
          "step",
          "stepnumber",
          "f0",
          "jumpkilleffect"
        )
      ))
      return(NULL)
      "into (required)"
    })
    
    output$stdcols <- renderUI({
      if (all(
        names(colsdf) == c(
          "filename",
          "interval_label",
          "start",
          "end",
          "step",
          "stepnumber",
          "f0",
          "jumpkilleffect"
        )
      ))
      return(NULL)
      colsstd <<- list(
        "filename" = 1,
        "interval label" = 2,
        "start" = 3,
        "end" = 4,
        "stepnumber" = 5,
        "f0" = 6,
        "jumpkilleffect" = 7
      )
      tags$div(
        title = "Choose new name from required columns (by default \"filename\" is taken for speaker differences)",
        selectInput(
          "stdcols",
          label = NULL,
          choices = colsstd,
          selected = 1,
          multiple = F,
          width = "80%"
        )
      )
    })
    
    output$confcols <- renderUI({
      if (all(
        names(colsdf) == c(
          "filename",
          "interval_label",
          "start",
          "end",
          "step",
          "stepnumber",
          "f0",
          "jumpkilleffect"
        )
      ))
      return(NULL)
      tags$div(title = "Check this box to confirm renaming.",
               checkboxInput("confcols", "", value = F))
    })
    
    output$include_int <- renderUI({
      if (int_available == 0)
        return(NULL)
      tags$div(title = "Include intensity information from datafile.",
               checkboxInput("include_int", "Include intensity", T))
    })
    
    output$include_dur <- renderUI({
      tags$div(title = "Include duration information.",
               checkboxInput("include_dur", "Include duration", T))
    })
    
    output$prep_data <- renderUI({
      tags$div(title = "Applies cleaning/octave jump range selection/speaker correction to the data. Apply once after uploading data.",
               actionButton("prep_data", "Proceed"))
    })
    output$summary <- renderText({
      readdata()
    })
    updateTabsetPanel(session, inputId = "outputs", selected = "summary")
  })
  
  observeEvent(input$cc, {
    output$dirdata <- renderUI({
      fileInput(
        "file_input",
        "Choose datafile",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        ),
        width = '75%'
      )
    })
    
    output$sepchoice <- renderUI({
      if (is.null(input$file_input)) {
        tags$div(
          title = "Default output from Praat script is comma-separated.",
          radioButtons(
            "sepchoice",
            label = "Separator",
            choices = list("comma" = 1, "tab" = 2),
            selected = 1,
            inline = T
          )
        )
      } else{
        return(NULL)
      }
    })
    
    output$strfact <- renderUI({
      if (is.null(input$file_input)) {
        tags$div(
          title = "Logical: should character vectors be converted to factors?",
          radioButtons(
            "strfact",
            label = "StringsAsFactors",
            choices = list("true" = 1, "false" = 2),
            selected = 2,
            inline = T
          )
        )
      } else{
        return(NULL)
      }
    })
    
    output$filenc <- renderUI({
      if (is.null(input$file_input)) {
        tags$div(
          title = "Choose UTF-16 for challenging ortography in the textgrid labels",
          radioButtons(
            "filenc",
            label = "File encoding",
            choices = list("UTF-8" = 1, "UTF-16" = 2),
            selected = 1,
            inline = T
          )
        )
      } else{
        return(NULL)
      }
    })
    
    output$skpnl <- renderUI({
      if (is.null(input$file_input)) {
        tags$div(
          title = "Logical: should nuls be skipped?",
          radioButtons(
            "skpnl",
            label = "SkipNul",
            choices = list("true" = 1, "false" = 2),
            selected = 2,
            inline = T
          )
        )
      } else{
        return(NULL)
      }
    })
    
    output$tsfz <- renderUI({
      return(NULL)
    })
    
    output$cc <- renderUI({
      return(NULL)
    })
  })
  
  observeEvent(input$file_input, {
    output$summary <- renderText({
      readdata()
    })
  })
  
  observeEvent(input$prep_data, {
    data <-
      as.data.frame(fread(
        file.path("www", "data_long.csv"),
        sep = ",",
        header = T,
        stringsAsFactors = F
      ))
    if (is.null(input$stdcols) == F &&
        input$stdcols != input$dfcols && input$confcols == T) {
      colnames(data)[colnames(data) == names(colsdf[as.numeric(input$dfcols)])] <-
        names(colsstd[as.numeric(input$stdcols)])
      clnms <- colnames(data)
      clnrs <- c(1:length(clnms))
      colsdf <<- setNames(as.list(as.numeric(clnrs)), c(clnms))
      updateSelectInput(session, "dfcols")
      update
    }
    
    gsub(
      pattern = ":;",
      replacement = ",",
      x = as.character(data$interval_label)
    ) -> data$interval_label
    "(Hz)" ->> ysc
    
    if (input$rem_empty == T) {
      data[data$err != "1", ] -> data
      subset(data, select = -c(err)) -> data
      "Cleaning applied." ->> cln
    }
    
    if (input$decl_corr == T) {
      for (f in unique(data$filename)) {
        for (s in unique(data$start[data$filename==f])){
          if (sd(data$f0[data$filename==f & data$start==s]) == 0){
            log10((as.numeric(as.character(data$f0[data$filename==f & data$start==s])) / 50)) * 39.87 -> st
            (-11)/(unique(data$end[data$filename==f & data$start==s])-s+1.5)*(unique(data$end[data$filename==f & data$start==s])-s) -> decl
            st[2:stepsN]+(2:stepsN*(decl/(stepsN))) -> st[2:stepsN]
            50*10^(st/39.87) -> data$f0[data$filename==f & data$start==s]
          }
        }
      }
    }
    
    if (input$f0_conv == "Hz") {
      "F0 values not converted (assuming Hz)." ->> stconv
    }
    if (input$f0_conv == "ST") {
      log10((as.numeric(as.character(data$f0)) / 50)) * 39.87 -> data$f0
      "(ST)" ->> ysc
      "Hertz values converted to ST." ->> stconv
    }
    if (input$f0_conv == "ERB") {
      16.6*log10(1+(as.numeric(as.character(data$f0)) / 165.4)) -> data$f0
      "(ERB)" ->> ysc
      "Hertz values converted to ERB." ->> stconv
    }
    
    subset(data,
           as.numeric(as.character(data$jumpkilleffect)) >= (1 - (input$jump_margin /
                                                                    100)) &
             as.numeric(as.character(data$jumpkilleffect)) <= (1 + (input$jump_margin /
                                                                      100))) -> data
    paste0(input$jump_margin, "% allowed jumpkilleffect.") ->> jkm
    paste0("f0 ", ysc) ->> ylb

    if (input$spkdiff == 2) {
      paste0("Speaker mean corrected f0 ", ysc) ->> ylb
      for (spk in levels(as.factor(data$filename))) {
        mean(as.numeric(as.character(data$f0[data$filename == spk]))) -> m
        (as.numeric(as.character(data$f0[data$filename == spk])) - m) -> data$f0[data$filename ==
                                                                                   spk]
      }
    }
    if (input$spkdiff == 3) {
      "Speaker standardized f0" ->> ylb
      for (spk in levels(as.factor(data$filename))) {
        mean(as.numeric(as.character(data$f0[data$filename == spk]))) -> m
        sd(as.numeric(as.character(data$f0[data$filename == spk]))) -> sd
        (as.numeric(as.character(data$f0[data$filename == spk])) - m) /
          sd -> data$f0[data$filename == spk]
      }
    }
    if (input$spkdiff == 4) {
      "Speaker min-max normalized f0" ->> ylb
      for (spk in levels(as.factor(data$filename))) {
        mean(as.numeric(as.character(data$f0[data$filename == spk]))) -> m
        min(as.numeric(as.character(data$f0[data$filename == spk]))) -> min
        max(as.numeric(as.character(data$f0[data$filename == spk]))) -> max
        (as.numeric(as.character(data$f0[data$filename == spk])) - min) /
          (max - min) -> data$f0[data$filename == spk]
      }
    }
    if (input$spkdiff == 5) {
      "Speaker robust scaled f0" ->> ylb
      for (spk in levels(as.factor(data$filename))) {
        median(as.numeric(as.character(data$f0[data$filename == spk]))) -> md
        quantile(as.numeric(as.character(data$f0[data$filename == spk])), 0.75) -> q75
        quantile(as.numeric(as.character(data$f0[data$filename == spk])), 0.25) -> q25
        (as.numeric(as.character(data$f0[data$filename == spk])) - md) /
          (q75 - q25) -> data$f0[data$filename == spk]
      }
    }
    if (input$spkdiff == 6) {
      "Octave-median scaled f0" ->> ylb
      for (spk in levels(as.factor(data$filename))) {
        median(as.numeric(as.character(data$f0[data$filename == spk]))) -> md
        log2(as.numeric(as.character(data$f0[data$filename == spk])) / md) -> data$f0[data$filename ==
                                                                                        spk]
      }
    }
    if (input$spkdiff == 7) {
      "First derivative (d1)" ->> ylb
      for (rws in 1:(nrow(data)/stepsN)) {
        pracma::gradient(as.numeric(as.character(data$f0[((rws*stepsN)-(stepsN-1)):(rws*stepsN)]))) -> data$f0[((rws*stepsN)-(stepsN-1)):(rws*stepsN)]
      }
    }
    fwrite(data, file.path("www", "data_long.csv"))
    data.table::dcast(as.data.table(data),
                    filename + interval_label + start + end ~ stepnumber,
                    value.var = "f0") -> datacast
    if("dB" %in% colnames(data)){
      if (input$include_int==T){
        for (spk in levels(as.factor(data$filename))) {
          mean(as.numeric(as.character(data$dB[data$filename == spk]))) -> m
          sd(as.numeric(as.character(data$dB[data$filename == spk]))) -> sd
          (as.numeric(as.character(data$dB[data$filename == spk])) - m) /
            sd -> data$dB[data$filename == spk]
        }
        }
      data.table::dcast(as.data.table(data),
                        filename + interval_label + start + end ~ stepnumber,
                        value.var = "dB") -> datacastdB
      datacastdB %>% select(tail(names(.), stepsN)) -> datacastdB
      rep(paste0("dB",1:stepsN)) -> colnames(datacastdB)
      cbind(datacast,datacastdB) -> datacast
    }
    fwrite(datacast, file.path("www", "data_wide.csv"))
    stepone <<- which(colnames(datacast) == "1")
    
    output$prep_data <- renderUI({
      return(NULL)
    })
    output$rem_empty <- renderUI({
      return(NULL)
    })
    output$decl_corr <- renderUI({
      return(NULL)
    })
    output$f0_header <- renderUI({
      return(NULL)
    })
    output$f0_conv <- renderUI({
      return(NULL)
    })
    output$jump_header <- renderUI({
      return(NULL)
    })
    output$jump_margin <- renderUI({
      return(NULL)
    })
    output$spkdiff_header <- renderUI({
      return(NULL)
    })
    output$spkdiff <- renderUI({
      return(NULL)
    })
    output$dfcols_header <- renderUI({
      return(NULL)
    })
    output$stdcols_header <- renderUI({
      return(NULL)
    })
    output$dfcols <- renderUI({
      return(NULL)
    })
    output$stdcols <- renderUI({
      return(NULL)
    })
    output$confcols <- renderUI({
      return(NULL)
    })
    output$include_int<- renderUI({
      return(NULL)
    })
    output$include_dur<- renderUI({
      return(NULL)
    })
    output$data_long <- renderTable({
      data
    })
    output$data_wide <- renderTable({
      datacast
    })
    showTab(inputId = "outputs", target = "Data (long)")
    showTab(inputId = "outputs", target = "Data (wide)")
    output$distm <- renderUI({
      tags$div(
        title = "Select distance measure to use for the computation of the distance matrix. Default is 'euclidean'",
        selectInput(
          "distm",
          label = "Distance measure:",
          choices = list(
            "euclidean (L2 norm)" = "euclidean",
            "root mean square error (RMSE)" = "rmse",
            "mean absolute scaled error (MASE)" = "mase",
            "dynamic time warping" = "dtw_basic",
            "pearson (sqrt(2*(1-ρ)))" = "cor",
            "autocorrelation" = "acf"
          ),
          selected = "complete",
          multiple = F,
          width = "100%"
        )
      )
    })
    
    
    output$sellink <- renderUI({
      tags$div(
        title = "Select linkage criterion for the 'hclust' function. Default is set to complete linkage. * = inversion risk",
        selectInput(
          "sellink",
          label = "Linkage criterion:",
          choices = list(
            "complete (default)" = "complete",
            "single" = "single",
            "average (= UPGMA)" = "average",
            "ward.D" = "ward.D",
            "ward.D2" = "ward.D2",
            "mcquitty (= WPGMA)" = "mcquitty",
            "centroid* (= UPGMC)" = "centroid",
            "median* (= WPGMC)" = "median"
          ),
          selected = "complete",
          multiple = F,
          width = "100%"
        )
      )
    })
    
    output$getdendro <- renderUI({
      actionButton("getdendro", "Dendrogram")
    })
    updateTabsetPanel(session, inputId = "outputs", selected = "summary")
    output$summary <- renderText({
      readdata()
    })
  })
  
  observeEvent(input$getdendro, {
    output$nclust <- renderUI({
      sliderInput(
        "nclust",
        "Number of clusters",
        min = 2,
        max = 50,
        value = numbclust,
        step = 1
      )
    })
    output$gettab <- renderUI({
      actionButton("gettab", "Table")
    })
    output$getplot <- renderUI({
      actionButton("getplot", "Plot")
    })
    output$dendro <- renderPlot({
      clustprep()
      plot(dendro)
    })
    showTab(inputId = "outputs", target = "Dendrogram")
    updateTabsetPanel(session, inputId = "outputs", selected = "Dendrogram")
  })
  
  observeEvent(input$gettab, {
    output$table <- renderTable({
      clustprep()
      clusttab()
    }, rownames = T)
    showTab(inputId = "outputs", target = "Table")
    updateTabsetPanel(session, inputId = "outputs", selected = "Table")
  })
  
  observeEvent(input$getplot, {
    output$plot <- renderPlot({
      clustprep()
      clusttab()
      withProgress(message = "Generating plot...", clustplot())
    }, height = ht)
    showTab(inputId = "outputs", target = "Plot")
    updateTabsetPanel(session, inputId = "outputs", selected = "Plot")
  })
  
  observeEvent(input$nclust, {
    input$nclust ->> numbclust
    if (numbclust < 4) {
      300 ->> ht
    } else{
      ceiling(numbclust / 4) * 300 ->> ht
    }
    output$dosubset <- renderUI({
      if (input$gettab == F && input$getplot == F)
        return(NULL)
      tags$div(title = "Removes the selected clusters from the wide data only. Number of remaining contours after subsettings can be read from Status tab.",
               actionButton("dosubset", "Apply subsetting"))
    })
    output$subset <- renderUI({
      if (input$gettab == F && input$getplot == F)
        return(NULL)
      selclust()
    })
    
    if(exists("dendro")){
      clustprep()
    }
    
    output$table <- renderTable({
      clusttab()
    }, rownames = T)
    
    output$plot <- renderPlot({
      clusttab()
      withProgress(message = "Generating plot...", clustplot())
    }, height = ht)
  })
  
  observeEvent(input$subset, {
    as.vector(input$subset, mode = "numeric") -> rem_clust
    if (is.null(input$subset)) {
      rem_clust = c()
    }
    rem_clust ->> rem_clust
  })
  
  observeEvent(input$dosubset, {
    datacast <-
      as.data.frame(fread(
        file.path("www", "data_wide.csv"),
        sep = ",",
        header = T,
        stringsAsFactors = F
      ))
    for (clust in rem_clust) {
      subset(datacast, datacast$cluster != clust) -> datacast
    }
    fwrite(datacast, file.path("www", "data_wide.csv"))
    clustprep()
    output$subset <- renderUI({
      selclust()
    })
    output$table <- renderTable({
      clusttab()
    }, rownames = T)
    output$plot <- renderPlot({
      clusttab()
      withProgress(message = "Generating plot...", clustplot())
    }, height = ht)
  })
  
  observe({
    if (input$outputs == "summary") {
      output$summary <- renderText({
        readdata()
      })
    }
  })
  
  observeEvent(input$goevaluate, {
    showTab(inputId = "outputs", target = "Evaluate")
    output$evaluate <- renderUI({
      fluidPage(
        fluidRow(
          column(
            width = 5,
            align = "left",
            sliderInput(
              "evalslider",
              "N clusters (range)",
              min = 2,
              max = 15,
              value = c(2, 10)
            )
          ),
          column(
            width = 4,
            align = "left",
            tags$div(
              title = "Select a method to perform evaluation. MDL performs information cost evaluation finding minimum description length (MDL). W/B cluster var performs evaluation aiming to find the cross-over point of within and between cluster variance.",
              radioButtons(
                "evalmethod",
                "Evaluation method:",
                choices = list("MDL" = 1, "W/B cluster var" = 2),
                selected = 1,
                inline = F
              )
            )
          ),
          column(
            width = 3,
            align = "left",
            uiOutput("evaldep")
          )
        ),
        HTML('<br>'),
        actionButton("runeval", "Run evaluation"),
        tags$hr(),
        plotOutput("evalplot"),
        textOutput("evalNclust")
      )
    })
    updateTabsetPanel(session, inputId = "outputs", selected = "Evaluate")
  })
  
  observeEvent(input$savecurrent, {
    save_current()
  })
  
  observeEvent(input$textgrid, {
    withProgress(gen_textgrid(), message = "Generating textgrids...")
  })
  
  observeEvent(input$runeval, {
    output$evalplot <- renderPlot({
      withProgress(evaluate(), message = "Running evaluation...")
    })
    updateTabsetPanel(session, inputId = "outputs", selected = "Evaluate")
    output$evalNclust <-
      renderText(
        if (isolate(input$evalmethod) == 1) {
          paste0("MDL at ",
                        evalNclust,
                        " clusters.")
        } else {
          paste0("")
        })
    })
  
  
  # functions ####
  
  tsf0 <- function() {
    mhsF0(
      f,
      beginTime = Tmin,
      endTime = Tmax,
      minF = as.numeric(input$pmin),
      maxF = as.numeric(input$pmax),
      windowShift = as.numeric(input$timestep),
      minProb = as.numeric(input$f0fit),
      toFile = F
      )
  }
  
  tsdB <- function() {
    rmsana(
      f,
      beginTime = Tmin,
      endTime = Tmax,
      toFile = F
    )
  }
    
  clustprep <- function() {
    withProgress(message = "Running cluster analysis...", {
      datacast <-
        as.data.frame(fread(
          file.path("www", "data_wide.csv"),
          sep = ",",
          header = T,
          stringsAsFactors = F
        ))
      incProgress(1 / 4)
      if (input$distm == "euclidean") {
        dist(datacast[, stepone:((stepone - 1) + stepsN)], method = input$distm) ->> distance_matrix
      }
      if (input$distm == "rmse") {
        rmse_distance <-function (r1, r2) {
          rmse(r1,r2)
        }
        as.matrix(datacast[, stepone:((stepone - 1) + stepsN)]) -> datacastmat
        rownames(datacastmat) <- 1:nrow(datacast)
        dist_make(datacastmat, rmse_distance) ->> distance_matrix
      }
      if (input$distm == "mase") {
        mase_distance <-function (r1, r2) {
                 mase(r1,r2)
                 }
        as.matrix(datacast[, stepone:((stepone - 1) + stepsN)]) -> datacastmat
        rownames(datacastmat) <- 1:nrow(datacast)
        dist_make(datacastmat, mase_distance) ->> distance_matrix
      }
      if (input$distm == "dtw_basic") {
        proxy::dist(datacast[, stepone:((stepone - 1) + stepsN)], method = input$distm) ->> distance_matrix
        stats::as.dist(distance_matrix) -> distance_matrix
      }
      if (input$distm == "cor" | input$distm == "acf") {
        TSDatabaseDistances(datacast[, stepone:((stepone - 1) + stepsN)], distance = input$distm) ->> distance_matrix
      }
      if (int_available==1){
        if (input$include_int==T){
        dist(datacast[, (stepone + stepsN):(stepone + stepsN + stepsN - 1)], method = "euclidean") -> distance_matrix_dB
        scale01 <- function(x){(x-min(x))/(max(x)-min(x))}
        scale01(distance_matrix) ->> distance_matrix
        scale01(distance_matrix_dB) -> distance_matrix_dB
        distance_matrix + distance_matrix_dB ->> distance_matrix
        }}
      if (input$include_dur==T){
        dist(datacast$end-datacast$start, method = "euclidean") -> distance_matrix_dur
        scale01 <- function(x){(x-min(x))/(max(x)-min(x))}
        scale01(distance_matrix) ->> distance_matrix
        scale01(distance_matrix_dur) -> distance_matrix_dur
        distance_matrix + distance_matrix_dur ->> distance_matrix
      }
      incProgress(1 / 4)
      hclust_avg <<- hclust(distance_matrix, method = input$sellink)
      ggdendrogram(hclust_avg, theme_dendro = T, labels = F) +
        theme(
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          strip.text = element_text(size = 20)) ->> dendro
      incProgress(1 / 4)
      suppressMessages(ggsave(file.path("www", "dendro.png"), dendro))
    })
  }
  
  clusttab <- function() {
    gc()
    datacast <-
      as.data.frame(fread(
        file.path("www", "data_wide.csv"),
        sep = ",",
        header = T,
        stringsAsFactors = F
      ))
    cut_avg <<- cutree(hclust_avg, k = numbclust)
    datacast <- mutate(datacast, cluster = cut_avg)
    mean(datacast$end - datacast$start) -> mdur
    if (mdur > 1) {
      1 ->> evaldep
    }
    if (mdur > 0.7 && mdur <= 1) {
      2 ->> evaldep
    }
    if (mdur > 0.45 && mdur <= 0.7) {
      3 ->> evaldep
    }
    if (mdur > 0.15 && mdur <= 0.45) {
      4 ->> evaldep
    }
    if (mdur > 0 && mdur <= 0.15) {
      5 ->> evaldep
    }
    fwrite(datacast, file.path("www", "data_wide.csv"))
    # uncomment line below to print distribution table based on interval labels; can be used to label relevant conditions/variables
    #  print(table(datacast$interval_label,datacast$cluster))
    output$data_long <- renderTable({
      data <-
        as.data.frame(fread(
          file.path("www", "data_long.csv"),
          sep = ",",
          header = T,
          stringsAsFactors = F
        ))
      data
    })
    output$data_wide <- renderTable({
      if (input$gettab == F && input$getplot == F)
        return(NULL)
      datacast <-
        as.data.frame(fread(
          file.path("www", "data_wide.csv"),
          sep = ",",
          header = T,
          stringsAsFactors = F
        ))
      datacast <-
        subset(datacast, select = c(0:(stepone - 1), cluster, stepone:(ncol(datacast)-1)))
    })
    output$goevaluate <- renderUI({
      tags$div(title = "Load the evaluation tab", actionButton("goevaluate", "Evaluate"))
    })
    output$savecurrent <- renderUI({
      tags$div(title = "Save plot and data of analysis with currently chosen number (N) of clusters (filenames: dendrogram.png, data_long_N.csv, data_wide_N.csv, table_N.csv, plot_N.png, evaluation_plot.png, evaluation_table.csv)", actionButton(
        "savecurrent",
        paste0("Save this (", numbclust, " clusters)")
      ))
    })
    output$textgrid <- renderUI({
      tags$div(title = "Generate TextGrids with the current clusters annotated for each contour interval.", actionButton("textgrid", "Generate TextGrids"))
    })
    sevt = c()
    for (c in 1:numbclust) {
      sev = c()
      for (stp in 0:(stepsN - 1)) {
        sd(subset(datacast[stp + stepone], datacast$cluster == c)[, 1]) /
          sqrt(nrow(subset(datacast, datacast$cluster == c))) -> se
        append(sev, se) -> sev
      }
      append(sevt, mean(sev)) -> sevt
    }
    rbind(table(datacast$cluster), sevt) -> table
    round(table, 2) -> table
    rownames(table) <- c("N", "M se")
    flagged = c()
    for (c in (1:numbclust)) {
      append(flagged,
             ifelse(
               table[1, c] == 1 |
                 table[2, c] > 2 * median(sevt, na.rm = T),
               yes = 1,
               no = 0
             )) -> flagged
    }
    rbind(table, flagged) -> table
    rem_clust = c()
    for (c in (1:numbclust)) {
      ifelse(table[3, c] != 1, "", append(rem_clust, c) -> rem_clust)
    }
    rem_clust ->> rem_clust
    output$subset <- renderUI({
      selclust()
    })
    fwrite(as.data.frame(table), file.path("www", "table.csv"), row.names = T)
    table
  }
  
  clustplot <- function() {
    incProgress(1 / 5)
    datacast <-
      as.data.frame(fread(
        file.path("www", "data_wide.csv"),
        sep = ",",
        header = T,
        stringsAsFactors = F
      ))
    data.table::melt(as.data.table(datacast),
         id.vars = c("filename", "interval_label", "start", "end", "cluster"),
         measure.vars = stepone:(stepone+stepsN-1)) -> dataplot
    as.numeric(dataplot$variable) -> dataplot$variable
    brks = c((stepsN * 0.25), (stepsN * 0.5), (stepsN * (0.75)), stepsN)
    panel_text <-
      data.frame(label = paste0("n=", as.character(as.data.frame(
        table(datacast$cluster)
      )[, 2])),
      cluster = 1:numbclust)
    if (input$include_dur==T){
      panel_text$label <- paste0(panel_text$label,", d=",round(as.data.frame(datacast %>% group_by(cluster) %>% summarise(mean=mean(end-start)))$mean,2))
    }
    wrap_labs <- (paste0(panel_text$cluster, " (", panel_text$label, ")"))
    attributes(wrap_labs)$names <- panel_text$cluster
    incProgress(1 / 5)
        ggplot(dataplot, aes(x = variable, y = value)) +
          stat_summary(
            fun = mean,
            group = "cluster",
            geom = "line",
            colour = "#02abea",
            size = 1,
            show.legend = F
          ) +
          stat_summary(
            fun.data = mean_sdl,
            group = "cluster",
            geom = "ribbon",
            alpha = .2,
            show.legend = F
          ) +
          scale_x_continuous(breaks = brks) +
          facet_wrap( ~ cluster, ncol = 4
                      ,labeller = as_labeller(wrap_labs)
                      ) +
          xlab("measurement number") +
          ylab(ylb) +
          theme(
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 20),
            strip.text = element_text(size = 20)
          ) -> plot
        incProgress(1 / 5)
        if (int_available==1){
          if (input$include_int==T){
          data.table::melt(as.data.table(datacast),
                           id.vars = c("filename", "interval_label", "start", "end", "cluster"),
                           measure.vars = (stepone+stepsN):(stepone+stepsN+stepsN-1),
                           value.name = "dB")$dB -> dataplot$dB
          rescale(x = dataplot$dB, to =  c(min(dataplot$value), max(dataplot$value))) -> dataplot$dB_resc
          a.diff <- max(dataplot$value) - min(dataplot$value)
          b.diff <- max(dataplot$dB) - min(dataplot$dB)
          a.min <- min(dataplot$value)
          b.min <- min(dataplot$dB)
          plot + 
            scale_y_continuous(ylb, sec.axis = sec_axis(~((. -a.min) * b.diff / a.diff) + b.min,name = "dB (standardized)")) +
            stat_summary(mapping = aes(y = dB_resc), 
                         data = dataplot,fun = mean, group = "cluster", geom = "line", colour = "#00ff00", size = 1, show.legend = F) +
            stat_summary(
              mapping = aes(y = dB_resc), 
              data = dataplot, fun.data = mean_sdl, group = "cluster", geom = "ribbon", alpha = .2, show.legend = F) +
            theme(axis.title.y.left = element_text(colour="#02abea")) +
            theme(axis.title.y.right = element_text(colour = "#00ff00")) -> plot
          incProgress(1 / 5)
        }}
        suppressMessages(ggsave(filename = file.path("www", "plot.png"), plot = plot))
    # uncomment line below for high res plot (tiff print quality)
    #   ggsave(filename = "plot.tiff", plot = plot, dpi=300, compression = 'lzw')
    plot
  }
  
  readdata <- function() {
    if (is.null(input$file_input) &
        file.exists(file.path("www", "data_long.csv")) == F)
      return(NULL)
    data <-
      as.data.frame(fread(
        file.path("www", "data_long.csv"),
        sep = ",",
        header = T,
        stringsAsFactors = F
      ))
    nrow(data) ->> nrowinput
    length(levels(as.factor(data$stepnumber))) ->> stepsN
    if (exists('df.gs')) {
      fileprop <- paste0("File loaded from time-series f0 analysis.")
    } else{
      fileprop <-
        paste0("File loaded using these arguments: ", sep, saf, enc, skn)
    }
   
    if (isTRUE(input$prep_data) && input$prep_data == T) {
      paste0(
        Sys.time(),
        ": cleaned datafile saved to 'data_long.csv' in ",
        file.path(getwd(), "www")
      ) -> filesave
      "0 rows marked for error removal" -> Nerr
    } else {
      paste0(Sys.time(),
             ": datafile saved to 'data_long.csv' in ",
             file.path(getwd(), "www")) -> filesave
      0 -> data$err
      suppressWarnings(as.numeric(data$f0)) -> data$f0
      if (abs(sum(na.omit(data$f0[data$f0 < 0]))) == 0) {
        for (r in 1:nrow(data)) {
          if (data$filename[r] == "" ||
              data$interval_label[r] == "" ||
              grepl("--undefined--", data$f0[r], fixed = T) ||
              is.na(data$f0[r])) {
            "1" -> data$err[data$filename == data$filename[r] &
                              data$start == data$start[r]]
          }
          if (data$stepnumber[r] != 1 && is.na(data$f0[r]) == F) {
            (12 * (log2(data$f0[r] / data$f0[r - 1]))) * (1 / (data$step[r] - data$step[r -
                                                                                          1])) -> d
            if (d > 72 | d < -96) {
              "1" -> data$err[data$filename == data$filename[r] &
                                data$start == data$start[r]]
            }
          }
        }
        if (nrow(data[data$err == "1",]) > 0) {
          suggest_cln <<- T
          updateCheckboxInput(session,"rem_empty", value = suggest_cln)
          paste0("WARNING: ",
                 nrow(data[data$err == "1",]),
                 " rows marked for error removal!") -> Nerr
          fwrite(data, file.path("www", "data_long.csv"))
        }else{
          suggest_cln <<- F
          updateCheckboxInput(session,"rem_empty", value = suggest_cln)
          "0 rows marked for error removal" -> Nerr
        }
      }else{
        if (input$spkdiff == 1 && rem_empty == T){
        "WARNING: could not run error detection on f0 (negative values in data)." -> Nerr
        }else{
          suggest_cln <<- F
          updateCheckboxInput(session,"rem_empty", value = suggest_cln)
          "0 rows marked for error removal" -> Nerr
        }
      }}
    
      filesave <-
        ifelse(
          file.exists("dendrogram.png"),
          paste0(
            filesave,
            "\n\n",
            Sys.time(),
            ": dendrogram saved to 'dendrogram.png' in ",
            file.path(getwd(), "www")
          ),
          filesave
        )
      filesave <-
        ifelse(
          file.exists(file.path("www", "table.csv")),
          paste0(
            filesave,
            "\n\n",
            Sys.time(),
            ": table saved to 'table.csv' in ",
            file.path(getwd(), "www")
          ),
          filesave
        )
      filesave <-
        ifelse(
          file.exists(file.path("www", "plot.png")),
          paste0(
            filesave,
            "\n\n",
            Sys.time(),
            ": plot saved to 'plot.png' in ",
            file.path(getwd(), "www")
          ),
          filesave
        )
      filesave <-
        ifelse(
          file.exists(file.path("www", "data_wide.csv")),
          paste0(
            filesave,
            "\n\n",
            Sys.time(),
            ": wide datafile with clusters annotated saved to 'data_wide.csv' in ",
            file.path(getwd(), "www")
          ),
          filesave
        )
      dashedline <- "--------------------------------------------"
      stepsN <- length(levels(as.factor(data$stepnumber)))
      steps <- paste0(stepsN, " measurement points per contour")
      spkN <- length(levels(as.factor(data$filename)))
      spk <- paste0(spkN, " speakers (filenames)")
      sscontours <-
        if (file.exists(file.path("www", "data_wide.csv"))) {
          datacast <-
            as.data.frame(
              fread(
                file.path("www", "data_wide.csv"),
                sep = ",",
                header = T,
                stringsAsFactors = F
              )
            )
          paste0(" (",
                 nrow(datacast),
                 " left after subsetting - ",
                 round((
                   nrow(datacast) / (nrowinput / stepsN)
                 ) * 100, 1),
                 "%)")
        } else{
          ""
        }
      ncontours <-
        paste0(nrowinput / stepsN, " contours in uploaded data", sscontours)
      updateTabsetPanel(session, inputId = "outputs", selected = "summary")
      paste("F0 scale: ", ylb, ".", sep = "") ->> scl
      paste(
        ifelse(
          file.exists(file.path("www", "data_wide.csv")),
          paste(fileprop, cln, stconv, jkm, scl, sep = "\n\n"),
          fileprop
        ),
        filesave,
        dashedline,
        ncontours,
        steps,
        spk,
        dashedline,
        Nerr,
        sep = "\n\n"
      ) -> summary
      write_file(summary, file.path("www", "summary.txt"), append = F)
      paste0(summary)
  }
  

    selclust <- function() {
      if (exists('rem_clust')) {
        checkboxGroupInput(
          "subset",
          "Remove observations in these clusters:",
          choices = 1:numbclust,
          selected = rem_clust,
          inline = T
        )
      }
    }
    
    evaluate <- function() {
      incProgress(1 / 5)
      
      for (r in isolate(input$evalslider[1]):isolate(input$evalslider[2])) {
        datacast <-
          as.data.frame(
            read.csv(
              file.path("www", "data_wide.csv"),
              sep = ",",
              header = T,
        row.names = NULL,
              stringsAsFactors = F
      )
          )
        cut_avg <<- cutree(hclust_avg, k = r)
        datacast <- mutate(datacast, cluster = cut_avg)
        
        if (isolate(input$evalmethod) == 2) {
          if (r == isolate(input$evalslider[1])) {
            as.data.frame(matrix(
              nrow = (
                isolate(input$evalslider[2]) - isolate(input$evalslider[1]) + 1
              ),
              ncol = 3
            )) ->> d.eval
            c("round", "within", "between") ->> colnames(d.eval)
            isolate(input$evalslider[1]):isolate(input$evalslider[2]) ->> d.eval$round
          }
          c() -> wc
          for (c in 1:r) {
            c() -> w
            for (m in stepone:(stepone + stepsN - 1)) {
              append(w, sd(datacast[datacast$cluster == c, m])) -> w
            }
            append(wc, mean(w)) -> wc
          }
          mean(na.omit(wc)) ->> d.eval$within[d.eval$round == r]
          
          c() -> bmvar
          for (m in stepone:(stepone + stepsN - 1)) {
            c() -> bm
            for (c in 1:r) {
              mean(na.omit(datacast[datacast$cluster == c, m])) -> b
              append(bm, b) -> bm
            }
            append(bmvar, abs(max(bm) - min(bm))) -> bmvar
          }
          mean(bmvar) ->> d.eval$between[d.eval$round == r]
          if (r == isolate(input$evalslider[2])) {
            rescale(as.numeric(d.eval$within)) ->> d.eval$within
            rescale(as.numeric(d.eval$between)) ->> d.eval$between
            fwrite(as.data.table(d.eval), file.path("www", "evaltable.csv"), row.names = T)
          }
        }
        fwrite(datacast, file.path("www", paste0("data_eval_", r, ".csv")))
      }
      
      if (isolate(input$evalmethod) == 1) {
        Epsilon <- 1
        isolate(input$smoothing) -> Smoothing
        N <-
          isolate(input$evalslider[2]) - isolate(input$evalslider[1]) + 1
        PathFmt <- file.path("www", "data_eval_%d.csv")
        
        getNormalParameters <- function(df) {
          aggregate(df[substr(names(df), 1, 1) == "X"], by = list(df$cluster), FUN = mean) -> mns
          mns$fn <- "mean"
          colnames(mns)[1] <- "cluster"
          aggregate(df[substr(names(df), 1, 1) == "X"], by = list(df$cluster), FUN = sd) -> sds
          colnames(sds)[1] <- "cluster"
          sds$fn <- "sd"
          rbind(mns, sds) -> df
          df
        }
        
        incProgress(1 / 5)
        
        loadData <- function(i, template) {
          i %>%
            (function(n)
              sprintf(template, i)) %>%
            fread(
              file = .,
              stringsAsFactors = FALSE,
              encoding = "UTF-8",
              data.table = F
            )
        }
        
        precisionInterval <- function(value) {
          floor(value / Epsilon) * Epsilon -> lb
          c(lb, lb + Epsilon) ->> lbeps
          lbeps
        }
        
        getRelInfoOfSample <- function(value, parameters) {
          precisionInterval(value) %>%
            pnorm(mean = parameters[parameters$fn == "mean", 1],
                  sd = parameters[parameters$fn == "sd", 1]) ->> x
          - log(x[2] - x[1])
        }
        
        incProgress(1 / 5)
        
        getRelInfoOfToken <- function(row, parameters) {
          (1:Xpoints) %>%
            sprintf("X%d", .) %>%
            map_dbl(function(Xfield) {
              getRelInfoOfSample(row[, Xfield], parameters[, c(Xfield, "fn")])
            }) %>%
            sum()
        }
        
        getRelInfoOfData <- function(df, parameters) {
          (1:length(df$cluster)) %>%
            map_dbl(function(ri) {
              getRelInfoOfToken(df[ri, ],
                                parameters[parameters$cluster == df$cluster[ri], ])
            }) %>%
            sum()
        }
        
        getInfo <- function(v) {
          data.frame(Ct = 1, v = v) %>%
            aggregate(Ct ~ v, ., sum) ->> w
          nlogn <- function(x)
            x * log(x)
          nlogn(sum(w)) - sum(nlogn(w))
        }
        
        mkInfoCost <- function(n) {
          n %>%
            loadData(PathFmt) -> df
          Xpoints <<- ncol(df[, substr(names(df), 1, 1) == "X"])
          df %>%
            getNormalParameters() ->> params
          df1 <- df
          df1$cluster <- 1
          df1 %>%
            getNormalParameters() ->> paramsAll
          params1 <<- params[params$fn == "mean", ]
          params1$cluster <- 1
          getRelInfoOfData(params1, paramsAll) / Smoothing +
            getInfo(df$cluster) +
            getRelInfoOfData(df, params) / Smoothing
        }
        
        incProgress(1 / 5)
        
        (isolate(input$evalslider[1]):isolate(input$evalslider[2])) %>%
          map_dbl(mkInfoCost) %>%
          data.frame(x = (
            isolate(input$evalslider[1]):isolate(input$evalslider[2])
          ),
          y = .) -> dx
        dx$x[which.min(dx$y)] ->> evalNclust
        fwrite(as.data.frame(dx), file.path("www", "evaltable.csv"), row.names = T)
        
        incProgress(1 / 5)
        
        ggplot(data = dx) +
            geom_line(aes(x = x, y = y)) +
            xlab("N clusters") +
            ylab("Information cost") +
            annotate(
              geom = "point",
              x = evalNclust,
              y = min(dx$y),
              size = 2
            ) +
            theme_bw(base_size = 20) ->> evalplot
        suppressMessages(ggsave(
          filename = file.path("www", "evalplot.png"),
          plot = evalplot
        ))
      }
      if (isolate(input$evalmethod) == 2) {
        ggplot(d.eval, aes(round)) +
          geom_line(mapping = aes(y = within, linetype = "within")) +
          geom_line(mapping = aes(y = between, linetype = "between")) +
          xlab("N clusters") +
          ylab("Scaled variance") +
          scale_linetype_manual("Cluster variance", values = c("within" =
                                                                  1, "between" = 2)) +
          theme_bw(base_size = 20) ->> evalplot
        suppressMessages(ggsave(
          filename = file.path("www", "evalplot.png"),
          plot = evalplot
        ))
      }
      plot(evalplot)
    }
    
    save_current <- function() {
      renderPlot({clustplot()}, height = ht)
      if (dir.exists("saved") == F) {
        dir.create("saved")
      }
      file.copy(file.path("www", "plot.png"),
                file.path("saved", paste0("plot_", numbclust, ".png")),
                overwrite = T)
      file.copy(file.path("www", "data_long.csv"),
                file.path("saved", paste0("data_long_", numbclust, ".csv")),
                overwrite = T)
      file.copy(file.path("www", "data_wide.csv"),
                file.path("saved", paste0("data_wide_", numbclust, ".csv")),
                overwrite = T)
      suppressMessages(ggsave(file.path("saved", "dendrogram.png"), dendro))
      file.copy(
        file.path("www", "evalplot.png"),
        file.path("saved", "evaluation_plot.png"),
        overwrite = T
      )
      file.copy(file.path("www", "table.csv"),
                file.path("saved", paste0("table_", numbclust, ".csv")),
                overwrite = T)
      file.copy(
        file.path("www", "evaltable.csv"),
        file.path("saved", "evaluation_table.csv"),
        overwrite = T
      )
      file.copy(file.path("www", "summary.txt"),
                file.path("saved", paste0("summary_", numbclust, ".txt")),
                overwrite = T)
      showNotification("Current analysis saved.")
    }
    
    gen_textgrid <- function() {
      data <-
        as.data.frame(fread(
          file.path("www", "data_long.csv"),
          sep = ",",
          header = T,
          stringsAsFactors = F
        ))
      datacast <-
        as.data.frame(fread(
          file.path("www", "data_wide.csv"),
          sep = ",",
          header = T,
          stringsAsFactors = F
        ))
      if (exists("filenames")) {
        rm("filenames")
      }
      levels(as.factor(data$filename)) ->> filenames
      if (dir.exists("saved") == F) {
        dir.create("saved")
      }
      for (spk in levels(as.factor(data$filename))) {
        incProgress(1 / length(levels(as.factor(
          data$filename
        ))))
        sink(file.path("saved", paste0(spk, ".TextGrid")))
        cat("File type = \"ooTextFile\"")
        cat("\n")
        cat("Object class = \"TextGrid\"")
        cat("\n")
        cat("\n")
        cat("xmin = 0")
        cat("\n")
        xmax <- max(datacast$start[datacast$filename == spk]) + .5
        cat(paste0("xmax = ", xmax))
        cat("\n")
        cat("tiers? <exists>")
        cat("\n")
        cat("size = 1")
        cat("\n")
        cat("item []:")
        cat("\n")
        cat("    item [1]:")
        cat("\n")
        cat("        class = \"IntervalTier\"")
        cat("\n")
        cat("        name = \"cluster\"")
        cat("\n")
        cat("        xmin = 0")
        cat("\n")
        cat(paste0("        xmax = ", xmax))
        cat("\n")
        cat(paste0("        intervals: size = ", nrow(arrange(
          datacast[datacast$filename == spk, ], start
        ))))
        cat("\n")
        for (contour in 1:nrow(arrange(datacast[datacast$filename == spk, ], start))) {
          cat(paste0("        intervals [", contour, "]:"))
          cat("\n")
          cat(paste0("            xmin = ", subset(
            arrange(datacast, start), filename == spk
          )[contour, "start"]))
          cat("\n")
          cat(paste0("            xmax = ", subset(
            arrange(datacast, start), filename == spk
          )[contour, "end"]))
          cat("\n")
          cat(paste0("            text = \"", subset(
            arrange(datacast, start), filename == spk
          )[contour, "cluster"], "\""))
          cat("\n")
        }
        sink()
      }
    }
    
    session$onSessionEnded(function() {
      stopApp()
    })
    
  }
  
  
  
  shinyApp(
    ui,
    server,
    onStart = function() {
      c() ->> colsdf
      0 ->> int_available
      8 ->> numbclust
      "No cleaning applied." ->> cln
      "" ->> jkm
      "" ->> ylb
      "" ->> rem_empty
      cat("Running contour clustering app\n")
      onStop(function() {
          if (oldwww == 0) {
            unlink("www", recursive = T)
            }
        cat("Stopped contour clustering app.\n")
        suppressWarnings(rm(list = c("df.g","df.gs","pnglist","tiers","colsdf","colsstd",
                                     "d.eval", "oldwww",
                                     "dendro","evalplot","hclust_avg","int_available","params","params1","paramsAll","suggest_cln","w"),envir = .GlobalEnv))
        suppressWarnings(rm(list = c("colX","f","inDir","int_available","mD","NAmsg","rempng","s","snd_ext","Tmax","Tmin","cln","cut_avg","distance_matrix","enc","evaldep","evalNclust","ht","jkm","lbeps","nrowinput","numbclust",
           "rem_clust","rem_empty","saf","scl","sep","skn","stconv","stepone","stepsN","x","Xpoints","ylb","ysc"),envir = .GlobalEnv))
      })
    },
    options = list("quiet" = T)
  )