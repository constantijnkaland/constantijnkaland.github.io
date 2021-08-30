# General information
# This script performs hierarchical cluster analysis on F0 contours measured with "time-series_F0.praat". 
# The code below is largely uncommented. Additional mouse-hoover help comments are provided for some buttons in the app. Detailed script comments can be found in the no-gui version of this script 'contour_clustering_.R', which has essentially the same functionality.
# A theoretical motivation is given in the accompanying paper. 
# Usage guidelines are given in the accompanying manual.
#
# Developed and tested using package versions:
# dplyr_1.0.7
# reshape2_1.4.4
# reshape_0.8.8
# ggplot2_3.3.5
# shiny_1.6.0
# ggdendro 0.1.22
#
# Constantijn Kaland, August 2021.
# https://constantijnkaland.github.io/contourclustering/


library(ggplot2)
library(reshape)
library(reshape2)
library(dplyr)
library(shiny)
library(ggdendro)

ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput("file_input", "Choose datafile",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"),width = '75%'
        ),
        uiOutput("sepchoice"),
        uiOutput("strfact"),
        uiOutput("filenc"),
        uiOutput("skpnl"),
        textOutput("choose"),
        tags$hr(),
        uiOutput("rem_empty"),
        uiOutput("semitone_conv"),
        uiOutput("jump_header"),
        uiOutput("jump_margin"),
        uiOutput("spkdiff_header"),
        uiOutput("spkdiff"),
        uiOutput("prep_data"),
        tags$hr(),
        uiOutput("nclust"),
        fluidRow(
          column(width = 5, align = "center", uiOutput("getdendro")),
          column(width = 4, align = "center", uiOutput("gettab")),
          column(width = 3, align = "center", uiOutput("getplot"))
          ),
        tags$hr(),
        uiOutput("subset"),
        uiOutput("dosubset"),
        tags$hr(),
        fluidRow(
          column(width = 6, align = "center", uiOutput("savecurrent")),
          column(width = 6, align = "center", uiOutput("textgrid"))
        )
      ),
      mainPanel(
        tabsetPanel(id = "outputs", type = "tabs",
                    tabPanel(value = "summary", 'Status', verbatimTextOutput("summary")),
                    tabPanel("Data (long)", tableOutput("data_long")),
                    tabPanel("Dendrogram", plotOutput("dendro")),
                    tabPanel("Table", tableOutput("table")),
                    tabPanel("Plot", plotOutput("plot")),
                    tabPanel("Data (wide)", tableOutput("data_wide"))
        )
        
      )
    )
  )
  

server <- function(input, output, session) {

  
# outputs ####
    
  output$choose <- reactive({
    if(is.null(input$file_input))
    {
      "! Before uploading: select correct file properties above."
    }
    else
    {
      inFile <- input$file_input
      if (input$sepchoice==1) {sep = ","}
      if (input$sepchoice==2) {sep = "\t"}
      if (input$strfact==1) {saf = T}
      if (input$strfact==2) {saf = F}
      if (input$filenc==1) {fenc = "UTF-8"}
      if (input$filenc==2) {fenc = "utf16"}
      if (input$skpnl==1) {snl = T}
      if (input$skpnl==2) {snl = F}
      data <- as.data.frame(read.csv(inFile$datapath,sep=sep, header = T,row.names = NULL,stringsAsFactors = saf,fileEncoding = fenc, skipNul = snl))
      write.table(data, "data_long.csv",sep = ",",row.names = F)
      updateTabsetPanel(session, inputId = "outputs", selected = "summary")
      paste("File uploaded. (", 
            ifelse(input$sepchoice==1,"sep=comma, ", "sep=tab, "),
            ifelse(input$strfact==1,"SaF=T, ", "SaF=F, "),
            ifelse(input$filenc==1,"enc=utf8, ", "enc=utf16, "),
            ifelse(input$skpnl==1,"skN=T)", "skN=F)"),
            sep = "")
    }
  })

  output$sepchoice<-renderUI({
    if (is.null(input$file_input)){
      tags$div(title="Default output from Praat script is comma-separated.", radioButtons("sepchoice", label = "Separator", choices = list("comma" = 1,"tab" = 2), selected = 1, inline = T))
    }else{
      return(NULL)
    }
  })
  
  output$strfact<-renderUI({
    if (is.null(input$file_input)){
      tags$div(title="Logical: should character vectors be converted to factors?", radioButtons("strfact", label = "StringsAsFactors", choices = list("true" = 1,"false" = 2),selected = 2, inline = T))
    }else{
      return(NULL)
    }
  })
  
  output$filenc<-renderUI({
    if (is.null(input$file_input)){
      tags$div(title="Choose UTF-16 for challenging ortography in the textgrid labels", radioButtons("filenc", label = "File encoding", choices = list("UTF-8" = 1,"UTF-16" = 2),selected = 1, inline = T))
    }else{
      return(NULL)
    }
  })
  
  output$skpnl<-renderUI({
    if (is.null(input$file_input)){
      tags$div(title="Logical: should nuls be skipped?", radioButtons("skpnl", label = "SkipNul", choices = list("true" = 1,"false" = 2),selected = 2, inline = T))
    }else{
      return(NULL)
    }
  })
  
  output$rem_empty<-renderUI({
    if (is.null(input$file_input))
      return(NULL)
    checkboxInput("rem_empty", "clean data (remove NA and f0 errors)", FALSE)
  })
  
  output$semitone_conv<-renderUI({
    if (is.null(input$file_input))
      return(NULL)
    tags$div(title="Convert to semitones re 50 Hz: f0(ST) = log10((f0(Hz)/50)*39.87", checkboxInput("semitone_conv", label = "convert f0 Hertz values to semitones", value = T))
  })
  
  output$jump_header<-renderUI({
    if (is.null(input$file_input))
      return(NULL)
    "Allowed % change after octave jump correction:"
    })

  output$jump_margin<-renderUI({
    if (is.null(input$file_input))
      return(NULL)
    numericInput("jump_margin", label = NULL, value = 10,min = 0, max = 100,width = '20%')
  })
  
  output$spkdiff_header<-renderUI({
    if (is.null(input$file_input))
      return(NULL)
    "Correct speaker differences:"
  })
  
  output$spkdiff<-renderUI({
    if (is.null(input$file_input))
      return(NULL)
    tags$div(title="Only apply when number of speakers is correctly detected. 1 = none, 2 = f0–mean(f0.spk), 3 = f0–mean(f0.spk) / sd(f0.spk), 4 = f0–min(f0.spk) / max(f0.spk)–min(f0.spk), 5 = f0–median(f0.spk) / 75q(f0.spk)–25q(f0.spk), 6 = log2(f0.spk/media(f0.spk))", 
             selectInput("spkdiff", label = NULL, 
                          choices = list("1: none" = 1,
                                         "2: subtract mean" = 2,
                                         "3: standardise" = 3,
                                         "4: min-max normalize" = 4,
                                         "5: robust scaler" = 5,
                                         "6: octave-median scaled" = 6),
                         selected = 1,multiple = F,width = "75%"))
  })
  
  output$prep_data<-renderUI({
    if (is.null(input$file_input))
      return(NULL)
    tags$div(title="Applies cleaning/octave jump range selection/speaker correction to the data. Apply once after uploading data.", 
             actionButton("prep_data", "Apply to data"))
  })
  
  output$getdendro<-renderUI({
    if (is.null(input$file_input))
      return(NULL)
    actionButton("getdendro", "Dendrogram")
  })


# observers ####
  
  observeEvent(input$file_input, {   
    output$summary <- renderText({
      hideTab(inputId = "outputs", target = "Data (long)")
      hideTab(inputId = "outputs", target = "Dendrogram")
      hideTab(inputId = "outputs", target = "Table")
      hideTab(inputId = "outputs", target = "Plot")
      hideTab(inputId = "outputs", target = "Data (wide)")
      readdata()
    })
  })
  
  observeEvent(input$prep_data, {
    data <- as.data.frame(read.csv("data_long.csv",sep=",", header = T,row.names = NULL,stringsAsFactors = F))
    gsub(pattern = ":;",replacement = ",",x = as.character(data$interval_label)) -> data$interval_label
    "(Hz)" ->> ysc
    if (input$rem_empty==T){
      subset(data, data$filename !="")-> data
      subset(data, data$interval_label !="")-> data
      droplevels(data) -> data
      subset(data, !grepl("--undefined--", data$f0))-> data
      subset(data, data$f0 !="")-> data
    }
    if (input$semitone_conv==T){
      log10((as.numeric(as.character(data$f0))/50))*39.87 -> data$f0
      "(ST)" ->> ysc
    }
    
    subset(data, between(as.numeric(as.character(data$jumpkilleffect)),left = (1-(input$jump_margin/100)),right = (1+(input$jump_margin/100)))) -> data
    if (input$spkdiff==1){
      paste("f0 ",ysc,sep="") ->> ylb
    }
    if (input$spkdiff==2){
      paste("Speaker mean corrected f0 ",ysc,sep="") ->> ylb
      for (spk in levels(as.factor(data$filename))) {
        mean(as.numeric(as.character(data$f0[data$filename==spk]))) -> m
        (as.numeric(as.character(data$f0[data$filename==spk]))-m) -> data$f0[data$filename==spk]
      }
    }
    if (input$spkdiff==3){
      "Speaker standardized f0" ->> ylb
      for (spk in levels(as.factor(data$filename))) {
        mean(as.numeric(as.character(data$f0[data$filename==spk]))) -> m
        sd(as.numeric(as.character(data$f0[data$filename==spk]))) -> sd
        (as.numeric(as.character(data$f0[data$filename==spk]))-m)/sd -> data$f0[data$filename==spk]
      }
    }
    if (input$spkdiff==4){
      "Speaker min-max normalized f0" ->> ylb
      for (spk in levels(as.factor(data$filename))) {
        mean(as.numeric(as.character(data$f0[data$filename==spk]))) -> m
        min(as.numeric(as.character(data$f0[data$filename==spk]))) -> min
        max(as.numeric(as.character(data$f0[data$filename==spk]))) -> max
        (as.numeric(as.character(data$f0[data$filename==spk]))-min)/(max-min) -> data$f0[data$filename==spk]
      }
    }
    if (input$spkdiff==5){
      "Speaker robust scaled f0" ->> ylb
      for (spk in levels(as.factor(data$filename))) {
        median(as.numeric(as.character(data$f0[data$filename==spk]))) -> md
        quantile(as.numeric(as.character(data$f0[data$filename==spk])), 0.75) -> q75
        quantile(as.numeric(as.character(data$f0[data$filename==spk])), 0.25) -> q25
        (as.numeric(as.character(data$f0[data$filename==spk]))-md)/(q75-q25) -> data$f0[data$filename==spk]
      }
    }
    if (input$spkdiff==6){
      "Octave-median scaled f0" ->> ylb
      for (spk in levels(as.factor(data$filename))) {
        median(as.numeric(as.character(data$f0[data$filename==spk]))) -> md
        log2(as.numeric(as.character(data$f0[data$filename==spk]))/md) -> data$f0[data$filename==spk]
      }
    }
    write.table(data, "data_long.csv",sep = ",",row.names = F)
    stepsN <<- length(levels(as.factor(data$stepnumber)))
    dcast(data,filename+interval_label+start+end~stepnumber,value.var="f0") -> datacast
    write.table(datacast, "data_wide.csv",sep = ",",row.names = F)
    stepone <<- which( colnames(datacast)=="1" )
    output$data_long <- renderTable({
      data
    })
    output$data_wide <- renderTable({
      datacast
    })
    showTab(inputId = "outputs", target = "Data (long)")
    showTab(inputId = "outputs", target = "Data (wide)")
    updateTabsetPanel(session, inputId = "outputs", selected = "summary")
    output$summary <- renderText({readdata()})
  })
  
  observeEvent(input$getdendro, {
    output$nclust<-renderUI({
      sliderInput("nclust", "Number of clusters",min = 2,max = 50,value = numbclust,step = 1)
    })
    output$gettab<-renderUI({
      actionButton("gettab", "Table")
    })
    output$getplot<-renderUI({
      actionButton("getplot", "Plot")
    })
    output$dendro<-renderPlot({
      clustprep()
      plot(dendro)
    })
    showTab(inputId = "outputs", target = "Dendrogram")
    updateTabsetPanel(session, inputId = "outputs", selected = "Dendrogram")
  })
    
  observeEvent(input$gettab, {
    output$table<-renderTable({
      clustprep()
      clusttab()
    },rownames = T)
    showTab(inputId = "outputs", target = "Table")
    updateTabsetPanel(session, inputId = "outputs", selected = "Table")
  })
  
  observeEvent(input$getplot, {
    output$plot<-renderPlot({
      clustprep()
      clusttab()
      clustplot()
    },height = ht)
    showTab(inputId = "outputs", target = "Plot")
    updateTabsetPanel(session, inputId = "outputs", selected = "Plot")
  })
  
  observeEvent(input$nclust, {
    input$nclust ->> numbclust
    if (numbclust < 4){300 ->> ht}else{ceiling(numbclust/4)*300 ->> ht}
    output$dosubset<-renderUI({
      if (input$gettab==F && input$getplot==F)
        return(NULL)
      tags$div(title="Removes the selected clusters from the wide data only. Number of remaining contours after subsettings can be read from Status tab.",
               actionButton("dosubset", "Apply subsetting"))
    })
    output$subset<-renderUI({
      if (input$gettab==F && input$getplot==F)
        return(NULL)
      selclust()
    })
    clustprep()
    output$table<-renderTable({
      clusttab()
    },rownames = T)
    output$plot<-renderPlot({
      clusttab()
      clustplot()
    },height = ht)
  })
  
  observeEvent(input$subset, {
    as.vector(input$subset,mode = "numeric") -> rem_clust
    if(is.null(input$subset)) {rem_clust = c()}
    rem_clust ->> rem_clust
  })
  
  observeEvent(input$dosubset, {
    datacast <- as.data.frame(read.csv("data_wide.csv",sep=",", header = T,row.names = NULL,stringsAsFactors = F))
    for (clust in rem_clust) {
      subset(datacast, datacast$cluster!=clust) -> datacast
    }
    write.table(datacast, "data_wide.csv",sep = ",",row.names = F)
    clustprep()
    output$subset<-renderUI({
      selclust()
    })
    output$table<-renderTable({
      clusttab()
    },rownames = T)
    output$plot<-renderPlot({
      clusttab()
      clustplot()
    },height = ht)
  })
  
  observe({
    if (input$outputs=="summary"){
      output$summary <- renderText({
        readdata()
      })
    }
  })
  
  observeEvent(input$savecurrent, {
    save_current()
  })
  
  observeEvent(input$textgrid, {
    gen_textgrid()
  })

  

# functions ####
  
clustprep <- function(){
  datacast <- as.data.frame(read.csv("data_wide.csv",sep=",", header = T,row.names = NULL,stringsAsFactors = F))
  distance_matrix <- dist(datacast[,stepone:((stepone-1)+stepsN)], method = 'euclidean')
  hclust_avg <<- hclust(distance_matrix, method = 'complete')
  if (file.exists("dendro.png")){
  }else{
    ggdendrogram(hclust_avg, theme_dendro = T, labels = F) ->> dendro
    ggsave("dendro.png", dendro)
  }
}

clusttab <- function(){
  datacast <- as.data.frame(read.csv("data_wide.csv",sep=",", header = T,row.names = NULL,stringsAsFactors = F))
  cut_avg <<- cutree(hclust_avg, k = numbclust)
  datacast <- mutate(datacast, cluster = cut_avg)
  write.table(datacast, "data_wide.csv",sep = ",",row.names = F)
# uncomment line below to print distribution table based on interval labels; can be used to label relevant conditions/variables
#  print(table(datacast$interval_label,datacast$cluster))
  output$data_long <- renderTable({
    if (is.null(input$file_input))
      return(NULL)
    data <- as.data.frame(read.csv("data_long.csv",sep=",", header = T,row.names = NULL,stringsAsFactors = F))
    data
  })
  output$data_wide <- renderTable({
    if (is.null(input$file_input))
      return(NULL)
    if (input$gettab==F && input$getplot==F)
      return(NULL)
    datacast <- as.data.frame(read.csv("data_wide.csv",sep=",", header = T,row.names = NULL,stringsAsFactors = F))
    datacast <- subset(datacast, select=c(0:(stepone-1),cluster,stepone:(stepone+(stepsN-1))))
  })
  output$savecurrent<-renderUI({
    if (is.null(input$file_input))
      return(NULL)
    tags$div(title="Save plot and data of analysis with currently chosen number (N) of clusters (filenames: dendrogram.png, data_long_N.csv, data_wide_N.csv, table_N.csv, plot_N.png)", actionButton("savecurrent", paste("Save this (",numbclust," clusters)", sep="")))
  })
  output$textgrid<-renderUI({
    if (is.null(input$file_input))
      return(NULL)
    tags$div(title="Generate a textgrid per speaker with the current clusters annotated per contour interval.", actionButton("textgrid", "Generate textgrids"))
  })
  sevt = c()
  for (c in 1:numbclust) {
    sev = c()
    for (stp in 0:(stepsN-1)) {
      sd(subset(datacast[stp+stepone], datacast$cluster==c)[,1])/
        sqrt(nrow(subset(datacast, datacast$cluster==c))) -> se
      append(sev,se) -> sev
    }
    append(sevt,mean(sev)) -> sevt
  }
  rbind(table(datacast$cluster),sevt) -> table
  round(table,2) -> table
  rownames(table) <- c("N","M se")
  flagged = c()
  for (c in (1:numbclust)) {
    append(flagged, ifelse(table[1,c]==1|table[2,c]>2*median(sevt, na.rm = T),yes = 1,no = 0)) -> flagged
  }
  rbind(table,flagged) -> table
  rem_clust = c()
  for (c in (1:numbclust)) {
    ifelse(table[3,c]!=1,"",append(rem_clust,c)->rem_clust)
  }
  rem_clust ->> rem_clust
  output$subset<-renderUI({
    selclust()
  })
  write.csv(table, "table.csv")
  table
}

clustplot <- function(){
  datacast <- as.data.frame(read.csv("data_wide.csv",sep=",", header = T,row.names = NULL,stringsAsFactors = F))
  melt(datacast,id.vars = c("filename","interval_label","start", "end","cluster")) -> dataplot
  as.numeric(dataplot$variable) -> dataplot$variable
  brks = c((stepsN*0.25),(stepsN*0.5),(stepsN*(0.75)),stepsN)
  panel_text <- data.frame(label = paste("n = ", as.character(as.data.frame(table(datacast$cluster))[,2]),sep=""), cluster = 1:numbclust)
  suppressWarnings(ggplot(dataplot, aes(x=variable, y = value)) +
    stat_summary(fun=mean, group="cluster", geom="line", show.legend = F) +
    stat_summary(fun.data = mean_sdl, group="cluster", geom = "ribbon", alpha = .2,show.legend = F) +
    scale_x_continuous(breaks=brks) + 
    facet_wrap(~cluster,ncol = 4) +
    xlab("measurement number") +
    ylab(ylb) +
    theme(axis.title = element_text(size = 20),axis.text = element_text(size=20),strip.text = element_text(size = 20)) +
    geom_text(data = panel_text, mapping = aes(x = 0.5*stepsN, y = 0.95*max(dataplot$value), label = label))) -> plot
    ggsave(filename = "plot.png", plot = plot)
# uncomment line below for high res plot (tiff print quality)
#   ggsave(filename = "plot.tiff", plot = plot, dpi=300, compression = 'lzw')
   plot 
}

readdata <- function(){
  if (is.null(input$file_input))
    return(NULL)
  data <- as.data.frame(read.csv("data_long.csv",sep=",", header = T,row.names = NULL,stringsAsFactors = F))
  filesave <- ifelse(input$prep_data==T, paste(Sys.time(),": cleaned datafile saved to 'data_long.csv' in ", getwd(), sep = ""),paste(Sys.time(),": datafile saved to 'data_long.csv' in ", getwd(), sep = ""))
  filesave <- ifelse(file.exists("dendrogram.png"),paste(filesave,"\n\n",Sys.time(),": dendrogram saved to 'dendrogram.png' in ", getwd(), sep=""),filesave)
  filesave <- ifelse(file.exists("table.csv"),paste(filesave,"\n\n",Sys.time(),": table saved to 'table.csv' in ", getwd(), sep=""),filesave)
  filesave <- ifelse(file.exists("plot.png"),paste(filesave,"\n\n",Sys.time(),": plot saved to 'plot.png' in ", getwd(), sep=""),filesave)
  filesave <- ifelse(file.exists("data_wide.csv"),paste(filesave,"\n\n",Sys.time(),": wide datafile with clusters annotated saved to 'data_wide.csv' in ", getwd(), sep=""),filesave)
  dashedline <- "--------------------------------------------"
  stepsN <- length(levels(as.factor(data$stepnumber)))
  steps <- paste(stepsN, " measurement points per contour", sep = "")
  spkN <- length(levels(as.factor(data$filename)))
  spk <- paste(spkN, " speakers (filenames)", sep="")
  sscontours <- if (file.exists("data_wide.csv")){
    datacast <- as.data.frame(read.csv("data_wide.csv",sep=",", header = T,row.names = NULL,stringsAsFactors = F))
    paste(" (",nrow(datacast)," left after subsetting - ",round((nrow(datacast)/(nrow(data)/stepsN))*100,1), "%)",sep = "")
  }else{
      ""
    }
  ncontours <- paste(nrow(data)/stepsN, " contours in uploaded data",sscontours, sep = "")
  emptyN <-(nrow(data)-nrow(subset(data, data$filename !="")))+(nrow(data)-nrow(subset(data, data$interval_label !="")))
  empty <- paste(emptyN, " rows with empty filenames/labels detected", sep = "")
  unusedN <- nrow(data)-nrow(droplevels(data)) 
  unused <- paste(unusedN, " unsused levels detected", sep = "")
  f0_errN <-nrow(data)-nrow(subset(data, !grepl("--undefined--", data$f0)))+nrow(data)-nrow(subset(data, data$f0 !=""))
  f0_err <- paste(f0_errN, " f0 measurement errors detected", sep = "")
  ifelse(test = (emptyN+unusedN+f0_errN)>0, paste(dashedline,"WARNING: apply cleanup to datafile", sep = "\n"), "") -> warn
  updateTabsetPanel(session, inputId = "outputs", selected = "summary")
  paste(filesave, dashedline, ncontours, steps, spk, dashedline, empty, unused, f0_err, warn, sep = "\n\n")
}

selclust <- function(){
  if (exists('rem_clust')){
    checkboxGroupInput("subset", "Remove these clusters:", choices = 1:numbclust,selected = rem_clust, inline = T)
  }
}

save_current <- function(){
    clustplot()
    file.copy("plot.png",paste("plot_",numbclust,".png",sep=""),overwrite = T)
    file.copy("data_long.csv",paste("data_long_",numbclust,".csv",sep=""),overwrite = T)
    file.copy("data_wide.csv",paste("data_wide_",numbclust,".csv",sep=""),overwrite = T)
    ggsave("dendrogram.png", dendro)
    file.copy("table.csv",paste("table_",numbclust,".csv",sep=""),overwrite = T)
}
  
gen_textgrid <- function(){
  data <- as.data.frame(read.csv("data_long.csv",sep=",", header = T,row.names = NULL,stringsAsFactors = F))
  datacast <- as.data.frame(read.csv("data_wide.csv",sep=",", header = T,row.names = NULL,stringsAsFactors = F))
  if (exists("filenames")){rm("filenames")}
  levels(as.factor(data$filename)) ->> filenames
  for (spk in levels(as.factor(data$filename))) {
    sink(paste(spk,".TextGrid",sep = ""))
    cat("File type = \"ooTextFile\"")
    cat("\n")
    cat("Object class = \"TextGrid\"")
    cat("\n")
    cat("\n")
    cat("xmin = 0")
    cat("\n")
    xmax <- max(datacast$start[datacast$filename==spk])+.5
    cat(paste("xmax = ",xmax,sep = ""))
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
    cat(paste("        xmax = ",xmax, sep = ""))
    cat("\n")
    cat(paste("        intervals: size = ",nrow(sort_df(datacast[datacast$filename==spk,],"start")),sep = ""))
    cat("\n")
    for (contour in 1:nrow(sort_df(datacast[datacast$filename==spk,],"start"))) {
      cat(paste("        intervals [",contour,"]:", sep = ""))
      cat("\n")
      cat(paste("            xmin = ",subset(sort_df(datacast, "start"),filename==spk)[contour,"start"],sep = ""))
      cat("\n")
      cat(paste("            xmax = ",subset(sort_df(datacast, "start"),filename==spk)[contour,"end"],sep = ""))
      cat("\n")
      cat(paste("            text = \"",subset(sort_df(datacast, "start"),filename==spk)[contour,"cluster"],"\"",sep = ""))
      cat("\n")
    }
    sink()
  }
}


}


  
shinyApp(ui, server, 
         onStart = function() {
           8 ->> numbclust
           c("data_long.csv", "data_wide.csv", "dendro.png", "plot.png", "table.csv") -> remfiles
           for (f in remfiles){
             if (file.exists(f)){
               file.remove(f)
               cat(paste("Removing old ", f,"\n", sep=""))
             }
           }
           cat("Starting contour clustering app\n")
        onStop(function() {
          for (f in remfiles){
            if (file.exists(f)){
              file.remove(f)
              cat(paste("Removing ", f,"\n", sep=""))
            }
          }
          cat("Stopping contour clustering app.\n")
          rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
          })
        })