# General information:
# This script runs as Shiny app and can be used to take (time-series) acoustic measurements and perform cluster analysis, as well
# as other statistical analyses on f0 contours and other acoustic cues. Measurements can alternatively be taken using the Praat
# script "time-series_f0.praat". Additional mouse-hover help comments are provided for most controls in the app. 
# More information, detailed documentation and references can be found on: https://constantijnkaland.github.io/contourclustering/.
#
# Developed and tested using R/package versions:
# R version 4.5.0 (2025-04-11)
# RStudio 2025.5.0.496
# cluster 2.1.8.1
# data.table 1.18.2.1
# dplyr 1.2.0
# dtwclust 6.0.0
# fda 6.3.0
# ggdendro 0.2.0
# ggplot2 4.0.2
# Hmisc 5.2.5
# Metrics 0.1.4
# pracma 2.4.6
# proxy 0.4.29
# purrr 1.2.1
# readr 2.1.6
# readtextgrid 0.2.0
# scales 1.4.0
# shiny 1.12.1
# sound 1.4.6
# stats 4.5.0
# stringr 1.6.0
# usedist 0.4.0
# utils 4.5.0
# wrassp 1.0.6
# zip 2.3.3
# zoo 1.8.15
# 
# Constantijn Kaland, February 2026.
# https://constantijnkaland.github.io/contourclustering/

options(warn = -1, error = NULL)

packages.initload <- c("data.table","dplyr","ggplot2","Hmisc","readr","shiny","stats","stringr","utils","zoo")
packages.flyload <- c("cluster","dtwclust","fda","ggdendro","Metrics","pracma","proxy","purrr","readtextgrid","scales","sound","usedist","wrassp","zip")
packages <- c(packages.initload,packages.flyload)
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == F)) {
  install.packages(packages[!installed_packages])
  if(any(installed_packages == F)){
    cat(paste0("The following package(s) could not be installed automatically: ", packages[which(installed_packages==F)], "\nIf this is the first time you are running Contour Clustering, please re-start the app.\nIf this message persists, consider installing the package(s) manually from https://cran.r-project.org/web/packages/.\n"))
    suppressMessages(suppressWarnings(stop()))
  }else{
  cat("New packages were installed. R session will be restarted automatically. Please restart Contour Clustering.\n")
  stopApp()
  .rs.restartR()
  }
}

suppressPackageStartupMessages(invisible(lapply(packages.initload, library, character.only = TRUE)))

# ui ####
ui <- tagList(
    tags$head(tags$style('.navbarmain .navbar-nav {width: 95%;}
                          .navbarmain .navbar-nav :nth-child(3),
                          .navbarmain .navbar-nav :nth-child(4),
                          .navbarmain .navbar-nav :nth-child(5),
                          .navbarmain .navbar-nav :nth-child(6){float:right}
                          .navbarsub .navbar-nav {width: 95%;}
                          .navbarsub .navbar-nav :nth-child(3),
                          .navbarsub .navbar-nav :nth-child(4),
                          .navbarsub .navbar-nav :nth-child(5),
                          .navbarsub .navbar-nav :nth-child(6){float:left}
                         '),
  ),
  div(class = "navbarmain", navbarPage(
  title = HTML('<input type="image" id="apptitle" src="https://constantijnkaland.github.io/contourclustering/logo.png" height = 20 align="center" class="action-button"/>'),
  id = "tabs_main",
  selected = "Data",
  windowTitle = "Contour Clustering",
  
  tabPanel(
    "Data",
    div(class = "navbarsub", navbarPage(
      title = NULL,
      id = "tabset_data",
      
      tabPanel(
        value = "Read folder",
        tags$div(title = "Read folder with audio files and textgrids in order to take time-series acoustic measurements.", "Read folder"),
        sidebarLayout( 
          sidebarPanel(
            tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
            tags$div(
              title = paste0(
                "Specify path to the local folder that contains sound files and TextGrids (data from server/cloud may not be readable), including final ",
                .Platform$file.sep,
                ". Only long formatted TextGrids with UTF-8 or ASCII encoding can be read, no other encodings and no TextGrids saved 'as short text file'."
              ),
              textInput(
                "dir_input",
                "Path to folder that contains sound files and TextGrids:",
                "",
                width = '100%'
              )),
            radioButtons(
              "snd_ext",
              "Select sound file extension:",
              choices = list(".wav" = 1, ".mp3" = 2),
              selected = 1,
              inline = T,
              width = '100%'
            ),
            tags$div(
              title = "Reads all TextGrids that have corresponding sound file in the specified folder All tiers from each TextGrid are read, consider using TextGrids with one tier for the unit of analysis to speed up processing.",
              actionButton("read_dir", "Read folder", width = '100%')),
            uiOutput("sel_tier"),
            uiOutput("to_measures"),
            width = 3
          ),
          mainPanel(style = "height:85vh; overflow-y: auto;",
                    fluidPage(
                      fluidRow(
                        column(width = 1),
                        column(width = 11, uiOutput("file_names"))
                      )
                    )
        )
        )
        ),
      
      tabPanel(
        value = "Load file",
        tags$div(title = "Load locally stored long data file with already taken acoustic measurements (data from server/cloud may not be readable).", "Load file"),
        sidebarLayout(
          sidebarPanel(
            uiOutput("upload"),
            uiOutput("sel_filenames"),
            uiOutput("sel_ints"),
            uiOutput("map_cols"),
            width = 3
          ),
          mainPanel(uiOutput("load_file"),width = 9)
        )
      ),
      
      tabPanel(value = "Data (TextGrids)", "Data (TextGrids)",
               fillPage(
                 column(style = "height:85vh; overflow-y: auto;", width = 11, tableOutput("df.g")),
                 column(width = 1, verticalLayout(div(align = "center", title = "Save TextGrid data as csv file to disk",
                                                      downloadButton("dl.df.g","Save"))
                 )
                 )
               )),
      
      tabPanel(style = "height:80vh; overflow-y: auto;", 
        title = "Acoustics",
        sidebarPanel(
          width = 3,
          div(class = "navbarsub", navbarPage(title = NULL,
            id = "tabset_ac",
            tabPanel(
              "f0",
              tags$div(align = "center", title = "Include f0 as measurement in Hertz (Hz, can be converted later). At least one of the acoustic cues needs to be included.", checkboxInput("incl_f0", "include", value = T)),
              HTML('<p style="margin-bottom:10px;"></p>'),
              uiOutput("f0_settings")
            ),
            tabPanel(
              "intensity",
              tags$div(align = "center", title = "Include intensity as measurement in decibels (dB, can be converted later). At least one of the acoustic cues needs to be included.", checkboxInput("incl_int", "include", value = F)),
              HTML('<p style="margin-bottom:10px;"></p>'),
              uiOutput("int_settings")
            ),
            tabPanel(
              "duration",
              tags$div(align = "center", title = "Include duration as measurement in seconds (s, can be converted later). Duration is taken be subtracting the end time from the start time of the interval. A single duration value is added per contour (at measurement point 1, rest shows as NA). At least one of the acoustic cues needs to be included.",checkboxInput("incl_dur", "include", value = F))
            ),
          )
          ),
          tags$hr(),
          actionButton(width = '100%', "do_measure", label = "Measure all")
        ),
        mainPanel(
          width = 9, fluidPage(fluidRow(
          column(width = 12, uiOutput("sample_plots"))
        ))
      )),
      
      tabPanel(value = "Speakers", "Speakers", uiOutput("speakers")),

      tabPanel(value = "Clean", "Clean", uiOutput("clean")),
      
      tabPanel(style = "height:80vh; overflow-y: auto;", value = "Representation", "Representation", uiOutput("repr")),
      
      tabPanel(value = "Data (upload)", "Data (upload)",
               fillPage(
                 column(style = "height:85vh; overflow-y: auto;", width = 12, tableOutput("df.u"))
                 )),
      
      tabPanel(value = "Data (long)", "Data (long)",
               fillPage(
                 column(style = "height:85vh; overflow-y: auto;", width = 11, tableOutput("df.l")),
                 column(width = 1, verticalLayout(div(align = "center", title = "Save data in long format as csv file to disk",
                     downloadButton("dl.df.l","Save"))
                       )
                     )
                 ))
    )
)
),
  
  tabPanel(title = "Clustering", style = "height:90vh; overflow-y: auto;",
              sidebarPanel(width = 3, uiOutput("cl.settings")),
              mainPanel(
                width = 9,
                div(class = "navbarsub", navbarPage(title = NULL, id = "tabs_cl",
                            tabPanel("Dendrogram", fillPage(
                              column(width = 11, plotOutput("cl.dendro")),
                              column(width = 1, div(align = "center", title = "Save plot as png file to disk",downloadButton("dl.dendro","Save"))),
                            )),
                            tabPanel("Plot", fillPage(
                                       column(width = 11, plotOutput("cl.plot")),
                                       column(width = 1, span(
                                         div(title = "Save plot as png file to disk",downloadButton("dl.plot","Save"))
                                         )),
                                       )),
                            tabPanel("Table", fillPage(
                              fluidRow(
                                column(width = 11, div(title = "Summary table with the number/percentage of observations and mean standard error per clustering variable, per cluster. See logfile for total number of observations in the most recent clustering.", HTML('<b>Summary table</b>'), style = 'font-size: 1.5em')),
                                column(width = 1, span(
                                  div(title = "Save table as csv file to disk",downloadButton("dl.table","Save"))
                                ))
                                ),
                              HTML('<br>'),
                              fluidRow(
                              column(width = 2, uiOutput("cl.sel")),
                              column(width = 1),
                              column(width = 9, tableOutput("cl.table"))
                            ),
                            tags$hr(),
                            uiOutput("cl.contab")
                            )
                            ),
                            tabPanel("Evaluate", fillPage(
                              fluidRow(
                                column(
                                  width = 3,
                                  align = "left",
                                  sliderInput(
                                    "eval.rg",
                                    "Number of clusters (range)",
                                    min = 2,
                                    max = 15,
                                    value = c(2, 10),
                                    width = '80%'
                                  )
                                ),
                                column(
                                  width = 2,
                                  align = "left",
                                  tags$div(
                                    title = "Select a method to perform evaluation. MDL performs information cost evaluation finding minimum description length (MDL; see Kaland & Ellison, 2023). W/B cluster var performs evaluation aiming to find the cross-over point of within and between cluster variance.",
                                    radioButtons(
                                      "eval.method",
                                      "Evaluation method:",
                                      choices = list("MDL" = 1, "W/B cluster var" = 2),
                                      selected = 1,
                                      inline = F
                                    )
                                  )
                                ),
                                column(width = 2, align = "left", uiOutput("eval.smooth")),
                                column(width = 3, align = "center", span(HTML('<br>'), tags$div(title = "Evaluation is performed on the actual values of the chosen variable for clustering. In case multiple variables are selected for clustering, evaluation is based on the first.", actionButton("do.eval", "Run evaluation"))))
                              ),
                              tags$hr(),
                              fluidRow(
                                column(width = 11, plotOutput("eval.plot")),
                                column(width = 1, span(
                                  div(title = "Save plot as png file to disk",downloadButton("dl.evalplot","Save"))
                                )),
                              ),
                              tags$hr(),
                              fluidRow(
                                column(width = 11, align = "center", tableOutput("eval.tab")),
                                column(width = 1, span(
                                  div(title = "Save table as csv file to disk",downloadButton("dl.evaltab","Save"))
                                )),
                              )
                            )),
                            tabPanel("Prototypes", fillPage(
                              fluidRow(
                                column(width = 11, div(title = "Prototype table listing the most prototypical contours per cluster.", HTML('<b>Prototype table</b>'), style = 'font-size: 1.5em')),
                                column(width = 1, span(
                                  div(title = "Save table as csv file to disk",downloadButton("dl.proto","Save"))
                                ))
                              ),
                              HTML('<br>'),
                              fluidRow(
                                uiOutput("proto.play")
                              )
                            )),
                )
              )
            )),
  
  tabPanel("Log",                
           fillPage(
             column(width = 11, style = "height:90vh; overflow-y: auto;", verbatimTextOutput("logfile")),
             column(width = 1, div(align = "center", title = "Save log as plain textfile to disk",downloadButton("dl.logfile","Save"))),
    )),
  
  tabPanel("Manual", uiOutput("manual")),

  tabPanel(tags$div(title = "In-app references to the packages, scales, metrics, formulas and techniques used. DOI links are best opened in a new window using a middle (scroll wheel) mouse click.", "References"),
           style = "height:90vh; overflow-y: auto;",
           HTML('
           <div class="csl-bib-body" style="line-height: 2; margin-left: 3em; text-indent:-2em;">
  <div class="csl-entry">Bittinger, K. (2017). <i>usedist: Distance Matrix Utilities</i> (p. 0.4.0) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.usedist" target="_blank">https://doi.org/10.32614/CRAN.package.usedist</a></div>
  <div class="csl-entry">Borchers, H. W. (2011). <i>pracma: Practical Numerical Math Functions</i> (p. 2.4.4) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.pracma" target="_blank">https://doi.org/10.32614/CRAN.package.pracma</a></div>
  <div class="csl-entry">Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y., Allen, J., McPherson, J., Dipert, A., &amp; Borges, B. (2012). <i>shiny: Web Application Framework for R</i> (p. 1.10.0) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.shiny" target="_blank">https://doi.org/10.32614/CRAN.package.shiny</a></div>
  <div class="csl-entry">Csárdi, G. (2017). <i>zip: Cross-Platform “zip” Compression</i> (p. 2.3.2) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.zip" target="_blank">https://doi.org/10.32614/CRAN.package.zip</a></div>
  <div class="csl-entry">De Looze, C., &amp; Hirst, D. (2014). The OMe (Octave-Median) scale: A natural scale for speech melody. <i>7th International Conference on Speech Prosody 2014</i>, 910–914. <a href="https://doi.org/10.21437/SpeechProsody.2014-170" target="_blank">https://doi.org/10.21437/SpeechProsody.2014-170</a></div>
  <div class="csl-entry">De Vries, A., &amp; Ripley, B. D. (2013). <i>ggdendro: Create Dendrograms and Tree Diagrams Using “ggplot2”</i> (p. 0.2.0) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.ggdendro" target="_blank">https://doi.org/10.32614/CRAN.package.ggdendro</a></div>
  <div class="csl-entry">Greenwood, D. D. (1961). Critical Bandwidth and the Frequency Coordinates of the Basilar Membrane. <i>The Journal of the Acoustical Society of America</i>, <i>33</i>(10), 1344–1356. <a href="https://doi.org/10.1121/1.1908437" target="_blank">https://doi.org/10.1121/1.1908437</a></div>
  <div class="csl-entry">Gubian, M., Torreira, F., &amp; Boves, L. (2015). Using Functional Data Analysis for investigating multidimensional dynamic phonetic contrasts. <i>Journal of Phonetics</i>, <i>49</i>, 16–40. <a href="https://doi.org/10.1016/j.wocn.2014.10.001" target="_blank">https://doi.org/10.1016/j.wocn.2014.10.001</a></div>
  <div class="csl-entry">Hamner, B., &amp; Frasco, M. (2012). <i>Metrics: Evaluation Metrics for Machine Learning</i> (p. 0.1.4) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.Metrics" target="_blank">https://doi.org/10.32614/CRAN.package.Metrics</a></div>
  <div class="csl-entry">Harrell Jr, F. E. (2003). <i>Hmisc: Harrell Miscellaneous</i> (p. 5.2-2) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.Hmisc" target="_blank">https://doi.org/10.32614/CRAN.package.Hmisc</a></div>
  <div class="csl-entry">Hermes, D. J. (1998). Measuring the Perceptual Similarity of Pitch Contours. <i>Journal of Speech, Language, and Hearing Research</i>, <i>41</i>(1), 73–82. <a href="https://doi.org/10.1044/jslhr.4101.73" target="_blank">https://doi.org/10.1044/jslhr.4101.73</a></div>
  <div class="csl-entry">Heymann, M. (2002). <i>sound: A Sound Interface for R</i> (p. 1.4.6) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.sound" target="_blank">https://doi.org/10.32614/CRAN.package.sound</a></div>
  <div class="csl-entry">Hyndman, R. J., &amp; Koehler, A. B. (2006). Another look at measures of forecast accuracy. <i>International Journal of Forecasting</i>, <i>22</i>(4), 679–688. <a href="https://doi.org/10.1016/j.ijforecast.2006.03.001" target="_blank">https://doi.org/10.1016/j.ijforecast.2006.03.001</a></div>
  <div class="csl-entry">Kaland, C., &amp; Ellison, T. M. (2023). Evaluating cluster analysis on f0 contours: An information theoretic approach on three languages. In R. Skarnitzl &amp; J. Volín (Eds.), <i>Proceedings of the 20th International Congress of Phonetic Sciences</i> (pp. 3448–3452). Guarant International. <a href="https://sfb1252.uni-koeln.de/sites/sfb_1252/user_upload/Pdfs_Publikationen/Kaland_Ellison_2023_Evaluating_cluster_analysis.pdf" target="_blank">https://sfb1252.uni-koeln.de/sites/sfb_1252/user_upload/Pdfs_Publikationen/Kaland_Ellison_2023_Evaluating_cluster_analysis.pdf</a></div>
  <div class="csl-entry">Kaufmann, L., &amp; Rousseeuw, P. (1987). Clustering by means of medoids. In Y. Dodge (Ed.), <i>Data Analysis based on the L1-Norm and Related Methods</i> (pp. 405–416). North Holland / Elsevier. <a href="https://www.researchgate.net/profile/Peter-Rousseeuw/publication/243777819_Clustering_by_Means_of_Medoids/links/00b7d531493fad342c000000/Clustering-by-Means-of-Medoids.pdf" target="_blank">https://www.researchgate.net/profile/Peter-Rousseeuw/publication/243777819_Clustering_by_Means_of_Medoids/links/00b7d531493fad342c000000/Clustering-by-Means-of-Medoids.pdf</a></div>
  <div class="csl-entry">Maechler, M., Rousseeuw, P., Struyf, A., &amp; Hubert, M. (1999). <i>cluster: “Finding Groups in Data”: Cluster Analysis Extended Rousseeuw et al.</i> (p. 2.1.8) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.cluster" target="_blank">https://doi.org/10.32614/CRAN.package.cluster</a></div>
  <div class="csl-entry">Mahr, T. (2020). <i>readtextgrid: Read in a “Praat” “TextGrid” File</i> (p. 0.1.2) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.readtextgrid" target="_blank">https://doi.org/10.32614/CRAN.package.readtextgrid</a></div>
  <div class="csl-entry">Meyer, D., &amp; Buchta, C. (2007). <i>proxy: Distance and Similarity Measures</i> (p. 0.4-27) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.proxy" target="_blank">https://doi.org/10.32614/CRAN.package.proxy</a></div>
  <div class="csl-entry">Ramsay, J. (2003). <i>fda: Functional Data Analysis</i> (p. 6.2.0) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.fda" target="_blank">https://doi.org/10.32614/CRAN.package.fda</a></div>
  <div class="csl-entry">Reynolds, A. P., Richards, G., De La Iglesia, B., &amp; Rayward-Smith, V. J. (2006). Clustering Rules: A Comparison of Partitioning and Hierarchical Clustering Algorithms. <i>Journal of Mathematical Modelling and Algorithms</i>, <i>5</i>(4), 475–504. <a href="https://doi.org/10.1007/s10852-005-9022-1" target="_blank">https://doi.org/10.1007/s10852-005-9022-1</a></div>
  <div class="csl-entry">Sarda-Espinosa, A. (2015). <i>dtwclust: Time Series Clustering Along with Optimizations for the Dynamic Time Warping Distance</i> (p. 6.0.0) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.dtwclust" target="_blank">https://doi.org/10.32614/CRAN.package.dtwclust</a></div>
  <div class="csl-entry">Scheffers, M. T. M. (1983). Simulation of auditory analysis of pitch: An elaboration on the DWS pitch meter. <i>The Journal of the Acoustical Society of America</i>, <i>74</i>(6), 1716–1725. <a href="https://doi.org/10.1121/1.390280" target="_blank">https://doi.org/10.1121/1.390280</a></div>
  <div class="csl-entry">Schubert, E., &amp; Rousseeuw, P. J. (2019). Faster k-Medoids Clustering: Improving the PAM, CLARA, and CLARANS Algorithms. In G. Amato, C. Gennaro, V. Oria, &amp; M. Radovanović (Eds.), <i>Similarity Search and Applications</i> (Vol. 11807, pp. 171–187). Springer International Publishing. <a href="https://doi.org/10.1007/978-3-030-32047-8_16" target="_blank">https://doi.org/10.1007/978-3-030-32047-8_16</a></div>
  <div class="csl-entry">Schubert, E., &amp; Rousseeuw, P. J. (2021). Fast and eager k -medoids clustering: O ( k ) runtime improvement of the PAM, CLARA, and CLARANS algorithms. <i>Information Systems</i>, <i>101</i>, 101804. <a href="https://doi.org/10.1016/j.is.2021.101804" target="_blank">https://doi.org/10.1016/j.is.2021.101804</a></div>
  <div class="csl-entry">Wickham, H. (2009). <i>stringr: Simple, Consistent Wrappers for Common String Operations</i> (p. 1.5.1) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.stringr" target="_blank">https://doi.org/10.32614/CRAN.package.stringr</a></div>
  <div class="csl-entry">Wickham, H., &amp; Henry, L. (2015). <i>purrr: Functional Programming Tools</i> (p. 1.0.4) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.purrr" target="_blank">https://doi.org/10.32614/CRAN.package.purrr</a></div>
  <div class="csl-entry">Wickham, H., Chang, W., Henry, L., Pedersen, T. L., Takahashi, K., Wilke, C., Woo, K., Yutani, H., Dunnington, D., &amp; Van Den Brand, T. (2007). <i>ggplot2: Create Elegant Data Visualisations Using the Grammar of Graphics</i> (p. 3.5.1) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.ggplot2" target="_blank">https://doi.org/10.32614/CRAN.package.ggplot2</a></div>
  <div class="csl-entry">Wickham, H., François, R., Henry, L., Müller, K., &amp; Vaughan, D. (2014). <i>dplyr: A Grammar of Data Manipulation</i> (p. 1.1.4) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.dplyr" target="_blank">https://doi.org/10.32614/CRAN.package.dplyr</a></div>
  <div class="csl-entry">Wickham, H., Hester, J., &amp; Bryan, J. (2015). <i>readr: Read Rectangular Text Data</i> (p. 2.1.5) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.readr" target="_blank">https://doi.org/10.32614/CRAN.package.readr</a></div>
  <div class="csl-entry">Wickham, H., Pedersen, T. L., &amp; Seidel, D. (2011). <i>scales: Scale Functions for Visualization</i> (p. 1.3.0) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.scales" target="_blank">https://doi.org/10.32614/CRAN.package.scales</a></div>
  <div class="csl-entry">Winkelmann, R., Bombien, L., Scheffers, M., &amp; Jochim, M. (2014). <i>wrassp: Interface to the “ASSP” Library</i> (p. 1.0.5) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.wrassp" target="_blank">https://doi.org/10.32614/CRAN.package.wrassp</a></div>
  <div class="csl-entry">Xu, Y., &amp; Sun, X. (2002). Maximum speed of pitch change and how it may relate to speech. <i>The Journal of the Acoustical Society of America</i>, <i>111</i>(3), 1399–1413. <a href="https://doi.org/10.1121/1.1445789" target="_blank">https://doi.org/10.1121/1.1445789</a></div>
  <div class="csl-entry">Zeileis, A., Grothendieck, G., &amp; Ryan, J. A. (2004). <i>zoo: S3 Infrastructure for Regular and Irregular Time Series (Z’s Ordered Observations)</i> (p. 1.8-12) [Dataset]. <a href="https://doi.org/10.32614/CRAN.package.zoo" target="_blank">https://doi.org/10.32614/CRAN.package.zoo</a></div>
  </div>')
    ),

  tabPanel("Settings", uiOutput("app.settings"))
  
)
)
)

# server ####
server <- function(input, output, session) {
  showNotification(id = "not.scr",paste0("For the best experience: use \"Actual size\" (Ctrl + 0) as zoom level and maximize this window."),type = "error",duration = NULL,closeButton = T)
  session$onSessionEnded(function() {
    suppressWarnings(unlink("www", recursive = T))
    cat("Stopped contour clustering app.\n")
    if (app.keep==0){
      suppressWarnings(rm(list = c("df.l","df.g","df.e","logfile","app.keep"),envir = .GlobalEnv))
    } else{
      suppressWarnings(rm(list = c("app.keep"),envir = .GlobalEnv))
    }
    stopApp()
  })
  if(packageVersion("shiny")<"1.10.0"){
    showNotification("Updating shiny...")
    update.packages("shiny")
    cat("Shiny updated - restarting R.\n")
    stopApp()
    .rs.restartR()
  }
  hideTab(inputId = "tabs_main", target = "Clustering")
  hideTab(inputId = "tabs_cl", target = "Dendrogram")
  hideTab(inputId = "tabs_cl", target = "Plot")
  hideTab(inputId = "tabs_cl", target = "Table")
  hideTab(inputId = "tabs_cl", target = "Evaluate")
  hideTab(inputId = "tabs_cl", target = "Prototypes")
  hideTab(inputId = "tabset_data", target = "Data (TextGrids)")
  hideTab(inputId = "tabset_data", target = "Data (upload)")
  hideTab(inputId = "tabset_data", target = "Data (long)")
  hideTab(inputId = "tabset_data", target = "Clean")
  hideTab(inputId = "tabset_data", target = "Acoustics")
  hideTab(inputId = "tabset_data", target = "Speakers")
  hideTab(inputId = "tabset_data", target = "Representation")
  paste0(Sys.time(), ": Started contour clustering app.", "\n") ->> logfile
  cat("Running contour clustering app... \n")
  reactiveVal(10) -> app.fileinput
  reactiveVal(3000) -> app.plot.w
  reactiveVal(2000) -> app.plot.h
  reactiveVal(F) -> zipfiles
  F ->> app.keep
  options(shiny.maxRequestSize = 10 * 1024^2)
  reactiveVal("") -> inDir
  reactiveVal("") -> snd.ext
  reactiveVal(50) -> disp_rows
  reactiveVal(20) -> npoints
  reactiveVal(NULL) -> m.rows
  reactiveVal(0) -> samplemode
  reactiveVal(0) -> clean.set
  reactiveVal() -> seps
  reactiveVal(8) -> nclust
  reactiveValues(c_plot = NULL, d_plot = NULL, s_tab = NULL, c_tab = NULL, e_plot = NULL, p_tab = NULL) -> savethis
  reactiveVal(300) -> height
  reactiveVal() -> cc.y1
  reactiveVal("solid") -> app.yL.type
  reactiveVal("#000000") -> app.yL.col
  reactiveVal("dashed") -> app.yR.type
  reactiveVal("#000000") -> app.yR.col
  reactiveVal("Table") -> cl.tab.open
  reactiveVal(1000000000) -> scr.w
  reactiveVal(1000000000) -> scr.h
  
# outputs ####

  output$logfile <- renderText({
    logfile
  })
  
  output$app.settings <- renderUI({
    fillPage(
      sidebarPanel(
        width = 2,
             tags$div(
               title = "Limits the number of rows in datasets to display in app. This saves render time for large datasets. This is a display setting only, saving/processing is done on all data and is therefore not affected by this setting.",
               radioButtons("app.disp_rows", "Number of rows to display:",choices = c("25" = 25,"50" = 50,"200" = 200,"all" = 0), selected = disp_rows(), inline = F, width = '100%')
               )
             ),
      sidebarPanel(
        width = 2,
             tags$div(
               title = "Sets the file upload limit in megabytes.",
               radioButtons("app.upl_lim", "File upload limit (MB):",choices = c("5" = 5,"10" = 10,"20" = 20,"100" = 95.368), selected = app.fileinput(), inline = F, width = '100%')
             )
             ),
      sidebarPanel(
        width = 2,
        span(
         HTML('<b>Saving:</b><br><br>'),
         splitLayout(
          tags$div(title = "Specify dimensions in pixels for saving plots (directed to 'ggsave()').", numericInput("app.plot.w","Plot width:",min = 500,max = 10000,value = app.plot.w(),step = 500)),
          tags$div(title = "Specify dimensions in pixels for saving plots (directed to 'ggsave()').", numericInput(inputId = "app.plot.h",label = "Plot height:",min = 500,max = 10000, value = app.plot.h(), step = 500))
         ),
         tags$div(title = "Applies to clicking 'Save this'. Either saves output files as individual files or together in a zip file with the date and number of clusters in the name for future reference.", checkboxInput("app.zip",HTML('<b>"Save this" to zip</b>'),value = zipfiles()))
        )
        ),
      sidebarPanel(
        width = 2,
        tags$div(
          HTML('<b>Line aesthetics:</b><br><br>'),
          title = "Specifies the linetypes and colors (as hex code) of the y variable(s) in the plot.",
          radioButtons("app.yL.type", "Left y var:",choices = c("solid","dashed"), selected = app.yL.type(), inline = T, width = '100%'),
          textInput("app.yL.col", NULL,value = app.yL.col(), width = '100%'),
          tags$hr(),
          tags$div(style="text-align: right",
          radioButtons("app.yR.type", "Right y var:",choices = c("solid","dashed"), selected = app.yR.type(), inline = T, width = '100%'),
          textInput("app.yR.col", NULL,value = app.yR.col(), width = '100%')
          )
        )
      ),
      sidebarPanel(
        width = 2,
        tags$div(title = "Runs the garbage collection function 'gc()' of R. This happens automatically in R and should only be used if the app is slow, in which case the effects might be minimal.", actionButton("app.gc","Garbage collection"))),
      sidebarPanel(
        width = 2,
                   tags$div(title = "Ticking the box keeps objects created by the app (data and log) in the Global Environment of R after closing the app. Useful for debugging and/or manual data processing outside the app in the same session.", span(HTML('<b>Global Environment:</b><br>'),checkboxInput("app.keep","Keep objects",value = app.keep))))
      )
  })
  
  output$manual <- renderUI({
    fluidPage(
      align = "center",
      tags$iframe(
        style = "height:90vh; width:100%",
        src = "https://constantijnkaland.github.io/contourclustering/manual.pdf",
        loading = "lazy"
      )
    )
  })
  
  output$load_file <- renderUI({
    as.list(c()) -> clms
    c("char", "num", "cc") -> clsss
    fluidPage(
      fluidRow(
        column(
          width = 6,
          tags$div(title = "Required columns are minimally needed to perform contour clustering. They need to be specified before proceeding. All numeric values are rounded to three decimals.", HTML('<b>Required columns:</b>'))
        ),
        column(
          width = 6,
          tags$div(title = "Optional columns may be added to the data. Can be left unspecified.", HTML('Optional columns:'))
        )
      ),
      fluidRow(
        column(width = 4, HTML('<br>')),
        column(width = 2, HTML('<br>')),
        column(width = 4, HTML('<br>')),
        column(width = 2, HTML('<br>'))
      ),
      fluidRow(
        column(width = 4, HTML('<br>')),
        column(width = 2, HTML('<br>')),
        column(width = 4, HTML('<br>')),
        column(width = 2, HTML('<br>'))
      ),
      fluidRow(
        column(
          width = 3,
          tags$div(
            title = "Character column with the filenames of the audio/textgrid files from which the measurements were taken. The entries do not require an extension.",
            selectInput(
              inputId = "cols.filename",
              label = "filename",
              choices = clms,
              multiple = F
            )
          )
        ),
        column(width = 2, HTML('<br>')),
        column(width = 1, HTML('<br>')),
        column(
          width = 3,
          tags$div(
            title = "Optional numeric column with the time-series intensity measurement points in dB.",
            selectInput(
              inputId = "cols.cc.int.dB",
              label = "intensity",
              choices = clms,
              multiple = F
            )
          )
        ),
        column(width = 2, HTML('<br>')),
        column(width = 1, HTML('<br>'))
      ),
      fluidRow(
        column(
          width = 3,
          tags$div(
            title = "Numeric column with the interval start times in seconds. Refers to unique time points in the audio file.  If these times are not specified correctly, the data cleaning process will likely cause errors.",
            selectInput(
              inputId = "cols.start",
              label = "start time",
              choices = clms,
              multiple = F
            )
          )
        ),
        column(width = 2, HTML('<br>')),
        column(width = 1, HTML('<br>')),
        column(
          width = 3,
          tags$div(
            title = "Optional numeric column with singleton duration values (at stepnumber 1) in seconds",
            selectInput(
              inputId = "cols.cc.dur.s",
              label = "duration",
              choices = clms,
              multiple = F
            )
          )
        ),
        column(width = 2, HTML('<br>')),
        column(width = 1, HTML('<br>'))
      ),
      fluidRow(
        column(
          width = 3,
          tags$div(
            title = "Numeric column with the interval end times in seconds. Refers to unique time points in the audio file. If these times are not specified correctly, the data cleaning process will likely cause errors.",
            selectInput(
              inputId = "cols.end",
              label = "end time",
              choices = clms,
              multiple = F
            )
          )
        ),
        column(width = 2, HTML('<br>')),
        column(width = 1, HTML('<br>')),
        column(
          width = 3,
          tags$div(
            title = "Optional numeric column with the (cumulative) time of the measurement point from the start of the interval.",
            selectInput(
              inputId = "cols.steptime",
              label = "step time",
              choices = clms,
              multiple = F
            )
          )
        ),
        column(width = 2, HTML('<br>')),
        column(width = 1, HTML('<br>'))
      ),
      fluidRow(
        column(
          width = 3,
          tags$div(
            title = "Character column with the anonymized IDs referring to the speakers. Can be identical to 'filename' if each audio file was recorded from a different speaker. Numeric values will be interpreted as character values. Select 'map later' to extract speaker IDs from another character column.",
            selectInput(
              inputId = "cols.speaker",
              label = "speaker",
              choices = clms,
              multiple = F
            )
          )
        ),
        column(width = 2, HTML('<br>')),
        column(width = 1, HTML('<br>')),
        column(
          width = 3,
          tags$div(
            title = "Optional numeric column with change ratios before/after killing octave jumps in Praat.",
            selectInput(
              inputId = "cols.jumpkilleffect",
              label = "jumpkilleffect",
              choices = clms,
              multiple = F
            )
          )
        ),
        column(width = 2, HTML('<br>')),
        column(width = 1, HTML('<br>'))
      ),
      fluidRow(align = "left",
        column(
          width = 3,
          tags$div(
            title = "Character column with the text labels belonging to the interval, which are usually taken from the TextGrids. Can be any kind of information specifying the interval.",
            selectInput(
              inputId = "cols.interval_label",
              label = "interval label",
              choices = clms,
              multiple = F
            )
          )
        ),
        column(width = 2, HTML('<br>')),
        column(width = 1, HTML('<br>')),
        column(
          width = 3,
          tags$div(
            title = "Optional additional column that needs specification for the class of the values (character or numeric). Choose 'cc' to use the values as dependent variable for clustering.",
            selectInput(
              inputId = "cols.add1",
              label = "additional 1",
              choices = clms,
              multiple = F
            )
          )
        ),
        column(
          width = 2,
          selectInput(
            "cl.add1",
            label = HTML('<br>'),
            choices = clsss,
            multiple = F,
            selected = "char"
          )
        ),
        column(width = 1, HTML('<br>'))
      ),
      fluidRow(
        column(
          width = 3,
          tags$div(
            title = "Column with integers referring to the sequence number of the time-series measurement points. Ranging from 1 to the total number of measurement points taken. 1 = first measurement point in the time-series for a given interval.",
            selectInput(
              inputId = "cols.stepnumber",
              label = "step number",
              choices = clms,
              multiple = F
            )
          )
        ),
        column(width = 2, HTML('<br>')),
        column(width = 1, HTML('<br>')),
        column(
          width = 3,
          tags$div(
            title = "Optional additional column that needs specification for the class of the values (character or numeric). Choose 'cc' to use the values as dependent variable for clustering.",
            selectInput(
              inputId = "cols.add2",
              label = "additional 2",
              choices = clms,
              multiple = F
            )
          )
        ),
        column(
          width = 2,
          selectInput(
            "cl.add2",
            label = HTML('<br>'),
            choices = clsss,
            multiple = F,
            selected = "char"
          )
        ),
      column(width = 1, HTML('<br>'))
      ),
      fluidRow(
        column(
          width = 3,
          tags$div(
            title = "Numeric column with the time-series f0 values",
            selectInput(
              inputId = "cols.f0",
              label = "f0",
              choices = clms,
              multiple = F
            )
          )
        ),
        column(width = 2,
               tags$div(
                 title = "Select f0 scale of uploaded values. Accepted scales are: Hertz, equivalent rectangular bandwidth (ERB), semitone (ST re. 50 Hz). Uploading (speaker) standardized values here will cause errors. Any conversion can be done later.",
                 selectInput(
                   inputId = "cols.f0.scale",
                   label = 'f0 scale in uploaded data',
                   choices = c("Hz","ERB","ST"),
                   selected =  "Hz",
                   multiple = F
                 )
               )),
        column(width = 1, HTML('<br>')),
        column(
          width = 3,
          tags$div(
            title = "Optional additional column that needs specification for the class of the values (character or numeric). Choose 'cc' to use the values as dependent variable for clustering.",
            selectInput(
              inputId = "cols.add3",
              label = "additional 3",
              choices = clms,
              multiple = F
            )
          )
        ),
        column(
          width = 2,
          selectInput(
            "cl.add3",
            label = HTML('<br>'),
            choices = clsss,
            multiple = F,
            selected = "char"
          )
        ),
      column(width = 1, HTML('<br>'))
      ),
    )
  })
  
  output$f0_settings <- renderUI({
    if (input$incl_f0 == F) {
      return(NULL)
    } else {
      span(
        splitLayout(
          isolate(
          numericInput(
            "f0.min",
            "f0 floor (Hz)",
            value = 75,
            min = 0,
            max = 200,
            step = 25
          )),
          isolate(numericInput(
            "f0.max",
            "f0 ceiling (Hz)",
            value = 500,
            min = 200,
            max = 800,
            step = 25
          ))
        ),
        HTML('<p style="margin-bottom:10px;"></p>'),
        splitLayout(
          tags$div(
            title = "Package 'wrassp':  minimum quality value of f0 fit, defaults to 0.52. More accurate/less f0 candidates > 0.52 > less accurate/more f0 candidates.",
            numericInput(
              "f0.fit",
              "f0 fit",
              value = 0.52,
              min = 0,
              max = 1,
              step = 0.1
            )
          ),
          tags$div(
            title = "Smoothing bandwith for kernel smoothing. Snugger < smoother.",
            numericInput(
              "f0.smooth_bw",
              "Smoothing bandwith",
              value = 1,
              min = 0,
              max = 100,
              step = 1
            )
          )
        ),
        HTML('<p style="margin-bottom:10px;"></p>'),
        splitLayout(
          numericInput(
            "f0.timestep",
            "Time-step (ms)",
            value = 10,
            min = 1,
            max = 50,
            step = 1
          ),
          tags$div(
            title = "Number of measurement points to represent the smoothed contour. See hints in sidepanel on the maximum number of measurement points.",
            isolate(
            numericInput(
              "npoints.f0",
              "N measurement pts",
              value = npoints(),
              min = 2,
              max = 100,
              step = 1
            )))
        ),
        tags$hr(),
        fluidRow(
          column(width = 4,
                 tags$div(title = "Sample N random intervals from the TextGrids. Unless clicked, the same sample is used.",
                 actionButton("do_sample", label = "Take sample"))),
          column(width = 3,
                 tags$div(title = HTML("Number of random intervals to sample. Effectuated by 'Take sample'."),
                 numericInput("n_sample",  label = NULL, value = 5, min = 1, max = nrow(df.g), step = 1))),
          column(width = 5,
                 tags$div(title = "Visualize the chosen sample to inspect the settings. Black line: tracked f0 (wrassp::mhsF0(); see Scheffers, 1983, doi:10.1121/1.390280); red dashed line: inter/extra-polated and smoothed f0 for clustering", 
                 actionButton("disp_sample", "Visualize sample")))
        ),
        tags$hr(),
        renderText(paste0(
          "With a median interval duration of ",
          round(median(df.g$xmax-df.g$xmin), 2),
          " s (tier: ",
          paste(input$sel_tier,collapse = ", "),
          ") and a time-step of ",
          input$f0.timestep,
          " ms, f0 measurement resolution will be ~",
          floor((1000 / input$f0.timestep) * median(df.g$xmax-df.g$xmin)) - 1,
          " points per interval (tracking accuracy). Praat settings default to an f0 floor of 75 Hz with 10 ms timestep. Smoothing accuracy will be determined by setting the measurement points per interval and adjusting the smoothing bandwidth."
        ))
      )
    }
  })
  
  output$int_settings <- renderUI({
    if (input$incl_int == F) {
      return(NULL)
    } else {
    tags$div(
      title = "Number of measurement points to represent the intensity contour. Linked to the number of points for the f0 contour.",
      numericInput(
        "npoints.int",
        "N measurement pts",
        value = npoints(),
        min = 2,
        max = 100,
        step = 1
      ))
    }
  })
  
  output$sample_plots <- renderUI({
    if (samplemode()==1) {
      load.quiet("splines")
      ns <- session$ns
      obsList <- list()
      tagList(lapply(1:length(isolate(m.rows())), function(i) {
        fluidRow(span(
          plotUI(paste0("plot", i)),
          playUI(paste0("play", i)),
          HTML('<br><br>')
        ))
      }))
    } else {
      return(NULL)
    }
  })
  
  output$speakers <- renderUI({
    if ("speaker" %in% colnames(df.l)==F){
      showNotification("No speaker column found in data.",type ="error")
      return(NULL)
    } else {
      if(disp_rows()==0){
        disp_rows(nrow(df.l))
      }
      fillPage(
                fluidRow(
                  column(width = 4, tags$div(title = "Only columns with character values are listed here.", selectInput("spk.cols", "Select column to extract speaker IDs from:",choices = names(which(sapply(df.l, function(col) is.character(col))==T))[!names(which(sapply(df.l, function(col) is.character(col))==T))=="speaker"],multiple = F))),
                  column(width = 4, tags$div(title = "Choose separator for string splitting from all punctuation characters (regex class [:punct:]) as found in selected column. Choose first option (space) to avoid string splitting and map column as is.", selectInput("spk.sep", "Character string separator:", choices = c(" "), selected = " ", multiple = F))),
                  column(width = 4, uiOutput("spk.sepcol"))
                ),
                fluidRow(
                  column(style = "height:75vh; overflow-y: auto;",width = 4, renderTable(colnames = F, head(df.l[,get(input$spk.cols)], disp_rows()))),
                  column(style = "height:75vh; overflow-y: auto;",width = 4, renderTable(head(str_split(string = df.l[,get(input$spk.cols)],pattern = input$spk.sep,simplify = T), disp_rows()))),
                  column(style = "height:75vh; overflow-y: auto;",width = 4, uiOutput("spk.colsel"))
                )
      )
    }
  })
  
  output$spk.sepcol <- renderUI({
    if(is.null(input$spk.sep)==F){
      if(input$spk.sep!=""){
        fluidRow(
          column(width = 6, tags$div(title = "The selected column will become the column with speaker IDs in the long data.", selectInput("spk.sepcol","Select separated column for speaker ID:", 
                                                                                                                                          choices = c("",paste0("V", c(1:ncol(str_split(string = df.l[,get(input$spk.cols)],pattern = input$spk.sep,simplify = T))))),
                                                                                                                                          multiple = F))),
          column(width = 6, HTML('<br>'),tags$div(align = "right", title = "Map the selected columns as speaker IDs in long data.", actionButton("spk.mapcol","Map as speaker IDs")))
        )
      }} else {
        NULL
      }
  })
  
  output$spk.colsel <- renderUI({
    if(is.null(input$spk.sepcol)==F){
      if(input$spk.sepcol!=""){
        fluidRow(
          column(width = 6, renderTable(colnames = F, head(str_split(string = df.l[,get(input$spk.cols)],pattern = input$spk.sep,simplify = T)[,which(colnames(as.data.table(str_split(string = df.l[,get(input$spk.cols)],pattern = input$spk.sep,simplify = T)))==input$spk.sepcol)], disp_rows())))
        )
      }}else{
        NULL
      }
  })
  
  output$clean <- renderUI({
    check.clean() -> clean.output
    output$clean.text <- renderText(clean.output$text)
    logging(paste0("Cleaning check:\n\t",gsub("\n","\n\t",clean.output$text)))
    fillPage(
      fluidRow(
        column(width = 12, verbatimTextOutput("clean.text"))
      ),
      HTML('<br>'),
      fluidRow(
        column(width = 12, uiOutput("clean.set"))
      ),
      HTML('<br>'),
      fluidRow(
        column(width = 2, uiOutput("clean.recheck")),
        column(width = 2, uiOutput("clean.cont"))
      ),
      HTML('<br>'),
      fluidRow(
        column(width = 12, uiOutput("clean.ign"))
      )
    )
  })
  
  output$repr <- renderUI({
    spk.corr.list <- list(
      "none" = "n",
      "standardize" = "std",
      "octave-median rescaling (Hz only)" = "oMe"
    )
    f0.derivs.list <- list("none" = "n",
                           "velocity (d1)" = "d1",
                           "acceleration (d2)" = "d2",
                           "jerk (d3)" = "d3")
    output$cc.cols <- renderText({paste(gsub("cc.","",colnames(df.l)[which(substr(colnames(df.l),1,3)=="cc.")]),collapse = "\n")})
    fluidPage(
      fluidRow(
        column(width = 5,
               HTML('<b>Use the options below to select how the acoustic cue(s) should be represented. Each representation can then be added to the data file. All added representations become available for clustering in the next step.</b>')
               )
      ),
      renderUI({
        if("cc.f0.Hz" %in% colnames(df.l)){
          span(
      tags$hr(),
      fluidRow(
        column(width = 1, div(HTML('<br><b>f0</b>'), style = 'display: inline-block; vertical-align: bottom; font-size: 1.5em')),
        column(width = 2,
      tags$div(
        title = "Choose f0 scale. Hz is available by default. Conversion formulas for f0 in Hz: Semitone (ST) re. 50 Hz: log10((f0 / 50)) * 39.87; Equivalent Rectangular Bandwidth (ERB, see Greenwood, 1961, p.1352, Eq. 2, doi:10.1121/1.1908437): 16.7*log10((0.006046*f0)+1)",
        selectInput("f0.scale", "Scale:",choices = c("Hz", "ERB", "ST"), selected = "Hz", width = '80%'))
      ),
      column(width = 2,
             tags$div(
               title = "Only apply when number of speakers is correctly detected. Standardize (z-score, applied to all chosen scales) = f0–mean(f0.spk) / sd(f0.spk), Octave-median rescaling (De Looze & Hirst, 2014, doi:10.21437/SpeechProsody.2014-171) = log2(f0.spk/median(f0.spk))",
               selectInput("f0.corr","Speaker correction:", choices = spk.corr.list,width = '80%'))
      ),
      column(width = 2,
             tags$div(
               title = "Derivatives are taken from the speaker corrected f0 values (if any).",
               selectInput("f0.deriv", "Derivative:",choices = f0.derivs.list, width = '80%')),
      ),
      column(width = 2,
             tags$div(title = "Number of principal components to include in functional principal component analysis (fPCA, see Gubian et al., 2015, doi:10.1016/j.wocn.2014.10.001). Individual principal components (PCs) can be (de-)selected before adding them as representation. Per added PC, a single value is added per contour (at measurement point 1, rest shows as NA). Choose zero to not do fPCA. ",
                      numericInput(inputId = "f0.Npc",
                                   label = "N principal components:",
                                   value = 0,
                                   min = 0,
                                   max = 8,
                                   step = 1,
                                   width = '80%'))),
      column(width = 2,br(),
             tags$div(title = "Add selected representation to data (column name starting with 'cc.')",
                      style = 'margin-top: 5px;',
             actionButton("do.f0.repr","Add f0 representation")))
      )
      )}else{
        return(NULL)
      }
        }),
      fluidRow(
        column(width = 12, renderUI({
          if(is.null(input$f0.Npc) || "cc.f0.Hz" %in% colnames(df.l)==F){
            return(NULL)
          }else{
            if(input$f0.Npc==0 || input$tabset_data!="Representation"){
              return(NULL)
            }else{
            do.repr("pc")$colname -> n.repr
            span(
              tags$hr(),
            fluidRow(
              column(width = 1, div(HTML('<br><b>f0 - fPCA</b>'), style = 'display: inline-block; vertical-align: bottom; font-size: 1.5em')),
              column(width = 2,
                     span(
                       tags$div(
                                title = "Specify lambda (smoothing to be applied to estimated functional parameter).",
                                numericInput(inputId = "f0.Lbd",
                                             label = "λ:",
                                             value = 1,
                                             min = 1,
                                             max = 1000,
                                             step = 1,
                                             width = '50%')),
                       HTML('<br>'),
                       HTML('<br>'),
                       HTML('<br>'),
                       renderUI({
                         if(is.null(input$f0.Lbd)==F && input$tabset_data=="Representation"){
                           do.fpca(n.repr)$varprop -> vp
                           checkboxGroupInput(inputId = "sel.PCs",label = "PC (variance proportion):",choices = vp, selected = vp,inline = F)
                         }else{
                           return(NULL) 
                         }
                       })
                       )
                     ),
              column(width = 4, renderUI({
                       if(is.null(input$f0.Lbd)==F && input$tabset_data=="Representation"){
                         renderPlot({do.fpca(n.repr)$plot})
                       }else{
                        return(NULL) 
                       }
                })
              )
            )
            )
          }}
        })
          )
      ),
      renderUI({
        if("cc.int.dB" %in% colnames(df.l)){
          span(
            tags$hr(),
            fluidRow(
              column(width = 1, div(HTML('<br><b>intensity</b>'), style = 'display: inline-block; vertical-align: bottom; font-size: 1.5em')),
              column(width = 2,
                     tags$div(title = "Apply intensity correction (standardize/z-score) per recording (filename) or per speaker.",
                     selectInput("int.corr", "Intensity correction:", choices = c("none", "filename","speaker"), width = '80%'))),
              column(width = 6),
              column(width = 2,br(),
                     tags$div(title = "Add selected representation to data (column name starting with 'cc.')",
                              style = 'margin-top: 5px;',
                              actionButton("do.int.repr","Add intensity representation")))
            )
         )
        }else{
          return(NULL)
        }
      }),
      renderUI({
        if("cc.dur.s" %in% colnames(df.l)){
          span(
            tags$hr(),
            fluidRow(
              column(width = 1, div(HTML('<br><b>duration</b>'), style = 'display: inline-block; vertical-align: bottom; font-size: 1.5em')),
              column(width = 2,
                     tags$div(title = "Apply duration correction (standardize/z-score) per speaker.",
                              selectInput("dur.corr", "Duration correction:", choices = c("none","speaker"), width = '80%'))),
              column(width = 6),
              column(width = 2,br(),
                     tags$div(title = "Add selected representation to data (column name starting with 'cc.')",
                              style = 'margin-top: 5px;',
                              actionButton("do.dur.repr","Add duration representation")))
            )
          )
        }else{
          return(NULL)
        }
      }),
      tags$hr(),
      HTML('<br>'),
      fluidRow(
        column(width = 2,
               HTML('<p align="left"><b>Currently available in the data:</b></p>'),
               verbatimTextOutput("cc.cols")
        ),
        column(width = 7),
        column(width = 2, tags$div(title = "Continue to clustering.",
                 actionButton("cont.repr","Continue"))
      )
    )
    )
  })
  
  output$cc.Ncl <- renderUI({
    sliderInput(
    "cc.Ncl",
    "Number of clusters:",
    min = 2,
    max = 30,
    value = nclust(),
    step = 1
  )
  })
  
  output$cl.settings <- renderUI({
    if(is.null(df.l)==F){
    fluidPage(
      uiOutput("cc.vars"),
      HTML('<p style="margin-bottom:10px;"></p>'),
      tags$div(
        title = "Select which distance metric to express (dis)similarities between observations. Default is 'euclidean/RMSE' (see Hermes, 1998, doi:10.1044/jslhr.4101.73). MASE: see Hyndman & Koehler, 2006, doi: 10.1016/j.ijforecast.2006.03.001.",
        selectInput(
          "cc.distm",
          label = "Distance metric:",
          choices = list(
            "euclidean (L2 norm = RMSE)" = "euclidean",
            "mean absolute scaled error (MASE)" = "mase",
            "dynamic time warping" = "dtw_basic",
            "pearson (sqrt(2*(1-ρ)))" = "cor"
          ),
          selected = "euclidean",
          width = "100%"
        )
      ),
      HTML('<p style="margin-bottom:10px;"></p>'),
  tags$div(title = "Select clustering method. HAC provides a dendrogram. PAM is recommended for large datasets to reduce runtime.",selectInput("cc.method","Clustering method:",choices = list("Hierarchical Agglomerative (HAC)" = "hac", "K-medoids/PAM" = "pam"),selected = "")),
  renderUI({
    if(is.null(input$cc.method)==F){
      if(input$cc.method=="hac"){span(
        HTML('<p style="margin-bottom:10px;"></p>'),
        tags$div(
          title = "Select linkage criterion for HAC ('hclust()' function). Default is set to complete linkage. * = inversion risk",
          selectInput(
            "cc.link",
            label = "Linkage criterion (HAC):",
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
        ))
      }else{
        span(
          HTML('<p style="margin-bottom:10px;"></p>'),
          tags$div(
            title = "Select the optimization to use for PAM to reduce runtime with large datasets. Optimizer does not affect cluster assignment. K-medoids: Kaufman & Rousseeuw, 1987; PAM1-2: Reynolds et al., 2006, doi: 10.1007/s10852-005-9022-1; FastPAM1-3: Schubert & Rousseeuw, 2019, doi:10.1007/978-3-030-32047-8_16; FasterPAM: Schubert and Rousseeuw, 2021, doi:10.1016/j.is.2021.101804)",
            selectInput(
              "cc.opt",
              label = "Optimizer (PAM):",
              choices = list(
                "K-medoids (default)" = 0,
                "PAM1" = 1,
                "PAM2" = 2,
                "FastPAM1" = 3,
                "FastPAM2" = 4,
                "FastPAM3" = 5,
                "FasterPAM" = 6
              ),
              selected = 0,
              multiple = F,
              width = "100%"
            )
          ))
      }}else{
        return(NULL)
      }
  }),
  uiOutput("cc.Ncl"),
  renderUI({
    if(is.null(input$cc.vars)==F && is.null(input$cc.method)==F){
      updateActionButton(session,"do.cc",disabled = F)
      gsub("cc.","", colnames(df.l)[substr(colnames(df.l),1,3)=="cc." & str_detect(colnames(df.l),paste(c("PC","dur"),collapse = "|"))==F]) -> y.choices
      span(
        tags$hr(),
      fluidRow(
          column(width = 12, splitLayout(HTML('<b>Summarize cluster:</b>'), tags$div(
            title = "Central tendency value to summarize each cluster in the plot. Does not affect clustering. Does affect prototype computation.",
            radioButtons("plot.vals",NULL,choices = c("mean","median"),inline = T,width = '100%')
          ),cellWidths = c("55%","45%")))
        ),
      fluidRow(
        column(width = 12, splitLayout(HTML('<b>Plot columns:</b>'), tags$div(
          title = "Number of panels per row in the plot.",
          numericInput("plot.cols",NULL,value = 4,min = 1,max = 6,step = 1,width = '50%')
          ),cellWidths = c("40%","60%")))
               ),
      fluidRow(
        column(width = 6, tags$div(
          title = "Variable to be displayed on the default (left) Y-axis in the plot. Only time-series variables are available.",
          selectInput("plot.y1",label = "Plot y-axis:",choices = y.choices,width = '100%'))),
        renderUI({
          if(length(y.choices)>1){
            updateSelectInput(session,"plot.y1",label = tags$span(style="color: black;","Plot y-axis L: "))
            column(width = 6, tags$div(
              title = "Variable to be displayed on the second (right) Y-axis in the plot. Only time-series variables are available.",
              selectInput("plot.y2",tags$span(style="color: black;","Plot y-axis R: "),choices = c("none","prototype", y.choices),width = '100%')))
          }else{
            return(NULL)
          } 
        })))
    }else{
      updateActionButton(session,"do.cc",disabled = T)
      return(NULL)
    }
  }),
  tags$hr(),
  tags$div(
    title = "Performs clustering with the chosen settings. Clustering outputs won't be updated unless this button is clicked.", 
    actionButton(width = '100%', "do.cc","Do clustering",disabled = T)),
  HTML('<br>'),
  HTML('<br>'),
  tags$div(
    align = "center",
    title = "Saves most recent cluster outputs, data and logfile to a subfolder called 'saved' in the folder from which the app is run. Overwrites existing files by default.", 
    actionButton(width = '50%', "savethis","Save this",disabled = T))
    )}else{
      return(NULL)
    }
  })
  
  output$eval.smooth <- renderUI({
    if(is.null(input$eval.method)==F){
      if(input$eval.method==1){
          tags$div(
            title = "Adjust bending factor to obtain U-shaped curve. Value indicates the dependency between measurement points; higher for more dependency. Higher dependency for smaller units of analysis. Recommended values lie between 1 and 5 approximately.",
            numericInput(
              "eval.smooth",
              "Bending factor:",
              value = 4,
              width = '80%'
            )
          )
      } else {
        return(NULL)
      }} else {
        return(NULL)
      }
  })
  

# observers ####
  
  observeEvent(input$apptitle, {
    browseURL("http://constantijnkaland.github.io/contourclustering/")
  })
  
  obs.scr <- observeEvent(input$dimension, {
    if(input$dimension[1]>scr.w() || input$dimension[2]>scr.h()){
      removeNotification("not.scr",session)
      obs.scr$destroy()
    } else {
    scr.w(input$dimension[1])
    scr.h(input$dimension[2])
    }
  })
  
  observeEvent(input$tabs_main, {
    if(input$tabs_main=="Log"){
    output$dl.logfile <- downloadHandler(
        filename = function() {paste("log.txt")},
        content = function(file) {write_file(logfile, file, append = F)}
      )
    output$logfile <- renderText({
      logfile
    })
    }
    if(input$tabs_main=="Data" && input$tabset_data=="Data (long)"){
      if(disp_rows()==0){
        disp_rows(nrow(df.l))
      }
    }
    if(input$tabs_main=="Data" && input$tabset_data=="Data (upload)"){
      if(disp_rows()==0){
        disp_rows(nrow(df.u))
      }
    }
    if(input$tabs_main=="Data" && input$tabset_data=="Representation"){
      updateSelectInput(session, "f0.Npc", selected = 0)
    }
    if(input$tabs_main=="Clustering"){
      output$cc.vars <- renderUI({
        isolate(tags$div(
          title = "A distance matrix is calculated for each selected variable, rescaled to range 0-1, and then summed with the distance matrices of the other variables (if any)",
          selectInput("cc.vars","Select variable(s) for clustering:", choices = gsub("cc.","", colnames(df.l)[substr(colnames(df.l),1,3)=="cc."]),multiple = T)))
      })
    }
  })
  
  observeEvent(input$app.disp_rows, {
    if(disp_rows()!=input$app.disp_rows){
      disp_rows(as.numeric(input$app.disp_rows))
      showNotification(paste0("Maximum display rows set to: ",ifelse(input$app.disp_rows==0,"all",input$app.disp_rows)))
      logging(paste0("Maximum display rows set to: ",ifelse(input$app.disp_rows==0,"all",input$app.disp_rows)))
    }

  })
  
  observeEvent(input$app.upl_lim, {
    if(app.fileinput()!=as.numeric(input$app.upl_lim)){
    options(shiny.maxRequestSize = as.numeric(input$app.upl_lim) * 1024^2)
    app.fileinput(as.numeric(input$app.upl_lim))
    showNotification(paste0("File upload limit is set to: ",floor((as.numeric(input$app.upl_lim)*1024^2)/1000000)," MB"))
    logging(paste0("File upload limit is set to: ",floor((as.numeric(input$app.upl_lim)*1024^2)/1000000)," MB"))
    }
  })
  
  observeEvent(c(input$app.plot.w,input$app.plot.h), {
    app.plot.w(input$app.plot.w)
    app.plot.h(input$app.plot.h)
  })
  
  observeEvent(c(input$app.yL.type, input$app.yL.col, input$app.yR.type, input$app.yR.col), {
    if(input$app.yL.type!=app.yL.type() | input$app.yL.col!=app.yL.col() | input$app.yR.type!=app.yR.type() | input$app.yR.col!=app.yR.col()){
    app.yL.type(input$app.yL.type)
    app.yL.col(input$app.yL.col)
    app.yR.type(input$app.yR.type)
    app.yR.col(input$app.yR.col)
    showNotification("Line aesthetics set.")
    logging(paste0("Line aesthetics set. Y-left: ",app.yL.type(),", ",app.yL.col(),". Y-right: ",app.yR.type(),", ",app.yR.col(),"."))
    }
  })
  
  observeEvent(input$app.zip, {
    if(zipfiles()!=input$app.zip){
      zipfiles(input$app.zip)
      showNotification(paste0("Zipping files set to: ",input$app.zip))
      logging(paste0("Zipping files set to: ",input$app.zip))
    }
  })
  
  observeEvent(input$app.keep, {
    if(app.keep!=input$app.keep){
      input$app.keep ->> app.keep
      showNotification(paste0("Keep objects set to: ",input$app.keep))
      logging(paste0("Keep objects set to: ",input$app.keep))
    }
  })
  
  observeEvent(input$app.gc, {
    print(gc(full = T)) -> gc.msg
    showNotification("Garbage collected, output written to console.")
  })
  
  observeEvent(input$tabset_data, {
    if(input$tabset_data=="Load file"){
      fileinput()
    }
    if(input$tabset_data=="Data (long)"){
      if(disp_rows()==0){
        disp_rows(nrow(df.l))
      }
      showNotification("Loading data...",duration = ceiling(disp_rows()/1000))
      output$dl.df.l <- downloadHandler(
        filename = function() {paste("df_l.csv")},
        content = function(file) {fwrite(df.l, file)}
      )
      output$df.l <- renderTable(head(df.l, disp_rows()), digits = 3)
    }
    if(input$tabset_data=="Data (upload)"){
      if(disp_rows()==0){
        disp_rows(nrow(df.u))
      }
      showNotification("Loading data...",duration = ceiling(disp_rows()/1000))
      output$df.u <- renderTable(head(df.u, disp_rows()))
    }
    if(input$tabset_data=="Data (TextGrids)"){
      if(disp_rows()==0){
        disp_rows(nrow(df.g))
      }
      showNotification("Loading data...",duration = ceiling(disp_rows()/1000))
      output$dl.df.g <- downloadHandler(
        filename = function() {paste("df_g.csv")},
        content = function(file) {fwrite(df.g, file)}
      )
      output$df.g <- renderTable(head(df.g, disp_rows()), digits = 3)
    }
  })
  
  observeEvent(input$savethis,{
    if(dir.exists(file.path(getwd(),"saved"))==F){
      dir.create(file.path(getwd(),"saved"))
    }
    for(n in names(reactiveValuesToList(savethis))){
      if(is.null(savethis[[n]])==F){
        if(str_detect(n,"plot")){
          suppressMessages(ggsave(file.path("saved",paste0(n,".png")), savethis[[n]], width = app.plot.w(),height = app.plot.h(),units = "px"))
        }
        if(str_detect(n,"tab")){
          fwrite(savethis[[n]],file.path("saved",paste0(n,".csv")))
        }
      }
    }
    for(o in ls(envir = .GlobalEnv)[str_detect(ls(envir = .GlobalEnv),"df.")]){
      fwrite(get(o),file.path("saved",paste0(o,".csv")))
    }
    logging(paste0("All current outputs saved in folder: ", file.path(getwd(),"saved")))
    write_file(logfile, file.path("saved", "logfile.txt"), append = F)
    if(zipfiles()==T){
      load.quiet("zip")
      list.files(file.path(getwd(),"saved"),full.names = T) -> file.list
      file.list[which(difftime(Sys.time(), file.info(file.list)$mtime,units = "m")<0.5)] -> file.list
      zip::zip(file.path("saved",paste0("cc_saved_",Sys.Date(),"_",max(df.l$cluster),".zip")),files = file.list, mode = "cherry-pick")
      unlink(file.list)
    }
    showNotification(paste0("All current outputs saved in folder: ", file.path(getwd(),"saved")), type = "message")
  })
  
  obs.upload <- observeEvent(input$upload, {
      fread(input$upload$datapath) ->> df.u
      logging(paste0("Data uploaded from file '",input$upload$name,"' (size: ",round(input$upload$size/1000000,2)," MB)."))
      removeTab(inputId = "tabset_data", target = "Read folder")
      obs.read_dir$destroy()
      obs.sel_filenames_switch$destroy()
      obs.sel_tier$destroy()
      obs.sel_interval$destroy()
      obs.to_measures$destroy()
      showTab(inputId = "tabset_data", target = "Data (upload)")
      as.list(colnames(df.u)) -> clms
      updateSelectInput(
        session,
        inputId = "cols.filename",
        choices = clms,
        selected = "filename"
      )
      updateSelectInput(
        session,
        inputId = "cols.start",
        choices = clms,
        selected = "start"
      )
      updateSelectInput(
        session,
        inputId = "cols.end",
        choices = clms,
        selected = "end"
      )
      updateSelectInput(
        session,
        inputId = "cols.speaker",
        choices = c("[map later]",clms),
        selected = "speaker"
      )
      updateSelectInput(
        session,
        inputId = "cols.interval_label",
        choices = clms,
        selected = "interval_label"
      )
      updateSelectInput(
        session,
        inputId = "cols.stepnumber",
        choices = clms,
        selected = "stepnumber"
      )
      updateSelectInput(
        session,
        inputId = "cols.f0",
        choices = clms,
        selected = clms[which(str_detect(clms,"f0"))[1]]
      )
      updateSelectInput(
        session,
        inputId = "cols.cc.int.dB",
        choices = as.list(c(" ", colnames(df.u))),
        selected = clms[which(str_detect(clms,"int.dB"))[1]]
      )
      updateSelectInput(
        session,
        inputId = "cols.cc.dur.s",
        choices = as.list(c(" ", colnames(df.u))),
        selected = clms[which(str_detect(clms,"dur"))[1]]
      )
      updateSelectInput(
        session,
        inputId = "cols.steptime",
        choices = as.list(c(" ", colnames(df.u))),
        selected = clms[which(str_detect(clms,"steptime"))[1]]
      )
      updateSelectInput(
        session,
        inputId = "cols.jumpkilleffect",
        choices = as.list(c(" ", colnames(df.u))),
        selected = "jumpkilleffect"
      )
      updateSelectInput(
        session,
        inputId = "cols.add1",
        choices = as.list(c(" ", colnames(df.u))),
        selected = ""
      )
      updateSelectInput(
        session,
        inputId = "cols.add2",
        choices = as.list(c(" ", colnames(df.u))),
        selected = ""
      )
      updateSelectInput(
        session,
        inputId = "cols.add3",
        choices = as.list(c(" ", colnames(df.u))),
        selected = ""
      )
      updateSelectInput(session,
                        "cols.f0.scale",
                        selected = ifelse(mean(as.numeric(df.u[[as.character(clms[which(str_detect(clms,"f0"))[1]])]]),na.rm = T)>75,"Hz",
                                          ifelse(mean(as.numeric(df.u[[as.character(clms[which(str_detect(clms,"f0"))[1]])]]),na.rm = T)>20,"ST","ERB")))
      output$sel_filenames <- renderUI({
        if(is.null(input$cols.filename)){
          return(NULL)
        }else{
        tags$div(title = "Select filename(s). Leave blank to select all.",
                 selectInput(
                   "cols.filename.sel",
                   "Select filename(s):",
                   choices = unique(df.u[[input$cols.filename]]),
                   selected = c(),
                   multiple = T,
                   width = '100%'
                 ))
        }
      })
      output$sel_ints <- renderUI({
        if(is.null(input$cols.interval_label)){
          return(NULL)
        } else {
        tags$div(title = "Select interval label(s). Leave blank to select all.",
                 selectInput(
                   "cols.interval.sel",
                   "Select interval label(s):",
                   choices = unique(df.u[[input$cols.interval_label]]),
                   selected = c(),
                   multiple = T,
                   width = '100%'
                 ))
        }
      })
  })
  
  obs.cols.speaker <- observe({
    if (is.null(input$cols.speaker)) {
      output$map_cols <- NULL
    } else {
      if (input$cols.filename == "" |
          input$cols.start == "" |
          input$cols.end == "" |
          input$cols.speaker == "" |
          input$cols.interval_label == "" |
          input$cols.stepnumber == "" | input$cols.f0 == "") {
        output$map_cols <- NULL
      } else {
        output$map_cols <- renderUI({span(
          tags$hr(),
          actionButton("map_cols", "Map columns", width = '100%')
        )})
      }
    }
  })
  
  obs.map_cols <- observeEvent(input$map_cols, {
    showNotification(id = "nt.mapcols", "Mapping columns...",duration = NULL)
    if(input$cols.speaker=="[map later]"){
      rep(NA, nrow(df.u)) -> spk.val
      showTab(inputId = "tabset_data", target = "Speakers")
      updateNavbarPage(session, "tabset_data",selected = "Speakers")
    }else{
      df.u[[input$cols.speaker]] -> spk.val
      showTab(inputId = "tabset_data", target = "Clean")
      updateNavbarPage(session, "tabset_data",selected = "Clean")
    }
    cbind(
      "filename" = df.u[[input$cols.filename]],
      "start" = df.u[[input$cols.start]],
      "end" = df.u[[input$cols.end]],
      "speaker" = spk.val,
      "interval_label" = df.u[[input$cols.interval_label]],
      "stepnumber" = df.u[[input$cols.stepnumber]],
      "f0" = df.u[[input$cols.f0]]
    ) ->> df.l
    for (n in c("cc.int.dB", "cc.dur.s", "steptime", "jumpkilleffect", "add1", "add2", "add3")) {
      if (nchar(input[[paste0("cols.", n)]]) != 0 &&
          input[[paste0("cols.", n)]] != " ") {
        cbind(df.l, df.u[[input[[paste0("cols.", n)]]]]) ->> df.l
        if (substr(n, 1, 3) == "add") {
          c(colnames(df.l)[-length(colnames(df.l))], input[[paste0("cols.", n)]]) ->> colnames(df.l)
        } else {
          c(colnames(df.l)[-length(colnames(df.l))], n) ->> colnames(df.l)
        }
      }
    }
    as.data.table(df.l) ->> df.l
    if(input$cols.f0.scale=="ERB"){
      round(as.numeric(df.l$f0), 3) ->> df.l$cc.f0.ERB
      f0.conv(df.l$f0,"ERB","Hz") ->> df.l$cc.f0.Hz
    }
    if(input$cols.f0.scale=="ST"){
      round(as.numeric(df.l$f0), 3) ->> df.l$cc.f0.ST
      f0.conv(df.l$f0,"ST","Hz") ->> df.l$cc.f0.Hz
    }
    if(input$cols.f0.scale=="Hz"){
      round(as.numeric(df.l$f0), 3) ->> df.l$cc.f0.Hz
    }
    subset(df.l, select = -f0) ->> df.l
    round(as.numeric(df.l$start), 3) ->> df.l$start
    round(as.numeric(df.l$end), 3) ->> df.l$end
    as.integer(df.l$stepnumber) ->> df.l$stepnumber
    npoints(max(df.l$stepnumber))
    if ("int" %in% colnames(df.l)) {
      round(as.numeric(df.l$int), 3) ->> df.l$int
    }
    for (a in c("add1", "add2", "add3")) {
      if (input[[paste0("cl.", a)]] == "num") {
        round(as.numeric(df.l[[input[[paste0("cols.", a)]]]]), 3) ->> df.l[[input[[paste0("cols.", a)]]]]
      }
      if (input[[paste0("cl.", a)]] == "cc") {
        round(as.numeric(df.l[[input[[paste0("cols.", a)]]]]), 3) ->> df.l[[input[[paste0("cols.", a)]]]]
        ifelse(substr(input[[paste0("cols.", a)]],1,3)=="cc.", input[[paste0("cols.", a)]], paste0("cc.", input[[paste0("cols.", a)]])) ->> names(df.l)[names(df.l) == input[[paste0("cols.", a)]]]
      }
    }
    for (c in names(which(unlist(lapply(df.l,class))=="character"))){
      if(all.digits(gsub("\\.","",paste(unique(na.omit(df.l[[c]])),collapse = "")))){
        as.numeric(df.l[[c]]) ->> df.l[[c]]
      }
    }
    if(is.null(input$cols.filename.sel)==F){
      subset(df.l, filename %in% input$cols.filename.sel) ->> df.l
    }
    if(is.null(input$cols.interval.sel)==F){
      subset(df.l, interval_label %in% input$cols.interval.sel) ->> df.l
    }
    removeTab(inputId = "tabset_data", target = "Data (upload)")
    rm(df.u, envir = .GlobalEnv)
    showTab(inputId = "tabset_data", target = "Data (long)")
    removeNotification(id = "nt.mapcols", session)
  })
  
  obs.read_dir <- observeEvent(input$read_dir, {
    removeTab(inputId = "tabset_data", target = "Load file")
    obs.upload$destroy()
    obs.cols.speaker$destroy()
    obs.map_cols$destroy()
    inDir(input$dir_input)
    if (dir.exists(inDir()) == F) {
      showNotification("Specified folder does not exist.", type = "error")
    }
    else if (length(list.files(inDir(), pattern = "*.TextGrid", ignore.case = T)) == 0 || length(list.files(inDir(), pattern = "*.wav|*.mp3", ignore.case = T)) == 0) {
      showNotification("No audio and/or TextGrids in folder.", type = "error")
    } else {
      load.quiet("readtextgrid")
      if (substr(inDir(), nchar(inDir()), nchar(inDir())) != .Platform$file.sep) {
        showNotification(paste0("Added final ", .Platform$file.sep, " to path"), type = "warning")
        inDir(paste0(inDir(), .Platform$file.sep))
      }
      if (input$snd_ext == 1) {
        snd.ext(".wav")
      }
      if (input$snd_ext == 2) {
        snd.ext(".mp3")
      }
      list.files(inDir(), pattern = "*.TextGrid", ignore.case = T) -> file_names
      as.data.frame(str_split(file_names, pattern = "\\.", simplify = T))[, 1] -> file_names
      as.data.frame(file_names) -> file_names
      file_names$rm <- 0
      for (r in (1:nrow(file_names))) {
        if (file.exists(paste0(inDir(), file_names$file_names[r], snd.ext())) == F) {
          1 -> file_names$rm[r]
        } else {
          0 -> file_names$rm[r]
        }
      }
      file_names[file_names$rm == 0, 1] -> file_names
      df.g <- c()
      withProgress(message = "Reading folder ", {
        for (g in file_names) {
          incProgress(1 / ((length(
            file_names
          ) * 1.2)),detail = g)
          if (!as.character(guess_encoding(paste0(inDir(), g, ".TextGrid"))[1, 1]) %in% c("UTF-8", "ASCII")) {
            showNotification("Cannot read TextGrids. Convert encoding of TextGrids to UTF-8 or ASCII.", type = "error")
            showNotification(paste0("Error occurred with file: ", inDir(), g, ".TextGrid.\n"),type = "warning", duration = NULL)
            logging(paste0("Error occurred with file: ", inDir(), g, ".TextGrid."))
            break
          }
          if (substr(read_lines(paste0(inDir(), g, ".TextGrid"), n_max = 1, skip = 3), 1, 4) != "xmin") {
            showNotification("Cannot read TextGrids. Choose only TextGrids in long format.", type = "error")
            showNotification(paste0("Error occurred with file: ", inDir(), g, ".TextGrid.\n"),type = "warning", duration = NULL)
            logging(paste0("Error occurred with file: ", inDir(), g, ".TextGrid."))
            break
          }
          read_textgrid(paste0(inDir(), g, ".TextGrid")) -> tgrid
          tgrid[tgrid$tier_type == "IntervalTier", ] -> tgrid
          rbind(df.g, tgrid) -> df.g
        }
        as.data.table(df.g) -> df.g
        substr(df.g$file, 1, nchar(df.g$file) - 9) -> df.g$name_trim
        df.g ->> df.g
        showTab("tabset_data", "Data (TextGrids)")
        logging(paste0("Sound files and TextGrids read from folder: ", inDir()))
      })
      output$to_measures <- renderUI(span(
        tags$hr(),
        tags$div(
          title = "Continue to acoustic analysis settings using the selected filenames and interval tier",
          actionButton("to_measures", label = "To acoustic analysis", width = '100%'))
      ))
      output$file_names <- renderUI(fluidPage(
        HTML('<br>'),
        HTML(
          '<b>Filenames for which audio and TextGrid were found in folder:</b>',
          '<br>'
        ),
        checkboxInput(
          inputId = "sel_filenames_switch",
          label = "Select all/none",
          value = T
        ),
        HTML('<br>'),
        fluidRow(
          column(width = 2),
          column( 
            width = 7,
            align = "left",
            checkboxGroupInput(
              "sel_filenames",
              label = NULL,
              choices = unique(df.g$name_trim),
              selected = unique(df.g$name_trim)
            )
          )
        )
      ))
      output$sel_tier <- renderUI({
        span(
          tags$hr(),
          div(title = "Select tier(s) from the available interval tiers in the TextGrids. Selecting multiple tiers is possible, however only advised if the (phonological) units segmented on the tiers are comparable. In case of multiple tiers, the tier name will be prepended to the interval label.",
              isolate(
          selectInput(
            "sel_tier",
            "Select interval tier(s) for analysis:",
            choices = unique(df.g$tier_name),
            selected = 1,
            multiple = T,
            width = '100%'
          ))),
          div(title = "Select interval(s) from the selected interval tier(s) in the TextGrids. Leave blank to select all non-empty.",
              selectInput(
                "sel_interval",
                "Select interval label(s) (optional):",
                choices = c(),
                selected = 1,
                multiple = T,
                width = '100%'
              )),
        )
      })
    }
  })
  
  obs.sel_filenames_switch <- observeEvent(input$sel_filenames_switch, {
    if (input$sel_filenames_switch == T) {
      updateCheckboxGroupInput(session,
                               inputId = "sel_filenames",
                               selected = unique(df.g$name_trim))
    } else {
      updateCheckboxGroupInput(session, inputId = "sel_filenames", selected = "")
    }
  }, ignoreNULL = T)
  
  obs.sel_tier <- observeEvent(input$sel_tier, {
    if(is.null(input$sel_interval)){
      int_text <- unique(df.g$text[df.g$text!="" & df.g$tier_name %in% input$sel_tier])
    } else {
      int_text <- c(input$sel_interval)
    }
    updateCheckboxGroupInput(session,
                             inputId = "sel_filenames",
                             selected = unique(df.g$name_trim[df.g$tier_name %in% input$sel_tier & df.g$text %in% int_text]))
    updateSelectInput(session, "sel_interval",choices = unique(df.g$text[df.g$text!="" & df.g$tier_name %in% input$sel_tier]),selected = c())
    if(is.null(input$sel_tier)){
      updateCheckboxGroupInput(session,
                               inputId = "sel_filenames",
                               selected = unique(df.g$name_trim))
    }
  },ignoreNULL = F,ignoreInit = T)
  
  obs.sel_interval <- observeEvent(input$sel_interval, {
    if(is.null(input$sel_interval)){
      int_text <- unique(df.g$text[df.g$text!="" & df.g$tier_name %in% input$sel_tier])
    } else {
      int_text <- c(input$sel_interval)
    }
    updateCheckboxGroupInput(session,
                             inputId = "sel_filenames",
                             selected = unique(df.g$name_trim[df.g$tier_name %in% input$sel_tier & df.g$text %in% int_text]))
  },ignoreNULL = F,ignoreInit = T)
  
  obs.to_measures <- observeEvent(input$to_measures, {
    if(is.null(input$sel_interval)){
      int_text <- unique(df.g$text[df.g$text!="" & df.g$tier_name %in% input$sel_tier])
    } else {
      int_text <- c(input$sel_interval)
    }
    if (nrow(
      subset(
        df.g,
        tier_name %in% c(input$sel_tier) &
        text %in% int_text &
        name_trim %in% input$sel_filenames
      )
    ) == 0) {
      showNotification("With the current selection there are no intervals left for analysis.",
                       type = "error")
    } else {
      subset(
        df.g,
        tier_name %in% c(input$sel_tier) &
          text %in% int_text &
          name_trim %in% input$sel_filenames
      ) ->> df.g
      if(length(which(df.g$xmin==df.g$tier_xmin | df.g$xmax==df.g$tier_xmax))>0){
        showNotification("Selected tier(s) have boundaries that coincide with audio file start/end. Corrected by a 1 ms shift.",type = "warning")
        ifelse(df.g$xmin==df.g$tier_xmin,df.g$xmin+0.001,df.g$xmin) ->> df.g$xmin
        ifelse(df.g$xmax==df.g$tier_xmax,df.g$xmax-0.001,df.g$xmax) ->> df.g$xmax
        logging("Selected tier(s) have boundaries that coincide with audio file start/end. Corrected by a 1 ms shift.")
      }
      if(length(input$sel_tier)>1){
        paste0(df.g$tier_name,"_",df.g$text) ->> df.g$text
      }
      logging(paste0("Tier(s) '",paste(input$sel_tier,collapse = ", "),
                     ifelse(is.null(input$sel_interval),"",paste0("' and interval(s) '",input$sel_interval)),
                     "' selected for analysis, from ",length(unique(df.g$name_trim)), " filename(s), totalling ",nrow(df.g)," intervals."))
      showTab("tabset_data", "Acoustics")
      updateNavbarPage(session, inputId = "tabset_data", selected = "Acoustics")
    }
  })
  
  obs.npoints.f0 <- observeEvent(c(input$npoints.f0), {
    npoints(input$npoints.f0)
  })

  obs.npoints.int <- observeEvent(input$npoints.int, {
    npoints(input$npoints.int)
  })
  
  obs.do_sample <- observeEvent(input$do_sample, {
    m.rows(sort(sample(1:nrow(df.g), input$n_sample)))
    showNotification(paste0("New sample taken (n = ",input$n_sample,")"))
  })
  
  obs.disp_sample <- observeEvent(input$disp_sample, {
    showNotification(id = "nt.sample", "Sampling...",duration = NULL)
    load.quiet("sound")
    samplemode(0)
    if (is.null(m.rows()) || length(m.rows())==nrow(df.g)) {
      m.rows(sort(sample(1:nrow(df.g), input$n_sample)))
    }
    if(dir.exists("www")==F){
      dir.create("www")
    }
    for(x in 1:length(m.rows())){
      if(df.g$xmin[isolate(m.rows()[x])]==df.g$tier_xmin[isolate(m.rows()[x])] && df.g$xmax[isolate(m.rows()[x])]==df.g$tier_xmax[isolate(m.rows()[x])]){
        loadSample(paste0(inDir(), df.g$name_trim[isolate(m.rows()[x])], snd.ext())) -> snd  
      }else{
        cutSample(paste0(inDir(), df.g$name_trim[isolate(m.rows()[x])], snd.ext()),
                  df.g$xmin[isolate(m.rows()[x])],
                  df.g$xmax[isolate(m.rows()[x])]) -> snd
      }
      saveSample(snd,file.path("www",paste0("play",x,".wav")),overwrite = T)
    }
    make.df.l()
    tsf0()
    plotServerList <- lapply(1:length(isolate(m.rows())), function(i) {
      plotServer(
        paste0("plot", i),
        df.g$name_trim[isolate(m.rows()[i])],
        round(df.g$xmin[isolate(m.rows()[i])], 3),
        isolate(npoints()),
        input$f0.min,
        input$f0.max
      )
    })
    playServerList <- lapply(1:length(isolate(m.rows())), function(i) {
      playServer(paste0("play", i))
    })
    samplemode(1)
    removeNotification(id = "nt.sample",session)
  })
  
  obs.do_measure <- observeEvent(input$do_measure, {
   samplemode(0)
    suppressWarnings(unlink("www", recursive = T))
    if(any(input$incl_f0, input$incl_int, input$incl_dur)==F){
      showNotification("No acoustic cues selected.", type = "error")
    }else{
      m.rows(1:nrow(df.g))
      make.df.l()
        if(input$incl_f0==T){
          tsf0()
          subset(df.l, select = -c(steptime, track)) ->> df.l
          logging(paste0("F0 measures taken with the following settings: ",input$npoints.f0, " measurement points, ",input$f0.min,"-",input$f0.max," Hz range, timestep: ",input$f0.timestep," ms, f0 fit: ",input$f0.fit,", smoothing: ",input$f0.smooth_bw,"."))
        }
        if(input$incl_int==T){
          tsint()
          logging(paste0("Intensity measures taken with the following settings: dB scale, ",input$npoints.int, " measurement points."))
        }
        if(input$incl_dur==T){
          df.l$end[df.l$stepnumber==1]-df.l$start[df.l$stepnumber==1] ->> df.l$cc.dur.s[df.l$stepnumber==1]
          logging("Duration measures taken in seconds.")
        }
      showTab("tabset_data", "Speakers")
      showTab("tabset_data", "Data (long)")
      updateNavbarPage(session, "tabset_data",selected = "Speakers")
    }
  })
  
  obs.spk.cols <- observeEvent(input$spk.cols,{
      seps(
        unique(
          as.vector(
            str_extract_all(
              as.vector(
                unlist(
                  subset(df.l, select = input$spk.cols),
                )), '[[:punct:]]',simplify = T))))
    updateSelectInput(session,"spk.sep",choices = c(" ", seps()))
  })
  
  obs.spk.mapcol <- observeEvent(input$spk.mapcol, {
    as.vector(
      str_split(
        string = df.l[,get(input$spk.cols)],
        pattern = input$spk.sep,simplify = T)[,which(colnames(as.data.table(str_split(string = df.l[,get(input$spk.cols)],
                                                                                      pattern = input$spk.sep,
                                                                                      simplify = T)))==input$spk.sepcol)
        ]
    ) ->> df.l$speaker
    logging("Speaker column added.")
    showNotification("Speaker column mapped.")
    removeTab(inputId = "tabset_data", target = "Speakers")
    showTab("tabset_data", "Clean")
    updateNavbarPage(session, "tabset_data",selected = "Clean")
    obs.spk.cols$destroy()
    obs.spk.mapcol$destroy()
  })
  
  obs.clean.set <- observeEvent(input$clean.set,{
    clean.set(isolate(input$clean.set))
  })
  
  obs.clean.recheck <- observeEvent(input$clean.recheck,{
    unique(c(which(interaction(df.l$filename,df.l$start) %in% unique(interaction(df.l$filename,df.l$start)[is.na(df.l$f)])),
      which(interaction(df.l$filename,df.l$start) %in% unique(interaction(df.l$filename,df.l$start)[is.na(df.l$speaker)])),
      which(interaction(df.l$filename,df.l$start) %in% unique(interaction(df.l$filename,df.l$start)[is.na(df.l$cc.f0.Hz)])),
      which(is.na(as.vector(unique(interaction(df.l$filename,df.l$start)[df.l$cc.f0.Hz < 0])))==F),
      which(interaction(df.l$filename,df.l$start) %in% unique(interaction(df.l$filename,df.l$start)[df.l$interval_label==" "]))
    )) -> rm
    if(length(rm)!=0){
    df.l[-rm,] ->> df.l
    }
    if("cc.f0.Hz" %in% colnames(df.l)){
      for(r in which(df.l$stepnumber==1)){
        if(df.l$d.f0[r]>isolate(clean.set())){
          1 ->> df.l$d.f0[r:(r+max(df.l$stepnumber,na.rm = T)-1)]
        }else{
          0 ->> df.l$d.f0[r:(r+max(df.l$stepnumber,na.rm = T)-1)]
        }
        }
        df.l[df.l$d.f0!=1,] ->> df.l
    }
    check.clean()$text -> clean.text
    logging(paste0("Cleaning applied:\n\t",gsub("\n","\n\t",clean.text)))
    output$clean.text <- renderText(clean.text)
  },once = T)
  
  obs.clean.cont <- observeEvent(input$clean.cont, {
    if("d.f0" %in% colnames(df.l)){
      subset(df.l, select = -d.f0) ->> df.l
    }
    removeNotification("not.scr",session)
    obs.scr$destroy()
    removeTab("tabset_data", "Read folder")
    obs.read_dir$destroy()
    obs.sel_filenames_switch$destroy()
    obs.sel_tier$destroy()
    obs.sel_interval$destroy()
    obs.to_measures$destroy()
    removeTab(inputId = "tabset_data", target = "Speakers")
    showTab(inputId = "tabset_data", target = "Representation")
    updateNavbarPage(session, "tabset_data", "Representation")
    obs.spk.cols$destroy()
    obs.spk.mapcol$destroy()
    obs.clean.set$destroy()
    obs.clean.recheck$destroy()
    obs.clean.ign$destroy()
    obs.clean.cont$destroy()
  })
  
  obs.clean.ign <- observeEvent(input$clean.ign,{
    if(input$clean.ign==T){
    showNotification("You were warned!", type = "warning",closeButton = F)
      output$clean.cont <- renderUI({actionButton("clean.cont", "Continue with cleaned data")})
    }else{
      output$clean.cont <- renderUI({actionButton("clean.cont", "Continue with cleaned data",disabled = T)})
    }
  })
  
  observeEvent(input$f0.corr,{
    if(input$f0.corr=="oMe"){
      updateSelectInput(session,"f0.scale",choices = "Hz")
    }else{
      updateSelectInput(session,"f0.scale",choices = c("Hz","ERB","ST"),selected = input$f0.scale)
    }
  })
  
observeEvent(input$do.f0.repr,{
    do.repr("cc")$colname -> n.repr
    output$cc.cols <- renderText({paste(gsub("cc.","",colnames(df.l)[which(substr(colnames(df.l),1,3)=="cc.")]),collapse = "\n")})
  })
  
  observeEvent(input$do.int.repr,{
    if(input$int.corr!="none"){
      for (f in unique(df.l[[input$int.corr]])) {
        round((df.l$cc.int.dB[df.l[[input$int.corr]] == f] - mean(df.l$cc.int.dB[df.l[[input$int.corr]] == f])) /
          sd(df.l$cc.int.dB[df.l[[input$int.corr]] == f]),3) ->> df.l$cc.int.std[df.l[[input$int.corr]] == f]
      }
    }
    logging(paste0("Intensity representation added: 'cc.int.std' (",input$int.corr," based)."))
    output$cc.cols <- renderText({paste(gsub("cc.","",colnames(df.l)[which(substr(colnames(df.l),1,3)=="cc.")]),collapse = "\n")})
  })
  
  observeEvent(input$do.dur.repr,{
    if(input$dur.corr!="none"){
      for (f in unique(df.l[[input$dur.corr]])) {
        round((df.l$cc.dur.s[df.l[[input$dur.corr]] == f & df.l$stepnumber==1] - mean(df.l$cc.dur.s[df.l[[input$dur.corr]] == f], na.rm = T)) /
          sd(df.l$cc.dur.s[df.l[[input$dur.corr]] == f], na.rm = T),3) ->> df.l$cc.dur.std[df.l[[input$dur.corr]] == f & df.l$stepnumber==1]
      }
    }
    logging("Duration representation added: 'cc.dur.std' (speaker based).")
    output$cc.cols <- renderText({paste(gsub("cc.","",colnames(df.l)[which(substr(colnames(df.l),1,3)=="cc.")]),collapse = "\n")})
  })
  
  observeEvent(input$cont.repr,{
    which(substr(colnames(df.l),1,3)=="pc.") -> pc.cols
    if(length(pc.cols)!=0){
      subset(df.l, select = -pc.cols) ->> df.l
    }
    removeTab("tabset_data", "Acoustics")
    obs.npoints.f0$destroy()
    obs.npoints.int$destroy()
    obs.do_sample$destroy()
    obs.disp_sample$destroy()
    obs.do_measure$destroy()
    showTab(inputId = "tabs_main", target = "Clustering")
    updateNavbarPage(session, "tabs_main", "Clustering")
    if(object.size(df.l)>1000000){
    showNotification("Potentially large dataset detected. Consider running PAM clustering to reduce runtime on slow machines.", type = "warning")
    }
  })
  
  observeEvent(input$cc.vars,{
    if(any(is.na(subset(df.l, select = paste0("cc.",input$cc.vars))))){
      updateSelectInput(session, "cc.distm",
        choices = list(
          "euclidean (L2 norm = RMSE)" = "euclidean",
          "dynamic time warping" = "dtw_basic"
        ),
        selected = "euclidean")
    } else {
      updateSelectInput(session, "cc.distm",
                        choices = list(
                          "euclidean (L2 norm = RMSE)" = "euclidean",
                          "mean absolute scaled error (MASE)" = "mase",
                          "dynamic time warping" = "dtw_basic",
                          "pearson (sqrt(2*(1-ρ)))" = "cor"
                        ),
                        selected = "euclidean")
    }
  })
  
  observeEvent(input$cc.Ncl, {
    show.contab(0)
    if(input$cc.Ncl>(nrow(df.l)/max(df.l$stepnumber)))
      {
      updateSliderInput(session,"cc.Ncl",value = nrow(df.l)/max(df.l$stepnumber))
      showNotification("Maximum number of clusters should not exceed number of observations.",type = "error")
      }else{
        nclust(input$cc.Ncl)
        }
  })
  
  observeEvent(input$plot.cols,{
    height(ceiling(nclust()/input$plot.cols)*400)
  })
  
  observeEvent(input$do.cc,{
    height(ceiling(nclust()/input$plot.cols)*400)
    cc.y1(input$plot.y1)
    do.cc(nclust(),0)
    output$dl.dendro <- downloadHandler(
      filename = function() {paste("dendrogram.png")},
      content = function(file) {suppressMessages(ggsave(file, savethis$d_plot, width = app.plot.w(),height = app.plot.h(),units = "px"))}
    )
    output$dl.plot <- downloadHandler(
      filename = function() {paste("plot.png")},
      content = function(file) {suppressMessages(ggsave(file, savethis$c_plot, width = app.plot.w(),height = app.plot.h(),units = "px"))}
    )
    output$dl.table <- downloadHandler(
      filename = function() {paste("table.csv")},
      content = function(file) {fwrite(savethis$s_tab, file)}
    )
    output$dl.proto <- downloadHandler(
      filename = function() {paste("proto.csv")},
      content = function(file) {fwrite(savethis$p_tab, file)}
    )
    output$cl.dendro <- renderPlot(savethis$d_plot)
    output$cl.plot <- renderPlot(savethis$c_plot, height = height())
    output$cl.table <- renderTable(savethis$s_tab)
    output$cl.sel <- renderUI({verticalLayout(HTML('<br>'),
                                              div(title = "Removes the selected clusters from the data. This will affect the next clustering and cannot be reversed within one session. Only recommended to discard outliers or erroneous observations.", actionButton("cl.rem","Remove clusters:",width = '100%')),
                                              HTML('<br>'),
                                              isolate(selectInput("cl.sel",label = NULL, choices = 1:nclust(),selected = which(savethis$s_tab$flagged=="*"),multiple = T,width = '100%'))
                                              )})
    showTab(inputId = "tabs_cl", target = "Plot")
    showTab(inputId = "tabs_cl", target = "Table")
    updateSelectInput(session, "cl.contab.colx",selected = "")
    showTab(inputId = "tabs_cl", target = "Evaluate")
    showTab(inputId = "tabs_cl", target = "Prototypes")
  })
  
  observeEvent(input$cl.rem, {
    show.contab(0)
    updateActionButton(session,"savethis",disabled = T)
    subset(df.l, !(cluster %in% input$cl.sel)) ->> df.l
    updateSelectInput(session,"cl.sel",choices = unique(df.l$cluster),selected = "")
    if(input$cc.Ncl>(nrow(df.l)/max(df.l$stepnumber)))
    {
      updateSliderInput(session,"cc.Ncl",value = nrow(df.l)/max(df.l$stepnumber))
      showNotification("Maximum number of clusters should not exceed number of observations",type = "error")
    }
    if((nrow(df.l)/max(df.l$stepnumber))<2){
      updateActionButton(session,"do.cc",disabled = T)
      showNotification("No observations left to cluster",type = "error")
    }
    showNotification(paste0("Cluster ",paste(input$cl.sel,collapse = ", ")," removed from data. Redo clustering to process remaining data."),type = "warning")
    logging(paste0("Cluster ",paste(input$cl.sel,collapse = ", ")," removed from data."))
  })
  
  observeEvent(input$do.eval, {
    isolate(do.eval()$plot) -> p
    output$dl.evalplot <- downloadHandler(
      filename = function() {paste("eval_plot.png")},
      content = function(file) {suppressMessages(ggsave(file, p, width = app.plot.w(),height = app.plot.h(),units = "px"))}
    )
    output$eval.plot <- renderPlot({p})
    output$dl.evaltab <- downloadHandler(
      filename = function() {paste("eval_table.csv")},
      content = function(file) {fwrite(df.e, file)}
    )
    output$eval.tab <- renderTable(df.e, digits = 3)
  })
  
# functions ####
  
  load.quiet <- function(p) {
    suppressPackageStartupMessages(library(p,warn.conflicts = F,quietly = T,character.only = T))
  }
  
  logging <- function(l) {
    paste0(logfile, Sys.time(), ": ", l, "\n") ->> logfile
  }
  
  fileinput <- function() {
  output$upload <- renderUI({
    tags$div(
      title = paste0("Upload long data file (each row represents a measurement point). Current file upload limit is ",floor((app.fileinput() * 1024^2)/1000000)," MB. Go to 'Settings' to change the limit."),
      fileInput(
        "upload",
        label = NULL,
        accept = ".csv",
        buttonLabel = "Find .csv",
        placeholder = paste0("requires long data file (max ",floor((app.fileinput() * 1024^2)/1000000)," MB)"),
        multiple = F
      )
    )
  })
  }
  
  f0.conv <- function(f0,from,to){
    if(from=="Hz" && to=="ST"){
      return(log10((f0 / 50)) * 39.87)
    }
    if(from=="ST" && to=="Hz"){
      return(50*(2 ^ (f0/12)))
    }
    if(from=="Hz" && to=="ERB"){
      return(16.7*log10((0.006046*f0)+1))
    }
    if(from=="ERB" && to=="Hz"){
      return(165.4 * (10^(0.06*f0)-1))
    }
  }
  
  make.df.l <- function() {
    c() -> dfl
    for (r in m.rows()) {
      rbind(
        dfl,
        cbind(
          "filename" = rep(df.g$name_trim[r], npoints()),
          "start" = rep(df.g$xmin[r], npoints()),
          "end" = rep(df.g$xmax[r], npoints()),
          "speaker" = rep(NA, npoints()),
          "interval_label" = rep(df.g$text[r], npoints()),
          "steptime" = df.g$xmin[r] + cumsum(rep((df.g$xmax[r] - df.g$xmin[r]) /
                                                   (npoints() + 1),
                                                 npoints()
          )),
          "stepnumber" = 1:npoints()
        )
      ) -> dfl
    }
    as.data.table(dfl) -> dfl
    round(as.numeric(dfl$start), 3) -> dfl$start
    round(as.numeric(dfl$end), 3) -> dfl$end
    round(as.numeric(dfl$steptime), 3) -> dfl$steptime
    as.integer(dfl$stepnumber) -> dfl$stepnumber
    dfl ->> df.l
  }
  
  tsf0 <- function() {
    load.quiet("wrassp")
    f0 <- c()
    track <- c()
    withProgress(message = paste0("F0 "), {
    for (r in m.rows()) {
      incProgress(1 / ((length(
        m.rows()
      ) * 1.1)),detail = df.g$name_trim[r])
      if ((df.g$xmax[r] - df.g$xmin[r]) > (input$f0.timestep / 500)) {
        mhsF0(
          paste0(inDir(), df.g$name_trim[r], snd.ext()),
          beginTime = df.g$xmin[r],
          endTime = df.g$xmax[r],
          minF = as.numeric(input$f0.min),
          maxF = as.numeric(input$f0.max),
          windowShift = as.numeric(input$f0.timestep),
          minProb = as.numeric(input$f0.fit),
          toFile = F
        ) -> f0.object
        as.vector(ifelse(f0.object$pitch == 0, NA, f0.object$pitch)) -> f0.values
        if (sum(is.na(f0.values)) == length(f0.values)) {
          rep(NA, npoints()) -> f0.values
          rep(NA, npoints()) -> f0.track
        } else {
          approx(f0.values, n = npoints(), na.rm = F)$y -> f0.values
          f0.values -> f0.track
          na.approx(f0.values, na.rm = F) -> f0.values
          approxExtrap(
            1:npoints(),
            f0.values,
            xout = 1:npoints(),
            method = "constant"
          )$y -> f0.values
          ksmooth(
            1:npoints(),
            f0.values,
            kernel = "normal",
            bandwidth = input$f0.smooth_bw,
            n.points = npoints()
          )$y -> f0.values
          round(f0.values,3) -> f0.values
        }
      }
      append(f0, f0.values) -> f0
      append(track, f0.track) -> track
    }
    })
    cbind(df.l, "track" = as.numeric(track)) -> df.l
    cbind(df.l, "cc.f0.Hz" = as.numeric(f0)) -> df.l
    df.l ->> df.l
  }
  
  tsint <- function() {
    load.quiet("wrassp")
    int <- c()
    withProgress(message = paste0("Intensity "), {
    for (r in m.rows()) {
      incProgress(1 / ((length(
        m.rows()
      ) * 1.1)),detail = df.g$name_trim[r])
    rmsana(
      paste0(inDir(), df.g$name_trim[r], snd.ext()),
      beginTime = df.g$xmin[r],
      endTime = df.g$xmax[r],
      toFile = F
    )$rms -> int.values
    if(ncol(int.values)>1){
      rowMeans(int.values) -> int.values
    }
    na.approx(int.values, na.rm = F) -> int.values
    approx(int.values, n = npoints())$y -> int.values
    append(int, int.values) -> int
    }
    })
    cbind(df.l, "cc.int.dB" = round(as.numeric(int),3)) -> df.l
    df.l ->> df.l
  }
  
  plotUI <- function(id) {
    ns <- NS(id)
      div(align = "center",
          style = "padding-bottom: 1em;",
          plotOutput(ns("plot"), width = "50%"))
  }
  
  playUI <- function(id) {
    ns <- NS(id)
    div(align = "center",
        style = "padding-bottom: 1em;",
        uiOutput(ns("play"), width = "50%"))
  }
  
  playServer <- function(id){
    moduleServer(id, function(input, output, session) {
      output$play <- renderUI({tags$audio(src = paste0(id,".wav"), type = "audio/wav", controls = NA)})
    })
  }
  
  plotServer <- function(id, f, t, n, i, a) {
    moduleServer(id, function(input, output, session) {
      output$plot <- renderPlot({
        ggplot(mapping = aes(x = df.l$steptime[df.l$filename == f &
                                                 df.l$start == t])) +
          geom_line(mapping = aes(y = df.l$track[df.l$filename == f &
                                                   df.l$start == t]), na.rm = TRUE) +
          geom_line(
            mapping = aes(y = df.l$cc.f0.Hz[df.l$filename == f & df.l$start == t]),
            linetype = "dashed",
            colour = "#FF0000",
            na.rm = TRUE
          ) +
          coord_cartesian(ylim = c(i, a)) +
          labs(
            title = paste0("filename: ", f),
            subtitle = paste0("interval label: ", df.l$interval_label[df.l$filename ==
                                                                        f & df.l$start == t][1]),
            x = "Interval time (s)",
            y = "f0 (Hz)"
          ) -> p
        if (sum(is.na(df.l$cc.f0.Hz[df.l$filename == f &
                                    df.l$start == t])) == n) {
          p +
            annotate(
              "text",
              x = df.l$steptime[df.l$filename == f &
                                  df.l$start == t][floor(n / 2)],
              y = mean(c(i, a)),
              label = "No reliable f0 detected.",
              colour = "#FF0000",
              size = 7
            ) -> p
        }
        p + theme(text = element_text(size = 20)) -> p
        p
      })
    })
  }
  
  
  check.clean <- function() {
    showNotification(id = "check.clean","Checking data for errors...",duration = NULL)
    length(unique(df.l$filename)) -> unq.f
    length(unique(df.l$speaker)) -> unq.s
    length(unique(df.l$stepnumber)) -> unq.step
    nrow(df.l)/unq.step -> unq.i
    length(unique(interaction(df.l$filename,df.l$start)[is.na(df.l$f)])) -> na.f
    length(unique(interaction(df.l$filename,df.l$start)[is.na(df.l$speaker)])) -> na.s
    length(unique(interaction(df.l$filename,df.l$start)[df.l$interval_label==" "])) -> na.i
    if("cc.f0.Hz" %in% colnames(df.l)){
    length(unique(interaction(df.l$filename,df.l$start)[is.na(df.l$cc.f0.Hz)])) -> na.f0
    length(which(is.na(as.vector(unique(interaction(df.l$filename,df.l$start)[df.l$cc.f0.Hz < 0])))==F)) -> neg.f0
    0 ->> df.l$d.f0
    for(r in 1:nrow(df.l)){
      if(df.l$stepnumber[r]!=1 &&
         is.na(df.l$cc.f0.Hz[r])==F &&
         is.na(df.l$cc.f0.Hz[r-1])==F &&
         all(df.l$cc.f0.Hz[c(r,r-1)]>0)){
        (log10((df.l$cc.f0.Hz[r] / df.l$cc.f0.Hz[r-1])) * 39.87) * (1 / ((df.l$end[r] - df.l$start[r]) / (unq.step + 1))) -> d
        if (d >= 72 | d <= -96) {
          df.l$d.f0[r]+1 ->> df.l$d.f0[r]
        }
      }
      if(df.l$stepnumber[r]==max(df.l$stepnumber,na.rm = T)){
        sum(df.l$d.f0[(r-max(df.l$stepnumber,na.rm = T)+2):r]) ->> df.l$d.f0[r-max(df.l$stepnumber,na.rm = T)+1]
      }
    }
    length(which(df.l$d.f0[df.l$stepnumber==1]>isolate(clean.set()))) -> d.f0
    paste0(
    na.f0," intervals have missing f0 values","\n",
    neg.f0," intervals have negative f0 values","\n",
    d.f0, " intervals have a rate of f0 change beyond the maximum (",paste0(paste0(as.vector(table(df.l$d.f0[df.l$stepnumber==1])),"x",names(table(df.l$d.f0[df.l$stepnumber==1]))), collapse = ", "),")"
    ) -> f0.prb
    } else {
      0 -> d.f0
      0 -> na.f0
      0 -> neg.f0
      paste0(
       "0 problems found in f0 values (none in data)","\n"
      ) -> f0.prb
    }
    if(sum(na.f,na.s,na.f0,neg.f0,na.i,d.f0)==0){
      output$clean.set <- renderUI({div(title = "Maximum number of cases per contour for which the f0 velocity between two measurement points is allowed to be beyond the maximum rates are taken from Xu & Sun (2002, Table X; 72 ST/s for rises and 96 ST/s for falls, doi:10.1121/1.1445789).",
                                        isolate(numericInput("clean.set","Max N velocity exceedings",value = 0,min = 0,max = max(df.l$stepnumber,na.rm = T)-1,step = 1)))})
      output$clean.recheck <- renderUI({div(title = "Performs cleaning and checking of data for: missing filenames, speaker IDs or interval labels. If included, f0 is checked for missing or negative values and for rising/falling rates. Maximum rates are taken from Xu & Sun (2002, Table X; 72 ST/s for rises and 96 ST/s for falls, doi:10.1121/1.1445789).",
                                            actionButton("clean.recheck","Clean and re-check",disabled = T))})
      output$clean.cont <- renderUI({actionButton("clean.cont", "Continue with cleaned data")})
      output$clean.ign <- renderUI({NULL})
    } else {
      output$clean.set <- renderUI({div(title = "Maximum number of cases per contour for which the f0 velocity between two measurement points is allowed to be beyond the maximum rates are taken from Xu & Sun (2002, Table X; 72 ST/s for rises and 96 ST/s for falls, doi:10.1121/1.1445789).",
                                        isolate(numericInput("clean.set","Max N velocity exceedings",value = 0,min = 0,max = max(df.l$stepnumber,na.rm = T)-1,step = 1)))})
      output$clean.recheck <- renderUI({div(title = "Performs cleaning and checking of data for: missing filenames, speaker IDs or interval labels. If included, f0 is checked for missing or negative values and for rising/falling rates. Maximum rates are taken from Xu & Sun (2002, Table X; 72 ST/s for rises and 96 ST/s for falls, doi:10.1121/1.1445789).",
                                            actionButton("clean.recheck","Clean and re-check"))})
      output$clean.cont <- renderUI({actionButton("clean.cont", "Continue with cleaned data",disabled = T)})
      output$clean.ign <- renderUI({
        div(title = "This enables to continue without cleaning and is likely to produce errors and app crashes. Not recommend.",
            checkboxInput("clean.ign","Ignore problems",F))
      })
    }
    removeNotification("check.clean", session)
    return(list(
      text = paste0(
        "Data summary:","\n",
        "=============","\n",
        unq.f, " filenames","\n",
        unq.s, " speakers","\n",
        unq.step, " measurement points","\n",
        unq.i, " intervals (contours)","\n",
        "\n","\n",
        "Problems found:","\n",
        "===============","\n",
        na.f," intervals have missing filenames","\n",
        na.s," intervals have missing speaker IDs","\n",
        na.i, " intervals have ' ' (space) as interval label","\n",
        f0.prb
      ),
      n = sum(na.f,na.s,na.f0,neg.f0,na.i,d.f0)
      ))
  }
  
  do.fpca <- function(c){
    if(input$f0.Npc!=0){
    showNotification(id = "nt.fpca","Performing fPCA...",duration = NULL)
    load.quiet("fda")
    df.p <- c()
    for (p in 1:npoints()){
      rbind(df.p, df.l[[c]][df.l$stepnumber==p]) -> df.p
    }
    list(
      id = colnames(df.p),
      cue = as.matrix(df.p)
    ) -> df.p
    cue_basis = create.bspline.basis(breaks = 1:npoints())
    harmLfd_cue = int2Lfd(2)
    cuefdPar = fdPar(cue_basis,harmLfd_cue,input$f0.Lbd)
    cuefd = smooth.basis(1:npoints(),df.p$cue,cuefdPar)
    cuepca = pca.fd(cuefd$fd,nharm=input$f0.Npc)
    harmfd = cuepca$harmonics
    harmvals = eval.fd(1:npoints(),harmfd)
    paste0("PC",1:input$f0.Npc," (",round(cuepca$varprop,3),")") -> vp
    suppressWarnings(cbind.data.frame(step = rep(1:npoints(),input$f0.Npc),
                     PC = rep(paste0("PC",1:input$f0.Npc), each = npoints()),
                     value = as.vector(harmvals)) -> dataPCplot)
    ggplot(dataPCplot) +
      geom_line(aes(x=step,y=value)) +
      xlab("measurement point") +
      ylab("PC (harmonic) value") +
      facet_wrap(~ PC) +
      theme_bw(base_size = 20) +
      labs(title = gsub("pc.","", c)) -> p
    removeNotification("nt.fpca", session)
    return(list(plot = p, varprop = vp, scores = cuepca$scores))
    }else{
      return(list(plot = NULL, varprop = NULL, scores = NULL))  
    }
  }
  
  do.repr <- function(m){
    paste(m,"f0",input$f0.scale,input$f0.corr,input$f0.deriv,sep = ".") -> n.repr
    gsub(".n","",n.repr) -> n.repr
    if(input$f0.scale!="Hz"){
      f0.conv(df.l$cc.f0.Hz,"Hz",input$f0.scale) ->> df.l[[n.repr]]
    }else{
      df.l$cc.f0.Hz ->> df.l[[n.repr]]
    }
    if(input$f0.corr=="std"){
      for (spk in unique(df.l$speaker)) {
        (df.l[[n.repr]][df.l$speaker == spk] - mean(df.l[[n.repr]][df.l$speaker == spk])) /
          sd(df.l[[n.repr]][df.l$speaker == spk]) ->> df.l[[n.repr]][df.l$speaker == spk]
      }
    }
    if(input$f0.corr=="oMe"){
      for (spk in unique(df.l$speaker)) {
        log2(df.l[[n.repr]][df.l$speaker == spk] / median(df.l[[n.repr]][df.l$speaker == spk])) ->> df.l[[n.repr]][df.l$speaker == spk]
      }
    }
    if(input$f0.deriv!="n"){
      load.quiet("pracma")
    }
    if(input$f0.deriv=="d1"){
      for (rws in 1:(nrow(df.l)/npoints())) {
        pracma::gradient(df.l[[n.repr]][((rws*npoints())-(npoints()-1)):(rws*npoints())]) ->> df.l[[n.repr]][((rws*npoints())-(npoints()-1)):(rws*npoints())]
      }
    }
    if(input$f0.deriv=="d2"){
      for (rws in 1:(nrow(df.l)/npoints())) {
        pracma::gradient(gradient(df.l[[n.repr]][((rws*npoints())-(npoints()-1)):(rws*npoints())])) ->> df.l[[n.repr]][((rws*npoints())-(npoints()-1)):(rws*npoints())]
      }
    }
    if(input$f0.deriv=="d3"){
      for (rws in 1:(nrow(df.l)/npoints())) {
        pracma::gradient(gradient(gradient(df.l[[n.repr]][((rws*npoints())-(npoints()-1)):(rws*npoints())]))) ->> df.l[[n.repr]][((rws*npoints())-(npoints()-1)):(rws*npoints())]
      }
    }
    if(m=="cc" && input$f0.Npc!=0){
      as.numeric(substr(input$sel.PCs,3,3)) -> sel.PCs
      do.fpca(n.repr)$scores -> scores
      for(p in sel.PCs){
        round(scores[,p],3) ->> df.l[df.l$stepnumber==1,paste0(n.repr,".PC",p)]
      }
    }
    for(c in colnames(df.l)[which(substr(colnames(df.l),1,3)=="cc.")]){
      round(as.numeric(df.l[[c]]),3) ->> df.l[[c]]
    }
    if(m=="cc"){
      logging(paste0("F0 representation added: ",gsub("cc.","",n.repr),ifelse(input$f0.Npc!=0,paste0(", with ",input$f0.Npc," principal components ","(λ: ",input$f0.Lbd,"), selected PCs: ",paste0(sel.PCs,collapse = ", ")),"")))
      }
    return(list(colname = n.repr))
  }
  
  do.cc <- function(n,eval){
    showNotification(id = "nt.cc",paste0("Performing clustering (n = ",n,") ..."),duration = NULL)
    if(eval==1){
      input$cc.vars[1] -> vars
    }else{
      input$cc.vars -> vars
    }
   npoints(max(df.l$stepnumber))
   cc.d <- dist(matrix(data = 0,nrow = nrow(df.l)/npoints()))
   for(var in vars){
     matrix(na.omit(df.l[[paste0("cc.",var)]]),ncol = ifelse(str_detect(var,paste(c("PC","dur"),collapse = "|")),1,npoints()), byrow = T) -> m
     if (input$cc.distm == "euclidean") {
       stats::dist(m, method = "euclidean") -> d
     }
     if (input$cc.distm == "mase") {
       load.quiet("usedist")
       load.quiet("Metrics")
       mase_distance <-function (r1, r2) {
         Metrics::mase(r1,r2)
       }
       usedist::dist_make(m, mase_distance) -> d
     }
     if (input$cc.distm == "dtw_basic") {
       load.quiet("proxy")
       load.quiet("dtwclust")
       proxy::dist(m, method = "dtw_basic",upper = F) -> d
       stats::as.dist(d) -> d
     }
     if (input$cc.distm == "cor") {
       load.quiet("proxy")
       proxy::dist(m, method = "correlation",upper = F) -> d
       stats::as.dist(d) -> d
     }
     scale01 <- function(x){(x-min(x))/(max(x)-min(x))}
     scale01(d) -> d
     cc.d + d -> cc.d 
   }
   if(input$cc.method=="hac"){
     load.quiet("ggdendro")
     showTab(inputId = "tabs_cl", target = "Dendrogram")
     hclust(cc.d, method = input$cc.link) -> hac
     ggdendrogram(hac, theme_dendro = T, labels = F) +
       theme(
         axis.title = element_text(size = 20),
         axis.text = element_text(size = 20),
         strip.text = element_text(size = 20)) -> dendro
     savethis$d_plot <- dendro
     cutree(hac, k = n) -> cluster
   }
   if(input$cc.method=="pam"){
     load.quiet("cluster")
     hideTab(inputId = "tabs_cl", target = "Dendrogram")
     if(exists("dendrogram",envir = .GlobalEnv)){
       rm("dendrogram", envir = .GlobalEnv)
     }
     dendro <- NULL
     cluster::pam(cc.d, k = n,pamonce = input$cc.opt)$clustering -> cluster
   }
  if(eval==1){
    removeNotification("nt.cc",session)
    return(as.data.frame(cbind(m,cluster)))
  }
  rep(cluster, each = npoints()) ->> df.l$cluster
  do.proto()
  paste0(1:n,
         " (n=", as.vector(table(df.l$cluster)/npoints()),
         ")") -> labs
  if("cc.dur.s" %in% colnames(df.l) && any(grepl("dur", vars))){
    str_replace_all(labs,
                pattern = "\\)",
                replacement = paste0(", d=",round(as.vector(unlist((df.l %>% group_by(cluster) %>% summarise(mean=mean(cc.dur.s,na.rm=T),median=median(cc.dur.s,na.rm=T)))[input$plot.vals])),2),")")
                ) -> labs
  }
  attributes(labs)$names <- 1:n
  ggplot(df.l, aes(x = stepnumber, y = df.l[[paste0("cc.",cc.y1())]])) +
    stat_summary(
      fun = input$plot.vals,
      group = "cluster",
      geom = "line",
      colour = app.yL.col(),
      linetype = app.yL.type(),
      linewidth = 1,
      show.legend = F
    ) +
    {if(input$plot.vals=="mean")
    stat_summary(
      fun.data = mean_sdl,
      fun.args = list(mult = 1),
      group = "cluster",
      geom = 'ribbon',
      alpha = .2,
      show.legend = F
    )} +
    {if(input$plot.vals=="median")
      stat_summary(
        fun.data = median_hilow,
        group = "cluster",
        geom = 'ribbon',
        alpha = .2,
        show.legend = F
      )} +
    facet_wrap( ~ cluster, ncol = input$plot.cols
                ,labeller = as_labeller(labs)
    ) +
    xlab("measurement point") +
    ylab(paste0(cc.y1()," (",input$plot.vals,")")) +
    theme(
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 20),
      strip.text = element_text(size = 20)
    ) -> p
  if(is.null(input$plot.y2)==F){
  if (input$plot.y2!="none"){
    if (input$plot.y2=="prototype"){
      p +
        geom_line(aes(y=df.l[[paste0("cc.",cc.y1())]][df.l$proto=="x"]),
                data = subset(df.l, proto =="x"),
                stat = "identity",
                colour = app.yR.col(),
                linetype = app.yR.type(),
                linewidth = 1,
                show.legend = F
      ) -> p
    }else{
    load.quiet("scales")
    rescale(x = df.l[[paste0("cc.",input$plot.y2)]], to =  c(min(df.l[[paste0("cc.",cc.y1())]]), max(df.l[[paste0("cc.",cc.y1())]]))) -> y2
    a.diff <- max(df.l[[paste0("cc.",cc.y1())]]) - min(df.l[[paste0("cc.",cc.y1())]])
    b.diff <- max(df.l[[paste0("cc.",input$plot.y2)]]) - min(df.l[[paste0("cc.",input$plot.y2)]])
    a.min <- min(df.l[[paste0("cc.",cc.y1())]])
    b.min <- min(df.l[[paste0("cc.",input$plot.y2)]])
    p + 
      scale_y_continuous(cc.y1(), sec.axis = sec_axis(~((. -a.min) * b.diff / a.diff) + b.min,name = input$plot.y2)) +
      stat_summary(mapping = aes(y = y2), 
                   data = df.l,
                   fun = input$plot.vals,
                   group = "cluster",
                   geom = "line", 
                   colour = app.yR.col(),
                   linetype = app.yR.type(),
                   linewidth = 1,
                   show.legend = F
                   ) +
      {if(input$plot.vals=="mean")
        stat_summary(
          mapping = aes(y = y2), 
          data = df.l,
          fun.data = mean_sdl,
          fun.args = list(mult = 1),
          group = "cluster",
          geom = 'ribbon',
          alpha = .2,
          show.legend = F
        )} +
      {if(input$plot.vals=="median")
        stat_summary(
          mapping = aes(y = y2), 
          data = df.l,
          fun.data = median_hilow,
          group = "cluster",
          geom = 'ribbon',
          alpha = .2,
          show.legend = F
        )} -> p}
    p +
      theme(axis.title.y.left = element_text(colour = app.yL.col())) +
      theme(axis.title.y.right = element_text(colour = app.yR.col())) -> p
  }}
  savethis$c_plot <- p
  as.data.table(cbind(rep("",nclust()),
                      1:nclust(),
                      as.vector(table(df.l$cluster)/npoints()),
                      round(100*(as.vector(table(df.l$cluster)/6)/sum(as.vector(table(df.l$cluster)/6))),2))) -> t
  colnames(t) <- c("flagged","cluster","N","%")
  for(v in input$cc.vars){
    sepc <- c()
  for (c in 1:nclust()) {
    sep = c()
    for (s in 1:max(df.l$stepnumber[is.na(df.l[[paste0("cc.",v)]])==F])) {
      sd(subset(df.l, cluster == c & stepnumber == s)[[paste0("cc.",v)]],na.rm = T) /
        sqrt(nrow(subset(df.l, cluster == c))) -> se
      append(sep, se) -> sep
    }
    append(sepc, mean(sep)) -> sepc
  }
  sepc -> t[[v]]
  flagged = c()
  for (c in 1:nclust()) {
    if(t[c,"N"] == 1 || t[[v]][c] > 2 * median(t[[v]], na.rm = T)){
      t[c,"flagged"] <- "*"
    }
  }
  colnames(t)[which(colnames(t) == v)] <- paste0("μ(se) ",v)
  }
  savethis$s_tab <-t
  removeNotification("nt.cc",session)
  updateActionButton(session,"savethis",disabled = F)
  show.contab(1)
  logging(paste0("Clustering performed: ",n," clusters; ",nrow(df.l)/npoints()," observations, variables: ",paste0(vars,collapse = ", "),ifelse(input$cc.method=="hac",paste0("; method: HAC; linkage: ",input$cc.link),paste0("; method: PAM (optimizer: 'pamonce=",input$cc.opt,"')")),"; distance metric: ",input$cc.distm))
  }
  
show.contab <- function(do) {
  if(do==1){
    output$cl.contab <- renderUI({
      if(is.null(df.l) || input$tabs_main!="Clustering"){
        return(NULL)
      } else {
        span(
          fluidRow(
            column(width = 11, div(title = "Generate a contingency table with one row per cluster and one column per level of the (split) character string column in the data as specified below. Values in the contingency table are counts. See logfile for total number of observations in the most recent clustering.", HTML('<b>Contingency table</b>'), style = 'display: inline-block; vertical-align: bottom; font-size: 1.5em')),
            column(width = 1, span(
              div(title = "Save table as csv file to disk",downloadButton("dl.contab","Save"))
            ))
          ),
          HTML('<br>'),
          fluidRow(
            column(width = 2, 
                   verticalLayout(
                     div(title = "Select the column in the datafile as variable from which to extract the levels that should become columns in the contigency table.",
                         selectInput("cl.contab.colx",label = "Get columns from:",choices = names(which(unlist(lapply(dplyr::select(df.l,-proto),class))=="character")))),
                     renderUI(
                       div(title = "Choose seperator chracter to split the selected column. Use empty 'separator' (first select option) to not split the character column.",
                           selectInput("cl.contab.sep",
                                       label = "Separator:",
                                       choices = c(" ", unique(as.vector(str_extract_all(as.vector(unlist(subset(df.l, select = input$cl.contab.colx),)), '[[:punct:]]',simplify = T))))))
                     ),
                     renderUI({
                       if(is.null(input$cl.contab.sep)){
                         return(NULL)
                       }
                       if(input$cl.contab.sep==""){
                         return(NULL)
                       } else {
                         div(title = "Select which (split) column to use in the contingency table. A column in the contingency table will be created for each unique value found in the selected (split) character column.",
                             selectInput("cl.contab.ncol","Will become columns:",
                                         choices = c("",paste0("V", c(1:ncol(str_split(string = df.l[,get(input$cl.contab.colx)],pattern = input$cl.contab.sep,simplify = T))), " '",str_split(string = df.l[,get(input$cl.contab.colx)],pattern = input$cl.contab.sep,simplify = T)[1,],"'" )),
                                         multiple = F))
                       }
                     })
                   )),
            column(width = 1),
            column(width = 8,
                   renderUI({
                     if(is.null(input$cl.contab.ncol)){return(NULL)}
                     if(input$cl.contab.ncol==""){return(NULL)}else{
                       as.vector(
                         str_split(
                           string = df.l[,get(input$cl.contab.colx)],
                           pattern = input$cl.contab.sep,simplify = T)
                         [,which(colnames(as.data.table(str_split(
                           string = df.l[,get(input$cl.contab.colx)],
                           pattern = input$cl.contab.sep,simplify = T)))==str_split(input$cl.contab.ncol," '",simplify = T)[1])]
                       ) -> x
                       df.l$cluster -> cluster
                       as.data.frame.matrix(xtabs(~cluster+x)/npoints()) -> contab
                       savethis$c_tab <- contab
                       output$dl.contab <- downloadHandler(
                         filename = function() {paste("contingency_table.csv")},
                         content = function(file) {fwrite(contab, file)}
                       )
                       renderTable(cbind.data.frame(cluster = 1:nclust(), contab),digits = 0)
                     }
                   })
            )
          )
        )
      }
    })
  }else{
    output$cl.contab <- renderUI({NULL})
  }
}  

do.eval <- function() {
  load.quiet("purrr")
 if (isolate(input$eval.method) == 1) {
  Epsilon <- 1
  
    getNormalParameters <- function(df) {
      aggregate(df[substr(names(df), 1, 1) != "c"], by = list(df$cluster), FUN = mean) -> mns
      mns$fn <- "mean"
      colnames(mns)[1] <- "cluster"
      aggregate(df[substr(names(df), 1, 1) != "c"], by = list(df$cluster), FUN = sd) -> sds
      colnames(sds)[1] <- "cluster"
      sds$fn <- "sd"
      rbind(mns, sds) -> df
      df
    }

    precisionInterval <- function(value) {
      floor(value / Epsilon) * Epsilon -> lb
      c(lb, lb + Epsilon) -> lbeps
      lbeps
    }

    getRelInfoOfSample <- function(value, parameters) {
      precisionInterval(value) %>%
        pnorm(mean = parameters[parameters$fn == "mean", 1],
              sd = parameters[parameters$fn == "sd", 1]) -> x
      - log(x[2] - x[1])
    }

    getRelInfoOfToken <- function(row, parameters) {
      (1:npoints()) %>%
        sprintf("V%d", .) %>%
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
        aggregate(Ct ~ v, ., sum) -> w
      nlogn <- function(x)
        x * log(x)
      nlogn(sum(w)) - sum(nlogn(w))
    }

    mkInfoCost <- function() {
      df %>%
        getNormalParameters() -> params
      df1 <- df
      df1$cluster <- 1
      df1 %>%
        getNormalParameters() -> paramsAll
      params1 <- params[params$fn == "mean", ]
      params1$cluster <- 1
      getRelInfoOfData(params1, paramsAll) / isolate(input$eval.smooth) +
        getInfo(df$cluster) +
        getRelInfoOfData(df, params) / isolate(input$eval.smooth)
    }
  cost <- c()
  for (r in isolate(input$eval.rg[1]):isolate(input$eval.rg[2])) {
    do.cc(r,1) -> df
    npoints(ncol(df)-1)
    append(cost,mkInfoCost()) -> cost
  }
  cbind.data.frame(clusters = isolate(input$eval.rg[1]):isolate(input$eval.rg[2]),cost = cost) ->> df.e
  ggplot(data = df.e) +
      geom_line(aes(x = clusters, y = cost)) +
      xlab("N clusters") +
      ylab("Information cost") +
      annotate(
        geom = "point",
        x = df.e$clusters[which.min(na.omit(df.e$cost))],
        y = min(df.e$cost,na.rm = T),
        size = 2
      ) +
      theme_bw(base_size = 20) -> p
 }
  if (isolate(input$eval.method) == 2) {
    load.quiet("scales")
    within <- c()
    between <- c()
    for (r in isolate(input$eval.rg[1]):isolate(input$eval.rg[2])) {
      do.cc(r,1) -> df
          c() -> wc
          for (c in 1:r) {
            c() -> w
            for (m in 1:(ncol(df)-1)) {
              append(w, sd(df[df$cluster == c, m])) -> w
            }
            append(wc, mean(w)) -> wc
          }
          append(within, mean(na.omit(wc))) -> within

          c() -> bmvar
          for (m in 1:(ncol(df)-1)) {
            c() -> bm
            for (c in 1:r) {
              mean(na.omit(df[df$cluster == c, m])) -> b
              append(bm, b) -> bm
            }
            append(bmvar, abs(max(bm) - min(bm))) -> bmvar
          }
          append(between,mean(bmvar)) -> between
    }
    rescale(within) -> within
    rescale(between) -> between
    cbind.data.frame(clusters = isolate(input$eval.rg[1]):isolate(input$eval.rg[2]),within = within, between = between) ->> df.e
    ggplot(df.e, aes(clusters)) +
          geom_line(mapping = aes(y = within, linetype = "within")) +
          geom_line(mapping = aes(y = between, linetype = "between")) +
          xlab("N clusters") +
          ylab("Scaled variance") +
          scale_linetype_manual("Cluster variance", values = c("within" =
                                                                 1, "between" = 2)) +
          theme_bw(base_size = 20) -> p
    
  }
  savethis$e_plot <- p
  logging(paste0("Evaluation performed: ",
                 input$eval.rg[1],
                 "-",
                 input$eval.rg[2],
                 " clusters, variable: ",
                 input$cc.vars[1],
                 ifelse(input$eval.method==1,
                        paste0(", MDL (bending: ",input$eval.smooth,")"),
                        ", W/B var")
  ))
  return(list(plot = p))
  
}

do.proto <- function() {
  "" ->> df.l$proto
  for(c in 1:length(unique(df.l$cluster))){
   for(cntr in which(df.l$cluster==c & df.l$stepnumber==1)){
     sqrt(mean((
       as.vector(unlist(lapply(split(na.omit(df.l[[paste0("cc.",cc.y1())]][df.l$cluster==c]),
                                     f = df.l$stepnumber[df.l$cluster==c &
                                                           is.na(df.l[[paste0("cc.",cc.y1())]])==F]), FUN = input$plot.vals)))-
         df.l[[paste0("cc.",cc.y1())]][cntr:(cntr+max(df.l$stepnumber[is.na(df.l[[paste0("cc.",cc.y1())]])==F])-1)]
       )^2)) ->> df.l$proto[cntr]
   }
     "x" ->> df.l$proto[df.l$cluster==c][which.min(df.l$proto[df.l$cluster==c]):(which.min(df.l$proto[df.l$cluster==c])+max(df.l$stepnumber)-1)]
     "" ->> df.l$proto[df.l$cluster==c & df.l$proto!="x"]
  }
  savethis$p_tab <- df.l[df.l$proto=="x" & df.l$stepnumber==1] %>% sort_by(.,.$cluster) %>% dplyr::select(cluster,filename,start,end,interval_label)
  if(exists("df.g")){
    m.rows(NULL)
    for(r in 1:nrow(savethis$p_tab)){
      m.rows(append(m.rows(),which(df.g$name_trim==savethis$p_tab$filename[r] & df.g$text==savethis$p_tab$interval_label[r] & round(df.g$xmin,3)==savethis$p_tab$start[r])))
    }
    load.quiet("sound")
    suppressWarnings(unlink("www", recursive = T))
    dir.create("www")
    for(x in 1:length(m.rows())){
        cutSample(paste0(inDir(), df.g$name_trim[isolate(m.rows()[x])], snd.ext()),
                  df.g$xmin[isolate(m.rows()[x])],
                  df.g$xmax[isolate(m.rows()[x])]) -> snd
      saveSample(snd,file.path("www",paste0("play",x,".wav")),overwrite = T)
    }
    playServerList <- lapply(1:length(isolate(m.rows())), function(i) {
      playServer(paste0("play", i))
    })
    output$proto.play <- renderUI({
      tagList(lapply(1:length(isolate(m.rows())), function(i) {
        fluidRow(span(
          playUI(paste0("play", i)),
          div(align = "center",
          renderTable(savethis$p_tab[i,])),
          HTML('<br><br>')
        ))
      }))
    })
  }else{
    output$proto.play <-renderTable(savethis$p_tab)
  }
}

}

# app ####
shinyApp(ui = ui, server = server, options = list("quiet" = T 
                                                  #, launch.browser = T
                                                  ))


# version list ####
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