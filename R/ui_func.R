#' RingdateR GUI
#'
#' This function creates the shiny user interface
#' @keywords GUI
#' @export
#' @importFrom shinyalert useShinyalert
#' @importFrom shinyjs useShinyjs
#' @import shiny
#' @import shinydashboard
#' @import shinycssloaders
#' @importFrom magrittr %>%

ui <- function(){
  dashboardPage(skin="green",

                  dashboardHeader(title = "RingdateR",
                                tags$li(class = "dropdown",
                                tags$style(HTML("
                                        .main-header {max-height: 75px; width = 400px; font-size:40px;}
                                                        .main-header .logo {height: 75px; padding-left = 0px;}
                                                        .sidebar-toggle {height: 75px;
                                                        padding-top: 20px;
                                                        size = 200%;
                                                        background:#FFFFFF;}
                                                        .icon {height: 75px;}

                                                        .navbar {min-height:75px}

                                                        .main-header .logo {
                                                        font-weight: bold;
                                                        font-size: 35px;
                                                        padding-top:15px;
                                                        }

                                                        h1 {color:#FFFFFF}
                                                        h2 {color:#FFFFFF; font-weight: bold; padding:-10,0,0,0}
                                                        h3 {color:#000000;font-size:30px; font-weight: bold; padding:-20,0,0,0}
                                                        h4 {color:#000000;font-size:24px; padding:50,0,0,0}
                                                        h5 {color:#000000;font-size:24px;}
                                                        h6 {color:#000000;font-size:20px;}
                                                        p {color:#000000;font-size:20px;}

                                                        .clearfix {overflow: auto;}

                                                        background{color:#FFFFFF}

                                                        .box.box-solid.box-primary>.box-header {
                                                        color:#fff;
                                                        height:75px;
                                                        padding-top:0px;
                                                        background:#2ca25f
                                                        }

                                                        .box.box-solid.box-primary{
                                                        border-bottom-color:#222d32;
                                                        border-left-color:#222d32;
                                                        border-right-color:#222d32;
                                                        border-top-color:#222d32;
                                                        background:#FFFFFF;
                                                        padding: 0,0,0,0;
                                                        }

                                                        .progress-bar {
                                                        background-color: #2ca25f;
                                                        background-image: -webkit-gradient(linear, 0 100%, 100% 0, color-stop(0.25, rgba(255, 255, 255, 0.6)), color-stop(0.25, transparent), color-stop(0.5, transparent), color-stop(0.5, rgba(255, 255, 255, 0.6)), color-stop(0.75, rgba(255, 255, 255, 0.6)), color-stop(0.75, transparent), to(transparent));
                                                        background-image: -webkit-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                                                        background-image: -moz-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                                                        background-image: -o-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                                                        background-image: linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                                                        -webkit-background-size: 40px 40px;
                                                        -moz-background-size: 40px 40px;
                                                        -o-background-size: 40px 40px;
                                                        background-size: 40px 40px;
                                                        text.align: center;
                                                        font-size:18px;
                                                        }

                                                        .shiny-notification {
                                                        height: 100px;
                                                        width: 800px;
                                                        position:fixed;
                                                        top: calc(50% - 50px);
                                                        left: calc(50% - 400px);
                                                        font-size:24px;
                                                        }

                                                        .text{color: red;
                                                        font-size: 20px;
                                                        font-style: italic;
                                                        }

                                                        #close{ background-color:#ff8899;}
                                                        #close:hover{background-color:#ff5050; }

                                                        .selectize-input { font-size: 24px; line-height: 28px;}
                                                        .selectize-dropdown { font-size: 24px; line-height: 28px; }

                                                        [type = 'number'] {font-size:30px;
                                                        height:40px;}
                                                        [type = 'text'] {font-size:30px;
                                                        height:40px;}

                                                        input[type=checkbox] {transform: scale(1.5); margin-top:10px;}

                                                        .main-sidebar{width: 230px;}
                                                        .left-side, .main-sidebar {padding-top: 75px;
                                                        padding-left: 10px}
                                                        "))
                                )
                ),

                dashboardSidebar( useShinyjs(),
                   useShinyalert(),

                                  tags$h2(id = "side_h2_1","Plot options"),
                                  numericInput("text.size", label = h2(id = "side_h2_2","Plot text size"), value = 12), # changes the plot text size
                                  numericInput("plot.line", label = h2(id = "side_h2_3"," Plotted line thickness"), value = 0.5), # changes the plotted line thickness
                                  numericInput("line.width", label = h2(id = "side_h2_4"," Axis line weighting"), value = 0.5), # Change the axis thickness
                                  tags$hr(),
                                  h2(id = "side_h2_5","Text display settings"),
                                  selectInput("font_select", label = h2(id = "side_h2_6","Select font size"),
                                                     choices = list("Small" = 1, "Medium" = 2, "Large" = 3),
                                                     selected = 2)


                ),
                dashboardBody(
                  div(id = "form",
                      tabsetPanel(id = "navbar",
                                  tabPanel(h4(id = "nav_h4_1", "Starting Point"), value ="start",
                                           fluidRow(box(width=10, height = "100px", title = h3(id = "st_pt_h3_1", "Welcome to RingdateR (V1 beta)", actionButton("st_general_hlp", h6("I - Quick Help/about")))),

                                                    div(style = "text-align: center; font-size:18px",
                                                        actionButton("close",label = tags$b(p("Stop RingdateR. Returns to launcher")), icon = icon("fas fa-skull fa-2x"), onclick = "setTimeout(function(){window.close();},500);",
                                                                     style ="white-space: normal;
                                                                              width: 250px;


                                                                              "))),


                                           fluidRow(column(3,
                                                  box(width = NULL, title = h2(id = "st_pt_h2_1", "Step 1: Analysis options"), status = "primary",
                                                      div(style="text-align: right", actionButton("st_analysis_hlp","", icon = icon("info"))),
                                                      solidHeader = TRUE,
                                                      align="left",
                                                      selectInput("detrending_select", h4(id = "st_pt_h4_1", "Choose a detrending method"),
                                                                  choices = list("Do nothing to my data" = 1,
                                                                                 "Convert to z-scores" = 2,
                                                                                 "Spline detrending" = 3,
                                                                                 "Mod. negative exponential" = 4,
                                                                                 "Friedman" = 5,
                                                                                 "ModHugershoff" = 6,
                                                                                 "First difference" = 7
                                                                  ), selected = 3),
                                                     # checkboxInput("diff", h4(id = "st_pt_h4_3", "Detrend by subtraction?"), F),
                                                      numericInput("splinewindow", h4(id = "st_pt_h4_2", "Select a spline window (only applies to spline detrendning option)"), value = 21, min = 5, max = 200),
                                                    #  checkboxInput("PT", h4(id = "st_pt_h4_3", " Power Transform data?"), F),
                                                      checkboxInput("total_overlap", h4(id = "st_pt_h4_4", "Automatically set lag limits? (uses all possible leads/lags)"), TRUE),
                                                      h6(id = "st_pt_h6_1", "Alternatively set limits for lead-lag range"),
                                                      numericInput("neg_lag",h4(id = "st_pt_h4_5", "Select negative lag (years)"), min = 0, value =-20),
                                                      numericInput("pos_lag", h4(id = "st_pt_h4_6", "Select positive lag (years)"), min = 0, value =20),
                                                      numericInput("cor_win", h4(id = "st_pt_h4_7", "Select correlation window (years)"), min = 5, value =21),
                                                      h6(id = "st_pt_h6_2","Correlation window needs to be an odd number. Even numbers will be converted to odd by adding 1."))),
                                           column(3,
                                                  box(width = NULL, status = "primary",
                                                      solidHeader = TRUE,
                                                      title = h2(id = "st_pt_h2_2","Step 2: Load undated series"),
                                                      div(style="text-align: right", actionButton("st_undate_hlp","", icon = icon("info"))),
                                                      selectInput("multi_chron", h4(id = "st_pt_h4_8","What type of undated data do you want to load?"), choices = list("Load individual undated series (.csv, .pos, .lps, .rwl, .txt, .xlsx)" = 1, "Load undated chronologies (.rwl, .csv, .xlsx)" = 2), selected = 1),
                                                      fileInput('file1', h4(id = "st_pt__h4_9",'Load data for pairwise analysis'), multiple = TRUE, buttonLabel = h6("Select a file"), placeholder = "No file selected",
                                                                accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv', '.pos', '.lps', '.rwl', '.txt', '.xlsx')),
                                                      tags$hr(),
                                                      checkboxInput("pair_detrend", h4(id = "st_pt_h4_10", "Apply detrending"), TRUE),
                                                      selectInput("year_inc_select", h4(id = "st_pt_h4_11", "First column is years or increment count?"), choices = list("Years" = 1, "Increment count" = 2), selected = 1),
                                                      tags$hr()

                                                  ),

                                                  box( width = NULL, status = "primary",
                                                      solidHeader = TRUE,
                                                      title = h2(id = "st_pt_h2_3","Load a dated chronology"),
                                                      div(style="text-align: right", actionButton("st_chron_hlp","", icon = icon("info"))),
                                                      fileInput('file2', h4(id = "st_pt_h4_12", 'Load chronology data (.rwl, .csv, .xlsx).'), multiple = FALSE, buttonLabel = h6("Select a file"), placeholder = "No file selected",
                                                                accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv', '.rwl', '.xlsx')),
                                                      #checkboxInput("master_chron_load", h4("Is this a master chronology (two column format: year, detrended mean chronology)?"), F),
                                                      checkboxInput("chron_detrend", h4(id = "st_pt_h4_13", "Apply detrending"), TRUE)


                                                  ),
                                                  box( width = NULL, status = "primary",
                                                      solidHeader = TRUE,
                                                      title = h2(id = "st_pt_h2_4","Use example data"),
                                                      div(style="text-align: right", actionButton("st_example_hlp","", icon = icon("info"))),
                                                      actionButton("example_undated", h4(id = "st_pt_h4_14", "Use example dated and undated series")))
                                           ),
                                           column(3,
                                                  box( width = NULL, title = h2(id = "st_pt_h2_5","Step 3: Series to analyse"), status = "primary",
                                                      solidHeader = TRUE,
                                                      div(style="text-align: right", actionButton("st_clear_data_hlp","", icon = icon("info"))),
                                                      actionButton("Go_clear_loaded", h4(id = "st_pt_h4_15","Clear all loaded data")),
                                                      plotOutput("load_plot", height = "10%") %>% withSpinner(type = 1, size = 2, color="#2ca25f", color.background = "#FFFFFF"),
                                                      div(tableOutput("analysis.data.out"), style = "font-size:24px" ))
                                           ),
                                           tags$script('
    Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {
        var id = "#" + x + "_progress";
        var idBar = id + " .bar";
        $(id).css("visibility", "hidden");
        $(idBar).css("width", "0%");
    });
  '),
                                           column(3,
                                                  box( width = NULL, title = h2(id = "st_pt_h2_6","Step 4: Choose analysis pathway"), status = "primary",
                                                      solidHeader = TRUE,
                                                      selectInput("mode_select", label = h3("Choose analysis pathway"), choices = list("Pairwise Anlysis Mode"=1, "Chronology Analysis Mode"=2), selected = 1),
                                                      actionButton("Go_pairwise", h4(id = "st_pt_h4_17","Run the analyses"), icon = icon("rocket"))
                                                      )
                                                  ))),

                                  ###################################################################

                                  tabPanel(h4(id = "nav_h4_2", "Loaded Data"),
                                           tabsetPanel(
                                             tabPanel(h4("Undated series"), value ="mand",
                                                      fluidRow(box(width=12,
                                                                   div(tableOutput("undated_data_table"), style = "font-size:24px")))),
                                             tabPanel(h4("Chronology data"), value ="chrono",
                                                      div(tableOutput("loaded_chronology_data_table"), style = "font-size:24px")),

                                             tabPanel(h4("Detrended data"), value ="mand",
                                                      downloadButton('download_detrend',label=h4(id = "ld_dt_h4_1","Save the detrended series (as .CSV)")),
                                                      div(tableOutput("detrended_data_table"), style = "font-size:24px"))#,

                                            # This can be used to show other data
                                            # tabPanel(h4("debug"), value ="mand",
                                            #          div(tableOutput("debug"), style = "font-size:24px"),
                                            #          div(tableOutput("debug2"), style = "font-size:24px"))

                                           )
                                  ),

                                  tabPanel(h4(id = "nav_h4_3", "Detrending Plot"), value ="mand",
                                           fluidRow(
                                             box(width=9,
                                                 plotOutput("detrending.plot.out", height = "100%")
                                             ),
                                             box(width=3,
                                                 downloadButton('downloadsingplt',label=h4(id="dt_plt_h4_1","Download plot as png")),
                                                 selectInput("first_series", label = h4(id="dt_plt_h4_2","Select series"), c("")),
                                                 selectInput("detrending_select_2", h4(id = "st_pt_h4_1", "Choose a detrending method"),
                                                             choices = list("Do nothing to my data" = 1,
                                                                            "Convert to z-scores" = 2,
                                                                            "Spline detrending" = 3,
                                                                            "Mod. negative exponential" = 4,
                                                                            "Friedman" = 5,
                                                                            "ModHugershoff" = 6,
                                                                            "First difference" = 7
                                                             ), selected = 3),
                                                 numericInput("splinewindow_2", h4(id = "st_pt_h4_2", "Select a spline window (only applies to spline detrendning option)"), value = 21, min = 5, max = 200)
                                             )
                                           )),

                                  tabPanel(h4(id = "nav_h4_4", "Results"), value ="pairwise",

                                           fluidRow(
                                             box(width=8, height = 1200,
                                                 div(style="text-align: left", actionButton("pw_general_hlp","", icon = icon("info"))),
                                                 plotOutput("pairwise_line_plot") %>% withSpinner(type = 1, size = 3, color="#2ca25f", color.background = "#FFFFFF"),
                                                 tags$hr(),
                                                 tabsetPanel(
                                                   tabPanel(h4(id="pw_rs_h4_1","Check for ring count errors"),
                                                            plotOutput("pairwise_hm_small") %>% withSpinner(type = 1, size = 3, color="#2ca25f",  color.background = "#FFFFFF")),
                                                   tabPanel(h4(id="pw_rs_h4_2","lead lag results"),
                                                            plotOutput("pairwise_lead_lag_bar_plot") %>% withSpinner(type = 1, size = 3, color="#2ca25f", color.background = "#FFFFFF")))),
                                             box(width = 4,
                                                 div(style="text-align: right", actionButton("pw_plot_menu_hlp","", icon = icon("info"))),

                                                 div(tableOutput("pair_ser_names"), style = "font-size:24px"),
                                                 div(tableOutput("single_pair_res"), style = "font-size:24px"),
                                                 tabsetPanel(
                                                   tabPanel(h4(id="pw_rs_h4_3","Sample selection"),
                                                            selectInput("pairwise_selec_method", label = h4(id="pw_rs_h4_4","Selection options"), choices = list("Manually choose samples" = 1, "Select row from results table" = 2), selected = 1),
                                                            selectInput("pairwise_series_1", label = h4(id="pw_rs_h4_5","Manually select first series"), h4(c(""))),
                                                            selectInput("pairwise_series_2", label = h4(id="pw_rs_h4_6","Manually select second series"), h4(c(""))),
                                                            selectInput("auto_lag", label = h4(id="pw_rs_h4_7","Select lag"),choices = list("Best match" = 1, "Second best" = 2, "Third best" = 3, "Custom lag" = 4), selected = 1),
                                                            numericInput("PS_2_lag", h4(id="pw_rs_h4_8","Enter value for custom lag"), value =0)),
                                                   tabPanel(h4(id="pw_rs_h4_9","Plot options"),
                                                            checkboxInput("pair_line_stand", h4(id="pw_rs_h4_10","Normalise the data in the plot"), FALSE),
                                                            selectInput("pairwise_colour_scale", label = h4(id="pw_rs_h4_11","Select colour scale"),choices = list("Blue-Grey-Red" = 1, "Grey-Red" = 2, "Grey-Blue" = 3, "White-Black" = 4), selected = 1),
                                                            checkboxInput("adjust_pair_lineX", h4(id="pw_rs_h4_12","adjust x axis scale line plot"), FALSE),
                                                            numericInput("Pairwise_line_X_max", h4(id="pw_rs_h4_13","Max X axis value"), value =""),
                                                            numericInput("Pairwise_line_X_min", h4(id="pw_rs_h4_14","Min X axis value"), value = ""))
                                                 ),
                                                 downloadButton("pair_plot_download", h4(id="pw_rs_h4_15","Save the line plot")),
                                                 downloadButton("pair_small_hm_downlaod", h4(id="pw_rs_h4_16","Save the heat map")),
                                                 downloadButton("pair_bar_plot_download", h4(id="pw_rs_h4_17","Save the bar graph"))

                                             )),

                                           fluidRow(
                                             box(width=3, title = h4(id="pw_rs_h4_18","Step 1: Filter results by statistics"),
                                                 div(style="text-align: right", actionButton("pw_stat_filt_hlp","", icon = icon("info"))),
                                                 checkboxInput("filter_3_check", h4(id="pw_rs_h4_19","Filter by stats"), FALSE),
                                                 numericInput("pair_r", h4(id="pw_rs_h4_20","R value"), value = 0.50),
                                                 numericInput("pair_sig", h4(id="pw_rs_h4_21","Significance value"), value =0.01),
                                                 numericInput("pair_overlap", h4(id="pw_rs_h4_22","Overlap"), value =50)
                                             ),
                                             box(width=3, title = h4(id="pw_rs_h4_23","Step 2: Filter results by series name"),
                                                 div(style="text-align: right", actionButton("pw_name_filt_hlp","", icon = icon("info"))),
                                                 checkboxInput("filter_1_check", h4(id="pw_rs_h4_24","Select target sample/filter table by sample"), FALSE),
                                                 selectInput("pairwise_filter_1", label = h4(""), c("")),
                                                 checkboxInput("filter_2_check", h4(id="pw_rs_h4_25","Filter using Series 2"), FALSE),
                                                 selectInput("pairwise_filter_2", label = h4(""), c(""))
                                             ),
                                             box(width = 3, title = h4(id="pw_rs_h4_26","Step 3: Check for Problematic Samples"),
                                                 div(style="text-align: right", actionButton("pw_prob_ck_hlp","", icon = icon("info"))),
                                                 numericInput("pair_prob_samp_wind", h4(id="pw_rs_h4_27","Set window to evaluate for problem samples (Years, must be even)"), value =20),
                                                 actionButton("prob_samp_check", h4(id="pw_rs_h4_28","Check for problematic samples")),
                                                 div(tableOutput("pairwise_prob_samples"), style = "font-size:24px")

                                             ),
                                             box(width = 3,
                                                 title = h4(id="pw_rs_h4_29","Step 4: Align samples"),
                                                 div(style="text-align: right", actionButton("pw_align_hlp","", icon = icon("info"))),
                                                 selectInput("align_all_pair", h4(id="pw_rs_h4_30","Series selection method"), choices = list("All series" = 1, "All series, excluding problem samples" = 2, "Manual sample selection" =3), selected = 1),
                                                 h4(id="pw_rs_h4_31","Manual sample selection"),
                                                 selectInput("align_ser_select", h4(id="pw_rs_h4_32","Manual sample selection"), label = c(""), multiple = TRUE),
                                                 actionButton("Go_initiate_chrono", h4(id="pw_rs_h4_33","Align selected data"))

                                             )

                                           ),
                                           fluidRow(
                                             box(width=12,downloadButton("pairwise_res_download", h4(id="pw_rs_h4_35","Download results table")),
                                                 div(DT::dataTableOutput("pairwise_res_table"), style = "font-size:24px"))
                                           )),

                                  tabPanel(h4(id = "nav_h4_5", "Full Heat Map"), value ="mand",
                                           fluidRow(box(width=12,
                                                        div(style="text-align: left", actionButton("pw_heatmap_hlp","", icon = icon("info"))),
                                                        plotOutput("plot_sing_hm", height = "100%") %>% withSpinner(type = 1, size = 3, color="#2ca25f", color.background = "#FFFFFF")
                                           )),
                                           fluidRow(box(width=4,
                                                        actionButton("go2", h4(id="pw_ht_h4_1","Produce heat map")),
                                                        h4(id="pw_ht_h4_2","Select series to analyse"),
                                                        selectInput("sing_first_series", label = h4(id="pw_ht_h4_3","First series"), c("")),
                                                        selectInput("sing_second_series", label = h4(id="pw_ht_h4_4","Second series"), c("")),
                                                        h4(id="pw_ht_h4_5","Save plot"),
                                                        downloadButton('downloadsinghtmp',label=h4(id="pw_ht_h4_6","Download plot as png"))),
                                                    box(width=4, h4(id="pw_ht_h4_7","Y axis options"),
                                                        checkboxInput("adj_lag1", h4(id="pw_ht_h4_8","Adjust lag scale"), value = FALSE),
                                                        numericInput("p_neg_lag1", h4(id="pw_ht_h4_9","Adjust negative lag (years)"), value =-20),
                                                        numericInput("p_pos_lag1", h4(id="pw_ht_h4_10","Adjust positivelag (years)"), value =20),
                                                        selectInput("pairwise_main_colour_scale", label = h4(id="pw_ht_h4_11","Select colour scale"),choices = list("Blue-Grey-Red" = 1, "Grey-Red" = 2, "Grey-Blue" = 3, "White-Black" = 4), selected = 1)
                                                        #numericInput("sing.hm.y.tick.spc", label = "Y-axis tick mark spacing", value = 10)
                                                    ),
                                                    box(width=4, h4(id="pw_ht_h4_12","x axis options"),
                                                        checkboxInput("adj_x_sing", h4(id="pw_ht_h4_13","Adjust X axis scale"), value = FALSE),
                                                        numericInput("min_x_sing", h4(id="pw_ht_h4_14","Adjust min X axis value (years)"), value =""),
                                                        numericInput("max_x_sing", h4(id="pw_ht_h4_15","Adjust min X axis value (years)"), value ="")
                                                    )
                                           )),

                                  tabPanel(h4(id = "nav_h4_6", "Aligned Data"), value = "initiatedres",
                                           fluidRow(box(width = 9,  height = 1200,
                                                        div(style="text-align: left", actionButton("al_pw_general_hlp","", icon = icon("info"))),
                                                        tabsetPanel(id = "align_data", tabPanel(h4("All aligned data"),
                                                                             h4(id="al_dt_h4_1","Black lines = individual aligned series; red line = arithmetic mean chronology"),
                                                                             plotOutput("initiated_chron_plot", height = "100%") %>% withSpinner(type = 1, size = 3, color="#2ca25f", color.background = "#FFFFFF")),
                                                                    tabPanel(h4(id="al_dt_h4_2","Sample depth plot"),
                                                                             plotOutput("sample_dist_plot") %>% withSpinner(type = 1, size = 3, color="#2ca25f", color.background = "#FFFFFF")),
                                                                    tabPanel(value = "prob_samp_plots", h4(id="al_dt_h4_3","Evaluate problematic samples"),
                                                                            # plotOutput("prob_sample_plot") %>% withSpinner(type = 1, size = 3, color="#2ca25f", color.background = "#FFFFFF"),
                                                                             plotOutput("align_prob_heatmap") %>% withSpinner(type = 1, size = 3, color="#2ca25f", color.background = "#FFFFFF"))


                                                        )),

                                                    box(width = 3,
                                                        div(style="text-align: left", actionButton("al_pw_menu_hlp","", icon = icon("info"))),
                                                        h4(id="al_dt_h4_4","Potential problem samples"),
                                                        actionButton("reevaluate", h4(id="al_dt_h4_5","Evaluate for problem samples")),
                                                        div(tableOutput("initiated_problems"), style="font-size:24px"),
                                                        numericInput("initiated_problems_bin", h4(id="al_dt_h4_6","Set window size for problem analysis (years; must be even)"), value = 20),
                                                        selectInput("align_prob_samp", label = h4(id="al_dt_h4_7","Select problem sample to evaluate"), c("")),
                                                        actionButton("aligned_prob_check", label = h4(id="al_dt_h4_8","Plot problem samples")),
                                                        tags$hr(),
                                                        h4(id="al_dt_h4_9","Plot options"),
                                                        checkboxInput("initiated_chron_plot_x_adj", h4(id="al_dt_h4_10","Adjust X axis"), FALSE),
                                                        numericInput("initiated_chron_plot_min_X", h4(id="al_dt_h4_11","Min X value"), value =""),
                                                        numericInput("initiated_chron_plot_max_X", h4(id="al_dt_h4_12","Min X value"), value =""),
                                                        numericInput("rbar_wind_init", h4(id="al_dt_h4_13","Select window to calculate Rbar and EPS"), value = 25),
                                                        h4(id="al_dt_h4_14","window must not be greater than the length of the series"),
                                                        numericInput("init_sample_name_sz", h4(id="al_dt_h4_15","Sample ID  size"), 5)
                                                    )),
                                           fluidRow(box(width=9,
                                                        div(style="text-align: left", actionButton("al_pw_download_hlp","", icon = icon("info"))),
                                                        downloadButton("initiated.chrono.raw", h4(id="al_dt_h4_16","Save the undetrended aligned series")),
                                                        downloadButton("initiated.chrono.detrend", h4(id="al_dt_h4_17","Save the detrended aligned series")),
                                                        downloadButton("remove_initiated_series", h4(id="al_dt_h4_18","Save undated file with dated series removed")),
                                                        downloadButton("create_initiated_chron_rwl", h4(id="al_dt_h4_19","Save RWL file")),
                                                        downloadButton("initiated_two_column", h4(id="al_dt_h4_20","Save mean chronology"))#,
                                           )),
                                           fluidRow(box(width= 9,
                                                        div(style="text-align: left", actionButton("al_pw_correl_with_replacement_hlp","", icon = icon("info"))),
                                                        h4(id="al_dt_h4_21","Correlations between individual series and mean chronology with replacement"),
                                                        div(tableOutput("initiated_chron_correl_replace"), style ="font-size: 24px")
                                           )))
                      )
                  )
                ))}
