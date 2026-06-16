#' RingdateR GUI
#'
#' This function creates the graphical user interface
#' @keywords GUI
#' @importFrom shinyalert useShinyalert
#' @importFrom shinyjs useShinyjs
#' @export
#' @import shiny
#' @import shinydashboard
#' @import shinycssloaders
#' @importFrom shinyWidgets dropdownButton
#' @importFrom magrittr %>%

ui <- function(){
  dashboardPage(skin="green",

        dashboardHeader(title = tags$div(class = "custom-title", "RingdateR"), tags$li(class = "dropdown")),

        dashboardSidebar(
            useShinyjs(),
            tags$h2(id = "side_h2_1","Plot options"),
            numericInput("text.size", label = h2(id = "side_h2_2","Plot text size"), value = 12, min = 1, max = 48), # changes the plot text size
            h2(id = "text_help", "Text size should be > 0"),
            numericInput("plot.line", label = h2(id = "side_h2_3"," Plotted line thickness"), value = 0.5, min = 0), # changes the plotted line thickness
            h2(id = "plot.line_help", "line thickness should be > 0"),
            numericInput("line.width", label = h2(id = "side_h2_4"," Axis line weighting"), value = 0.5, min = 0), # Change the axis thickness
            h2(id = "line.width_help", "Axis thickness should be > 0"),
            tags$hr()
            # h2(id = "side_h2_5","Text display settings"),
            # selectInput("font_select", label = h2(id = "side_h2_6","Select font size"),
            #                 choices = list("Small" = 1, "Medium" = 2, "Large" = 3),
            #                 selected = 2)
        ),
        dashboardBody(
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
            div(id = "form",
                tabsetPanel(id = "navbar",
                tabPanel(

################# Title bar of the starting point page
                    h4(id = "nav_h4_1", "Starting Point"), value ="start",
                    fluidRow(
                      box(
                        width = 12,
                        height = "100px",
                        title = h3(id = "st_pt_h3_1", "Welcome to RingdateR (V0.1.0)",
                                   actionButton("st_general_hlp", h6("I - Quick Help/about"))
                        ),
                        # Place the button here inside the box, but with CSS to push it far right
                        div(
                          style = "position: absolute; right: 20px; top: -100px; z-index: 1000;",  # absolute positioning
                          #style = "display: flex; justify-content: flex-end; align-items: center; height: 100%;",
                          actionButton(
                            "close",
                            label = tags$b(p("Stop RingdateR.")),
                            icon = icon("skull"),
                            onclick = "setTimeout(function(){window.close();},500);",
                            class = "btn btn-danger",
                            style = "white-space: normal; width: 150px;"
                          )
                        ),
                        style = "position: relative;"  # make box a relative container for absolute positioning
                      )
                    ),
        ############# Row of the different step boxes
                    fluidRow(
                            column(3,
                            #### set up the settings
                                box(width = NULL, title = h2(id = "st_pt_h2_1", "Step 1: Analysis options",
                                                             style = "margin-top: 5px; margin-bottom: 5px;"), status = "success",
                                    div(style="text-align: right", actionButton("st_analysis_hlp","", icon = icon("info"))),
                                        solidHeader = TRUE,
                                        align="left",
                                        tags$div(id = "st_pt_h4_1",
                                             h4("Choose a detrending method"),
                                             selectInput("detrending_select", label = NULL,
                                                         choices = list(
                                                           "Do nothing to my data" = 1,
                                                           "Convert to z-scores" = 2,
                                                           "Spline detrending" = 3,
                                                           "Mod. negative exponential" = 4,
                                                           "Friedman" = 5,
                                                           "ModHugershoff" = 6,
                                                           "First difference" = 7),
                                                         selected = 3)),
                                        checkboxInput("ARmod", h4(id = "st_pt_h4_4", "Apply AR1 pre-whitening"), TRUE),
                                        checkboxInput("logT", h4(id = "st_pt_h4_4", "Apply Log transform"), TRUE),
                                        numericInput("splinewindow", h4(id = "st_pt_h4_2", "Select a spline window (only applies to spline detrendning option)"), value = 32, min = 5, max = 200, step = 1),
                                        h3(id = "spline_len_help1", "Warning: Spline length should be >5 and <=200"),
                                        checkboxInput("total_overlap", h4(id = "st_pt_h4_4", "Automatically set lag limits? (uses all possible leads/lags)"), TRUE),
                                        h6(id = "st_pt_h6_1", "Alternatively set limits for lead-lag range"),
                                        numericInput("neg_lag",h4(id = "st_pt_h4_5", "Select lower lag limit (years)"), min = 0, value =-20),
                                        h3(id = "low_lag_warn_1", "Warning: Lowerlag should be lower than upper lag limit"),
                                        h3(id = "low_lag_warn_2", "Warning: Lower lag value should be an integer."),
                                        numericInput("pos_lag", h4(id = "st_pt_h4_6", "Select upper lag limit (years)"), min = 0, value =20),
                                        h3(id = "up_lag_warn_1", "Warning: Upper lag should be lower than upper lag limit"),
                                        h3(id = "up_lag_warn_2", "Warning: Upper lag value should be an integer."),
                                        numericInput("cor_win", h4(id = "st_pt_h4_7", "Select correlation window (years)"), min = 5, value =21),
                                        h6(id = "st_pt_h6_2","Correlation window needs to be an odd number. Even numbers will be converted to odd by adding 1."),
                                        htmlOutput("note"))),

                            #### load the data
                            column(3,
                                    #### Load undated data
                                box(width = NULL, status = "success",
                                    solidHeader = TRUE,
                                    title = h2(id = "st_pt_h2_2","Step 2: Load undated series",
                                               style = "margin-top: 5px; margin-bottom: 5px;"),
                                    div(style="text-align: right", actionButton("st_undate_hlp","", icon = icon("info"))),
                                    selectInput("multi_chron", h4(id = "st_pt_h4_8","What type of undated data do you want to load?"), choices = list("Load individual undated series (.csv, .pos, .lps, .rwl, .txt, .xlsx)" = 1, "Load undated chronologies (.rwl, .csv, .xlsx)" = 2), selected = 1),
                                    fileInput('file1', h4(id = "st_pt__h4_9",'Load data for pairwise analysis'), multiple = TRUE, buttonLabel = h6("Select a file"), placeholder = "No file selected",
                                                accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv', '.pos', '.lps', '.rwl', '.txt', '.xlsx')),
                                    tags$hr(),
                                    checkboxInput("pair_detrend", h4(id = "st_pt_h4_10", "Apply detrending"), TRUE),
                                    checkboxInput("apply_yrs", h4(id = "st_pt_h4_4", "Apply loaded years"), FALSE),
                                    checkboxInput("avg_replicates", h4(id = "st_pt_h4_4", "Average RingMeasurer Replicates"), TRUE),
                                    selectInput("year_inc_select", h4(id = "st_pt_h4_11", "First column is years or increment count?"), choices = list("Years" = 1, "Increment count" = 2), selected = 1)),

                                #### Load a dated chronology
                                box(width = NULL, status = "success",
                                    solidHeader = TRUE,
                                    title = h2(id = "st_pt_h2_3","Load a dated chronology",
                                               style = "margin-top: 5px; margin-bottom: 5px;"),
                                    div(style="text-align: right", actionButton("st_chron_hlp","", icon = icon("info"))),
                                    fileInput('file2', h4(id = "st_pt_h4_12", 'Load chronology data (.rwl, .csv, .xlsx).'), multiple = FALSE, buttonLabel = h6("Select a file"), placeholder = "No file selected",
                                                accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv', '.rwl', '.xlsx')),
                                    checkboxInput("chron_detrend", h4(id = "st_pt_h4_13", "Apply detrending"), TRUE)),

                                #### Load example data
                                box(width = NULL, status = "success",
                                    solidHeader = TRUE,
                                    title = h2(id = "st_pt_h2_4","Use example data",
                                               style = "margin-top: 5px; margin-bottom: 5px;"),
                                    div(style="text-align: right", actionButton("st_example_hlp","", icon = icon("info"))),
                                    actionButton("example_undated", h4(id = "st_pt_h4_14", "Use example dated and undated series")))),

                            ##### duisplay sample names of loaded data
                            column(3,
                                box(width = NULL, title = h2(id = "st_pt_h2_5","Step 3: Series to analyse",
                                                             style = "margin-top: 5px; margin-bottom: 5px;"), status = "success",
                                    solidHeader = TRUE,
                                    div(style="text-align: right", actionButton("st_clear_data_hlp","", icon = icon("info"))),
                                    actionButton("Go_clear_loaded", h4(id = "st_pt_h4_15","Clear all loaded data")),
                                    plotOutput("load_plot", height = "10%") %>% withSpinner(type = 1, size = 2, color="#2ca25f", color.background = "#FFFFFF"),
                                    div(tableOutput("analysis.data.out"), style = "font-size:24px" ))),

                            #### Some JS code to reset the file loading progress bars
                                tags$script('Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {
                                        var id = "#" + x + "_progress";
                                        var idBar = id + " .bar";
                                        $(id).css("visibility", "hidden");
                                        $(idBar).css("width", "0%");});'),

                            ##### Column for the run analyses section
                            column(3,
                                #### select analysis mode and run analysis
                                box( width = NULL, title = h2(id = "st_pt_h2_6","Step 4: Choose analysis pathway",
                                                              style = "margin-top: 5px; margin-bottom: 5px;"), status = "success",
                                    solidHeader = TRUE,
                                    selectInput("mode_select", label = "", choices = list("Pairwise Analysis Mode"=1, "Chronology Analysis Mode"=2), selected = 1),
                                    actionButton("Go_pairwise", h4(id = "st_pt_h4_17","Run the analyses"), icon = icon("rocket"))),

                                #### Generate report box
                                box( width = NULL, title = h2(id = "st_pt_h2_7","Alt. step: Evaluate the loaded chronology",
                                                              style = "margin-top: 5px; margin-bottom: 5px;"), status = "success",
                                    solidHeader = TRUE,
                                    h4("Produce a report of the crossdating statistics for the loaded chronology"),

                                    div(style="display:inline-block; ", dropdownButton(
                                        tags$h4("Report settings"),
                                        textInput("chron_report_name", label = h4("Report file name (do not add extension):"), "Chronology_report"),
                                        selectInput("chron_format", label = h4(id="pw_ht_h4_11","Document format"),choices = list("HTML" = 1, "Word" = 2, "pdf" = 3), selected = 1),
                                        checkboxInput(inputId = "chron_eval_verbose", label = h4("Verbose report?"), value = FALSE),
                                        numericInput(inputId = "chron_eval_EPS", label = h4("Window for EPS"), value = 50),
                                        numericInput(inputId = "chron_eval_probs", label = h4("Window for problem check"), value = 30),
                                        circle = FALSE, status = NULL,
                                        label =h4("Report settings"),
                                        inputId = "mydropdown",
                                        icon = icon("gear"), width = "400px")),
                                    div(style="display:inline-block; float:right;", downloadButton("chron_evaluate", h4(id = "st_pt_h4_17","Produce report"), icon = icon("rocket")))))
                    )# close the fluid row
                ),# close the starting point tab panel

###################################################################
######## The loaded Data Tab ######################################
###################################################################
            tabPanel(h4( "Loaded Data"),
                ### Create a sub tab panel for the different loaded datasets
                tabsetPanel(
                    tabPanel(h4("Undated series"), value ="mand",
                            fluidRow(box(width=12,
                                        downloadButton('download_undated_raw',label=h4(id = "ld_dt_h4_1","Save Undaed series (as .CSV)")),
                                        div(tableOutput("undated_data_table"), style = "font-size:24px")))),
                    tabPanel(h4("Chronology data"), value ="chrono",
                            div(tableOutput("loaded_chronology_data_table"), style = "font-size:24px")),

                    tabPanel(h4("Detrended data"), value ="mand",
                            downloadButton('download_detrend',label=h4(id = "ld_dt_h4_1","Save the detrended series (as .CSV)")),
                            div(tableOutput("detrended_data_table"), style = "font-size:24px"))#,

                # This can be used to show other data / debug
                # tabPanel(h4("debug"), value ="mand",
                #          div(tableOutput("debug"), style = "font-size:24px"),
                #          div(tableOutput("debug2"), style = "font-size:24px"))

                )# close the loaded data sub tab panel
            ),# close the main loaded data tab

###################################################################
######## Detrending plot page######################################
###################################################################

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
                        numericInput("splinewindow_2", h4(id = "st_pt_h4_2", "Select a spline window (only applies to spline detrendning option)"), value = 32, min = 5, max = 200, step = 1),
                        h3(id = "spline_len_help2", "Spline length should be >5 and <=200"),
                )# close the fluid row
            )), # close the detrending plot tab

###################################################################
########results  page##############################################
###################################################################

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
                                    numericInput("PS_2_lag", h4(id="pw_rs_h4_8","Enter value for custom lag"), value =0),
                                    h3(id = "custom_lag_help", "Warning: Custom lag value should be an integer")
                                    ),
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
                            h3(id = "pair_r_help", "R value should be >0 and <1"),
                            numericInput("pair_sig", h4(id="pw_rs_h4_21","Significance value"), value =0.01),
                            h3(id = "pair_sig_help", "Significance value value should be >0 and <1"),
                            numericInput("pair_overlap", h4(id="pw_rs_h4_22","Overlap"), value =50),
                            h3(id = "pair_overlap_help", "Overlape should be >0 ")
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
                            h3(id = "pair_prob_help", "Overlape should be >5 "),
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

###################################################################
######## Full heat map  page ######################################
###################################################################

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
###################################################################
######## Aligned data page ########################################
###################################################################
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
                                h3(id="aligned_prob_help", "Warning: Problem sample running window should be >5."),
                                selectInput("align_prob_samp", label = h4(id="al_dt_h4_7","Select problem sample to evaluate"), c("")),
                                actionButton("aligned_prob_check", label = h4(id="al_dt_h4_8","Plot problem samples")),
                                tags$hr(),
                                h4(id="al_dt_h4_9","Plot options"),
                                checkboxInput("initiated_chron_plot_x_adj", h4(id="al_dt_h4_10","Adjust X axis"), FALSE),
                                numericInput("initiated_chron_plot_min_X", h4(id="al_dt_h4_11","Min X value"), value =""),
                                numericInput("initiated_chron_plot_max_X", h4(id="al_dt_h4_12","Max X value"), value =""),
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
                                downloadButton("initiated_two_column", h4(id="al_dt_h4_20","Save mean chronology"))
                    )),
                    fluidRow(box(width= 9,

                                div(style="display:inline-block; margin:10px;", dropdownButton(
                                    tags$h4("Report settings"),
                                    textInput("summary_report_name", label = h4("Summary report file name (do not add extension):"), "Summary_report"),
                                    selectInput("format", label = h4(id="pw_ht_h4_11","Document format"),choices = list("HTML" = 1, "Word" = 2, "pdf" = 3), selected = 1),
                                    checkboxInput(inputId = "summary_verbose", label = h4("Verbose report?"), value = FALSE),
                                    numericInput(inputId = "summary_EPS", label = h4("Window for EPS"), value = 50),
                                    numericInput(inputId = "summary_probs", label = h4("Window for problem check"), value = 30),

                                    circle = FALSE, status = NULL,
                                    label =h4("Report settings"),
                                    inputId = "mydropdown",
                                    icon = icon("gear"), width = "400px"
                                )),
                                div(style="display:inline-block", downloadButton("summary_report_tmp", h4(id="al_dt_h4_20","Save summary report"))),

                                div(style="text-align: left", actionButton("al_pw_correl_with_replacement_hlp","", icon = icon("info"))),
                                h4(id="al_dt_h4_21","Correlations between individual series and mean chronology with replacement"),
                                div(tableOutput("initiated_chron_correl_replace"), style ="font-size: 24px")
                    )))
)
)
))}
