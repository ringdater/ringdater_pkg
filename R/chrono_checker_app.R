#' Quick Chronology Checker App
#'
#' This function creates the graphical user interface
#' @keywords GUI
#' @import shiny
#' @import ggplot2
#' @import grid
#' @import dplR
#' @import DT
#' @import dplR
#' @export

# Define UI for application
chrono_check_ui <- fluidPage(
  titlePanel("Chronology Check Plotter"),

  sidebarLayout(
    sidebarPanel(
      tags$p("This app allows you to check how well samples crossdate into a chronology."),
      tags$p("Load a chronology file (csv only; first column years, then samples in each other column date aligned)."),
      tags$p("Select a series from the summary table to analyze."),
      tags$p("The loaded data are detrended using a spline (by division)."),

      fileInput("file", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),

      numericInput("spline_length", "Spline Window Length", value = 21, min = 1, max = 100, step = 1),
      numericInput("lag", "enter a lag", value = 0),

      numericInput("text_size", "Text Size", value = 12, min = 1, max = 30, step = 1),
      numericInput("line_width", "Line Width", value = 1, min = 0.1, max = 5, step = 0.1),
      numericInput("plot_line", "Plot Line Width", value = 0.5, min = 0.1, max = 5, step = 0.1),
      actionButton("analyze", "Run Analysis"),
      downloadButton("downloadPlot", "Download Combined Plot")
    ),

    mainPanel(
      DTOutput("summary_table"),  # Output for the summary table
      plotOutput("combinedPlot", height = "1000px")  # Taller figure display
    )
  )
)

# Define server logic
chrono_check_server <- function(input, output, session) {

  # Reactive expression to load the CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = TRUE)
  })

  # Render the summary table for user to select a series
  output$summary_table <- renderDT({
    req(data())
    df <- data()

    # Create a summary data frame
    summary_df <- data.frame(
      Column_Name = colnames(df)[-1],
      Start_Year = sapply(df[-1], function(x) min(df[[1]][!is.na(x)], na.rm = TRUE)),
      End_Year = sapply(df[-1], function(x) max(df[[1]][!is.na(x)], na.rm = TRUE))
    )

    datatable(summary_df[,-1], options = list(pageLength = 10), selection = "single")  # Allow single row selection
  })

  # Run analysis and plot on button click
  observeEvent(input$analyze, {
    req(data(), input$spline_length, input$text_size, input$line_width, input$plot_line, input$summary_table_rows_selected)

    df <- data()  # Access data without re-reading the file
    selected_row <- input$summary_table_rows_selected  # Get selected row index

    if (is.null(selected_row)) return()  # Return if no row is selected

    # Use selected row to get the appropriate column
    selected_column_index <- selected_row + 1  # +1 because the first column is years
    selected_column_name <- colnames(df)[selected_column_index]  # Get the name of the selected column

    # Select the years and the chosen column
    lag <- as.numeric(input$lag)
    print(lag)
    sel_sample <- df[, c(1, selected_column_index)]
    sel_sample[,1] <- sel_sample[,1] + lag

    sel_sample <- normalise(sel_sample, detrending_select = 3, splinewindow = as.numeric(input$spline_length))

    # Remove the selected column from raw chron data
    raw_chron_data <- df[, -selected_column_index]  # All columns except the selected one

    # Use user-defined spline length
    det_chron_data <- normalise(raw_chron_data, detrending_select = 3, splinewindow = as.numeric(input$spline_length))

    row.names(det_chron_data) <- df[, 1]  # Set row names to years

    chrono <- chron(det_chron_data[,-1])
    chrono <- data.frame(years = as.numeric(row.names(chrono)), sgi = chrono[, 1])

    combined <- cbind(chrono, sel_sample[, 2])  # Combine chronology with selected sample
    colnames(combined) <- c("year", "mean_chronology", selected_column_name)

    # Lead-lag analysis
    cor_res <- lead_lag_analysis(the_data = combined,
                                 mode = 1,
                                 neg_lag = (-20 + lag),
                                 pos_lag = (20 + lag),
                                 complete = TRUE)


    # Generate plots with user-defined parameters
    plot1 <- line_plot(combined,
                       colnames(combined)[2],
                       colnames(combined)[3],
                       lag = lag,
                       text.size = as.numeric(input$text_size),
                       line.width = as.numeric(input$line_width),
                       plot.line = as.numeric(input$plot_line))

    plot2 <- lead_lag_bar(the_data = as.data.frame(cor_res[2]),
                          sample_1 = "mean_chronology",
                          sample_2 = selected_column_name)# Use the selected sample name


    plot2 <- plot2 + theme(
      axis.title.y = element_text(size = input$text_size),   # Y-axis label font size
      axis.text.y = element_text(size = input$text_size),    # Y-axis tick label font size
      axis.title.x = element_text(size = input$text_size),   # X-axis label font size
      axis.text.x = element_text(size = input$text_size)      # Adjust Tick label font size
    )

    plot3 <- heatmap_analysis(combined,
                              colnames(combined)[2],
                              colnames(combined)[3],
                              neg_lag = (-10 + lag),
                              pos_lag = (10 + lag),
                              win = 21,
                              center = 0,
                              complete = FALSE,
                              sel_col_pal = 1,
                              font_size = as.numeric(input$text_size),
                              axis_line_width = as.numeric(input$line_width),
                              plot_line = as.numeric(input$plot_line),
                              leg_size = 1)

    # Combine plots
    g1 <- ggplotGrob(plot1)
    g2 <- ggplotGrob(plot2)
    g3 <- ggplotGrob(plot3)
    g <- rbind(g1, g2, g3, size = "first")

    # Save combined plot for download
    output$downloadPlot <- downloadHandler(
      filename = "sample_vs_chron_plots.jpeg",
      content = function(file) {
        ggsave(file, g, dpi = 600, height = 12, width = 10, unit = "in")
      }
    )

    # Display combined plot in the app
    output$combinedPlot <- renderPlot({
      grid.draw(g)
    })
  })
}


