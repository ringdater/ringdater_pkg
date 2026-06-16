# This file declares global variables to satisfy R CMD check for Shiny apps

if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "input",             # Shiny input object
      "loading_df_data",   # variable used in your server logic
      "output",            # Shiny output object
      "session"            # Shiny session object
    )
  )
}
