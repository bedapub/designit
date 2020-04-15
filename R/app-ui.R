# varInput <- function(id, label = "Variable") {
#   ns <- NS(id)
#   tagList(
#     textInput(ns("var_name"), "Variable name", placeholder = ns("Variable")),
#     textInput(ns("var"), placeholder = "value1 value2 ... (list of values separated by spaces)")
#   )
# }

# app_ui <- fluidPage(
#   numericInput('num_vars', 'Number of variables')
# )

#' Factor input
#'
#' * one free text input to enter factor names (space separated --> add to field description)
#' * generate rows of free text input for factor levels (also space separated)
#' *
