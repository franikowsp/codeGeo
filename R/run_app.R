#' GeoGebra Coding Environment
#'
#' Runs GeoGebra coding applet.
#'
#' @import shiny
#' @import miniUI
#' @import shinyWidgets
#' @import keys
#'
#' @export
run_app <- function(.path, .coder, .type = "geogebra") {
  # Load data anonymously (requires to setup the coder file)
  # To provide fail safety, there should be an option to fall back on a default file
  ..file <- paste0(.path, .coder, ".RData")

  ..load_env <- new.env()
  load(file = ..file, envir = ..load_env)
  .data <- ..load_env[[.coder]]

  ..unitnames <- names(.data)
  ..selection <- .data[[..unitnames[1]]]
  ..n <- length(..selection[, "value"])

  if (.type == "geogebra") {
    typeOutput <- geogebraOutput(output_id = "geo", height = "700px", width = "700px")
  } else if (.type == "longText") {
    typeOutput <- longTextOutput(output_id = "text", height = "700px", width = "700px")
  }

  ui <-
    miniUI::miniPage(
      # Hotkeys
      keys::useKeys(),
      keys::keysInput("correct-key", "f"),
      keys::keysInput("partial-key", "g"),
      keys::keysInput("wrong-key", "j"),
      keys::keysInput("na-key", "b"),
      keys::keysInput("left-key", c("left")),
      keys::keysInput("right-key", c("right")),

      # Interface
      miniUI::gadgetTitleBar("GeoGebra Coder v0.0.1"),
      miniUI::miniTabstripPanel(
        miniUI::miniTabPanel("GeoGebra", icon = shiny::icon("sliders"),
                             miniUI::miniContentPanel(
                               shiny::fillCol(
                                 flex = c(0.2, 1),
                                 shiny::fillRow(
                                   shiny::div(
                                     shiny::tags$b("Status"),
                                     shiny::uiOutput("status"),
                                     shiny::uiOutput("n-coded"),
                                   ),
                                   shiny::numericInput(inputId = "subject",
                                                       label = paste0("Person (", ..n, ")"),
                                                       value = 1,
                                                       min = 1,
                                                       max = ..n),
                                   shiny::selectInput(inputId = "unitname",
                                                      label = "Aufgabe",
                                                      choices = ..unitnames,
                                                      selected = ..unitnames[1]),
                                   shiny::actionButton(inputId = "correct",
                                                       label = "richtig (f)",
                                                       style="background-color: #2E8B57; border-color: #2E8B57",
                                                       icon = shiny::icon("check")),
                                   shiny::actionButton(inputId = "partial",
                                                       label = "fast richtig (g)",
                                                       style="background-color: #acdb5a; border-color: #acdb5a",
                                                       icon = shiny::icon("check")),
                                   shiny::actionButton(inputId = "wrong",
                                                       label = "falsch (j)",
                                                       style="background-color: #DC143C; border-color: #DC143C",
                                                       icon = shiny::icon("xmark")),
                                   shiny::actionButton(inputId = "na",
                                                       label = "unsicher (b)",
                                                       style="background-color: #FFD700; border-color: #FFD700",
                                                       icon = shiny::icon("question")),
                                   shiny::textInput("comment", label = "Kommentar",
                                                    width = "100px")

                                   # Kommentarfunktion (offenes Feld darunter!)
                                   # Richtig, mit Toleranz richtig,
                                 ),
                                 typeOutput
                               )
                             )
        )


        # )
      )
    )

  server <- function(input, output, session) {
    reactiveCoded <- reactiveValues(data = .data)

    # GeoGebra Renderer
    output$geo <- renderGeogebra({
      ..geogebra <- reactiveCoded$data[[input$unitname]][input$subject, "value"]

      geogebra(..geogebra)
    })

    # LongText Renderer
    output$text <- renderLongText({
      ..longText <- reactiveCoded$data[[input$unitname]][input$subject, "value"]

      longText(..longText)
    })

    # Output
    output$status <- renderUI({
      code <- reactiveCoded$data[[input$unitname]][input$subject, .coder]

      if (is.null(code) || is.na(code)) {
        text <- "Nicht kodiert."
      } else if (code == 2) {
        text <- shiny::span("Richtig", style = "color: #2E8B57;")
      } else if (code == 1) {
        text <- shiny::span("Fast richtig", style = "color: #acdb5a;")
      } else if (code == 0) {
        text <- shiny::span("Falsch", style = "color: #DC143C;")
      } else if (code == 99) {
        text <- shiny::span("Unsicher", style = "color: #FFD700;")
      }

      shiny::p(text)
    })

    output$`n-coded` <- renderUI({
      ..current <- reactiveCoded$data[[input$unitname]]
      codes <- ..current[, .coder]

      if (is.null(codes)) {
        text <- "Keine Codes"
      } else {
        nCoded <- sum(! is.na(codes))

        if (nCoded == length(codes)) {
          text <- shiny::tags$span("Vollständig 👌", style = "color: seagreen;")
        } else {
          text <- paste0(nCoded, " von ", length(codes), " Codes")
        }
      }

      shiny::p(text)
    })

    # Coding
    observeEvent(ignoreInit = TRUE,
                 list(
                   input$correct,
                   input$`correct-key`
                 ), {
                   reactiveCoded$data[[input$unitname]][input$subject, .coder] <- 2
                 })

    observeEvent(ignoreInit = TRUE,
                 list(
                   input$partial,
                   input$`partial-key`
                 ), {
                   reactiveCoded$data[[input$unitname]][input$subject, .coder] <- 1
                 })

    observeEvent(ignoreInit = TRUE,
                 list(
                   input$wrong,
                   input$`wrong-key`
                 ), {
                   reactiveCoded$data[[input$unitname]][input$subject, .coder] <- 0
                 })

    observeEvent(ignoreInit = TRUE,
                 list(
                   input$na,
                   input$`na-key`
                 ), {
                   reactiveCoded$data[[input$unitname]][input$subject, .coder] <- 99
                 })

    # Navigation
    # Forward
    observeEvent(ignoreInit = TRUE,
                 list(
                   input$correct,
                   input$partial,
                   input$wrong,
                   input$na,
                   input$`correct-key`,
                   input$`partial-key`,
                   input$`wrong-key`,
                   input$`na-key`,
                   input$`right-key`
                 ), {
                   reactiveCoded$data[[input$unitname]][input$subject, paste0(.coder, "comment")] <- input$comment

                   next_subject <- input$subject + 1
                   ..n <- length(reactiveCoded$data[[input$unitname]][, "value"])
                   new_id <- ifelse(next_subject > ..n, ..n, next_subject)
                   updateNumericInput(session, "subject", value = new_id)

                   updateTextInput(session, "comment", value = reactiveCoded$data[[input$unitname]][new_id, paste0(.coder, "comment")])
                 })

    # Backward
    observeEvent(ignoreInit = TRUE,
                 list(
                   input$`left-key`
                 ), {
                   reactiveCoded$data[[input$unitname]][input$subject, paste0(.coder, "comment")] <- input$comment

                   previous_subject <- input$subject - 1
                   new_id <- ifelse(previous_subject < 1, 1, previous_subject)
                   updateNumericInput(session, "subject", value = new_id)

                   updateTextInput(session, "comment", value = reactiveCoded$data[[input$unitname]][new_id, paste0(.coder, "comment")])
                 })

    # Change unitname
    observeEvent(input$unitname, {
      ..n_new <- length(reactiveCoded$data[[input$unitname]][,1])

      updateNumericInput(session, "subject", value = 1,
                         max = ..n_new)

      updateTextInput(session, "comment", value = reactiveCoded$data[[input$unitname]][1, paste0(.coder, "comment")])

      updateNumericInput(session, "subject", label = paste0("Person (", ..n_new, ")"))
    })

    # Handle the Done button being pressed.
    observeEvent(input$done, {
      stopApp(
        if (TRUE) {
          assign(.coder, reactiveCoded$data)
          save(list = .coder, file = ..file)
        }
      )
    }, once = F)

  }

  shiny::runApp(list(ui = ui, server = server),
                # viewer = shiny::dialogViewer("run_app",
                #                              width = 1300,
                #                              height = 1000)
  )
}
