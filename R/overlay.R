#' overlay
#'
#' Shrouds shiny app behind overlay.
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @param duration Time in minutes until session expiry.
#' @export
overlay <- function(input, output, session, duration = 90) {
  gate <- shiny::reactiveValues(
    unlocked = FALSE,
    unlocked_at = NULL,
    expired = FALSE
  )

  show_overlay <- function(expired = FALSE) {
    shiny::showModal(shiny::modalDialog(
      title = if(expired) {
        "Session expired"
      }
      else {
        "Class code required"
      },
      if(expired) {
        shiny::p("This class code has expired. Ask your teacher to generate a new one.")
      },
      shiny::textInput(session$ns("class_code"), "Class code"),
      footer = shiny::tagList(
        shiny::actionButton(session$ns("submit_code"), "Submit")
      ),
      easyClose = FALSE
    ))
  }

  show_overlay()

  shiny::observeEvent(input$submit_code, {
    class_code <- input$class_code

    if(code_is_valid(class_code)) {
      gate$unlocked <- TRUE
      gate$unlocked_at <- Sys.time()
      gate$expired <- FALSE
      shiny::removeModal()
    }
    else {
      shiny::showNotification(
        "That code wasn't recognized. Check the code and try again.",
        type = "error"
      )
    }
  })

  shiny::observe({
    shiny::invalidateLater(1000, session)

    shiny::req(gate$unlocked, gate$unlocked_at)

    elapsed <- as.numeric(
      difftime(Sys.time(), gate$unlocked_at, units = "mins")
    )

    if(elapsed >= duration && !gate$expired) {
      gate$unlocked <- FALSE
      gate$expired <- TRUE
      show_overlay(expired = TRUE)
    }
  })

  invisible(gate)
}
