#' get_teacher_code
#'
#' If teacher launched app, gets teacher code.
#'
#' @param session Shiny session.
#'
#' @return The teacher code.
get_teacher_code <- function(session) {
  query <- shiny::parseQueryString(session$clientData$url_search)

  query$teacherCode
}

#' mark_used
#'
#' Marks teacher code as used.
#'
#' @param code Teacher code to mark as used.
mark_used <- function(code) {
  base_url <- "https://firestore.googleapis.com/v1/projects/msportal-accesscontrol/databases/(default)/documents/"
  full_url <- paste0(base_url, "launches/", code)
  access_token <- get_access_token()

  body <- list(
    fields = list(
      used = list(booleanValue = TRUE)
    )
  )

  req <- httr2::request(full_url) |>
    httr2::req_auth_bearer_token(access_token) |>
    httr2::req_method("PATCH") |>
    httr2::req_url_query(`updateMask.fieldPaths` = "used") |>
    httr2::req_body_json(body)

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

#' teacher_code_is_valid
#'
#' Checks if teacher code is valid.
#'
#' @param session Shiny session.
#'
#' @return Whether teacher code is valid.
teacher_code_is_valid <- function(session) {
  teacher_code <- get_teacher_code(session)

  # if teacher code is null, it is not valid
  if(is.null(teacher_code) || teacher_code == "") {
    return(NULL)
  }

  teacher_codes <- read_firebase("launches")
  codes <- sapply(teacher_codes$documents, function(x) x$fields$token$stringValue)
  # check that doc exists
  if(!(teacher_code %in% codes)) {
    return(FALSE)
  }
  fields <- teacher_codes$documents[[which(teacher_code == codes)[1]]]$fields
  # check that code is active
  if(fields$used$booleanValue) {
    return(FALSE)
  }
  # mark code as used
  mark_used(teacher_code)
  # check that expiresAt is in the future
  ts <- fields$expiresAt$timestampValue
  time <- as.POSIXct(
    ts,
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  )
  return(time > Sys.time())
}

#' get_teacher_email
#'
#' Gets teacher email
#'
#' @param session Shiny session.
#'
#' @return Teacher email
get_teacher_email <- function(session) {
  teacher_code <- get_teacher_code(session)

  teacher_codes <- read_firebase("launches")
  codes <- sapply(teacher_codes$documents, function(x) x$fields$token$stringValue)
  emails <- sapply(teacher_codes$documents, function(x) x$fields$teacherEmail$stringValue)

  emails[which(codes == teacher_codes)]
}

#' overlay
#'
#' Shrouds shiny app behind overlay.
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @param duration Time in minutes until session expiry.
#' @export
overlay <- function(input, output, session, appName, duration = 90, forceClassCode = FALSE) {

  request_code_overlay <- function() {
    shiny::showModal(shiny::modalDialog(
      title = shiny::span(
        style = "color: black;",
        "Class code required"
      ),
      shiny::div(
        style = "color: black;",
        shiny::textInput(session$ns("class_code"), "Class code")
      ),
      footer = shiny::tagList(
        shiny::actionButton(session$ns("submit_code"), "Submit")
      ),
      easyClose = FALSE
    ))
  }

  invalid_overlay <- function() {
    shiny::showModal(shiny::modalDialog(
      title = shiny::span(
        style = "color: red;",
        "Session expired or invalid"
      ),
      shiny::div(
        style = "color: red;",
        shiny::p("This session is expired or invalid. Start a new session from Dashboard.")
      ),
      footer = shiny::actionButton(
        session$ns("dismiss_expired"),
        "Dismiss"
      ),
      easyClose = FALSE
    ))
  }

  # at first, gate is locked, but not expired
  gate <- shiny::reactiveValues(
    unlocked = FALSE,
    unlocked_at = NULL,
    expired = FALSE,
    lock_dashboard = TRUE,
    class_code = "",
    teacher_email = ""
  )

  # a valid teacher code unlocks the gate and starts timer
  shiny::observe({
    shiny::req(!gate$unlocked, !gate$expired)

    valid <- teacher_code_is_valid(session)
    gate$teacher_email <- get_teacher_email()

    if(is.null(valid)) {
      request_code_overlay()
    }

    else if(isTRUE(valid)) {
      if(forceClassCode) {
        request_code_overlay()
      }
      else {
        gate$unlocked <- TRUE
        gate$unlocked_at <- Sys.time()
        gate$expired <- FALSE
      }
      gate$lock_dashboard <- FALSE
    }

    else if(isFALSE(valid)) {
      invalid_overlay()
    }
  })

  # submitting a valid student code unlocks the gate and starts timer
  shiny::observeEvent(input$submit_code, {
    shiny::req(!gate$unlocked, !gate$expired)

    class_code <- input$class_code

    if(code_is_valid(class_code, appName)) {
      gate$unlocked <- TRUE
      gate$unlocked_at <- Sys.time()
      gate$expired <- FALSE
      gate$class_code <- class_code
      shiny::removeModal()
    }
    else {
      shiny::showNotification(
        "That code wasn't recognized. Check the code and try again.",
        type = "error"
      )
    }
  })

  # when time is up, gate is locked
  shiny::observe({
    shiny::invalidateLater(1000, session)

    shiny::req(gate$unlocked, gate$unlocked_at)

    elapsed <- as.numeric(
      difftime(Sys.time(), gate$unlocked_at, units = "mins")
    )

    if(elapsed >= duration && !gate$expired) {
      gate$unlocked <- FALSE
      gate$expired <- TRUE
      invalid_overlay()
    }
  })

  # on clicking dismiss, close the app
  shiny::observeEvent(input$dismiss_expired, {
    shiny::stopApp()
  }, ignoreInit = TRUE)

  invisible(gate)
}
