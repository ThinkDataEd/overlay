#' get_access_token_temp
#'
#' Gets temporary firebase access token.
#'
#' @return The temporary access token.
get_access_token_temp <- function() {

  cred_path <- Sys.getenv("OVERLAY_CREDENTIALS")

  creds <- jsonlite::fromJSON(cred_path)

  # Exchange refresh token for access token
  resp <- httr2::request("https://oauth2.googleapis.com/token") |>
    httr2::req_body_form(
      client_id     = creds$client_id,
      client_secret = creds$client_secret,
      refresh_token = creds$refresh_token,
      grant_type    = "refresh_token"
    ) |>
    httr2::req_perform()

  httr2::resp_body_json(resp)$access_token
}

#' get_access_token
#'
#' Gets firebase access token.
#'
#' @return The access token.
get_access_token <- function() {
  cred_path <- Sys.getenv("OVERLAY_CREDENTIALS")

  options(
    googleAuthR.scopes.selected =
      "https://www.googleapis.com/auth/datastore"
  )

  googleAuthR::gar_auth_service(json_file = cred_path)

  googleAuthR::gar_token()$auth_token$credentials$access_token
}

#' read_firebase
#'
#' Reads from firebase.
#'
#' @param path Which collection to read from, choose classCodes or launches
#'
#' @return Contents of firebase collection.
read_firebase <- function(path) {
  base_url <- "https://firestore.googleapis.com/v1/projects/msportal-accesscontrol/databases/(default)/documents/"
  full_url <- paste0(base_url, path)
  access_token <- get_access_token()
  req <- httr2::request(full_url) |>
    httr2::req_auth_bearer_token(access_token) |>
    httr2::req_method("GET")
  resp <- httr2::req_perform(req)
  return(httr2::resp_body_json(resp))
}

#' is_code_valid
#'
#' Checks if class code is valid.
#'
#' @param code Class code.
#'
#' @return Logical indicating whether the supplied code is valid.
code_is_valid <- function(code) {
  classCodes <- read_firebase("classCodes")
  codes <- sapply(classCodes$documents, function(x) x$fields$code$stringValue)
  # check that doc exists
  if(!(code %in% codes)) {
    return(FALSE)
  }
  fields <- classCodes$documents[[which(code == codes)[1]]]$fields
  # check that code is active
  if(!fields$active$booleanValue) {
    return(FALSE)
  }
  # check that expiresAt is in the future
  ts <- fields$expiresAt$timestampValue
  time <- as.POSIXct(
    ts,
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  )
  return(time > Sys.time())
}
