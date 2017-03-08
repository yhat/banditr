#' Stream metrics back to Bandit.
#'
#' @export
#' @param tag Identifier for the metric.
#' @param value A number.
#' @return Response from the Bandit API.
#' @examples
#' \dontrun{
#' stream('x', 1)
#' stream('x', 2)
#' stream('x', 3)
#' stream('alpha', 100)
#' stream('zalpha', 20.5)
#' }
stream = function(tag, value) {
  if (! is.numeric(value)) {
    stop(paste0("value is not numeric! ", value))
  }

  creds <- get_credentials()
  job_id <- Sys.getenv('BANDIT_JOB_ID')

  url <- paste(creds$url, 'api', 'jobs', job_id, 'report', sep='/')
  data <- list(
    tag_name=jsonlite::unbox(tag),
    x=jsonlite::unbox(-1),
    y=jsonlite::unbox(value)
  )

  httr::PUT(
    url,
    body = jsonlite::toJSON(data),
    httr::authenticate(creds$username, creds$apikey, 'basic'),
    httr::add_headers("Content-Type"="application/json")
  )
}

# stream('x', 100)
# stream('x', 'boo')
