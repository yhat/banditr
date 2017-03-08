library(httr)
library(jsonlite)

get_credentials = function() {
  list(
    username=Sys.getenv('BANDIT_CLIENT_USERNAME'),
    apikey=Sys.getenv('BANDIT_CLIENT_APIKEY'),
    url=Sys.getenv('BANDIT_CLIENT_URL')
  )
}

stream = function(tag, value) {
  if (! is.numeric(value)) {
    stop(paste0("value is not numeric! ", value))
  }

  creds <- get_credentials()
  job_id <- Sys.getenv('BANDIT_JOB_ID')

  url <- paste(creds$url, 'api', 'jobs', job_id, 'report', sep='/')
  data <- list(
    tag_name=unbox(tag),
    x=unbox(-1),
    y=unbox(value)
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
