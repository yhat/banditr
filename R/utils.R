#' Get a pre-defined database connection from Bandit.
#'
#' @export
#' @param name Name of the database you'd like to get the connection for.
#' @return Database connection string.
#' @examples
#' get_connection('mysql-prod')
#' get_connection('dw')
get_connection = function(name) {
  Sys.getenv(paste0('DATABASE_', name))
}

get_credentials = function() {
  list(
    username=Sys.getenv('BANDIT_CLIENT_USERNAME'),
    apikey=Sys.getenv('BANDIT_CLIENT_APIKEY'),
    url=Sys.getenv('BANDIT_CLIENT_URL')
  )
}

unbox_all = function(myList) {
  if (length(myList)==0) {
    return (myList)
  }
  for(i in 1:length(myList)) {
    myList[[i]] <- lapply(myList[[i]], jsonlite::unbox)
  }
  myList
}

is_local = function() {
  Sys.getenv('BANDIT_CLIENT_USERNAME')=="" || Sys.getenv('BANDIT_CLIENT_APIKEY')=="" || Sys.getenv('BANDIT_CLIENT_URL')==""
}
