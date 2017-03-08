#' Track and associate metadata key/values with your Bandit job. These will be
#' stored on the Bandit server and displayed in the UI.
#'
#' @export
#' @param key Name for the metric you'd like to track (i.e. accuracy or training_size)
#' @param value Number/String/Character you'd like to track. (i.e. 0.84 or 250)
#' @examples
#' \dontrun{
#' metadata('ROC', 0.74)
#' metadata('accuracy', 0.97)
#' metadata('foo', 'bar')
#' }
metadata = function(key, value) {
  filename <- '/job/metadata/metadata.json'
  if(! file.exists(filename)) {
    rawJSON <- '{}'
  } else {
    rawJSON <- readLines(filename)
  }
  data <- jsonlite::fromJSON(rawJSON)
  data[[key]] <- value

  if (is_local()) {
    print(jsonlite::toJSON(data, auto_unbox=TRUE))
    return
  }
  write(jsonlite::toJSON(data, auto_unbox=TRUE), filename)
}
