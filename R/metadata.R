library(jsonlite)

metadata = function(key, value) {
  filename <- '/job/metadata/metadata.json'
  filename <- '/tmp/metadata.json'
  if(! file.exists(filename)) {
    rawJSON <- '{}'
  } else {
    rawJSON <- readLines(filename)
  }
  data <- fromJSON(rawJSON)
  data[[key]] <- value
  write(jsonlite::toJSON(data, auto_unbox=TRUE), filename)
}

# metadata('foo', 'bar')
