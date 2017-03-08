library(jsonlite)
library(mime)
library(openssl)

email = function(recipients, subject, body) {
  filename <- '/job/metadata/email.json'
  filename <- '/tmp/email.json'
  data <- list()
  data$recipients <- recipients
  data$subject <- jsonlite::unbox(subject)
  data$body <- jsonlite::unbox(body)
  data$attachments <- list()
  data$isHTML <- jsonlite::unbox(TRUE)

  write(jsonlite::toJSON(data), filename)
}

email("hi@yhat.com", "bye", "foo bar baz")
email(c("paul@yhathq.com", "hi@yhat.com"), "bye", "foo bar baz")


unbox.all = function(myList) {
  if (length(myList)==0) {
    return (myList)
  }
  for(i in 1:length(myList)) {
    myList[[i]] <- lapply(myList[[i]], unbox)
  }
  myList
}

add_attachment = function(filepath) {
  filename <- '/job/metadata/email.json'
  filename <- '/tmp/email.json'
  if(! file.exists(filename)) {
    rawJSON <- '{}'
  } else {
    rawJSON <- readLines(filename)
  }

  data <- jsonlite::fromJSON(rawJSON, simplifyDataFrame=FALSE)
  data$recipients <- data$recipients
  data$subject <- jsonlite::unbox(data$subject)
  data$body <- jsonlite::unbox(data$body)
  data$isHTML <- jsonlite::unbox(data$isHTML)
  data$attachments <- unbox.all(data$attachments)

  content <- readLines(filepath)
  content.base64 <- openssl::base64_encode(content)
  attachment <- list(
    type=jsonlite::unbox(mime::guess_type(filepath)),
    name=jsonlite::unbox(basename(filepath)),
    content=jsonlite::unbox(content.base64)
  )
  data$attachments[[length(data$attachments) + 1]] <- attachment
  write(jsonlite::toJSON(data), filename)
}

add_attachment("./test.json")
add_attachment("./test.json")
add_attachment("./test.json")
