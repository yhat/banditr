#' Create an email to send after your job has finished. Bandit will
#' automatically send this email for you.
#'
#' @export
#' @param recipients Character vector of email addresses.
#' @param subject Subject line of the email.
#' @param body Body of the email. Can be raw text or HTML. This needs to be an
#' atomic character vector (length 1).
#' @examples
#' \dontrun{
#' email("hi@yhat.com", "This is my Subject", "And you're seeing this body!")
#' email(c("paul@yhathq.com", "hi@yhat.com"), "Two People", "Will get this!")
#' email(c("smug@smugdouglas.com"), "HTML", "<p>Wow! HTML is <b>fun!</b></p>")
#' }
email = function(recipients, subject, body) {
  filename <- '/job/metadata/email.json'
  filename <- '/tmp/email.json'
  data <- list()
  data$recipients <- recipients
  data$subject <- jsonlite::unbox(subject)
  data$body <- jsonlite::unbox(body)
  data$attachments <- list()
  data$isHTML <- jsonlite::unbox(TRUE)

  if (is_local()) {
    print(jsonlite::toJSON(data, auto_unbox=TRUE))
    return
  }
  write(jsonlite::toJSON(data), filename)
}

#' Add an attachment to your email. This can be any filetype.
#'
#' @export
#' @param filepath Path to the file you'd like to attach. *Note* that this will
#' be the filepath *in your bandit job!*.
#' @examples
#' \dontrun{
#' add_attachment("my-plot.png")
#' add_attachment("train.csv")
#' add_attachment("scored-data.csv")
#' }
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
  data$attachments <- unbox_all(data$attachments)

  content <- readLines(filepath)
  content.base64 <- openssl::base64_encode(content)
  attachment <- list(
    type=jsonlite::unbox(mime::guess_type(filepath)),
    name=jsonlite::unbox(basename(filepath)),
    content=jsonlite::unbox(content.base64)
  )
  data$attachments[[length(data$attachments) + 1]] <- attachment
  if (is_local()) {
    print(jsonlite::toJSON(data, auto_unbox=TRUE))
    return
  }
  write(jsonlite::toJSON(data), filename)
}
