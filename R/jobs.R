#' Trigger a job to run on Bandit.
#'
#' @export
#' @param project Name of the job's project.
#' @param jobname Name of the job you'd like to run.
#' @return Response from the Bandit API.
#' @examples
#' \dontrun{
#' run('My-First-Project', 'theFirstJob')
#' run('bandit-churn', 'score-leads')
#' run('bandit-churn', 'update-database')
#' }
run = function(project, jobname) {
 creds <- get_credentials()
 job_id <- Sys.getenv('BANDIT_JOB_ID')
 url <- paste(creds$url, 'api', 'projects', creds$username, project, 'jobs', jobname, sep='/')
 if (is_local()) {
   print(url)
   return (list(status=jsonlite::unbox("OK"), message=jsonlite::unbox("DRY RUN")))
 }
 httr::GET(
   url,
   httr::authenticate(creds$username, creds$apikey, 'basic')
 )
}
