
# rec <- c("evelyn_sit@mas.gov.sg","chow_chong_yang@mas.gov.sg", "russell_teng@mas.gov.sg",
#           "lim_jie_rong@mas.gov.sg", "lim_yun_ching@mas.gov.sg", "willy_heng@mas.gov.sg",
#           "ethel_ngiam@mas.gov.sg", "stanley_neo@mas.gov.sg")
# here <- ".\\output\\notebooks\\trading_swot.pdf"

#' Distribute daily trading update
#'
#' @return Side effect only: sends email to distribution list
#' @export
#'
#' @examples \donottest{email_update()}
email_update <- function(){

  #get api key
  mg_api_key <- Sys.getenv("mg_api_key")

  body <- paste(paste0("Generated @ ", Sys.time()), "- hope you make some money today!")
  subject <- paste("Daily Trading Update", Sys.Date())

  cmd <- paste0("curl -s --user 'api:4d5a81d3200b431cd477455e203b219c-", mg_api_key, "' https://api.mailgun.net/v3/eurdiv.ourlittlefam.net/messages")
  cmd <- paste(cmd, "-F from='Eurdiv bot <donotreply@eurdiv.ourlittlefam.net>'")
  cmd <- paste(cmd, paste0("-F subject='", subject, "'"))
  # cmd <- paste(cmd, "-F text='Hope you make some money today!'")
  cmd <- paste(cmd, "-F to='Lim Yun Ching <yunching.lim@gmail.com>'")
  cmd <- paste(cmd, paste0("-F text='", body, "'"))
  cmd <- paste(cmd, "-F attachment=@output/notebooks/trading_swot.pdf")
  system(cmd)
}
