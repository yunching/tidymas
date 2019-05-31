library(mailR)

#edit list or recipients here
rec <- c("evelyn_sit@mas.gov.sg","chow_chong_yang@mas.gov.sg", "russell_teng@mas.gov.sg",
         "lim_jie_rong@mas.gov.sg", "lim_yun_ching@mas.gov.sg", "willy_heng@mas.gov.sg",
         "ethel_ngiam@mas.gov.sg", "stanley_neo@mas.gov.sg")

#edit the location depending on where your tidymas drive is
here <- "C:\\Users\\chongyang\\Downloads\\tidymas-master\\tidymas-master\\output\\notebooks\\trading_swot.pdf"

send.mail(
  from="throwawayhor@gmail.com", to = rec,
  subject = paste("Daily Trading update", Sys.Date(),""),
  body = paste(paste("Date Generated: ", Sys.time()), "Hope you make some money today!",sep="\n"),
  smtp = list(host.name = "smtp.gmail.com",
              port = 465,
              user.name = "throwawayhor@gmail.com",
              passwd = "3W5d4q6I",
              ssl = T),
  authenticate = T,  send = T,
  attach.files=here)
