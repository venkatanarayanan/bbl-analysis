# library(rvest)
# library(stringr)
# library(tidyverse)
# 
# com_urls <- sprintf('https://cricclubs.com/KFCT20BBL/ballbyball.do?matchId=%d&clubId=4064', 317:377)
# 
# get_commentary <- function(html_com_link){
#    read_html(html_com_link) %>%
#     html_nodes(xpath = '//*/li[2]') %>%
#     html_text() %>%
#     as.data.frame()
# }
# 
# bbl_commentary <- com_urls %>% map(get_commentary)
# bbl_commentary <- do.call("rbind", bbl_commentary)
# 
# names(bbl_commentary)[names(bbl_commentary) == "."] <- "commentary"
# bbl_commentary$commentary <- gsub("[\r\n|\t]", "", bbl_commentary$commentary)
# bbl_commentary$commentary <- str_trim(bbl_commentary$commentary)
# bbl_commentary <- as_tibble(bbl_commentary)
# bbl_commentary$commentary[bbl_commentary$commentary == ""] <- NA
# 
# write.csv(bbl_commentary, "C:/Users/Hunt and Badge/Downloads/bbl_comm.csv")
# bbl_commentary <- bbl_commentary %>% filter(!grepl("comes", commentary))
# bbl_commentary <- separate(bbl_commentary, commentary, c("ball_details", "action"), sep=",")
# bbl_commentary <- bbl_commentary %>% filter(!grepl("PlayersPlayer", ball_details))
# 
# bbl_commentary <- bbl_commentary %>% filter(!grepl("Player Matches Count", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!grepl("Schedule", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!grepl("BowlingRecords", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!grepl("Points Table", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!grepl("Grounds", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!grepl("Full Scorecard", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!grepl("toss", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!grepl("Players", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!grepl("Brisbane Heat", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!grepl("BBL08", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!grepl("Melbourne Stars", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!grepl("Sydney Thunder", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!grepl("Sydney Sixers", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!grepl("Melbourne Renegades", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!grepl("Hobart Hurricanes", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!grepl("Perth Scorchers", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!grepl("Adelaide Strikers", ball_details))
# bbl_commentary <- bbl_commentary %>% filter(!(is.na(ball_details)))
# bbl_commentary$action <- str_trim(bbl_commentary$action)