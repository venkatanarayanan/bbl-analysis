library(rvest)
library(stringr)
library(tidyverse)

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
bbl_commentary <- read.csv("C:/Users/Hunt and Badge/Downloads/bbl_comm.csv")
bbl_commentary <- bbl_commentary %>% filter(!grepl("comes", commentary))
bbl_commentary <- separate(bbl_commentary, commentary, c("ball_details", "action"), sep="!")
bbl_commentary <- bbl_commentary %>% filter(!grepl("PlayersPlayer", ball_details))

bbl_commentary <- bbl_commentary %>% filter(!grepl("Player Matches Count", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!grepl("Schedule", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!grepl("BowlingRecords", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!grepl("Points Table", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!grepl("Grounds", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!grepl("Full Scorecard", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!grepl("toss", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!grepl("Players", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!grepl("Brisbane Heat", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!grepl("BBL08", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!grepl("Melbourne Stars", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!grepl("Sydney Thunder", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!grepl("Sydney Sixers", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!grepl("Melbourne Renegades", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!grepl("Hobart Hurricanes", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!grepl("Perth Scorchers", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!grepl("Adelaide Strikers", ball_details))
bbl_commentary <- bbl_commentary %>% filter(!(is.na(ball_details)))
bbl_commentary$action <- str_trim(bbl_commentary$action)
bbl_commentary$ball_details <- str_replace(bbl_commentary$ball_details, "FOUR", "")
bbl_commentary$ball_details <- str_replace(bbl_commentary$ball_details, "SIX", "")
bbl_commentary$ball_details <- str_trim(bbl_commentary$ball_details)
bbl_commentary <- bbl_commentary[,-1]
bbl_commentary <- bbl_commentary %>% separate(ball_details, c("bowler_name", "details"), 
                            sep = " to ", extra = "merge")
bbl_commentary$details <- str_replace(bbl_commentary$details, "OUT", "")
bbl_commentary <- separate(bbl_commentary, details, c("batsman_name", "runs"), sep = ",")
bbl_commentary$runs <- str_trim(bbl_commentary$runs)

bbl_commentary <- bbl_commentary %>% 
  mutate(action = ifelse(grepl("LEG BYE", runs) & is.na(action),
                         "LEG BYE",
                         as.character(action)))

bbl_commentary <- bbl_commentary %>%
  mutate(description = ifelse(grepl("RUN OUT", action) & grepl("LEG BYE", runs),
                              action,
                              NA))

bbl_commentary <- bbl_commentary %>%
  mutate(action = ifelse(grepl("RUN OUT", action) & grepl("LEG BYE", runs),
         "LEG BYE OUT",
         as.character(action)))

bbl_commentary$runs <- str_replace(bbl_commentary$runs, "LEG BYE", "")
bbl_commentary$runs <- str_trim(bbl_commentary$runs)

bbl_commentary <- bbl_commentary %>% 
  mutate(action = ifelse(grepl("NO BALL", runs),
                         "NO BALL",
                         as.character(action)))

bbl_commentary$runs <- str_replace(bbl_commentary$runs, "NO BALL", "")
bbl_commentary$runs <- str_trim(bbl_commentary$runs)

bbl_commentary <- bbl_commentary %>% 
  mutate(action = ifelse(grepl("BYE", runs),
                         "BYE",
                         as.character(action)))

bbl_commentary$runs <- str_replace(bbl_commentary$runs, "BYE", "")
bbl_commentary$runs <- str_trim(bbl_commentary$runs)

bbl_commentary <- bbl_commentary %>% 
  mutate(description = ifelse(grepl("WIDE", batsman_name) & !is.na(action),
                              action,
                              as.character(description)))

bbl_commentary <- bbl_commentary %>% 
  mutate(action = ifelse(grepl("WIDE", batsman_name) & !is.na(action),
                         "WIDE OUT",
                         as.character(action)))

bbl_commentary <- bbl_commentary %>% 
  mutate(batsman_name = ifelse(grepl("WIDE", batsman_name) & !is.na(action),str_replace(batsman_name, "WIDE", ""),as.character(batsman_name)))

bbl_commentary <- bbl_commentary %>% 
  mutate(action = ifelse(grepl("[[:digit:]] WIDE", batsman_name),
                         "WIDE",
                         as.character(action)))

bbl_commentary <- bbl_commentary %>%
  mutate(runs = ifelse(grepl("[[:digit:]] WIDES", batsman_name),
                       str_extract(batsman_name, "[[:digit:]]"),
                       as.character(runs)))

bbl_commentary$batsman_name <- str_replace(bbl_commentary$batsman_name, 
                                           "[[:digit:]] WIDES", "")

bbl_commentary <- bbl_commentary %>% 
  mutate(action = ifelse(grepl("WIDE", batsman_name), "WIDE",as.character(action)))

bbl_commentary$batsman_name <- str_replace(bbl_commentary$batsman_name, "WIDE", "")
bbl_commentary$batsman_name <- str_trim(bbl_commentary$batsman_name)
bbl_commentary$runs <- str_trim(bbl_commentary$runs)

bbl_commentary <- bbl_commentary %>% 
  mutate(description = ifelse(grepl("CATCH", action),action,as.character(description)))

bbl_commentary <- bbl_commentary %>% 
  mutate(action = ifelse(grepl("CATCH", action),"OUT",as.character(action)))

bbl_commentary <- bbl_commentary %>% 
  mutate(description = ifelse(grepl("BOWLED", action),action,as.character(description)))
#
bbl_commentary <- bbl_commentary %>% 
  mutate(action = ifelse(grepl("BOWLED", action),"OUT",as.character(action)))

bbl_commentary <- bbl_commentary %>% 
  mutate(description = ifelse(grepl("LBW", action),action,as.character(description)))

bbl_commentary <- bbl_commentary %>% 
  mutate(action = ifelse(grepl("LBW", action), "OUT",as.character(action)))

bbl_commentary <- bbl_commentary %>% 
  mutate(description = ifelse(grepl("RUN OUT", action),action,as.character(description)))

bbl_commentary <- bbl_commentary %>% 
  mutate(action = ifelse(grepl("RUN OUT", action),"OUT",as.character(action)))

bbl_commentary <- bbl_commentary %>% 
  mutate(description = ifelse(grepl("STUMPED", action),action,as.character(description)))

bbl_commentary <- bbl_commentary %>% 
  mutate(action = ifelse(grepl("STUMPED", action), "OUT",as.character(action)))

bbl_commentary <- bbl_commentary %>% 
  mutate(description = ifelse(grepl("CTW", action),action,as.character(description)))

bbl_commentary <- bbl_commentary %>% 
  mutate(action = ifelse(grepl("CTW", action),"OUT",as.character(action)))
