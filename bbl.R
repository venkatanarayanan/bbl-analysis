# attach the necessary packages
library(readxl)
library(stringr)
library(forcats)
library(hms)
library(lubridate)
library(tidyverse)
library(janitor)
library(magrittr)
library(ggbeeswarm)
library(scales)
library(CGPfunctions)
library(ggalt)

# load the excel and convert the first two rows to a single row
# bbl_xlsx <- read_excel("<path to the file>")
# colnames(bbl_xlsx) <- paste0(colnames(bbl_xlsx), bbl_xlsx[1,])
# colnames(bbl_xlsx) <- gsub('^[[:punct:][:digit:]]+|[\r\n]', '', colnames(bbl_xlsx))
# bbl_xlsx <- bbl_xlsx[-1,]

# save it as a csv file
# write.csv(bbl_xlsx, "<path to the file>")

# read the csv file
bbl_csv <- read.csv("./data/bbl.csv")

# rename the variable names
bbl_csv <- clean_names(bbl_csv)
names(bbl_csv)[names(bbl_csv) == "x"] <- "s_no"

# convert the 'Date' variable to date class
bbl_csv$date <- as.Date(bbl_csv$date, "%d-%m-%Y")

# convert the 'Time.Local' variable to hms class
bbl_csv$time_local <- as_hms(parse_date_time(bbl_csv$time_local, "IMS p"))

# Handle incorrect and missing values
bbl_csv$home_team_h <- str_replace(bbl_csv$home_team_h, "Melborne Renegades",
                                     "Melbourne Renegades")
bbl_csv$home_team_h <- as.factor(bbl_csv$home_team_h)
bbl_csv$super_over <- fct_explicit_na(bbl_csv$super_over, "N")
bbl_csv$winner <- str_replace(bbl_csv$winner, "V", "NR")
bbl_csv$winner <- as.factor(bbl_csv$winner)
bbl_csv$dls_method <- fct_explicit_na(bbl_csv$dls_method, "N")
bbl_csv$neutral_alternative_venue <- fct_explicit_na(bbl_csv$neutral_alternative_venue,
                                                     "N")
bbl_csv$notes <- as.character(bbl_csv$notes)
bbl_csv$source <- as.character(bbl_csv$source)

# Creating new variables
bbl_csv %<>% mutate(won_toss_team = ifelse(won_toss == "H",
                                           as.character(home_team_h),
                                           as.character(away_team_a)),
                    batted_first_team = ifelse(batted_first == "H",
                                               as.character(home_team_h),
                                               as.character(away_team_a)),
                    winner_team = ifelse(winner == "H",
                                         as.character(home_team_h),
                                         ifelse(winner == "A",
                                                as.character(away_team_a),
                                                "NR")))

bbl_csv <- bbl_csv %>% mutate(season = ifelse(date > "2019-12-01",
                                   2020,
                                   ifelse(date > "2018-12-01" & date < "2019-12-01",
                                          2019,
                                          ifelse(date > "2017-12-01" & date < "2018-12-01",
                                                 2018,
                                                 NA))))

bbl_csv$venue <- fct_recode(bbl_csv$venue,
                            "WACA" = "WACA Ground",
                            "Manuka Oval" = "UNSW Canberra Oval",
                            "UoT Stadium" = "University Of Tasmania Stadium",
                            "Coffs Harbour" = "C.ex Coffs International Stadium",
                            "Ted Reserve" = "Ted Summerton Reserve",
                            "Sydney Showground" = "Sydney Showground Stadium")

# check the data types of the newly created variables
bbl_csv$won_toss_team <- as.factor(bbl_csv$won_toss_team)
bbl_csv$batted_first_team <- as.factor(bbl_csv$batted_first_team)
bbl_csv$winner_team <- as.factor(bbl_csv$winner_team)

# different data sets
results <- bbl_csv[, c(1:6, 8, 38:39)]
match_details <- bbl_csv[, c(1:8,12:19,30:33, 36:39)]

# Which venue has hosted the most games
venue_matches <- results %>% group_by(venue) %>% dplyr::summarise(no_of_matches = n())
ggplot(venue_matches,
       aes(x = reorder(venue, no_of_matches),
           y = no_of_matches)) +
  geom_bar(stat = "identity",
           fill = "#148F77") +
  geom_text(aes(label = no_of_matches),
            hjust = -0.5) +
  labs(title = "Number of BBL matches in different venues",
       subtitle = "(BBL01 - BBL09)",
       x = "Venue",
       y = "No. of matches",
       caption = "source: http://www.aussportsbetting.com/") +
  coord_flip() +
  theme_classic()

# create toss dataframe
toss_data <- match_details %>%
  group_by(won_toss_team, batted_first_team) %>%
  summarise(n = n()) %>%
  mutate(percent_value = n / sum(n),
         percentage = scales::percent(percent_value))
names <- paste0(c("", "\n"),
                levels(match_details$won_toss_team))

# Staggered Labels
ggplot(toss_data,
       aes(x = factor(won_toss_team,
                      label = names),
           y = percent_value,
           fill = batted_first_team)) +
  geom_bar(position = "stack",
           stat = "identity") +
  scale_y_continuous(breaks = seq(0,1, 0.2),
                     labels = scales::percent) +
  geom_text(aes(label = percentage),
            size = 2,
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Big bash toss analysis - Staggered axis labels",
       subtitle = "Percentage of games every team has chosen to bat first after winning the toss.Every team except the Melbourne Stars and the\nSydney Sixers have mostly preferred to bat first whenever they have won the toss.",
       caption = "Data: http://www.aussportsbetting.com/data/historical-twenty20-big-bash-results-and-odds-data/",
       x = "Team",
       y = "Percentage",
       fill = "First Batting Team")

# Rotated Labels
ggplot(toss_data,
       aes(x = won_toss_team,
           y = percent_value,
           fill = batted_first_team)) +
  geom_bar(position = "stack",
           stat = "identity") +
  scale_y_continuous(breaks = seq(0,1, 0.2),
                     labels = scales::percent) +
  geom_text(aes(label = percentage),
            size = 2,
            position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Big bash toss analysis - Rotated axis labels",
       subtitle = "Percentage of games every team has chosen to bat first after winning the toss.Every team except the Melbourne Stars and the\nSydney Sixers have mostly preferred to bat first whenever they have won the toss.",
       caption = "Data: http://www.aussportsbetting.com/data/historical-twenty20-big-bash-results-and-odds-data/",
       x = "Team",
       y = "Percentage",
       fill = "First Batting Team")

# Horizontal bars using coord_flip()
ggplot(toss_data,
       aes(x = won_toss_team,
           y = percent_value,
           fill = batted_first_team)) +
  geom_bar(position = "stack",
           stat = "identity") +
  scale_y_continuous(breaks = seq(0,1, 0.2),
                     labels = scales::percent) +
  geom_text(aes(label = percentage),
            size = 2,
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  labs(title = "Big bash toss analysis - Horizontal bars",
       subtitle = "Percentage of games every team has chosen to bat first after winning the toss.Every team except the Melbourne Stars and the\nSydney Sixers have mostly preferred to bat first whenever they have won the toss.",
       caption = "Data: http://www.aussportsbetting.com/data/historical-twenty20-big-bash-results-and-odds-data/",
       x = "Team",
       y = "Percentage",
       fill = "First Batting Team")

ggplot(match_details,
       aes(x = home_team_h,
           y = home_score,
           color = home_team_h)) +
  geom_point(size = 1.5) +
  labs(title = "Big Bash Home Team Scores (BBL01 - BBL09) - Overlapping points",
       subtitle = "geom_point()",
       caption = "Data: http://www.aussportsbetting.com/data/historical-twenty20-big-bash-results-and-odds-data/",
       x = "Team",
       y = "Score") +
theme(legend.position = "none")

ggplot(match_details,
       aes(x = home_team_h,
           y = home_score,
           color = home_team_h)) +
  geom_quasirandom(size = 1.5) +
  labs(title = "Big Bash Home Team Scores (BBL01 - BBL09)",
       subtitle = "geom_quasirandom()",
       caption = "Data: http://www.aussportsbetting.com/data/historical-twenty20-big-bash-results-and-odds-data/",
       x = "Team",
       y = "Score") +
theme(legend.position = "none")

# Create beeswarm plot to visualize the total scores of home teams
ggplot(match_details,
       aes(x = home_team_h,
           y = home_score,
           color = home_team_h)) +
  geom_beeswarm(priority = 'ascending') +
  labs(title = "Big Bash Analysis(BBL01 - BBL09)",
       subtitle = "geom_beeswarm() - analysing the total scores of home teams",
       caption = "Data: http://www.aussportsbetting.com/data/historical-twenty20-big-bash-results-and-odds-data/",
       x = "Team",
       y = "Score") +
  theme(legend.position = "none")

# Create 'winner_data' that shows the number of games won by the teams in 17/18, 18/19, 19/20
winner_data <- match_details %>%
  filter(!is.na(winner_team)) %>%
  group_by(winner_team, season) %>%
  count(winner_team) %>%
  filter(!is.na(season), winner_team != "NR")

# To create a slope graph the time variable must be a factor
winner_data$season <- factor(winner_data$season)
winner_data <- winner_data %>% arrange(n)

# Create a slope graph to show the number of wins
newggslopegraph(winner_data, season, n, winner_team) +
  labs(title = "Big Bash Analysis (BBL 01 - BBL 09)",
       subtitle = "Number of wins in 17/18, 18/19, 19/20",
       caption = "Data: https://www.kaggle.com/thekinginthenorth/bbl-historical-data")

# Create a wide format data of the winner_data to demonstrate dumbbell plot
winner_data_wide <- spread(winner_data, season, n)
names(winner_data_wide) <- c("winner_team", "y2018", "y2019", "y2020")

# Create a dumbbell plot to see the change in number of wins comparing 17/18 & 19/20
ggplot(winner_data_wide,
       aes(y = winner_team,
           x = y2018,
           xend = y2020)) +
  geom_dumbbell(colour_x = "red",
                colour_xend = "blue",
                size_x = 3,
                size_xend = 3) +
  labs(title = "Big Bash Analysis",
       subtitle = "Number of wins in 17/18 & 19/20",
       caption = "Data: https://www.kaggle.com/thekinginthenorth/bbl-historical-data",
       x = 'Number of wins',
       Y = "Team") +
  theme_minimal()

# Create 'loser' variable
bbl_csv <- bbl_csv %>%
  mutate(loser = ifelse(winner == "H", "A",
                        ifelse(winner == "A", "H", "NR")))
