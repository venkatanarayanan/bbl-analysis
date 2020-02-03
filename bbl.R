# attach the necessary packages
library(readxl)
library(stringr)
library(forcats)
library(hms)
library(lubridate)
library(janitor)

# load the excel and convert the first two rows to a single row
# bbl_xlsx <- read_excel("<path to the file>")
# colnames(bbl_xlsx) <- paste0(colnames(bbl_xlsx), bbl_xlsx[1,])
# colnames(bbl_xlsx) <- gsub('^[[:punct:][:digit:]]+|[\r\n]', '', colnames(bbl_xlsx))
# bbl_xlsx <- bbl_xlsx[-1,]

# save it as a csv file
# write.csv(bbl_xlsx, "<path to the file>")

# read the csv file
bbl_csv <- read.csv("<path to the file>")

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

# Exclude betting odds values
bbl_csv_final <- bbl_csv[, c(1:19, 30:33)]


