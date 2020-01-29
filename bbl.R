# attach the necessary packages
library(readxl)
library(stringr)
library(forcats)
library(hms)
library(lubridate)

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
names(bbl_csv)[names(bbl_csv) == "Neutral...AlternativeVenue"] <- "Neutral Venue"
names(bbl_csv)[names(bbl_csv) == "X"] <- "S.no"

# convert the 'Date' variable to date class
bbl_csv$Date <- as.Date(bbl_csv$Date, "%d-%m-%Y")

# convert the 'Time.Local' variable to hms class
bbl_csv$Time.Local. <- as_hms(parse_date_time(bbl_csv$Time.Local., "IMS p"))

# Handle incorrect and missing values
bbl_csv$Home.Team..H. <- str_replace(bbl_csv$Home.Team..H., "Melborne Renegades",
                                     "Melbourne Renegades")
bbl_csv$Home.Team..H. <- as.factor(bbl_csv$Home.Team..H.)
bbl_csv$SuperOver. <- fct_explicit_na(bbl_csv$SuperOver., "N")
bbl_csv$Winner <- str_replace(bbl_csv$Winner, "V", "NR")
bbl_csv$Winner <- as.factor(bbl_csv$Winner)
bbl_csv$DLSMethod. <- fct_explicit_na(bbl_csv$DLSMethod., "N")
bbl_csv$`Neutral Venue`<- fct_explicit_na(bbl_csv$`Neutral Venue`, "N")
bbl_csv$Notes <- as.character(bbl_csv$Notes)
bbl_csv$Source <- as.character(bbl_csv$Source)

# Exclude betting odds values
bbl_csv_final <- bbl_csv[, c(1:19, 30:33)]


