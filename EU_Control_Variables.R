#----------------------------------------------------------
# Extract keywords in context
#---------------------------------
# Setup

library(tidyverse)
library(quanteda)
library(data.table)
library(readxl)

#---------------------------------
# Data
#---------------------------------

setwd("C:/Users/stepa/EU_Thesis_Paper")
data<-fread("paper_data.csv")
inflation <-read_xlsx("czechia_inflation.xlsx")
head(inflation)
ches<-fread("CHES.csv")
data<-eudata
data<-data %>% filter(chair==0)

# --------------------------------

data$month <- as.Date(paste0(data$month, "-01")) # as date

#-----------------------------------------------------------------
# add governments

data$gov <- case_when(
  data$month >=("2022-01-01") & data$month <= ("2023-12-01") ~ "Fiala",
  data$month >=("2018-07-01") & data$month <= ("2021-12-01") ~ "Babiš II",
  data$month >=("2018-01-01") & data$month <= ("2018-06-01") ~ "Babiš I",
  data$month >=("2014-02-01") & data$month <= ("2017-12-01") ~ "Sobotka",
  data$month >=("2013-08-01") & data$month <= ("2014-01-01") ~ "Rusnok",
  data$month >=("2010-07-01") & data$month <= ("2013-07-01") ~ "Nečas",
  data$month >=("2009-05-01") & data$month <= ("2010-06-01") ~ "Fischer",
  data$month >=("2007-01-01") & data$month <= ("2009-04-01") ~ "Topolánek II",
  data$month >=("2006-09-01") & data$month <= ("2006-12-01") ~ "Topolánek I",
  data$month >=("2005-05-01") & data$month <= ("2006-08-01") ~ "Paroubek",
  data$month >=("2004-08-01") & data$month <= ("2005-04-01") ~ "Gross",
  data$month >=("2002-08-01") & data$month <= ("2004-07-01") ~ "Špidla",
  data$month >=("1998-08-01") & data$month <= ("2002-07-01") ~ "Zeman",
  data$month >=("1998-01-01") & data$month <= ("1998-07-01") ~ "Tošovský",
  data$month >=("1996-07-01") & data$month <= ("1997-12-01") ~ "Klaus II",
  data$month >=("1993-01-01") & data$month <= ("1996-06-01") ~ "Klaus I",
  TRUE ~ ""
)


#-----------------------------------------------------------------
#   # add government ideologies

data$gov_ideology <- case_when(
  
  grepl("Klaus I|Klaus II|Topolánek I|Topolánek II|Nečas|Fiala", data$gov) ~ "Right_Wing",
  grepl("Zeman|Špidla|Gross|Paroubek|Sobotka", data$gov) ~ "Left_Wing",
  grepl("Babiš I|Babiš II", data$gov) ~ "New_Parties",
  grepl("Tošovský|Fischer|Rusnok", data$gov) ~ "Technocratic",
  TRUE ~ data$gov
)
unique(data$gov_ideology)

data$government <- case_when(
    (data$gov %in% c("Klaus I", "Klaus II","Topolánek I","Topolánek II","Nečas","Fiala")) & (data$party == "ODS") ~ 1,
    (data$gov %in% c("Zeman", "Špidla","Gross","Paroubek", "Sobotka")) & (data$party == "ČSSD") ~ 1,
    (data$gov %in% c("Klaus I", "Klaus II","Špidla","Gross","Paroubek","Topolánek II","Sobotka","Fiala")) & (data$party == "KDU-ČSL") ~ 1,
    (data$gov %in% c("Sobotka","Babiš I","Babiš II")) & (data$party == "ANO") ~ 1,
    (data$gov %in% c("Nečas","Fiala")) & (data$party == "TOP 09") ~ 1,
    (data$gov %in% c("Fiala")) & (data$party == "STAN") ~ 1,
    (data$gov %in% c("Fiala")) & (data$party == "Piráti") ~ 1,
    TRUE ~ 0 # Default to 0 if none of the above conditions are met
  )




#-----------------------------------------------------------------
#  add election year dummy

data$election_year<-ifelse( 
  data$year == 1996, 1, 
  ifelse(data$year == 1998, 1,
         ifelse(data$year == 2002, 1,
                ifelse(data$year == 2006, 1,
                       ifelse(data$year == 2010, 1,
                              ifelse(data$year == 2013, 1,
                                     ifelse(data$year == 2017, 1,
                                            ifelse(data$year == 2021, 1,0))))))))

data$election_month<-ifelse( 
  data$month == "1996-05-01", 1, 
  ifelse(data$month == "1998-06-01", 1,
         ifelse(data$month ==  "2002-06-01", 1,
                ifelse(data$month == "2006-06-01", 1,
                       ifelse(data$month == "2010-05-01", 1,
                              ifelse(data$month == "2013-10-01", 1,
                                     ifelse(data$month == "2017-10-01", 1,
                                            ifelse(data$month == "2021-10-01", 1,0))))))))
data$election_period <- ifelse(
  data$month >= "1995-12-01" & data$month <= "1996-05-01", 1,
  ifelse(data$month >= "1998-01-01" & data$month <= "1998-06-01", 1,
         ifelse(data$month >= "2002-01-01" & data$month <= "2002-06-01", 1,
                ifelse(data$month >= "2006-01-01" & data$month <= "2006-06-01", 1,
                       ifelse(data$month >= "2010-01-01" & data$month <= "2010-05-01", 1,
                              ifelse(data$month >= "2013-05-01" & data$month <= "2013-10-01", 1,
                                     ifelse(data$month >= "2017-05-01" & data$month <= "2017-10-01", 1,
                                            ifelse(data$month >= "2021-05-01" & data$month <= "2021-10-01", 1, 0))))))))

data$eu_election_period <- ifelse(
  data$month >= "2004-01-01" & data$month <= "2004-06-01", 1,
                ifelse(data$month >= "2009-01-01" & data$month <= "2009-06-01", 1,
                       ifelse(data$month >= "2013-12-01" & data$month <= "2014-05-01", 1,
                              ifelse(data$month >= "2018-12-01" & data$month <= "2019-05-01", 1, 0))))



data$eu_treaty_ratification_period <- ifelse(
  # Maastricht
  data$month >= as.Date("1992-02-01") & data$month <= as.Date("1993-11-01"), 1
  #  Amsterdam
  ifelse(data$month >= as.Date("1997-10-01") & data$month <= as.Date("1999-05-01"), 1,
      # Nice
         ifelse(data$month >= as.Date("2001-02-01") & data$month <= as.Date("2003-02-01"), 1,
              # Lisbon
                ifelse(data$month >= as.Date("2007-12-01") & data$month <= as.Date("2009-12-01"), 1, 0))))




#-----------------------------------------------------------------
# Center the year, Andrew Gelman would be angry otherwise

data$year_centered <- data$year - median(data$year) # centering the year


#-----------------------------------------------------------------
# add yearly inflation

inflation<-c(20.8,	10.0,	9.1,	8.8,	8.5,	10.7,	2.1,
             3.9,	4.7,	1.8,	0.1,	2.8,	1.9,	2.5,
             2.8,	6.3,	1.0,	1.5,	1.9,	3.3, 	1.4, 	0.4, 
             0.3,	0.7,	2.5,	2.1,	2.8,	3.2,	3.8,	15.1,	10.7) # čsu data, jsem linej to dělat jinak

year<-c(1993,	1994,	1995,	1996,	1997,	1998,	1999,	2000,	2001,	2002,	2003,	2004,
        2005,	2006,	2007,	2008,	2009,	2010,	2011,	2012,	2013,	2014,
        2015,	2016,	2017,	2018,	2019,	2020,	2021,	2022,	2023)

yearly_inflation <- data.frame(year, inflation)

data <- # bind to data
  data %>% 
  left_join(yearly_inflation, by = "year")


#-----------------------------------------------------------------
# throw unnasigned MPs into Other

data$party <- case_when(
  grepl("TOP 09 a Starostové", data$party, fixed = TRUE) ~ "TOP 09",
  grepl("Nezařazení|\\(NEZ\\)|ODS \\(NEZ\\)|ČSSD \\(NEZ\\)|SPD \\(NEZ\\)|ÚSVIT \\(NEZ\\)", data$party) ~ "Other",
  grepl("Nez.-SZ", data$party) ~ "Other",
  grepl("TOP 09|TOP09", data$party) ~ "TOP 09",
  TRUE ~ data$party
)

# coalition TOP09 and STAN as only TOP09
data <- data %>% mutate(party = ifelse(party == "STAN" & year < 2016, "TOP 09", party))

data <- data %>%
  mutate(party = case_when(
    grepl("Starostové", party) ~ "TOP 09",
    TRUE ~ party
  ))


# find unique parties
unique(data$party)
# too much, will drop before graphs and stats


#------------------------------------
# designate wing as an analytical category following Jabůrek et al. (2024)

data$wing <- case_when(
  grepl("ČSSD|KSČM|LB", data$party) ~ "Left_Wing",
  grepl("ODS|KDU-ČSL|US|US-DEU|SPR-RSČ|ODA", data$party) ~ "Right_Wing",
  grepl("ANO|Piráti|STAN|SPD|Úsvit|VV|SZ|TOP 09", data$party) ~ "New_Parties",
  TRUE ~ "Other"
)


#----------------------------------
# 
data <- data %>% select(-1:-2)
data$word_count <- str_count(data$text, "\\S+")
sum(data$word_count)
write.csv(data, "eu_data.csv")
fches
czechia <- ches %>% 
  filter(country==21) %>% 
  dplyr::select(year, expert, party, family, eu_position, eu_salience, lrgen, galtan) %>% 
  filter(party %in% c("ANO2011", "CSSD", "KSCM", "KDU-CSL", "ODS", "Pirates", "TOP09", "STAN", "SPD"))

czechia$party <- case_when(
  grepl("ANO2011", czechia$party) ~ "ANO",
  grepl("CSSD", czechia$party) ~ "ČSSD",
  grepl("KSCM", czechia$party) ~ "KSČM",
  grepl("KDU-CSL", czechia$party) ~ "KDU-ČSL",
  grepl("Pirates", czechia$party) ~ "Piráti",
  grepl("TOP09", czechia$party) ~ "TOP 09",
  TRUE ~ czechia$party
)

head(czechia)


data<-data %>% 
  mutate(galtan = case_when(
  data$party == "ODS" & data$year <2006 ~ 3.890000, 
  data$party == "ODS" & data$year >=2006 & data$year <2010 ~ 3.750000, 
  data$party == "ODS" & data$year >=2010 & data$year <2014 ~ 6.111111, 
  data$party == "ODS" & data$year >=2014 & data$year <2019 ~ 6.000000, 
  data$party == "ODS" & data$year >=2019 ~ 7.037037, 
  
  data$party == "ČSSD" & data$year <2006 ~ 4.690000, 
  data$party == "ČSSD" & data$year >=2006 & data$year <2010 ~ 4.860000, 
  data$party == "ČSSD" & data$year >=2010 & data$year <2014 ~ 3.578948, 
  data$party == "ČSSD" & data$year >=2014 & data$year <2019 ~ 4.428571, 
  data$party == "ČSSD" & data$year >=2019 ~ 4.923077, 
  
  data$party == "KDU-ČSL" & data$year <2006 ~ 7.560000, 
  data$party == "KDU-ČSL" & data$year >=2006 & data$year <2010 ~ 7.170000, 
  data$party == "KDU-ČSL" & data$year >=2010 & data$year <2014 ~ 8.000000, 
  data$party == "KDU-ČSL" & data$year >=2014 & data$year <2019 ~ 7.642857, 
  data$party == "KDU-ČSL" & data$year >=2019 ~ 7.777778, 
  
  data$party == "KSČM" & data$year <2006 ~ 7.820000, 
  data$party == "KSČM" & data$year >=2006 & data$year <2010 ~ 7.670000, 
  data$party == "KSČM" & data$year >=2010 & data$year <2014 ~ 5.666667, 
  data$party == "KSČM" & data$year >=2014 & data$year <2019 ~ 6.571429, 
  data$party == "KSČM" & data$year >=2019 ~ 8.074074,
  

  data$party == "ANO" &  data$year <2019 ~ 4.454545, 
  data$party == "ANO" & data$year >=2019 ~ 5.730769,
  
  data$party == "TOP 09" & data$year <2014 ~ 5.833333, 
  data$party == "TOP 09" & data$year >=2014 & data$year <2019 ~ 5.500000, 
  data$party == "TOP 09" & data$year >=2019 ~ 4.851852,
  
  data$party == "Piráti" ~ 1.000000,
  
  data$party == "SPD"~ 9.370370,
  
  data$party == "STAN" ~ 4.076923,
  TRUE ~ NA_real_  # fallback value for all other cases
  ))




#-------------------------
# 1. (Reminder) Prepare your data$month column if it's not already in a date format.
#    Assuming data$month is a character string like "YYYY-MM", convert it to date:
   data$month <- as.Date(paste0(data$month, "-01"))
#
#    If data$month is already a full date like "1990-01-15", that's fine.
#    The key is that it's a 'Date' object for comparison.

# 2. List all EU Summit Months (1990s, 2000s, 2010s, and 2020s combined)
#    We'll represent these as the first day of the month for easy comparison.
eu_summit_dates_all <- c(
  # 1990s
  "1993-06-01", "1993-10-01", "1993-12-01",
  "1994-06-01", "1994-07-01", "1994-12-01",
  "1995-06-01", "1995-10-01", "1995-12-01",
  "1996-03-01", "1996-06-01", "1996-10-01", "1996-12-01",
  "1997-05-01", "1997-06-01", "1997-11-01", "1997-12-01",
  "1998-05-01", "1998-06-01", "1998-10-01", "1998-12-01",
  "1999-02-01", "1999-03-01", "1999-04-01", "1999-06-01",
  "1999-10-01", "1999-12-01",
  
  # 2000s
  "2000-03-01", "2000-06-01", "2000-10-01", "2000-12-01",
  "2001-03-01", "2001-06-01", "2001-09-01", "2001-10-01", "2001-12-01",
  "2002-03-01", "2002-06-01", "2002-10-01", "2002-12-01",
  "2003-02-01", "2003-03-01", "2003-04-01", "2003-06-01",
  "2003-10-01", # Covers both 4 Oct and 16-17 Oct
  "2003-12-01",
  "2004-03-01", "2004-06-01", "2004-11-01", "2004-12-01",
  "2005-03-01", "2005-06-01", "2005-10-01", "2005-12-01",
  "2006-03-01", "2006-06-01", "2006-10-01", "2006-12-01",
  "2007-03-01", "2007-06-01", "2007-10-01", "2007-12-01",
  "2008-03-01", "2008-06-01", "2008-07-01", "2008-09-01",
  "2008-10-01", # Covers both 12 Oct and 15-16 Oct
  "2008-11-01",
  "2008-12-01",
  "2009-03-01", # Covers both 1 Mar and 19-20 Mar
  "2009-04-01",
  "2009-06-01",
  "2009-09-01",
  "2009-10-01",
  "2009-11-01",
  "2009-12-01",
  
  # 2010s
  "2010-02-01", # 11 February
  "2010-03-01", # 25 March, 25–26 March
  "2010-05-01", # 7 May
  "2010-06-01", # 17 June
  "2010-09-01", # 16 September
  "2010-10-01", # 28–29 October
  "2010-12-01", # 16–17 December
  "2011-02-01", # 4 February
  "2011-03-01", # 11 March (twice), 24–25 March
  "2011-06-01", # 23–24 June
  "2011-07-01", # 21 July
  "2011-10-01", # 23 October, 23–26 October, 26 October
  "2011-12-01", # 9 December (twice)
  "2012-01-01", # 30 January (twice)
  "2012-03-01", # 1–2 March, 2 March
  "2012-05-01", # 23 May
  "2012-06-01", # 28–29 June (twice)
  "2012-10-01", # 18–19 October
  "2012-11-01", # 22–23 November
  "2012-12-01", # 13–14 December
  "2013-02-01", # 7–8 February
  "2013-03-01", # 14 March, 14–15 March
  "2013-05-01", # 22 May
  "2013-06-01", # 27–28 June
  "2013-10-01", # 24–25 October
  "2013-12-01", # 19–20 December
  "2014-03-01", # 6 March, 20–21 March
  "2014-05-01", # 27 May
  "2014-06-01", # 26–27 June
  "2014-07-01", # 16 July
  "2014-08-01", # 30 August
  "2014-10-01", # 23-24 October, 24 October
  "2014-12-01", # 18 December
  "2015-02-01", # 12 February
  "2015-03-01", # 19-20 March
  "2015-04-01", # 23 April
  "2015-06-01", # 22 June, 25-26 June
  "2015-07-01", # 7 July, 12 July
  "2015-09-01", # 23 September
  "2015-10-01", # 15 October
  "2015-11-01", # 12 November
  "2015-12-01", # 17-18 December
  "2016-02-01", # 18-19 February
  "2016-03-01", # 17-18 March
  "2016-06-01", # 28 June, 29 June
  "2016-09-01", # 16 September
  "2016-10-01", # 20-21 October
  "2016-12-01", # 15 December
  "2017-02-01", # 3 February (a.m. and p.m.)
  "2017-03-01", # 9 March, 10 March
  "2017-04-01", # 29 April
  "2017-06-01", # 22-23 June, 22 June evening
  "2017-10-01", # 19-20 October, 20 October
  "2017-11-01", # 17 November
  "2017-12-01", # 14-15 December, 15 December (twice)
  "2018-03-01", # 22-23 March, 23 March (twice)
  "2018-06-01", # 28-29 June, 29 June (twice)
  "2018-09-01", # 19-20 September, 20 September
  "2018-10-01", # 17 October, 18 October (a.m. and p.m.)
  "2018-12-01", # 13-14 December, 13 December, 14 December
  "2019-03-01", # 21 March, 22 March
  "2019-04-01", # 10 April
  "2019-05-01", # 9 May, 28 May
  "2019-06-01", # 20 June, 21 June (twice), 30 June (for 30 June-2 July)
  "2019-10-01", # 17 October (twice), 17-18 October
  "2019-12-01", # 12-13 December, 13 December (twice)
  
  # 2020s
  "2020-02-01", # 20–21 February
  "2020-03-01", # 10 March, 17 March, 26 March
  "2020-04-01", # 23 April
  "2020-06-01", # 19 June
  "2020-07-01", # 17–21 July
  "2020-08-01", # 19 August
  "2020-10-01", # 1–2 October, 15–16 October, 29 October
  "2020-11-01", # 19 November
  "2020-12-01", # 10–11 December, 11 December
  "2021-01-01", # 21 January
  "2021-02-01", # 25–26 February
  "2021-03-01", # 25 March (twice), 25–26 March
  "2021-05-01", # 7 May, 7–8 May, 24–25 May
  "2021-06-01", # 24–25 June, 25 June
  "2021-10-01", # 5 October, 20 October, 21–22 October
  "2021-12-01", # 16 December (twice)
  "2022-02-01", # 17 February, 24 February
  "2022-03-01", # 10–11 March, 23 March, 24–25 March
  "2022-05-01", # 30–31 May
  "2022-06-01", # 23–24 June, 24 June
  "2022-10-01", # 7 October, 19 October, 20–21 October
  "2022-12-01", # 15 December
  "2023-02-01", # 9 February
  "2023-03-01", # 22 March, 23 March, 24 March
  "2023-06-01", # 29–30 June
  "2023-10-01", # 6 October, 17 October, 25 October, 26–27 October, 27 October
  "2023-12-01" # 14–15 December
)

# Convert the list of summit dates to unique Date objects
# This ensures that months with multiple summits (like March 2020 or October 2023)
# are only listed once, which is correct for a 'month with a summit' binary.
summit_dates_unique <- unique(as.Date(eu_summit_dates_all))

# Create the binary variable
# Initialize to 0
data$eu_summit <- 0

# Set to 1 for months that match a summit month
# We use format(..., "%Y-%m-01") to ensure we're comparing month-year only
data$eu_summit[format(data$month, "%Y-%m-01") %in% format(summit_dates_unique, "%Y-%m-01")] <- 1

library(readxl)
inflation <-read_xlsx("czechia_inflation.xlsx")

data <-data %>% dplyr::select(-"inflation")
inflation$Month <- gsub('([0-9]{4})([A-Za-z]{3})', '\\1-\\2-01', inflation$Month)
inflation$Month <- gsub('Jan', '01', inflation$Month)
inflation$Month <- gsub('Feb', '02', inflation$Month)
inflation$Month <- gsub('Mar', '03', inflation$Month)
inflation$Month <- gsub('Apr', '04', inflation$Month)
inflation$Month <- gsub('May', '05', inflation$Month)
inflation$Month <- gsub('Jun', '06', inflation$Month)
inflation$Month <- gsub('Jul', '07', inflation$Month)
inflation$Month <- gsub('Aug', '08', inflation$Month)
inflation$Month <- gsub('Sep', '09', inflation$Month)
inflation$Month <- gsub('Oct', '10', inflation$Month)
inflation$Month <- gsub('Nov', '11', inflation$Month)
inflation$Month <- gsub('Dec', '12', inflation$Month)

inflation$Month  <- as.Date(paste0(inflation$Month , "-01"))
data$month  <- as.Date(paste0(data$month , "-01"))


# Ensure data$month is also a Date object and consistent (already done in previous steps if user followed suggestions)
# If data$month is not already a Date object, convert it:
# data$month <- as.Date(paste0(data$month, '-01')) # Assuming data$month is 'YYYY-MM' or similar

# Perform a left join to bind the inflation data to the `data` DataFrame by month
data <- left_join(data, inflation, by = c('month' = 'Month'))

print(head(data))
print(info(data))
# You can check the result (e.g., for the 2020s):
# table(data$eu_summit_month[format(data$month, "%Y") %in% as.character(2020:2026)])
# head(data[data$eu_summit_month == 1 & format(data$month, "%Y") %in% as.character(2020:2026), c("month", "eu_summit_month")], 20) # Display first 20 for verification