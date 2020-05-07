#Confirmed data set-----




# confirmed <- read_csv("data/baden-wurttemberg-confirmed.csv", col_types = cols(.default = "c")) %>% clean_data()
# #Deaths data set-----
# deaths<- read_csv("data/baden-wurttemberg-deaths.csv", col_types = cols(.default = "c")) %>% clean_data()

#smc_confirmed <- read_csv("data/smc-confirmed.csv")


#Link to column explanations: https://www.arcgis.com/home/item.html?id=f10774f1c63e40168479a1feb6c7ca74 
de_url <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv"
de_dest_file <- paste0("data/germany-corona-data-", today(), ".csv")
download_data(url = de_url, dest_file = de_dest_file)

de_data <- read_csv(file = de_dest_file,
                    col_types = "dccccdddcdcddcdd")
de_data$Meldedatum <- de_data$Meldedatum %>% str_sub(end = 10) %>% ymd() #Notification date: Date when the case became known to the health department
de_data$Datenstand <- de_data$Datenstand %>% str_sub(end = 10) %>% dmy() #Data status: Date when the data record was last updated
de_data$Refdatum <- de_data$Refdatum %>% str_sub(end = 10) %>% ymd() #Reference Date
de_data$Altersgruppe <- de_data$Altersgruppe %>% as.factor()
de_data$Geschlecht <- de_data$Geschlecht %>% as.factor()

bw_recovered <- de_data %>%
  filter(IdBundesland == 8) %>% #8 is Baden-Württemberg
  select(Meldedatum, AnzahlFall, AnzahlGenesen) %>% 
  group_by(Meldedatum) %>%
  summarise_at(c("AnzahlFall", "AnzahlGenesen"), sum, na.rm = TRUE)
names(bw_recovered) <- c("date", "confirmed", "recovered")

bw_recovered$cum_confirmed <- bw_recovered$confirmed %>% cumsum()
bw_recovered$cum_recovered <- bw_recovered$recovered %>% cumsum()
n <- nrow(bw_recovered)
alpha <- numeric(n)
beta <- numeric(n)
alpha[1] <- 2
beta[1] <- 1

for(i in 2:n) {
  tmp_alpha <- alpha[i - 1]
  tmp_beta <- beta[i - 1]
  N <- bw_recovered$confirmed[i]
  z <- bw_recovered$recovered[i]
  alpha[i] <- tmp_alpha + z
  beta[i] <- tmp_beta + N -z 
}

bw_recovered$alpha <- alpha
bw_recovered$beta <- beta
bw_recovered <- bw_recovered %>%
  mutate(est_avg = alpha / (alpha + beta),
         act_avg = cum_recovered / cum_confirmed)
  
  
  
#Baden-Wurttemberg data set-----
bw_url <- "https://sozialministerium.baden-wuerttemberg.de/fileadmin/redaktion/m-sm/intern/downloads/Downloads_Gesundheitsschutz/Tabelle_Coronavirus-Faelle-BW.xlsx"
dest_file <- paste0("data/baden-wurttemberg-corona-data-", today(), ".xlsx")
download_data(url = bw_url, dest_file = dest_file)
baden_wurttemberg_confirmed <- read_excel(path = dest_file,
                                          col_types = "text",
                                          sheet = 1,
                                          skip = 6,
                                          n_max = 44)
colnames(baden_wurttemberg_confirmed)[1] <- "city"

baden_wurttemberg_confirmed <- baden_wurttemberg_confirmed %>%
  gather(key = "date", value = "count", -city) %>%
  mutate(count = as.integer(count)) %>%
  #the origin in as.Date is needed to convert excel numeric dates to character dates
  mutate(date = as.Date(as.integer(date), origin = "1899-12-30")) %>% 
  select(date, count) %>%
  group_by(date) %>%
  summarise(count = sum(count, na.rm = TRUE))


new_cases <- baden_wurttemberg_confirmed$count - lag(baden_wurttemberg_confirmed$count)
new_cases[1] <- 1
baden_wurttemberg_confirmed$new_cases <- new_cases

bw_nrow <- nrow(baden_wurttemberg_confirmed)
new_case_rate <- numeric(bw_nrow)
new_case_rate[2:bw_nrow] <- new_cases[2:bw_nrow] / new_cases[1:(bw_nrow - 1)]
#baden_wurttemberg_confirmed$new_case_rate <- new_case_rate

dates <- seq(from = ymd("2020-01-01"), to = max(baden_wurttemberg_confirmed$date), by = "day")
week_dates <- week(dates)
bw_weeks <- week_dates[which(dates %in% baden_wurttemberg_confirmed$date)]

baden_wurttemberg_aggregated <- baden_wurttemberg_confirmed %>%
  group_by(week = isoweek(date)) %>%
  summarise(count_week = sum(count),
            count_new = sum(new_cases))
# start_week <- seq(from = 1, to = min(baden_wurttemberg_aggregated$week) - 1)
# start_count<- numeric(length(start_week))
# start_cases <- start_count
# start_df <- data.frame(week = start_week, count_week = start_count, count_new = start_cases)
# baden_wurttemberg_aggregated <- rbind(start_df, baden_wurttemberg_aggregated)   



# baden_wurttemberg_deaths <- deaths %>%
#   select(date, count) %>%
#   group_by(date) %>%
#   summarise(count = sum(count))
# 
# n <- nrow(baden_wurttemberg_deaths)
# time <- 0:(n-1)
# baden_wurttemberg_deaths$time <- time


# #SMC data set-----
smc_date <- today()
smc_file_name <- paste0("data/J7 Data/smc-confirmed-", smc_date,".csv")
smc <- read_csv(smc_file_name, col_types = "dDDDc") %>%
  count(test_date)

names(smc) <- c("date", "new_cases")

smc <- smc %>%
  mutate(count = cumsum(new_cases))

smc_aggregated <- smc %>%
  group_by(week = isoweek(date)) %>%
  summarise(count_week = sum(count),
            count_new = sum(new_cases))

#Combined Data set
bw_combined <- baden_wurttemberg_confirmed %>%
  select(date, count) %>%
  mutate(state = "BW")

smc_combined <- smc %>%
  select(date, count) %>%
  mutate(state = "SMC")

bw_smc_combined <- bind_rows(bw_combined, smc_combined)
write_csv(bw_smc_combined, path = "data/bw-smc-combined.csv")
    