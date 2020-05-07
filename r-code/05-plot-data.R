file_date <- today()
todays_date <- today()

#Plot the Baden-Wurttemberg predictions-----
ggplot(data = bw_confirmed_predictions, aes(x = date, y=count)) + 
  geom_line(aes(x=date, y=count), color = "blue", 
            linetype = "dashed", size=1) +
  scale_x_datetime(labels = label_date(), breaks = breaks_width("30 days")) +
  scale_y_continuous(labels = label_comma()) +
  geom_point(data = baden_wurttemberg_confirmed, aes(x = date, y = count) ,size = 1.5) +
  theme(legend.position = "none") + 
  ylab("Confirmed Cases") +
  xlab("Date") +
  theme_bw() +
  ggtitle(paste0("COVID-19 Confirmed Case Estimate for Baden-Wurttemberg (", todays_date, ")"))
ggsave(paste0("img/01-baden-wurttemberg-confirmed-cases-",file_date,".png"))


#Plot the SMC predictions-----
file_date <- today()
ggplot(data = smc_confirmed_predictions, aes(x = date, y=count)) + 
  geom_line(aes(x=date, y=count), color = "blue", 
            linetype = "dashed", size=1) +
  scale_x_datetime(labels = label_date(), breaks = breaks_width("30 days")) +
  scale_y_continuous(labels = label_comma()) +
  geom_point(data = smc_confirmed, aes(x = date, y = count) ,size = 1.5) +
  theme(legend.position = "none") + 
  ylab("Confirmed Cases") +
  xlab("Date") +
  theme_bw() +
  ggtitle(paste0("COVID-19 Confirmed Case Estimate for Stuttgart Military Community (", todays_date, ")"))
ggsave(paste0("img/02-smc-confirmed-cases-",file_date,".png"))


#Plot Baden-Wurttemberg weekly new cases vs confirmed cases-----
exp(seq(log(1), log(100000), length.out = 6))


baden_wurttemberg_aggregated %>%
  ggplot(aes(x = count_week, y = count_new)) +
  geom_path(size = 0.8) +
  geom_point(size = 2) +
  xlab("Weekly Confirmed Cases (Log Scale)") +
  ylab("Weekly New Cases (Log Scale)") +
  ggtitle(paste0("Trajectory of COVID-19 Cases for Baden-Wurttemberg (", today(), ")")) +
  scale_x_log10(limits = c(1, max(baden_wurttemberg_aggregated$count_week) * 10),
                labels = comma) +
  scale_y_log10(limits = c(1, max(baden_wurttemberg_aggregated$count_new) * 10),
                labels = comma) +
  theme_minimal(base_size = 18)

ggsave(paste0("img/03-bw-weekly-trend-",file_date,".png"))

#Plot SMC new cases vs confirmed cases-----
smc_aggregated %>%
  filter(count_week > 0) %>%
  ggplot(aes(x = count_week, y = count_new)) +
  geom_path(size = 0.8) +
  geom_point(size = 2) +
  xlab("Weekly Confirmed Cases (Log Scale)") +
  ylab("Weekly New Cases (Log Scale)") +
  ggtitle(paste0("Trajectory of COVID-19 Cases for SMC (", today(), ")")) +
  scale_x_log10(limits = c(1, 1000),
                labels = comma) +
  scale_y_log10(limits = c(1, 100),
                labels = comma) +
  theme_minimal(base_size = 18)

ggsave(paste0("img/04-smc-weekley-trend-",file_date,".png"))


#Age distribution of Baden-Wurttemberg Cases-----
de_data %>%
  filter(IdBundesland == 8) %>%
  count(Altersgruppe) %>%
  ggplot(data = .) +
  geom_col(aes(x = Altersgruppe, y = n)) +
  geom_text(aes(x = Altersgruppe, y = n + 100, label = n)) +
  theme_minimal()
ggsave(paste0("img/05-bw-age-distribution-",file_date,".png"))

#Summarize by week
# group_by(week = week(date)) %>%
#   select(week, count, new_cases) %>%
#   summarise_at(c("count", "new_cases"), sum, na.rm = TRUE)

