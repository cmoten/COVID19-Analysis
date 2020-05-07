#Gompertz estimate for confirmed cases within the state of Baden-Wurttemberg-----

time <- 0:199
new_dates <- first(baden_wurttemberg_confirmed$date) + time
alpha <- bw_infected
beta <- 1.25 #based on an R0 of 2.5 and a gamma of 2 days
k <- .15 #Best starting guess based on paper found at https://arxiv.org/pdf/2003.05447.pdf
todays_date <- today()


#Get best estimate of the parameters

confirmed_data <- baden_wurttemberg_confirmed %>% select(time, count)
colnames(confirmed_data) <- c("x", "y")


start_par <- c(a = alpha, b = beta, c = k)
confirmed_model_start <- model_nlxb(nlxb_formula, start_par, confirmed_data)
print(confirmed_model_start)
confirmed_model <- nlsfit(confirmed_data, model = 10, start = c(a = alpha, 
                                                                b = confirmed_model_start$coefficients[2], 
                                                                c = confirmed_model_start$coefficients[3]))


nlsplot(as.data.frame(confirmed_data), model = 10, start = c(a = alpha, 
                                             b = confirmed_model_start$coefficients[2], 
                                             c = confirmed_model_start$coefficients[3]))

new_alpha <- confirmed_model$Parameters[1, ]
new_beta <- confirmed_model$Parameters[2, ]
new_k <- confirmed_model$Parameters[3, ]
bw_confirmed_predictions <- gompertz(t = time,
                                     alpha = round(new_alpha, 0),
                                     beta = new_beta,
                                     k = new_k)



bw_confirmed_predictions <- data.frame(date = new_dates, count = round(bw_confirmed_predictions, 0))
bw_confirmed_predictions$date <- as.POSIXct(bw_confirmed_predictions$date)
baden_wurttemberg_confirmed$date <- as.POSIXct(baden_wurttemberg_confirmed$date)

bw_n <- nrow(baden_wurttemberg_confirmed)
bw_se_num <- (baden_wurttemberg_confirmed$count - bw_confirmed_predictions$count[1:bw_n])^2 %>% sum()
bw_se_denom <- bw_n - 2
bw_se <- sqrt(bw_se_num / bw_se_denom)


#Analysis of Stuttgart Military Community Confirmed Cases-----
# time <- 0:199
# new_dates <- first(smc_confirmed$date) + time
# 
# alpha <- smc_infected
# beta <- 1.25 #Best starting guess based on paper found at https://arxiv.org/pdf/2003.05447.pdf and trial and error
# k <- .15 #Best starting guess based on paper found at https://arxiv.org/pdf/2003.05447.pdf and trial and error
# todays_date <- today()
# 
# #Get best estimate of the parameters
# smc_data <- smc_confirmed[, -1]
# n <- nrow(smc_data)
# time <- 0:(n-1)
# smc_data$time <- time
# smc_data <- smc_data[, c(2, 1)]
# colnames(smc_data) <- c("x", "y")
# 
# 
# 
# start_par <- c(a = alpha, b = beta, c = k)
# smc_model_start <- model_nlxb(nlxb_formula, start_par, smc_data)
# print(smc_model_start)
# 
# 
# smc_model <- nlsfit(smc_data, model = 10, start = c(a = alpha, 
#                                                                 b = smc_model_start$coefficients[2], 
#                                                                 c = smc_model_start$coefficients[3]))


nlsplot(as.data.frame(smc_data), model = 10, start = c(a = alpha, 
                                                             b = smc_model_start$coefficients[2], 
                                                             c = smc_model_start$coefficients[3]))

new_alpha <- smc_model$Parameters[1, ]
new_beta <- smc_model$Parameters[2, ]
new_k <- smc_model$Parameters[3, ]



smc_confirmed_predictions <- gompertz(t = time,
                                     alpha = round(new_alpha, 0),
                                     beta = new_beta,
                                     k = new_k)



smc_confirmed_predictions <- data.frame(date = new_dates, count = round(smc_confirmed_predictions, 0))
smc_confirmed_predictions$date <- as.POSIXct(smc_confirmed_predictions$date)
smc_confirmed$date <- as.POSIXct(smc_confirmed$date)

  smc_n <- nrow(smc_confirmed)
  smc_se_num <- (smc_confirmed$count - smc_confirmed_predictions$count[1:smc_n])^2 %>% sum()
  smc_se_denom <- smc_n - 2
  smc_se <- sqrt(smc_se_num / smc_se_denom)











  

  
  
