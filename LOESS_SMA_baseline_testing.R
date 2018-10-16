setwd("C:/Users/Miles/Desktop/COI")
install.packages(c("tidyverse","lubridate","gridExtra"))
library(tidyverse)
library(lubridate)
library(gridExtra)
library(broom)
install.packages("smooth")
library(smooth)

data <- read.csv2("LD2011_2014.txt",  sep = ";")
class(data$X)
data$X <- as.character(data$X)
class(data$X)
data$X <- ymd_hms(data$X)
tail(data)

wdata <- data[,1:13]
wdata$date_time <- wdata$X


d1 <- wdata %>% 
  filter(X > "2011-12-31 23:45:00")

head(d1)

MT_4_data <- d1[,c(5, 14)]
head(MT_4_data)


# Extracting Date Objects from date_time also we can easily add more variables                  
day <- as.data.frame(as.factor(as.integer(wday(MT_4_data$date_time, label = FALSE))))#levels(day) "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" = 1:7  
month <- as.data.frame(month(MT_4_data$date_time, label = FALSE))
hour <- as.data.frame(hour(MT_4_data$date_time))
hour <- hour + 1
minute <- as.data.frame(minute(MT_4_data$date_time))
minute <- minute + 1
year <- as.data.frame(as.factor(year(MT_4_data$date_time)))


structured_data <- cbind(MT_4_data, day, month, hour, minute, year)
colnames(structured_data) <- c("MT_004", "date_time", "day", "month", "hour", "minute", "year")


#visualize data frame 
head(structured_data, 10)


#Train 1 locally weighted regression MT_004
MT_004 <- loess(MT_004 ~ as.numeric(day) + as.numeric(month) + as.numeric(hour) + I(as.numeric(month)*as.numeric(hour)), 
                        data = structured_data, 
                        span = 0.065, 
                        family = "symmetric", 
                        surface = "direct")
#Model summary
summary(MT_004)



# Quantitative check of model error rate 
result <- augment(MT_004)
local_reg_result <- cbind(structured_data, result)
MT_004_root_mean_square_error <- sqrt(mean((local_reg_result$.fitted - local_reg_result$kW)^2))


#Visual Check of fitted model vs realization
a <- ggplot(local_reg_result, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("span= 7%")
b <- ggplot(local_reg_result, aes(x = date_time, y = MT_004)) + geom_line() 
grid.arrange(a,b)


head(d1)
train <- d1[,c("date_time", "MT_004", "MT_006", "MT_008", "MT_009", "MT_011")]


# Extracting date objects from date time and combining them in train                  
day <- as.data.frame(as.factor(as.integer(wday(train$date_time, label = FALSE))))#levels(day) "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" = 1:7  
month <- as.data.frame(month(train$date_time, label = FALSE))
hour <- as.data.frame(hour(train$date_time))
hour <- hour + 1
minute <- as.data.frame(minute(train$date_time))
minute <- minute + 1
year <- as.data.frame(as.factor(year(train$date_time)))


all_train <- cbind(train, day, month, hour, minute, year)
colnames(all_train) <- c("date_time", "MT_004", "MT_006", "MT_008", "MT_009", "MT_011", "day", "month", "hour", "minute", "year")
names(all_train)


#Train 2 locally weighted regression MT_006
MT_006 <- loess(MT_006 ~ as.numeric(day) + as.numeric(month) + as.numeric(hour) + I(as.numeric(month)*as.numeric(hour)), data = all_train, span = .07, family = "symmetric", surface = "direct")

result_MT_006 <- augment(MT_006)
local_reg_result_MT_006 <- cbind(all_train$date_time, result_MT_006)
a <- ggplot(local_reg_result_MT_006, aes(x = all_train$date_time, y = .fitted)) + geom_line() + ggtitle("rmse = 27.1, span = 7%")
b <- ggplot(local_reg_result_MT_006, aes(x = all_train$date_time, y = MT_006)) + geom_line()
grid.arrange(a,b)


#Train 3 locally weighted regression MT_008
MT_008 <- loess(MT_008 ~ as.numeric(day) + as.numeric(month) + as.numeric(hour) + I(as.numeric(month)*as.numeric(hour)), data = all_train, span = .07, family = "symmetric", surface = "direct")
result_MT_008 <- augment(MT_008)
loc_res_MT_008 <- cbind(all_train$date_time, result_MT_008)
c <- ggplot(loc_res_MT_008, aes(x = all_train$date_time, y = .fitted )) + geom_line() + ggtitle("rmse = 29.9 , span = 7%")
d <- ggplot(loc_res_MT_008, aes(x = all_train$date_time, y = MT_008)) + geom_line()
grid.arrange(c,d)


#Train 4 locally weighted reg MT_009
MT_009 <- loess(MT_009 ~ as.numeric(day) + as.numeric(month) + as.numeric(hour) + I(as.numeric(month)*as.numeric(hour)), data = all_train, span = .07, family = "symmetric", surface = "direct")
result_MT_009 <- augment(MT_009)
loc_res_MT_009 <- cbind(all_train$date_time, result_MT_009)
e <- ggplot(loc_res_MT_009, aes(x = all_train$date_time, y = .fitted)) + geom_line() + ggtitle("rmse = 12.2* , span = 7%")
f <- ggplot(loc_res_MT_009, aes(x = all_train$date_time, y = MT_009)) + geom_line()
grid.arrange(e,f)


#Train 5 locally weighted reg MT_011
MT_011 <- loess(MT_011 ~ as.numeric(day) + as.numeric(month) + as.numeric(hour) + I(as.numeric(month)*as.numeric(hour)), data = all_train, span = .07, family = "symmetric", surface = "direct")
result_MT_011 <- augment(MT_011)
loc_res_MT_011 <- cbind(all_train$date_time, result_MT_011)
g <- ggplot(loc_res_MT_011, aes(x = all_train$date_time, y = .fitted)) + geom_line() + ggtitle("rmse = 6.2, span = 7%")
h <- ggplot(loc_res_MT_011, aes(x = all_train$date_time, y = MT_011)) + geom_line()
grid.arrange(g,h)


##ALL useable data with values was used to calculate RMSE for model selection upto this point. Now we will consider in and out of sample error rates for 3 months

#Test Subset
head(all_train)
tail(all_train)
test_d1 <- all_train %>% 
            filter(date_time >= "2014-10-01")


#Train Subset
train_d1 <- all_train %>%
              filter(date_time < "2014-10-01")


#TEST MODEL 1 MT_004
MT_004_test <- loess(MT_004 ~ as.numeric(day) + as.numeric(month) + as.numeric(hour) + I(as.numeric(month)*as.numeric(hour)), data = train_d1, span = 0.065, family = "symmetric", surface = "direct", degree = 2)
summary(MT_004_test) # Train RMSE 18.30

MT_004_test.0 <- lm(MT_004 ~ lo(day, span = 0.065) + ns(as.numeric(month)) + ns(as.numeric(hour)), data = train_d1)

pred_MT_004 <- as.data.frame(predict(MT_004_test, newdata = test_d1[, c("day", "month", "hour")]))
test_1 <- test_d1[, c("date_time", "MT_004")]
result_MT_004 <- cbind(test_1, pred_MT_004)
head(result_MT_004)
colnames(result_MT_004) <- c("date_time", "MT_004", ".fitted")
rmse_test1 <- sqrt(mean((result_MT_004$.fitted - result_MT_004$MT_004)^2))
rmse_test1 




a.t <- ggplot(result_MT_004, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("train rmse = 18.3")
b.t <- ggplot(result_MT_004, aes(x = date_time, y = MT_004)) + geom_line()

grid.arrange(a.t, b.t)


#TEST MODEL 2 MT_006
MT_006_test <- loess(MT_006 ~ as.numeric(day) + as.numeric(month) + as.numeric(hour) + I(as.numeric(month)*as.numeric(hour)), data = train_d1, span = 0.065, family = "symmetric", surface = "direct")
summary(MT_006_test) #Train RMSE 24.65

pred_MT_006 <- as.data.frame(predict(MT_006_test, newdata = test_d1[, c("day", "month", "hour")]))
test_2 <- test_d1[, c("date_time", "MT_006")]
result_MT_006 <- cbind(test_2, pred_MT_006)
head(result_MT_006)
colnames(result_MT_006) <- c("date_time", "MT_006", ".fitted")
rmse_test2 <- sqrt(mean((result_MT_006$.fitted - result_MT_006$MT_006)^2))
rmse_test2

c.t <- ggplot(result_MT_006, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("train rmse = 24.65  test rmse = 32.22")
d.t <- ggplot(result_MT_006, aes(x = date_time, y = MT_006)) + geom_line() 

grid.arrange(c.t, d.t)

#TEST MODEL 3 MT_008
MT_008_test <- loess(MT_008 ~ as.numeric(day) + as.numeric(month) + as.numeric(hour) + I(as.numeric(month)*as.numeric(hour)), data = train_d1, span = 0.065, family = "symmetric", surface = "direct")
 summary(MT_008_test) #Train RMSE 28.16

pred_MT_008 <- as.data.frame(predict(MT_008_test, newdata = test_d1[, c("day", "month", "hour")]))
test_3 <- test_d1[, c("date_time", "MT_008")]
result_MT_008 <- cbind(test_3, pred_MT_008)
head(result_MT_008)
colnames(result_MT_008) <- c("date_time", "MT_008", ".fitted")
rmse_test3 <- sqrt(mean((result_MT_008$.fitted - result_MT_008$MT_008)^2))
rmse_test3

c.t <- ggplot(result_MT_008, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("train rmse = 28.16  test rmse = 34.95")
d.t <- ggplot(result_MT_008, aes(x = date_time, y = MT_008)) + geom_line()

grid.arrange(c.t, d.t)

#TEST MODEL 4 MT_009
MT_009_test <- loess(MT_009 ~ as.numeric(day) + as.numeric(month) + as.numeric(hour) + I(as.numeric(month)*as.numeric(hour)), data = train_d1, span = 0.065, family = "symmetric", surface = "direct")
summary(MT_009_test) #Train RMSE 12.14

pred_MT_009 <- as.data.frame(predict(MT_009_test, newdata = test_d1[, c("day", "month", "hour")]))
test_4 <- test_d1[, c("date_time", "MT_009")]
result_MT_009 <- cbind(test_4, pred_MT_009)
head(result_MT_009)
colnames(result_MT_009) <- c("date_time", "MT_009", ".fitted")
rmse_test4 <- sqrt(mean((result_MT_009$.fitted - result_MT_009$MT_009)^2))
rmse_test4

e.t <- ggplot(result_MT_009, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("train rmse = 11.5  test rmse = 14.82") 
f.t <- ggplot(result_MT_009, aes(x = date_time, y = MT_009)) + geom_line() 

grid.arrange(e.t, f.t)

#TEST MODEL 5 MT_011
MT_011_test <- loess(MT_011 ~ as.numeric(day) + as.numeric(month) + as.numeric(hour) + I(as.numeric(month)*as.numeric(hour)), data = train_d1, span = 0.065, family = "symmetric", surface = "direct")
summary(MT_011_test)#Train RMSE 6.11 #using surface = "direct" gives Train RMSE 5.88

pred_MT_011 <- as.data.frame(predict(MT_009_test, newdata = test_d1[, c("day", "month", "hour")]))
head(test_d1)
test_5 <- test_d1[, c("date_time", "MT_011")]
result_MT_011 <- cbind(test_5, pred_MT_011)
head(result_MT_011)
colnames(result_MT_011) <- c("date_time", "MT_011", ".fitted")
rmse_test5 <- sqrt(mean((result_MT_011$.fitted - result_MT_011$MT_011)^2))
rmse_test5

g.t <- ggplot(result_MT_011, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("train rmse = 5.88  test rmse = 20.72")
h.t <- ggplot(result_MT_011, aes(x = date_time, y = MT_011)) + geom_line()

grid.arrange(g.t, h.t)

##consider in and out of sample error rates for 1 day

#Test Subset
test_d2 <- all_train %>% 
  filter(date_time >= "2014-10-01" & date_time <= "2014-10-02")

#Train subset is same as before
train_d1


#TEST 1 
pred_MT_004_1 <- as.data.frame(predict(MT_004_test, newdata = test_d2[, c("day", "month", "hour")]))
test_1.1 <- test_d2[, c("date_time", "MT_004")]
result_MT_004_1 <- cbind(test_1.1, pred_MT_004_1)
head(result_MT_004_1)
colnames(result_MT_004_1) <- c("date_time", "MT_004", ".fitted")
rmse_test1.1 <- sqrt(mean((result_MT_004_1$.fitted - result_MT_004_1$MT_004)^2))
rmse_test1.1 #17.012

mape_test1 <- sum(abs((result_MT_004_1$.fitted - result_MT_004_1$MT_004)/result_MT_004_1$MT_004))/nrow(result_MT_004_1)
mape_test1 #12.5% no interacts

a.1 <- ggplot(result_MT_004_1, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle(" RMSE = 17.01, AER = 12.36% ")
b.1 <- ggplot(result_MT_004_1, aes(x = date_time, y = MT_004)) + geom_line()

grid.arrange(a.1,b.1)


#Train DATA FOR 2 day SMA MT_004
tail(train_d1$MT_004, 9)
train_MT_004 <- train_d1 %>% select(date_time, MT_004) %>% filter(date_time >= "2014-09-29" & date_time < "2014-10-01")
prior2_MT_004 <- train_MT_004$MT_004
 
#SMA model lag length determined by AICc minimization
sma_pred_MT_004 <- sma(prior2_MT_004, h = 97)
ls(sma_pred_MT_004)

#SMA Model RMSE 
rmse_test1.2 <- sqrt(mean((sma_pred_MT_004$forecast - result_MT_004_1$MT_004)^2))
rmse_test1.2 #37.85


mape_test1.2 <- sum(abs((sma_pred_MT_004$forecast - result_MT_004_1$MT_004)/result_MT_004_1$MT_004))/nrow(result_MT_004_1)
mape_test1.2 #17.58%


#GENERATING DEMAND RESPONSE EVENT

##oct_1_DR 3pm-4am oct_2_DR
oct_1_EVENT <- result_MT_004_1 %>% 
  filter(date_time >= "2014-10-01 11:00:00" & date_time <= "2014-10-01 24:00:00")%>%
  select(MT_004)

mean(oct_1_EVENT$MT_004) #122.03
sd(oct_1_EVENT$MT_004) #42.39
min(oct_1_EVENT$MT_004) #81.30
max(oct_1_EVENT$MT_004) #205.28

oct_1_DR <- oct_1_EVENT - 75  


##oct_1_BEFORE 3pm
oct_1_BEFORE <- result_MT_004_1 %>%
  filter(date_time < "2014-10-01 11:00:00")%>%
  select(MT_004)


DEMAND <- bind_rows(oct_1_BEFORE, oct_1_DR)
result_MT_004_1.1 <- cbind(result_MT_004_1, DEMAND)
head(result_MT_004_1.1)
colnames(result_MT_004_1.1) <- c("date_time", "MT_004", "BASELINE", "DEMAND")

ACTUAL <- ggplot(result_MT_004_1.1, aes(x = date_time, y = MT_004)) + geom_line() + ggtitle("  Actual Meter Reading in KW ")
ggplot(result_MT_004_1.1, aes(x = date_time, y = DEMAND)) + geom_line()
BASE_EVENT <- ggplot(result_MT_004_1.1, aes(x = date_time, y = BASELINE)) + 
  geom_line(color = "blue", size = 1, linetype = 2) + 
  geom_line(aes(y = DEMAND)) +
  ggtitle("  BASELINE(blue) & DEMAND(black) MAPE: 12.36%") +
  xlab("DEMAND RESPONSE 3pm - 4am")

grid.arrange(ACTUAL, BASE_EVENT)


#TEST 2
pred_MT_006_1 <- as.data.frame(predict(MT_006_test, newdata = test_d2[, c("day", "month", "hour")]))
test_2.1 <- test_d2[, c("date_time", "MT_006")]
result_MT_006_1 <- cbind(test_2.1, pred_MT_006_1)
head(result_MT_006_1)
colnames(result_MT_006_1) <- c("date_time", "MT_006", ".fitted")
rmse_test2.1 <- sqrt(mean((result_MT_006_1$.fitted - result_MT_006_1$MT_006)^2))
rmse_test2.1 #19.33 gauss, 18.07 symm

mape_test2 <- sum(abs((result_MT_006_1$.fitted - result_MT_006_1$MT_006)/result_MT_006_1$MT_006))/nrow(result_MT_006_1)
mape_test2 #9.36% gauss, 8.88% symm


c.1 <- ggplot(result_MT_006_1, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("MAPE: 8.88% ")
d.1 <- ggplot(result_MT_006_1, aes(x = date_time, y = MT_006)) + geom_line() 

grid.arrange(c.1,d.1)

#Train DATA FOR 2 day SMA MT_006
tail(train_d1$MT_006, 9)
train_MT_006 <- train_d1 %>% select(date_time, MT_006) %>% filter(date_time >= "2014-09-29" & date_time < "2014-10-01")
prior2_MT_006 <- train_MT_006$MT_006

sma_pred_MT_006 <- sma(prior2_MT_006, h = 97)
ls(sma_pred_MT_004)

#SMA Model RMSE 
rmse_test1.3 <- sqrt(mean((sma_pred_MT_006$forecast - result_MT_006_1$MT_006)^2))
rmse_test1.3 #69.60

#SMA model MAPE
mape_test1.3 <- sum(abs((sma_pred_MT_006$forecast - result_MT_006_1$MT_006)/result_MT_006_1$MT_006))/nrow(result_MT_006_1)
mape_test1.3 #28.06%


#TEST 3
pred_MT_008_1 <- as.data.frame(predict(MT_008_test, newdata = test_d2[, c("day", "month", "hour")]))
test_3.1 <- test_d2[, c("date_time", "MT_008")]
result_MT_008_1 <- cbind(test_3.1, pred_MT_008_1)
head(result_MT_008_1)
colnames(result_MT_008_1) <- c("date_time", "MT_008", ".fitted")
rmse_test3.1 <- sqrt(mean((result_MT_008_1$.fitted - result_MT_008_1$MT_008)^2))
rmse_test3.1 #21.65 symm, 21.89 gauss

mape_test3 <- sum(abs((result_MT_008_1$.fitted - result_MT_008_1$MT_008)/result_MT_008_1$MT_008))/nrow(result_MT_008_1)
mape_test3 #6.72% symm, 6.83% gauss

e.1 <- ggplot(result_MT_008_1, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("MAPE: 6.72%")
f.1 <- ggplot(result_MT_008_1, aes(x = date_time, y = MT_008)) + geom_line()

grid.arrange(e.1, f.1)

#Train DATA FOR 2 day SMA MT_008
tail(train_d1$MT_008, 9)
train_MT_008 <- train_d1 %>% select(date_time, MT_008) %>% filter(date_time >= "2014-09-29" & date_time < "2014-10-01")
prior2_MT_008 <- train_MT_008$MT_008

sma_pred_MT_008 <- sma(prior2_MT_008, h = 97)
ls(sma_pred_MT_008)

#SMA Model RMSE 
rmse_test1.3 <- sqrt(mean((sma_pred_MT_008$forecast - result_MT_008_1$MT_008)^2))
rmse_test1.3 #59.70

#SMA model MAPE
mape_test1.3 <- sum(abs((sma_pred_MT_008$forecast - result_MT_008_1$MT_008)/result_MT_008_1$MT_008))/nrow(result_MT_008_1)
mape_test1.3 #17.10%


#TEST 4
pred_MT_009_1 <- as.data.frame(predict(MT_009_test, newdata = test_d2[, c("day", "month", "hour")]))
test_4.1 <- test_d2[, c("date_time", "MT_009")]
result_MT_009_1 <- cbind(test_4.1, pred_MT_009_1)
head(result_MT_009_1)
colnames(result_MT_009_1) <- c("date_time", "MT_009", ".fitted")
rmse_test4.1 <- sqrt(mean((result_MT_009_1$.fitted - result_MT_009_1$MT_009)^2))
rmse_test4.1 #15.42 symm, 18.18 gauss

mape_test4 <- sum(abs((result_MT_009_1$.fitted - result_MT_009_1$MT_009)/result_MT_009_1$MT_009))/nrow(result_MT_009_1)
mape_test4 #23.03% symm, 28.11%


g.1 <- ggplot(result_MT_009_1, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("train rmse = 11.5, test rmse = 15.42")
h.1 <- ggplot(result_MT_009_1, aes(x = date_time, y = MT_009)) + geom_line()

grid.arrange(g.1, h.1)
#TEST 5
pred_MT_011_1 <- as.data.frame(predict(MT_011_test, newdata = test_d2[, c("day", "month", "hour")]))
test_5.1 <- test_d2[, c("date_time", "MT_011")]
result_MT_011_1 <- cbind(test_5.1, pred_MT_011_1)
head(result_MT_011_1)
colnames(result_MT_011_1) <- c("date_time", "MT_011", ".fitted")
rmse_test5.1 <- sqrt(mean((result_MT_011_1$.fitted - result_MT_011_1$MT_011)^2))
rmse_test5.1 #5.51 symm, 5.64 gauss

mape_test3 <- sum(abs((result_MT_011_1$.fitted - result_MT_011_1$MT_011)/result_MT_011_1$MT_011))/nrow(result_MT_011_1)
mape_test3 #13.04% symm, 13.28% gauss

i.1 <- ggplot(result_MT_011_1, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("train rmse = 5.88, test rmse = 5.42")
j.1 <- ggplot(result_MT_011_1, aes(x = date_time, y = MT_011)) + geom_line() 

grid.arrange(i.1, j.1)


