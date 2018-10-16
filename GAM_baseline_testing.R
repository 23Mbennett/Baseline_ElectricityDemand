setwd("C:/Users/Miles/Desktop/COI")
library(tidyverse)
library(lubridate)
library(gridExtra)
library(broom)
install.packages("gam")
library(gam)
install.packages("mgcv")
library(mgcv)


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
month <- as.data.frame(as.numeric(month(MT_4_data$date_time, label = FALSE)))
hour <- as.data.frame(hour(MT_4_data$date_time))
hour <- hour + 1
minute <- as.data.frame(minute(MT_4_data$date_time))
minute <- minute + 1
year <- as.data.frame(as.factor(year(MT_4_data$date_time)))


structured_data <- cbind(MT_4_data, day, month, hour, minute, year)
colnames(structured_data) <- c("MT_004", "date_time", "day", "month", "hour", "minute", "year")


#visualize data frame 
head(structured_data, 10)

#############################################################################################################################
#Train on MT_004
x <- ggplot(structured_data, aes(x = hour, y = MT_004)) + geom_jitter()
y <- ggplot(structured_data, aes(x = month, y = MT_004)) + geom_jitter()
z <- ggplot(structured_data, aes(x = day, y = MT_004)) + geom_jitter()

grid.arrange(x, y, z)

MT_004 <- gam(MT_004 ~ day + s(as.numeric(month), bs = "gp", k = 11) + s(hour, bs = "gp", k = 23), data = structured_data) # 66.8%
plot(MT_004, all.terms = T, shade = T, residuals = T, pages = 1, seWithMean = T, se = T, shade.col = "blue")
summary(MT_004)
gam.check(MT_004)

plot.gam(MT_004, residuals = TRUE, all.terms = TRUE, pch = 1,  pages = 1) #plot partial effects and partial residuals
coef(MT_004) # see the coefficients and therefore number of basis functions used for each predictor
MT_004$sp #extract the smoothing parameter chosen by the Restricted Maximum likelihood Estimation

MT_004 <- bam(MT_004 ~ day + s(hour, by = day) + s(month, bs = "gp", k = 11) + s(hour, bs = "gp", k = 23), data = structured_data, method = "REML") #70.6%

MT_004 <- bam(MT_004 ~ day + s(hour, by = day) + s(month, by = day) + s(month, bs = "gp", k = 11) + s(hour, bs = "gp", k = 23), data = structured_data, method = "REML") #70.8%

MT_004 <- bam(MT_004 ~ day + ti(hour, by = day, k = 6) + ti(month, by = day, k = 6) + s(month, bs = "gp", k = 12) + s(hour, bs = "gp", k = 24) + te(month, hour, bs = c("gp","gp"), k = 12), data = structured_data, method = "REML") #78.3%

MT_004 <- bam(MT_004 ~ day + ti(hour, by = day, k = 6) + ti(month, by = day, k = 6) + s(month, bs = "cc", k = 12) + s(hour, bs = "gp", k = 24) + te(month, hour, bs = c("cc","gp"), k = 12), data = structured_data, method = "REML") #78.2%


MT_004 <- bam(MT_004 ~ lo(as.numeric(day), degree = 2, span = 0.50) + ti(hour, by = day, k = 6) + ti(month, by = day, k = 6) + s(month, bs = "gp", k = 12) + s(hour, bs = "gp", k = 24) + ti(month, hour, bs = c("gp","gp"), k = 12), data = structured_data, method = "REML") #77.7% *BEST MODEL 1 day out of SAMLPE!

plot.gam(MT_004, scheme = 1, residuals = TRUE, se = T, seWithMean = T, shade.col = "light blue", cex = 2, all.terms = T, shade = T, shift = coef(MT_004)[1], pages = 1)

summary(MT_004)
par(mfrow = c(2,2))
gam.check(MT_004)

plot(MT_004, shade = T, residuals = T, pages = 1, seWithMean = T, se = T, shade.col = "blue", cex = 2, shift = coef(MT_004)[1]) #plots on 15,16,17***,18
plot(MT_004, scheme = 1, select = 17, col = "lightblue")



MT_004.1 <- gam(MT_004 ~ day + s(as.numeric(month), bs = "gp", k = 11) + s(hour, bs = "gp", k = 23) + s(as.numeric(month), by = day), data = structured_data) # 67%
MT_004.2 <- gam(MT_004 ~ day + te(as.numeric(month), bs = "gp", k = 11) + te(hour, bs = "gp", k = 23) + s(hour, by = day) + ti(as.numeric(month), hour), data = structured_data, method = "REML")  # 70.6% BEST MODEL with 1 INTERACTION
plot(MT_004.2, all.terms = T, shade = T, residuals = T, pages = 1, seWithMean = T, se = T, shade.col = "blue", cex = 2, shift = coef(MT_004.2)[1])

gam.check(MT_004.2)

########################IDENTIFIED CORE MODEL COMPONENTS##########################################


hr <- lm(MT_004 ~ hour + I(hour^2) + I(hour^3) + I(hour^4) + I(hour^5) + I(hour^6), data = structured_data)
summary(hr) ##ALMOST AS GOOD AS hr2

hr2 <- lm(MT_004 ~ bs(hour, df =6, degree = 2), data = structured_data)
summary(hr2)
plot(hr2)

dy <- lm(MT_004 ~ day + I(day^2) + I(day^3), data = structured_data)
summary(dy)
plot(dy)

dy2 <- lm(MT_004 ~ bs(day, df = 6), data = structured_data) 
summary(dy2)

mt <- lm(MT_004 ~ month + I(month^2) + I(month^3), data = structured_data)
summary(mt)

mt2 <- lm(MT_004 ~ bs(month, df = 6, degree = 3), data = structured_data)
summary(mt2)

ch.hr <- smooth.spline(structured_data$hour, structured_data$MT_004, cv = TRUE)

fit0 <- lm(MT_004 ~ as.factor(day) + as.factor(month) + ns(hour, df =6), data = structured_data) #RSE 22.74
fit0.1 <- gam(MT_004 ~ s(month, 6) + s(hour, 6) + s(day, 6), data = structured_data)#23.62
fit <- lm(MT_004 ~ bs(month, df = 6, degree = 4) + bs(hour, df =6, degree = 3) + as.factor(day),  data = structured_data)#RSE 23.39
fit2 <- lm(MT_004 ~ ns(month, df = 6) + ns(hour, df= 6) + as.factor(day), data = structured_data) #RSE 22.89
fit3 <- lm(MT_004 ~ as.factor(month) + ns(hour, 6) + lo(day), data = structured_data) #22.98
##changed month and day to factors to use random effects in GAM
fit4 <- bam(MT_004 ~ s(month, bs = "re") + s(hour, k = 15, bs = "gp") + s(day, bs = "re") + te(month, hour, bs = c("re","gp")) + te(day, hour, bs = c("re","gp")), data = structured_data, method = "REML" ) #**BEST MODEL 20.71 
fit5 <- gam(MT_004 ~ s(month, bs = "re") + s(hour, bs = "gp") + s(day, bs = "re") + te(month, hour, bs = c("re","gp")), data = structured_data, method = "REML") #20.81

fit4.1 <- gam(MT_004 ~ ti(month, bs = "re") + ti(hour, bs = "gp") + ti(month, hour, bs = c("re", "gp")), data = structured_data, method = "REML") #NOT GOOD
 
fit6 <- bam(MT_004 ~ s(hour, bs="gp") + month + s(hour, by = day), data = structured_data, method = "REML")

###############################################################################################################

#MT_004
MT_004 <- bam(MT_004 ~ lo(as.numeric(day)) + ti(hour, by = day, k = 6) + ti(month, by = day, k = 6) + s(month, bs = "gp", k = 12) + s(hour, bs = "gp", k = 24) + ti(month, hour, bs = c("gp","gp"), k = 12), data = structured_data, method = "REML") 
gam.check(MT_004)

# Quantitative check of model error rate 
result <- MT_004$fitted.values
gam_result <- cbind(structured_data, result)
MT_004_root_mean_square_error <- sqrt(mean((gam_result$result - gam_result$MT_004)^2))
MT_004_root_mean_square_error #18.186

#Visual Check of fitted model vs realization
a <- ggplot(gam_result, aes(x = date_time, y = result)) + geom_line() + ggtitle("rmse = 18.19, GAM")
b <- ggplot(gam_result, aes(x = date_time, y = MT_004)) + geom_line() 
grid.arrange(a,b)


head(d1)
train <- d1[,c("date_time", "MT_004", "MT_006", "MT_008", "MT_009", "MT_011")]


# Extracting date objects from date time and combining them in train                  
day <- as.data.frame(as.factor(as.integer(wday(train$date_time, label = FALSE))))#levels(day) "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" = 1:7  
month <- as.data.frame(as.numeric(month(train$date_time, label = FALSE)))
hour <- as.data.frame(hour(train$date_time))
hour <- hour + 1
minute <- as.data.frame(minute(train$date_time))
minute <- minute + 1
year <- as.data.frame(as.factor(year(train$date_time)))


all_train <- cbind(train, day, month, hour, minute, year)
colnames(all_train) <- c("date_time", "MT_004", "MT_006", "MT_008", "MT_009", "MT_011", "day", "month", "hour", "minute", "year")
names(all_train)


#Train 2 gam model MT_006

MT_006 <- bam(MT_006 ~ day + ti(hour, by = day, k = 6) + ti(month, by = day, k = 6) + s(month, bs = "gp", k = 12) + s(hour, bs = "gp", k = 24) + ti(month, hour, bs = c("gp","gp"), k = 12), data = all_train, method = "REML")
summary(MT_006)

result_MT_006 <- MT_006$fitted.values
gam_MT_006 <- cbind(all_train, result_MT_006)
sqrt(mean((gam_MT_006$MT_006 - gam_MT_006$result_MT_006)^2))
a <- ggplot(gam_MT_006, aes(x = date_time, y = result_MT_006)) + geom_line() + ggtitle("rmse = 25.49, GAM")
b <- ggplot(gam_MT_006, aes(x = date_time, y = MT_006)) + geom_line()
grid.arrange(a,b)


#Train 3 GAM MT_008

MT_008 <- bam(MT_008 ~ day + ti(hour, by = day, k = 6) + ti(month, by = day, k = 6) + s(month, bs = "gp", k = 12) + s(hour, bs = "gp", k = 24) + ti(month, hour, bs = c("gp","gp"), k = 12), data = all_train, method = "REML")
summary(MT_008)

result_MT_008 <- MT_008$fitted.values
gam_MT_008 <- cbind(all_train, result_MT_008)
sqrt(mean((gam_MT_008$MT_008 - gam_MT_008$result_MT_008)^2))

c <- ggplot(gam_MT_008, aes(x = date_time, y = result_MT_008)) + geom_line() + ggtitle("rmse = 27.97 , GAM")
d <- ggplot(gam_MT_008, aes(x = date_time, y = MT_008)) + geom_line()
grid.arrange(c,d)


#Train 4 GAM MT_009
MT_009 <- bam(MT_009 ~ day + ti(hour, by = day, k = 6) + ti(month, by = day, k = 6) + s(month, bs = "gp", k = 12) + s(hour, bs = "gp", k = 24) + ti(month, hour, bs = c("gp","gp"), k = 12), data = all_train, method = "REML")
summary(MT_009)

result_MT_009 <- MT_009$fitted.values
gam_MT_009 <- cbind(all_train, result_MT_009)
sqrt(mean((gam_MT_009$MT_009 - gam_MT_009$result_MT_009)^2))

e <- ggplot(gam_MT_009, aes(x = date_time, y = result_MT_009)) + geom_line() + ggtitle("rmse = 13.79, GAM")
f <- ggplot(gam_MT_009, aes(x = date_time, y = MT_009)) + geom_line()
grid.arrange(e,f)


#Train 5 
MT_011 <- bam(MT_011 ~ day + ti(hour, by = day, k = 6) + ti(month, by = day, k = 6) + s(month, bs = "gp", k = 12) + s(hour, bs = "gp", k = 24) + ti(month, hour, bs = c("gp","gp"), k = 12), data = all_train, method = "REML")
summary(MT_011)

result_MT_011 <- MT_011$fitted.values
gam_MT_011 <- cbind(all_train, result_MT_011)
sqrt(mean((gam_MT_011$MT_011 - gam_MT_011$result_MT_011)^2))

g <- ggplot(gam_MT_011, aes(x = date_time, y = result_MT_011)) + geom_line() + ggtitle("rmse = 5.83, GAM")
h <- ggplot(gam_MT_011, aes(x = date_time, y = MT_011)) + geom_line()
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
MT_004_test <- MT_004 <- bam(MT_004 ~ lo(as.numeric(day)) + ti(hour, by = day, k = 6) + ti(month, by = day, k = 6) + s(month, bs = "gp", k = 12) + s(hour, bs = "gp", k = 24) + ti(month, hour, bs = c("gp","gp"), k = 12), data = train_d1, method = "REML") 
summary(MT_004_test) 

pred_MT_004 <- as.data.frame(predict(MT_004_test, newdata = test_d1[, c("day", "month", "hour")]))
test_1 <- test_d1[, c("date_time", "MT_004")]
result_MT_004 <- cbind(test_1, pred_MT_004)
head(result_MT_004)
colnames(result_MT_004) <- c("date_time", "MT_004", ".fitted")
rmse_test1 <- sqrt(mean((result_MT_004$.fitted - result_MT_004$MT_004)^2))
rmse_test1

a.t <- ggplot(result_MT_004, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("test rmse = 22.25")
b.t <- ggplot(result_MT_004, aes(x = date_time, y = MT_004)) + geom_line()

grid.arrange(a.t, b.t)

#TEST MODEL 2 MT_006
MT_006_test <- bam(MT_006 ~ lo(as.numeric(day)) + ti(hour, by = day, k = 6) + ti(month, by = day, k = 6) + s(month, bs = "gp", k = 12) + s(hour, bs = "gp", k = 24) + ti(month, hour, bs = c("gp","gp"), k = 12), data = train_d1, method = "REML") 
summary(MT_006_test)

pred_MT_006 <- as.data.frame(predict(MT_006_test, newdata = test_d1[, c("day", "month", "hour")]))
test_2 <- test_d1[, c("date_time", "MT_006")]
result_MT_006 <- cbind(test_2, pred_MT_006)
head(result_MT_006)
colnames(result_MT_006) <- c("date_time", "MT_006", ".fitted")
rmse_test2 <- sqrt(mean((result_MT_006$.fitted - result_MT_006$MT_006)^2))
rmse_test2

c.t <- ggplot(result_MT_006, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("test rmse = 26.62")
d.t <- ggplot(result_MT_006, aes(x = date_time, y = MT_006)) + geom_line() 

grid.arrange(c.t, d.t)

#TEST MODEL 3 MT_008
MT_008_test <- bam(MT_008 ~ lo(as.numeric(day)) + ti(hour, by = day, k = 6) + ti(month, by = day, k = 6) + s(month, bs = "gp", k = 12) + s(hour, bs = "gp", k = 24) + ti(month, hour, bs = c("gp","gp"), k = 12), data = train_d1, method = "REML")
summary(MT_008_test)

pred_MT_008 <- as.data.frame(predict(MT_008_test, newdata = test_d1[, c("day", "month", "hour")]))
test_3 <- test_d1[, c("date_time", "MT_008")]
result_MT_008 <- cbind(test_3, pred_MT_008)
head(result_MT_008)
colnames(result_MT_008) <- c("date_time", "MT_008", ".fitted")
rmse_test3 <- sqrt(mean((result_MT_008$.fitted - result_MT_008$MT_008)^2))
rmse_test3

c.t <- ggplot(result_MT_008, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("test rmse = 31.08")
d.t <- ggplot(result_MT_008, aes(x = date_time, y = MT_008)) + geom_line()

grid.arrange(c.t, d.t)

#TEST MODEL 4 MT_009
MT_009_test <-bam(MT_009 ~ lo(as.numeric(day)) + ti(hour, by = day, k = 6) + ti(month, by = day, k = 6) + s(month, bs = "gp", k = 12) + s(hour, bs = "gp", k = 24) + ti(month, hour, bs = c("gp","gp"), k = 12), data = train_d1, method = "REML")
summary(MT_009_test)

pred_MT_009 <- as.data.frame(predict(MT_009_test, newdata = test_d1[, c("day", "month", "hour")]))
test_4 <- test_d1[, c("date_time", "MT_009")]
result_MT_009 <- cbind(test_4, pred_MT_009)
head(result_MT_009)
colnames(result_MT_009) <- c("date_time", "MT_009", ".fitted")
rmse_test4 <- sqrt(mean((result_MT_009$.fitted - result_MT_009$MT_009)^2))
rmse_test4

e.t <- ggplot(result_MT_009, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("test rmse = 15.34") 
f.t <- ggplot(result_MT_009, aes(x = date_time, y = MT_009)) + geom_line() 

grid.arrange(e.t, f.t)

#TEST MODEL 5 MT_011
MT_011_test <- bam(MT_011 ~ lo(as.numeric(day)) + ti(hour, by = day, k = 6) + ti(month, by = day, k = 6) + s(month, bs = "gp", k = 12) + s(hour, bs = "gp", k = 24) + ti(month, hour, bs = c("gp","gp"), k = 12), data = train_d1, method = "REML")
summary(MT_011_test)

pred_MT_011 <- as.data.frame(predict(MT_011_test, newdata = test_d1[, c("day", "month", "hour")]))
head(test_d1)
test_5 <- test_d1[, c("date_time", "MT_011")]
result_MT_011 <- cbind(test_5, pred_MT_011)
head(result_MT_011)
colnames(result_MT_011) <- c("date_time", "MT_011", ".fitted")
rmse_test5 <- sqrt(mean((result_MT_011$.fitted - result_MT_011$MT_011)^2))
rmse_test5

g.t <- ggplot(result_MT_011, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("test rmse = 25.26")
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
rmse_test1.1

mape_test1 <- sum(abs((result_MT_004_1$.fitted - result_MT_004_1$MT_004)/result_MT_004_1$MT_004))/nrow(result_MT_004_1)
mape_test1


a.1 <- ggplot(result_MT_004_1, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("test rmse = 17.57")
b.1 <- ggplot(result_MT_004_1, aes(x = date_time, y = MT_004)) + geom_line()

grid.arrange(a.1,b.1)

##STATS on RESULT_MT_004_1 MT_004
mean(result_MT_004_1$MT_004) #106
sd(result_MT_004_1$MT_004)   #32
min(result_MT_004_1$MT_004)  #69
max(result_MT_004_1$MT_004)  #205

##STATS on RESULT_MT_004_1 fitted
mean(result_MT_004_1$.fitted) #94
sd(result_MT_004_1$.fitted)   #30
min(result_MT_004_1$.fitted)  #69
max(result_MT_004_1$.fitted)  #166


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
  ggtitle("  BASELINE(blue) & DEMAND(black) AVG ERROR RATE: 13%") +
  xlab("DEMAND RESPONSE 3pm - 4am")
  
grid.arrange(ACTUAL, BASE_EVENT)







#TEST 2
pred_MT_006_1 <- as.data.frame(predict(MT_006_test, newdata = test_d2[, c("day", "month", "hour")]))
test_2.1 <- test_d2[, c("date_time", "MT_006")]
result_MT_006_1 <- cbind(test_2.1, pred_MT_006_1)
head(result_MT_006_1)
colnames(result_MT_006_1) <- c("date_time", "MT_006", ".fitted")
rmse_test2.1 <- sqrt(mean((result_MT_006_1$.fitted - result_MT_006_1$MT_006)^2))
rmse_test2.1

c.1 <- ggplot(result_MT_006_1, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("test rmse = 15.79")
d.1 <- ggplot(result_MT_006_1, aes(x = date_time, y = MT_006)) + geom_line() 

grid.arrange(c.1,d.1)

#TEST 3
pred_MT_008_1 <- as.data.frame(predict(MT_008_test, newdata = test_d2[, c("day", "month", "hour")]))
test_3.1 <- test_d2[, c("date_time", "MT_008")]
result_MT_008_1 <- cbind(test_3.1, pred_MT_008_1)
head(result_MT_008_1)
colnames(result_MT_008_1) <- c("date_time", "MT_008", ".fitted")
rmse_test3.1 <- sqrt(mean((result_MT_008_1$.fitted - result_MT_008_1$MT_008)^2))
rmse_test3.1

e.1 <- ggplot(result_MT_008_1, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("test rmse = 19.86")
f.1 <- ggplot(result_MT_008_1, aes(x = date_time, y = MT_008)) + geom_line()

grid.arrange(e.1, f.1)

#TEST 4
pred_MT_009_1 <- as.data.frame(predict(MT_009_test, newdata = test_d2[, c("day", "month", "hour")]))
test_4.1 <- test_d2[, c("date_time", "MT_009")]
result_MT_009_1 <- cbind(test_4.1, pred_MT_009_1)
head(result_MT_009_1)
colnames(result_MT_009_1) <- c("date_time", "MT_009", ".fitted")
rmse_test4.1 <- sqrt(mean((result_MT_009_1$.fitted - result_MT_009_1$MT_009)^2))
rmse_test4.1

g.1 <- ggplot(result_MT_009_1, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("test rmse = 17.57")
h.1 <- ggplot(result_MT_009_1, aes(x = date_time, y = MT_009)) + geom_line()

grid.arrange(g.1, h.1)
#TEST 5
pred_MT_011_1 <- as.data.frame(predict(MT_011_test, newdata = test_d2[, c("day", "month", "hour")]))
test_5.1 <- test_d2[, c("date_time", "MT_011")]
result_MT_011_1 <- cbind(test_5.1, pred_MT_011_1)
head(result_MT_011_1)
colnames(result_MT_011_1) <- c("date_time", "MT_011", ".fitted")
rmse_test5.1 <- sqrt(mean((result_MT_011_1$.fitted - result_MT_011_1$MT_011)^2))
rmse_test5.1

i.1 <- ggplot(result_MT_011_1, aes(x = date_time, y = .fitted)) + geom_line() + ggtitle("test rmse = 5.37")
j.1 <- ggplot(result_MT_011_1, aes(x = date_time, y = MT_011)) + geom_line() 

grid.arrange(i.1, j.1)