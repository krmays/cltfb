## Predicting final rank in CFB playoff ranking given only 12 stats available to CFP committee ##

data <- readxl::read_excel("12stats.xlsx")
#data <- subset(data, Year != 2020) #For excluding 2020 season, if desired

G5 <- subset(data, G5 == 1) #For looking at ranked G5 teams only
ten <- subset(data, Rank >= 10) #For looking at Top 10 teams only

model <- stats::lm(data$Rank ~ A+B+C+D+E+F+G+H+I+J+K+L, data = data)
summary(model) #All teams

model.G5 <- lm(Rank ~ A+B+C+D+E+F+G+H+I+J+K+L, data = G5)
summary(model.G5) #G5 only

model.10 <- lm(Rank ~ A+B+C+D+E+F+G+H+I+J+K+L, data = ten)
summary(model.10) #Top 10 only

both <- step(model, direction = 'both')
#both.G5 <-step(model.G5, direction = 'both')
#both.10 <-step(model.10, direction = 'both')


new.model <- lm(Rank ~ A + D + F + J + K + L, data = data)
summary(new.model)

#new.model.G5 <- lm(Rank ~ B + D + E + G + H + J, data = G5)
#summary(new.model.5)

#new.model.10 <-lm(Rank ~ A + F + H + J + K, data = ten)
#summary(new.model.10)

# Confirm model is improved
# Would prefer this as a loop that just checks for new < old

BIC(model)
BIC(new.model)
AIC(model)
AIC(new.model)

#get matrix of coefficients and calculate estimate of y
coef <- new.model$coefficients
#clt <- data[which((data$Team == as.character("Charlotte")) & (data$Year = "2021"))] #This needs work

#Check model assumptions
res <- residuals(new.model)
fit.1 <- new.model$fitted.values
plot(fit.1, res, xlab='Fitted Values', ylab= 'Residuals', main='Residuals vs Fitted Values')
qqnorm(res)


#################
## Next steps: ##
#################

## Comparing results with quantile regression ##
qr <- quantreg::rq(Rank ~ A+B+C+D+E+F+G+H+I+J+K+L, tau = 0.5, data = data)
summary(qr)
both <- step(qr, direction = 'both')
new.qr <- quantreg::rq(Rank ~ A + B + D + F + I + J + L, tau = 0.5, data = data)
summary(new.qr)

## Loop and average across quantiles ##

## Add other predictors (since 12 stats alone are clearly not sufficient) ##

## Automate stat collection/update for future years ##
