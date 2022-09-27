##################################
# Analysis
##################################
setwd('D:/Academic/Assignments/Financial_Econometrics_SD/')
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# Utility function to calculate IV estimator
iv_est <- function(Y, df) {
    Z <- cbind(1, df$TeamsSearch, sqrt(df$CovidSearch), df$ZoomSearch, df$SkypeSearch, df$MeetSearch)
    X <- cbind(1, df$TeamsSearch, sqrt(df$CovidNewCases), df$ZoomSearch, df$SkypeSearch, df$MeetSearch)
    
    betaiv <- solve(crossprod(Z, X), crossprod(Z, Y))
    prediv <- X %*% betaiv
    
    Rsq.iv <- 1 - sum((Y - prediv)^2)/sum((Y - mean(Y))^2)
    
    betaols <- solve(crossprod(X), crossprod(X, Y))
    predols <- X %*% betaols
    MSE <- sum((Y - predols)^2)/(nrow(X) - ncol(X))   # sigma^2
    
    var.betaols <- MSE * solve(crossprod(X))
    var.betaiv <- MSE * (solve(crossprod(Z, X)) %*% crossprod(Z) %*% solve(crossprod(X, Z)))
    
    H <- nrow(X) * t(betaols - betaiv) %*% solve(var.betaiv - var.betaols, tol = 1e-30) %*% (betaols - betaiv)
    H <- abs(H)
    pval <- pchisq(H, df = ncol(X), lower.tail = FALSE)
    
    return(list("Rsq" = Rsq.iv, "betaIV" = betaiv, "betaols" = betaols,
                "var.betaols" = var.betaols, "var.betaiv" = var.betaiv, "H" = H,
                "pval" = pval, "pred.iv" = prediv, "pred.ols" = predols))
}



# Read the data
df <- read_csv('./datasets/final/FinalData.csv')
df <- df[complete.cases(df), ]
df <- df %>% filter(Day > as.Date('2020-01-29')) %>% select(-PriceZM)

df %>% 
    select(Day, GOOGL = PriceGOOGL, MSFT = PriceMSFT) %>% 
    pivot_longer(2:3, names_to = "Stock", values_to = "Price") %>%
    ggplot(aes(x = Day, y = Price, color = Stock)) +
    geom_line(size = 1) + scale_y_continuous(trans = "log2") + theme_bw() + 
    xlab("Date") + ylab("Daily Closing Price")



# Checking for linearity plots
ggplot(df, aes(x = CovidNewCases, y = log(PriceMSFT) )) + geom_point() + xlab("Daily New Confirmed Cases of Covid") + ylab("log(Closing Stock Price of Microsoft)") + theme_bw() + geom_smooth(method = "loess")
ggplot(df, aes(x = CovidNewCases, y = log(PriceGOOGL) )) + geom_point() + xlab("Daily New Confirmed Cases of Covid") + ylab("log(Closing Stock Price of Alphabet Inc.)") + theme_bw() + geom_smooth(method = "loess")
ggplot(df, aes(x = sqrt(CovidNewCases), y = log(PriceMSFT) )) + geom_point() + xlab(expression(sqrt(Daily~New~Confirmed~Cases~of~Covid))) + ylab("log(Closing Stock Price of Microsoft)") + theme_bw() + geom_smooth(method = "lm")
ggplot(df, aes(x = sqrt(CovidNewCases), y = log(PriceGOOGL) )) + geom_point() + xlab(expression(sqrt(Daily~New~Confirmed~Cases~of~Covid))) + ylab("log(Closing Stock Price of Alphabet Inc.)") + theme_bw() + geom_smooth(method = "lm")

# Correlation Plot
cormat <- df %>% mutate(`Sqrt(CovidNewCases)` = sqrt(CovidNewCases), logPriceGOOGL = log(PriceGOOGL), logPriceMSFT = log(PriceMSFT)) %>% 
    select(-c(Day, CovidNewCases, CovidSearch, PriceGOOGL, PriceMSFT)) %>%
    cor() 

cormat["TeamsSearch", "logPriceMSFT"] <- -cormat["TeamsSearch", "logPriceMSFT"]
cormat["logPriceMSFT", "TeamsSearch"] <- -cormat["logPriceMSFT", "TeamsSearch"]
cormat["MeetSearch", "logPriceGOOGL"] <- -cormat["MeetSearch", "logPriceGOOGL"]
cormat["logPriceGOOGL", "MeetSearch"] <- -cormat["logPriceGOOGL", "MeetSearch"]
cormat["ZoomSearch", ] <- -cormat["ZoomSearch", ]
cormat[, "ZoomSearch"] <- -cormat[, "ZoomSearch"]

corrplot::corrplot(cormat)
ggplot(df, aes(x = log(PriceMSFT), y = log(PriceGOOGL) )) + geom_point() + theme_bw() + xlab("log(Closing Stock Price of Microsoft)") + ylab("log(Closing Stock Price of Alphabet Inc.)") + geom_smooth(method = "lm")

cor(df$CovidSearch, df$CovidNewCases)
cor(sqrt(df$CovidSearch), sqrt(df$CovidNewCases))


# First model
# PriceGOOGL is response
mod1 <- iv_est(log(df$PriceGOOGL), df)
mod1$betaIV
mod1$betaols
mod1$H
mod1$pval

resid <- log(df$PriceGOOGL) - mod1$pred.iv
armod1 <- arima(resid, order = c(1,0,0))

ggplot() + geom_abline(slope = 0, intercept = 0, color = "blue", size = 1, linetype = "dashed") +
    geom_point(aes(x = mod1$pred.iv, y = armod1$residuals)) + 
    xlab("Fitted Values") + ylab("Residuals") + theme_bw() 

orig_resid <- df$PriceGOOGL - exp(mod1$pred.iv)

par(mfrow = c(1, 2))
acf(orig_resid, main = "")
pacf(orig_resid, main = "")
par(mfrow = c(1, 1))


mod2 <- iv_est(log(df$PriceMSFT), df)
mod2$betaIV
mod2$betaols
mod2$H
mod2$pval

resid <- log(df$PriceMSFT) - mod2$pred.iv
armod2 <- arima(resid, order = c(1,0,0))

ggplot() + geom_abline(slope = 0, intercept = 0, color = "blue", size = 1, linetype = "dashed") +
    geom_point(aes(x = mod2$pred.iv, y = armod2$residuals)) + 
    xlab("Fitted Values") + ylab("Residuals") + theme_bw() 

orig_resid <- df$PriceMSFT - exp(mod2$pred.iv)

par(mfrow = c(1, 2))
acf(orig_resid, main = "")
pacf(orig_resid, main = "")
par(mfrow = c(1, 1))



# Solving SES system
G <- c(7.2991799995, -0.0003829650, 0.0001346181, -0.0008498662, -0.0008195646, 0.0011458364)
M <- c(5.220395,  -5.565861e-05, 1.877830e-04, -3.683824e-04, -8.467593e-04, 2.145390e-04)

alpha1 <- G[2]/M[6]
beta1 <- M[2]/G[6]
alpha <- c(G[1] - alpha1 * M[1], alpha1, G[3] - alpha1 * M[3], G[4] - alpha1 * M[4], G[5] - alpha1 * M[5], (1 - alpha1 * beta1)*G[6])
beta <- c(M[1] - beta1 * G[1], beta1, M[3] - beta1 * G[3], M[4] - beta1 * G[4], M[5] - beta1 * G[5], (1 - alpha1 * beta1)*M[6])
alpha
beta







