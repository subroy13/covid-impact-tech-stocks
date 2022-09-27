setwd('D:/Academic/Assignments/Financial_Econometrics_SD/')

###################################
# Data Cleaning and Preprocessing
##################################

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# Processing the Searching Results and Popularity
processdf <- function(filepath) {
    # first join by common columns
    dfList <- list()
    for (i in 1:4) {
        df1 <- read_csv(paste0(filepath, 'grp1_gap', i, '.csv'), skip=2, na="<1")
        df2 <- read_csv(paste0(filepath, 'grp2_gap', i, '.csv'), skip=2, na="<1")
        df1[is.na(df1)] <- 0.5
        df2[is.na(df2)] <- 0.5
        
        common_col <- intersect(colnames(df1), colnames(df2))[2]
        scaling_ratio <- as.vector(df1[, common_col]) / as.vector(df2[, common_col])
        scaling_ratio <- mean(scaling_ratio[scaling_ratio < Inf], na.rm = TRUE)
        
        df2 <- df2 %>% mutate(across(2:4, function(x) { x * scaling_ratio } ))
        
        df <- cbind(df1, df2 %>% select(-intersect(colnames(df1), colnames(df2))))
        df[, common_col] <- (df1[, common_col] + df2[, common_col])/2
        dfList[[i]] <- df
    }
    
    # now join based on days
    for (i in 1:3) {
        df1 <- dfList[[i]]
        df2 <- dfList[[i+1]]
        common_dates <- intersect(df1$Day, df2$Day)
        subdf1 <- as.matrix(df1 %>% filter(Day %in% common_dates) %>% select(-Day))
        subdf2 <- as.matrix(df2 %>% filter(Day %in% common_dates) %>% select(-Day))
        scaling_ratio <- subdf1 / subdf2
        scaling_ratio <- mean(scaling_ratio[scaling_ratio < Inf], na.rm = TRUE)
        
        df2 <- df2 %>% mutate(across(2:4, function(x) { x * scaling_ratio } ))
        dfList[[i+1]] <- df2
    }
    
    outdf <- bind_rows(dfList) %>% group_by(Day) %>% summarise(across(.fns = mean))
    return(outdf)
}

filepath <- './datasets/final/global_search/'
searchdf <- processdf(filepath)
plot(searchdf$Day, searchdf$`covid test: (Worldwide)`, type = "l")


# Processing the covid data
coviddf <- read_csv('./datasets/final/covid_total_cases.csv')
f <- approxfun(x = c(as.Date('2020-01-01'), coviddf$date[-1]), y = c(0, diff(coviddf$World) ))
coviddf <- tibble(Day = searchdf$Day, NewCases = sapply(searchdf$Day, FUN = f)) %>%
    replace_na(list(Day = NA, NewCases = 0))
plot(coviddf$Day, coviddf$NewCases, type = "l")

# Processing the stock data
stockdf <- bind_rows(
    read_csv('./datasets/final/GOOGL_stock.csv') %>% filter(Date %in% coviddf$Day) %>%
        mutate(StockName = "GOOGL") %>% select(Day = Date, StockName, Close),
    read_csv('./datasets/final/MSFT_stock.csv') %>% filter(Date %in% coviddf$Day) %>%
        mutate(StockName = "MSFT") %>% select(Day = Date, StockName, Close),
    read_csv('./datasets/final/ZM_stock.csv') %>% filter(Date %in% coviddf$Day) %>%
        mutate(StockName = "ZM") %>% select(Day = Date, StockName, Close)
) %>% pivot_wider(id_cols = "Day", names_from = "StockName", values_from = "Close")
plot(stockdf$Day, stockdf$ZM, type = "l")


# Combining everything into one data
finaldf <- searchdf %>% left_join(coviddf, by = "Day") %>% left_join(stockdf, by = "Day")
colnames(finaldf) <- c("Day", "TeamsSearch", "SkypeSearch", "MeetSearch", "ZoomSearch", "CovidSearch", "CovidNewCases", "PriceGOOGL", "PriceMSFT", "PriceZM")
write_csv(finaldf, './datasets/final/FinalData.csv')

