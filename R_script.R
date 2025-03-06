# Install needed packages

install.packages("expss")
install.packages("tidyverse")
install.packages("xtable")
install.packages("readxl")
install.packages("stargazer")
install.packages("maditr")
install.packages("dyplr")

library(expss)
library(tidyverse)
library(xtable)
library(readxl)
library(stargazer)
library(dplyr)

# Import sorted portfolio 

portfolio_bm <- read_excel("C:/Users/forti/Downloads/R data/PF_BM.xlsx", 
                           col_types = c("numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", "numeric"))

# Import Fama&French 3 factors

ff_factor <- read_excel("C:/Users/forti/Downloads/R data/FF_DAILY.xlsx", col_types = c("numeric", "numeric", "numeric", "numeric", "numeric"))

colnames(ff_factor)[1] <- "DATE"
colnames(ff_factor)[2] <- "MKT"
colnames(portfolio_bm)[1] <- "DATE"

ff_factor <- na.omit(ff_factor)
portfolio_bm <- na.omit(portfolio_bm)

# Put DATE into the DATE Format
portfolio_bm$DATE <- as.character(portfolio_bm$DATE)  # Convert to character 
ff_factor$DATE <- as.character(ff_factor$DATE)

portfolio_bm$DATE <- as.Date(paste0(substr(portfolio_bm$DATE, 1, 4), "-", 
                                    substr(portfolio_bm$DATE, 5, 6), "-", 
                                    substr(portfolio_bm$DATE, 7, 8)))

ff_factor$DATE <- as.Date(paste0(substr(ff_factor$DATE, 1, 4), "-", 
                                 substr(ff_factor$DATE, 5, 6), "-", 
                                 substr(ff_factor$DATE, 7, 8)))

data1 <- inner_join(ff_factor, portfolio_bm, by = "DATE")



# compute excess returns
data1[,6:15] <- data1[,6:15] - data1$RF

# compute difference portfolio

data1$LMH <- data1$`Lo 10` - data1$`Hi 10`

#####5a) CAPM analysis

# prepare table for output
results_CAPM <- matrix(nrow=5, ncol=12)
colnames(results_CAPM) <- c(' ', 'Lo 10', 'Dec 2', 'Dec 3', 'Dec 4', 'Dec 5', 
                            'Dec 6', 'Dec 7', 'Dec 8', 'Dec 9', 'Hi 10', 'LMH')
results_CAPM[1:5,1] <- c("CAPM Alpha", "t-stat", "CAPM Beta", "t-stat", "CAPM R^2")

# run time series regression for each portfolio

for(i in 1:11){
  if (i < 11) reg1 <- lm( data1[[5+i]] ~ MKT , data=data1)
  else reg1 <- lm(data1$LMH ~ MKT , data=data1)  # LMH Portfolio
  reg1_summary <- summary(reg1)
  results_CAPM[1,1+i] <- round(((1+reg1$coefficients[1]/100)^252-1)*100, digits=3)
  results_CAPM[2,1+i] <- round(reg1_summary$coefficients[1,3], digits=3)
  results_CAPM[3,1+i] <- round(reg1$coefficients[2], digits=3)
  results_CAPM[4,1+i] <- round(reg1_summary$coefficients[2,3], digits=3)
  results_CAPM[5,1+i] <- round(reg1_summary$adj.r.squared, digits=3)
}

print(xtable(results_CAPM, type='latex')) 

#####5b) Fama-French 3-factor model

# prepare table for output

results_FF <- matrix(nrow=9, ncol=11)
colnames(results_FF) <- c(' ', 'Lo 10', 'Dec 2', 'Dec 3', 'Dec 4', 'Dec 5', 
                          'Dec 6', 'Dec 7', 'Dec 8', 'Dec 9', 'Hi 10')
results_FF[1:9,1] <- c("FF Alpha", "t-stat", "Market Beta", "t-stat",
                       "Size Beta", "t-stat", "Value Beta", "t-stat", "FF R^2")

# run time series regression for each portfolio

for(i in 1:10){
  reg2 <- lm( data1[[5+i]] ~ MKT + SMB + HML , data=data1)
  reg2_summary <- summary(reg2)
  results_FF[1,1+i] <- round(((1+reg2$coefficients[1]/100)^252-1)*100, digits=3)
  results_FF[2,1+i] <- round(reg2_summary$coefficients[1,3], digits=3)
  results_FF[3,1+i] <- round(reg2$coefficients[2], digits=3)
  results_FF[4,1+i] <- round(reg2_summary$coefficients[2,3], digits=3)
  results_FF[5,1+i] <- round(reg2$coefficients[3], digits=3)
  results_FF[6,1+i] <- round(reg2_summary$coefficients[3,3], digits=3)
  results_FF[7,1+i] <- round(reg2$coefficients[4], digits=3)
  results_FF[8,1+i] <- round(reg2_summary$coefficients[4,3], digits=3)
  results_FF[9,1+i] <- round(reg2_summary$adj.r.squared, digits=3) 
}
print(xtable(results_FF, type='latex')) 

#####5c) Add 2 other factor: 

#1. Momentum -> Highest stock returns minus lowest stock returns over 1 year (252 trading days)

# Calculate past cumulative returns (252-day rolling returns)
# Stock Returns are split up into deciles -> Highest stock returns in Hi10 and lowest stock returns in Lo 10
momentum_data <- portfolio_bm %>%
  arrange(DATE) %>%
  mutate(across(starts_with("Decile"), ~ rollapply(.x, width = 252, FUN = prod, fill = NA) - 1))

# Identify "Winner" and "Loser" Portfolios
# Winners: Top decile, Losers: Bottom decile
winner_returns <- momentum_data$`Hi 10`    
loser_returns <- momentum_data$`Lo 10`   

# Create the MOM Factor
momentum_data$MOM <- winner_returns - loser_returns
momentum_data <- na.omit(momentum_data)
mean(momentum_data$MOM)

# Merge the two datasets to add the MOM factor to data1
data1.1 <- merge(data1, momentum_data[c("MOM", "DATE")], by = "DATE", all.x = TRUE)
mean(data1.1$MOM)

#Add MOM factor to Fama&Frech 3 Factor Model
# prepare table for output

results_4F <- matrix(nrow=11, ncol=11)
colnames(results_4F) <- c(' ', 'Lo 10', 'Dec 2', 'Dec 3', 'Dec 4', 'Dec 5', 
                          'Dec 6', 'Dec 7', 'Dec 8', 'Dec 9', 'Hi 10')
results_4F[1:11,1] <- c("FF Alpha", "t-stat", "Market Beta", "t-stat",
                       "Size Beta", "t-stat", "Value Beta", "t-stat", "Momentum Beta", "t-stat", "R^2")

# run time series regression for each portfolio 

for(i in 1:10){
  reg3 <- lm( data1.1[[5+i]] ~ MKT + SMB + HML + MOM , data=data1.1)
  reg3_summary <- summary(reg3)
  results_4F[1,1+i] <- round(((1+reg3$coefficients[1]/100)^252-1)*100, digits=3)
  results_4F[2,1+i] <- round(reg3_summary$coefficients[1,3], digits=3)
  results_4F[3,1+i] <- round(reg3$coefficients[2], digits=3)
  results_4F[4,1+i] <- round(reg3_summary$coefficients[2,3], digits=3)
  results_4F[5,1+i] <- round(reg3$coefficients[3], digits=3)
  results_4F[6,1+i] <- round(reg3_summary$coefficients[3,3], digits=3)
  results_4F[7,1+i] <- round(reg3$coefficients[4], digits=3)
  results_4F[8,1+i] <- round(reg3_summary$coefficients[4,3], digits=3)
  results_4F[9,1+i] <- round(reg3$coefficients[5], digits=3)
  results_4F[10,1+i] <- round(reg3_summary$coefficients[5,3], digits=3)
  results_4F[11,1+i] <- round(reg3_summary$adj.r.squared, digits=3) 
}
print(xtable(results_4F, type='latex')) 

##### Question 6, Fama-MacBeth

#Import dataset with 25 sorted portfolios formed on Size and Book-to-Market Ratio

library(readr)

portfolio_bm_size <- read_csv("C:/Users/forti/Downloads/R data/PF_BM_SIZE_v100.csv")
View(portfolio_bm_size)
                               

# Import Fama & Frech factors again but as monthly data (needed for merging with the monthly data of the 25 sorted portfolios)
ff_factors_monthly <- read_csv("C:/Users/forti/Downloads/R data/FF_MONTHLY_v100.csv")

colnames(ff_factors_monthly)[1] <- "DATE"
colnames(ff_factors_monthly)[2] <- "MKT"
colnames(portfolio_bm_size)[1] <- "DATE"


portfolio_bm_size <- na.omit(portfolio_bm_size)
ff_factors_monthly <- na.omit(ff_factors_monthly)

#Merge the dataset with the factors

# Put DATE into the DATE Format
portfolio_bm_size$DATE <- as.character(portfolio_bm_size$DATE)  # Convert to character 
ff_factors_monthly$DATE <- as.character(ff_factors_monthly$DATE)

portfolio_bm_size$DATE <- as.Date(paste0(substr(portfolio_bm_size$DATE, 1, 4), "-", substr(portfolio_bm_size$DATE, 5, 6), "-01"))
ff_factors_monthly$DATE <- as.Date(paste0(substr(ff_factors_monthly$DATE, 1, 4), "-", substr(ff_factors_monthly$DATE, 5, 6), "-01"))



data2 <- inner_join(ff_factors_monthly, portfolio_bm_size, by = "DATE")

data2[,6:30] <- data2[,6:30] - data2$RF

# Create Difference Portfolio (BIG HiBM - Small LoBM)
data2$DIFF <- data2$`BIG HiBM` - data2$`SMALL LoBM`

### 1. Fama-Macbeth with Full-Sample Betas

# 1. time series regression for every test asset to get full-sample betas

betas <- NULL
for(i in 1:25){
  reg4 <- lm( data2 [[5+i]] ~ MKT + SMB + HML + DIFF , data=data2)
  betas <- rbind(betas, reg4$coefficients[2:5])
}

# 2. cross-sectional regression for every month

lambdas <- NULL
for(t in 1:nrow(data2)){
  reg5 <- lm( t(data2[t,6:30]) ~ betas )
  lambdas <- rbind(lambdas, reg5$coefficients)
}

# Compute average lambdas

colMeans(lambdas)

# Compute t-stats for average lambdas

lambdas_tstatistic <- nrow(lambdas)^0.5 * colMeans(lambdas)/apply(lambdas, 2, sd)

stargazer(colMeans(lambdas), type = "latex", out = "meanlambdas.latex")     
stargazer(lambdas_tstatistic, type = "latex", out = "lambdas_tstat.latex")     


###2. Fama-Macbeth with Rolling Betas (Rolling Window of 5 years/60 months)

# Introduce rolling betas

rolling_betas <- array(NA, dim = c(25, nrow(data2) - 59, 4))  # 25 assets, rolling months, 4 factors
dates <- data2$DATE[-(1:59)] # the first 59 months are excluded since rolling betas start from the 60th month (-> after 5 years)

for (i in 1:25) {
  for (t in 60:nrow(data2)) {
    rollingbetas_data <- data2[(t-59):t, ]  # Past 60 months
    reg6 <- lm(rollingbetas_data[[5 + i]] ~ rollingbetas_data$MKT + rollingbetas_data$SMB + 
                 rollingbetas_data$HML + rollingbetas_data$DIFF)
    
    rolling_betas[i, t - 59, ] <- reg6$coefficients[-1]  # Store only factor betas
  }
}
#Cross sectional regression with Rolling betas

rolling_lambdas <- matrix(NA, nrow = ncol(rolling_betas), ncol = 5)  # Store lambda estimates

for (t in 1:(nrow(data2) - 59)) {
  excess_returns_t <- as.numeric(data2[t + 59, 6:30])  # Monthly returns for assets
  rolling_betas_t <- rolling_betas[, t, ]  # Corresponding betas
  
  reg7 <- lm(excess_returns_t ~ rolling_betas_t)  # Cross-sectional regression
  rolling_lambdas[t, ] <- reg7$coefficients 
}

 
# Averages and T-Statistics
avg_rolling_lambdas <- colMeans(rolling_lambdas, na.rm=TRUE)  # Average lambda estimates
rolling_lambdas_tstatistic <- nrow(rolling_lambdas)^0.5 * avg_rolling_lambdas/apply(rolling_lambdas, 2, sd)    # T-stats

stargazer(avg_rolling_lambdas, type = "latex", out = "rolling_lambdas.latex")     
stargazer(rolling_lambdas_tstatistic, type = "latex", out = "rolling_lambdas_tstat.latex")   


  

######### 4) Descriptive Statistics (At the bottom because some new variables are created during the code)
  
 
#a) Descriptive Statistics for the BM-sorted Portfolio and its factors/Difference Portfolios

 ### Create Summary Statistics Table
  
  # Summary Statistics Dataset
  # to use the select function, we downloaded the tidyverse package
  
  datasummary <- select(data1.1, c(MKT, SMB, HML, LMH, MOM, RF, `Lo 10`, `Dec 2`, `Dec 3`, `Dec 4`, `Dec 5`, `Dec 6`, `Dec 7`, `Dec 8`, `Dec 9`, `Hi 10`))
  
  # Max for each column
  max <- apply(datasummary, 2, max)
  
  # Min for each column
  min <- apply(datasummary, 2, min)
  
  # Means for each column
  means <- apply(datasummary, 2, mean)
  
  # Medians for each column
  medians <- apply(datasummary, 2, median)
  
  # 10th percentile for each column
  percentiles_10 <- apply(datasummary, 2, function(x) quantile(x, 0.1, na.rm=TRUE))
  
  # 90th percentile for each column
  percentiles_90 <- apply(datasummary, 2, function(x) quantile(x, 0.9, na.rm=TRUE))
  
  # Standard deviations for each column
  standard_deviations <- apply(datasummary, 2, sd)
  
  # Number of observations
  obs <- apply(datasummary,2,length)
  
  
  
  #Create the Summary Statistics table
  descriptive_table <- cbind("Min" = min, "10th Quantile" = percentiles_10, "Median" = medians, "Mean" = means, "90th Quantile" = percentiles_90, "Max" = max, "S.D." = standard_deviations, "No. Observations" = obs)
  print(xtable(descriptive_table, type='text')) 
  
  #To print out the Summary Statistics table (with the stargazer package)
  stargazer(descriptive_table, type = "latex", out = "DESCRIPTIVDATA.html")    
  
  
#b) Descriptive Statistics for the 25 sorted Portfolios sorted after size and BM
  
  datasummary2 <- select(data2, c(`SMALL LoBM`, `SMALL HiBM`, `ME2 BM1`, `ME2 BM5`, `ME3 BM1`, `ME3 BM5`, `ME4 BM1`, `ME4 BM5`,  `BIG LoBM`, `BIG HiBM`, DIFF))
  
  # Max for each column
  max <- apply(datasummary2, 2, max)
  
  # Min for each column
  min <- apply(datasummary2, 2, min)
  
  # Means for each column
  means <- apply(datasummary2, 2, mean)
  
  # Medians for each column
  medians <- apply(datasummary2, 2, median)
  
  # 10th percentile for each column
  percentiles_10 <- apply(datasummary2, 2, function(x) quantile(x, 0.1, na.rm=TRUE))
  
  # 90th percentile for each column
  percentiles_90 <- apply(datasummary2, 2, function(x) quantile(x, 0.9, na.rm=TRUE))
  
  # Standard deviations for each column
  standard_deviations <- apply(datasummary2, 2, sd)
  
  # Number of observations
  obs <- apply(datasummary2,2,length)
  
  
  #Create the Summary Statistics table
  descriptive_table2 <- cbind("Min" = min, "10th Quantile" = percentiles_10, "Median" = medians, "Mean" = means, "90th Quantile" = percentiles_90, "Max" = max, "S.D." = standard_deviations, "No. Observations" = obs)
  print(xtable(descriptive_table2, type='text')) 
  
  #To print out the Summary Statistics table (with the stargazer package)
  stargazer(descriptive_table2, type = "latex", out = "DESCRIPTIVDATA2.html")  
  
  
  
  
