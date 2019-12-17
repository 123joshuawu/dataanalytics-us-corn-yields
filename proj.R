
library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)

data <- read.csv('~/dataanalytics/proj/FeedGrains.csv')

View(data)

data$SC_Group_ID <- as.factor(data$SC_Group_ID)
# summary(data$SC_Group_ID)

# summary(data$SC_Group_Desc)

data$SC_GroupCommod_ID <- as.factor(data$SC_GroupCommod_ID)
# summary(data$SC_GroupCommod_ID)

# summary(data$SC_Group_Desc)

data$SC_Geography_ID <- as.factor(data$SC_Geography_ID)
# summary(data$SC_Geography_ID)

# summary(data$SortOrder)

# summary(data$SC_GeographyIndented_Desc)

data$SC_Commodity_ID <- as.factor(data$SC_Commodity_ID)
# summary(data$SC_Commodity_ID)

# summary(data$SC_Commodity_Desc)

data$SC_Attribute_ID <- as.factor(data$SC_Attribute_ID)
# summary(data$SC_Attribute_ID)

# summary(data$SC_Attribute_Desc)

data$SC_Unit_ID <- as.factor(data$SC_Unit_ID)
# summary(data$SC_Unit_ID)

# summary(data$SC_Unit_Desc)

#data$Year_ID <- as.factor(data$Year_ID)
# summary(data$Year_ID)

data$SC_Frequency_ID <- as.factor(data$SC_Frequency_ID)
# summary(data$SC_Frequency_ID)

# summary(data$SC_Frequency_Desc)

data$Timeperiod_ID <- as.factor(data$Timeperiod_ID)
# summary(data$Timeperiod_ID)

# summary(data$Timeperiod_Desc)

# summary(data$Amount)

corn <- data %>% filter(data$SC_GroupCommod_Desc == 'Corn')

#######

marketprice <- corn %>% filter(corn$SC_Attribute_Desc == 'Prices, market')

dollarsperbushel <- marketprice %>% filter(marketprice$SC_Unit_Desc == 'Dollars per bushel')

yellowcorn_dpb_mp <- dollarsperbushel %>% filter(dollarsperbushel$SC_Commodity_Desc == 'Corn, No. 2 yellow')
whitecorn_dpb_mp <- dollarsperbushel %>% filter(dollarsperbushel$SC_Commodity_Desc == 'Corn, No. 2 white')

yellowcorn_annual_dpb_mp <- filter(yellowcorn_dpb_mp, Timeperiod_ID == 31)
whitecorn_annual_dpb_mp <- filter(whitecorn_dpb_mp, Timeperiod_ID == 31)

# yellow corn has price for each source
plot(yellowcorn_annual_dpb_mp$Year_ID, yellowcorn_annual_dpb_mp$Amount, main='Yellow Corn Prices', xlab='Year', ylab='Price (Dollars per bushel)')
plot(whitecorn_annual_dpb_mp$Year_ID, whitecorn_annual_dpb_mp$Amount, main='White Corn Prices', xlab='Year', ylab='Price (Dollars per bushel)', type = 'l')

corn_annual_b_imports <- corn %>% filter(SC_Attribute_Desc == 'Imports, to U.S. from specified source', SC_Geography_ID == 25, Timeperiod_ID == 31, SC_Commodity_Desc == 'Corn', SC_Unit_Desc == 'Bushels')
plot(corn_annual_b_imports$Year_ID, corn_annual_b_imports$Amount, main='Total Annual Corn Imports', xlab='Year', ylab='Bushels', type='l')

corn_annual_b_exports <- corn %>% filter(SC_Attribute_Desc == 'Exports, from U.S. to specified destination', SC_Geography_ID == 25, Timeperiod_ID == 31, SC_Commodity_Desc == 'Corn', SC_Unit_Desc == 'Bushels')
plot(corn_annual_b_exports$Year_ID, corn_annual_b_exports$Amount, main='Total Annual Corn Exports', xlab='Year', ylab='Bushels', type='l')

corn_annual_mb_supply <- corn %>% filter(SC_Attribute_Desc == 'Total supply', SC_Unit_Desc == 'Million bushels', Timeperiod_ID == '31')
plot(corn_annual_mb_supply$Year_ID, corn_annual_mb_supply$Amount, xlab = 'Year', ylab = 'Million bushels', main = 'Total Corn Supply', type='l')

corn_annual_mb_production <- corn %>% filter(SC_Attribute_Desc == 'Production', SC_GroupCommod_Desc == 'Corn', Timeperiod_Desc == 'Commodity Market Year', SC_GeographyIndented_Desc == 'United States')
corn_annual_mb_production <- corn_annual_mb_production[order(corn_annual_mb_production$Year_ID), ]
plot(corn_annual_mb_production$Year_ID, corn_annual_mb_production$Amount, xlab = 'Year', ylab = 'Million bushels', main = 'Total Corn Production US', type = 'l')

corn_annual_mb_domesticuse <- corn %>% filter(SC_Attribute_Desc == 'Total domestic use', Timeperiod_ID == 31)
plot(corn_annual_mb_domesticuse$Year_ID, corn_annual_mb_domesticuse$Amount, main = 'Total Domestic Corn Usage', xlab = 'Year', ylab = 'Million bushels', type = 'l')

corn_annual_bpa_yield <- corn %>% filter(SC_Attribute_Desc == 'Yield per harvested acre', Timeperiod_ID == 69)
plot(corn_annual_bpa_yield$Year_ID, corn_annual_bpa_yield$Amount, main = 'Yield per acre', xlab = 'Commodity Market Year', ylab = 'Bushels per acre', type = 'l')



time_line <- function(d) {
  attr_desc <- levels(droplevels(d$SC_Attribute_Desc))
  timeper_desc <- levels(droplevels(d$SC_Frequency_Desc))
  unit_desc <- levels(droplevels(d$SC_Unit_Desc))
  stopifnot(length(attr_desc) == 1)
  stopifnot(length(timeper_desc) == 1)
  stopifnot(length(unit_desc) == 1)
  plot(d$Year_ID, d$Amount, main = attr_desc, xlab = timeper_desc, ylab = unit_desc, type = 'l')
}

attr_time_line <- function(d, t="") {
  #timeper_desc <- levels(droplevels(d$SC_Frequency_Desc))
  unit_desc <- levels(droplevels(d$SC_Unit_Desc))
  #stopifnot(length(timeper_desc) == 1)
  stopifnot(length(unit_desc) == 1)
  ggplot(d, aes(Year_ID, Amount, color = SC_Attribute_Desc)) +
    geom_line() +
    labs(x = "Year", y = unit_desc, color = "") +
    ggtitle(t)
}

commod_time_line <- function(d, t="") {
  unit_desc <- levels(droplevels(d$SC_Unit_Desc))
  stopifnot(length(unit_desc) == 1)
  ggplot(d, aes(Year_ID, Amount, color = SC_Commodity_Desc)) +
    geom_line() +
    labs(x = "Year", y = unit_desc, color = "") +
    ggtitle(t)
}

corn_acreages <- corn %>% 
  filter(SC_Attribute_Desc %in% c("Harvested acreage", "Planted acreage") & 
           SC_Unit_Desc == "Million acres" & 
           Year_ID > 1925) %>%
  select(SC_Attribute_Desc, Amount, SC_Unit_Desc, Timeperiod_Desc, Year_ID)
attr_time_line(corn_acreages)

corn_amts <- rbind(corn_annual_mb_domesticuse, corn_annual_mb_production, corn_annual_mb_supply) %>% 
  filter(Year_ID > 1975, SC_Frequency_Desc == "Annual") %>%
  select(SC_Attribute_Desc, Amount, SC_Unit_Desc, Timeperiod_Desc, Year_ID)
attr_time_line(corn_amts, "US Corn Statistics After 1975")

corn_usages <- corn %>% 
  filter(str_detect(SC_Attribute_Desc, "use") & 
           SC_Frequency_Desc == "Annual" &
           Amount > 1000 &
           SC_Unit_Desc == "Million bushels" &
           SC_Attribute_Desc != "Total domestic use") %>%
  select(SC_Attribute_Desc, Amount, SC_Unit_Desc, Timeperiod_Desc, Year_ID)
attr_time_line(corn_usages, "Types of Corn Usage Greater than 1000 Million Bushels")

# Supply and use
supp <- data %>% filter(SC_Group_Desc == "Supply and use" & SC_Frequency_Desc == "Annual")
commod_time_line(supp %>% filter(SC_Attribute_Desc == "Planted acreage"), "US Planted Acreages of Main Feed Grains")
commod_time_line(supp %>% filter(SC_Attribute_Desc == "Harvested acreage" & SC_Unit_Desc == "Million acres"), "US Harvested Acreages of Main Feed Grains")
#commod_time_line(supp %>% filter(SC_Attribute_Desc == "Domestic disappearance"), "Domestic disappearance")
commod_time_line(supp %>% filter(SC_Attribute_Desc == "Total domestic use" & SC_Unit_Desc == "Million bushels"), "US Total Domestic Usage of Main Feed Grains")
commod_time_line(supp %>% filter(SC_Attribute_Desc == "Yield per harvested acre" & SC_Unit_Desc == "Bushels per acre"), "US Yields of Main Feed Grains")
commod_time_line(supp %>% filter(SC_Attribute_Desc == "Total supply" & SC_Unit_Desc == "Million bushels"), "US Total Supply of Main Feed Grains")
commod_time_line(supp %>% filter(SC_Attribute_Desc == "Imports, market year" & SC_Unit_Desc == "Million bushels"), "US Imports of Main Feed Grains")
#commod_time_line(supp %>% filter(SC_Attribute_Desc == "Imports, trade year"))
commod_time_line(supp %>% filter(SC_Attribute_Desc == "Exports, market year" & SC_Unit_Desc == "Million bushels"), "US Exports of Main Feed Grains")

corn_exp_loc <- function (d, t="") {
  unit_desc <- levels(droplevels(d$SC_Unit_Desc))
  stopifnot(length(unit_desc) == 1)
  ggplot(d, aes(Year_ID, Amount, color = SC_GeographyIndented_Desc)) + 
    geom_line() +
    ggtitle(t) +
    labs(x = "Year", y = unit_desc, color = "")
}

corn_exp <- corn %>% filter(SC_Attribute_Desc == "Exports, from U.S. to specified destination" & SC_GeographyIndented_Desc != "World")

corn_exp_loc(corn_exp %>% 
               filter(SC_Commodity_Desc == "Corn" &
                        Amount > 10000),
             "Corn export")
corn_exp_loc(corn_exp %>%
               filter(SC_Commodity_Desc == "Corn grain" &
                        Amount > 10000),
             "Corn grain export")
corn_exp_loc(corn_exp %>%
               filter(SC_Commodity_Desc == "Corn products" &
                        Amount > 1000),
             "Corn products export")

corn_imp <- corn %>% 
  filter(SC_Attribute_Desc == "Imports, to U.S. from specified source" &
           SC_GeographyIndented_Desc != "World" &
           SC_Frequency_Desc == "Annual")
corn_exp_loc(corn_imp %>%
               filter(SC_Commodity_Desc == "Corn" &
                        Amount > 400),
             "Corn import")  

#-----------------------------------------------------------
# Examining Corn Yield
# Break between values before and after 1940
# Greater residuals after 1940
corn_yield <- corn %>% 
  filter(SC_Attribute_Desc == "Yield per harvested acre" & 
           SC_Unit_Desc == "Bushels per acre") %>%
  select(Amount, Year_ID)
cy_a1940 <- corn_yield %>% filter(Year_ID >= 1940)
cy_b1940 <- corn_yield %>% filter(Year_ID < 1940)
ggplot(corn_yield,
       aes(Year_ID, Amount)) + 
  geom_line() +
  ggtitle("US Corn Yields Between 1866 and 2019") + 
  labs(x = "Year", y = "Bushels per Acre")

# LOESS model
corn_yield_loess <- loess(Amount ~ Year_ID, corn_yield)
plot(corn_yield_loess$x, 
     corn_yield_loess$fitted, 
     type="l",
     xlab = "Year",
     ylab = "Bushels per Acre",
     main = "US Corn Yields with LOESS regression",
     col = 'red')
lines(corn_yield_loess)
corn_yield_loess_df <- data.frame("Year"=corn_yield_loess$x, 
                                  "Residual"=corn_yield_loess$residuals)
corn_yield_loess_df$PercentageResidual <- mapply(function (x, y) x / y, 
                                                 corn_yield_loess_df$Residual, 
                                                 corn_yield_loess$fitted)
# US Corn Yield Residuals
ggplot(corn_yield_loess_df, 
       aes(Year_ID, Residual)) +
  scale_fill_gradient(low="red", high="green") +
  #  scale_color_continuous(low="red", high="green") +
  geom_col(aes(fill=Residual)) +
  guides(fill = FALSE) +
  labs(x = "Year", y = "Bushels per Acre") +
  ggtitle("US Corn Yield Residuals")

# Ratio of Residual to Fitted Corn Yield
ggplot(corn_yield_loess_df, 
       aes(Year_ID, PercentageResidual)) +
  scale_fill_gradient(low="red", high="green") +
  #  scale_color_continuous(low="red", high="green") +
  geom_col(aes(fill=PercentageResidual)) +
  guides(fill = FALSE) + 
  labs(x = "Year", y = "") +
  ggtitle("US Corn Yield Ratio of Residual to Fitted")


# Linear models of two segments
corn_yield_linear_model_B1940 <- lm(cy_b1940$Amount ~ cy_b1940$Year_ID)
corn_yield_linear_model_A1940 <- lm(cy_a1940$Amount ~ cy_a1940$Year_ID)
# Plot two segments
plot(corn_yield$Year_ID, 
     corn_yield$Amount, 
     type="l",
     xlab = 'Year',
     ylab = 'Bushels per Acre',
     main = 'US Corn Yields with Linear Fit of Before and After 1940')
lines(cy_b1940$Year_ID, corn_yield_linear_model_B1940$fitted.values, type="l", col="red")
lines(cy_a1940$Year_ID, corn_yield_linear_model_A1940$fitted.values, type="l", col="red")
abline(v = 1940)

# Plot residuals of two segments
ggplot(
  data = data.frame(
    Year=corn_yield$Year_ID, 
    Residual=c(corn_yield_linear_model_B1940$residuals, 
               corn_yield_linear_model_A1940$residuals)),
  aes(x = Year, y = Residual)) +
  scale_fill_gradient(low="red", high="green") +
  geom_col(aes(fill=Residual)) +
  guides(fill = FALSE) +
  labs(x = "Year", y = "Bushels per Acre") +
  ggtitle("US Corn Yield Residuals of Linear Fits of Before and After 1940") +
  geom_vline(xintercept = 1940)

# Normalized residuals
ggplot(
  data = data.frame(
    Year=corn_yield$Year_ID, 
    Residual=mapply(function (x, y) x / y, 
                    c(corn_yield_linear_model_B1940$residuals, 
                      corn_yield_linear_model_A1940$residuals),
                    c(corn_yield_linear_model_B1940$fitted.values,
                      corn_yield_linear_model_A1940$fitted.values))),
  aes(x = Year, y = Residual)) +
  scale_fill_gradient(low="red", high="green") +
  geom_col(aes(fill=Residual)) +
  guides(fill = FALSE) +
  labs(x = "Year", y = "Bushels per Acre") +
  ggtitle("US Corn Yield Ratios of Residual to Fitted of Before and After 1940") +
  geom_vline(xintercept = 1940)

# Total Corn production
ggplot(corn_annual_mb_production,
       aes(Year_ID, Amount)) + 
  geom_line()
corn_prod_loess <- loess(Amount ~ Year_ID, corn_annual_mb_production)
plot(corn_annual_mb_production$Year_ID, 
     corn_annual_mb_production$Amount, 
     type = "l",
     xlab = "Year",
     ylab = "Millions of Bushels",
     main = "US Total Corn Production with LOESS Regression")
lines(corn_prod_loess$x, corn_prod_loess$fitted, col = 'red')

corn_prod_resid <- data.frame('Year'=corn_prod_loess$x,
                   'Residuals'=corn_prod_loess$residuals)
ggplot(corn_prod_resid, 
       aes(Year_ID, Residuals)) +
  scale_fill_gradient(low="red", high="green") +
  #  scale_color_continuous(low="red", high="green") +
  geom_col(aes(fill=Residuals)) +
  guides(fill = FALSE) +
  labs(x = "Year", y = "Millions of bushels") +
  ggtitle("US Total Corn Production Residuals")
corn_prod_resid$ResidRat <- mapply(function (x, y) x / y,
                                    corn_prod_resid$Residuals,
                                    corn_prod_loess$fitted)
ggplot(corn_prod_resid, 
       aes(Year_ID, ResidRat)) +
  scale_fill_gradient(low="red", high="green") +
  #  scale_color_continuous(low="red", high="green") +
  geom_col(aes(fill=ResidRat)) +
  guides(fill = FALSE) +
  labs(x = "Year", y = "Ratio") +
  ggtitle("US Total Corn Production Ratios of Residual to Fitted")

# Total Corn Harvested Acreage
corn_harvest <- corn %>% filter(SC_Attribute_Desc == "Harvested acreage"& 
                                  SC_Unit_Desc == "Million acres")
ggplot(corn_harvest, aes(Year_ID, Amount)) + 
  geom_line()
corn_harvest_loess <- loess(Amount ~ Year_ID, corn_harvest)
plot(corn_harvest$Year_ID, 
     corn_harvest$Amount, 
     type = "l",
     xlab = "Year",
     ylab = "Millions of Bushels",
     main = "US Total Corn Production with LOESS Regression")
lines(corn_harvest_loess$x, corn_harvest_loess$fitted, col = 'red')

corn_harvest_resid <- data.frame('Year'=corn_harvest_loess$x,
                              'Residuals'=corn_harvest_loess$residuals)
ggplot(corn_harvest_resid, 
       aes(Year_ID, Residuals)) +
  scale_fill_gradient(low="red", high="green") +
  #  scale_color_continuous(low="red", high="green") +
  geom_col(aes(fill=Residuals)) +
  guides(fill = FALSE) +
  labs(x = "Year", y = "Millions of bushels") +
  ggtitle("US Corn Harvested Acreage Residuals")
corn_harvest_resid$ResidRat <- mapply(function (x, y) x / y,
                                   corn_harvest_resid$Residuals,
                                   corn_harvest_loess$fitted)
ggplot(corn_harvest_resid, 
       aes(Year_ID, ResidRat)) +
  scale_fill_gradient(low="red", high="green") +
  #  scale_color_continuous(low="red", high="green") +
  geom_col(aes(fill=ResidRat)) +
  guides(fill = FALSE) +
  labs(x = "Year", y = "Ratio") +
  ggtitle("US Corn Harvested Acreage Ratios of Residual to Fitted")

# Absolute Values of Residuals
corn_yield_loess_df$AbsResidual <- abs(corn_yield_loess_df$Residual)
corn_yield_loess_df$AbsPercResidual <- abs(corn_yield_loess_df$PercentageResidual)*100
corn_yield_abs_resid <- melt(corn_yield_loess_df,
                             id.vars = c("Year_ID"),
                             measure.vars = c(
                               "AbsPercResidual",
                               "AbsResidual"
                             ))
ggplot(corn_yield_abs_resid, 
       aes(x=Year_ID, y=value, color=variable)) + 
  scale_y_continuous(
    position='right',
    sec.axis = sec_axis(~./100, name="Percentage of Fitted")) + 
  geom_segment(aes(xend=Year_ID,yend=0), alpha=0.5) + 
  geom_smooth(method='loess', 
              se=FALSE,
              span=2) +
  labs(x = "Year", y = "Bushels per Acre") +
  ggtitle("Comparing US Corn Yield Residuals to Residual Percentages") +
  scale_color_discrete(name = "",
                       breaks = c("AbsPercResidual",
                                  "AbsResidual"),
                       labels = c("Residual as Percentage of Fitted",
                                  "Raw Residual"))
  

#   Percentage Residuals
plot(corn_yield_loess_df$PercentageResidual,
     corn_prod_resid$ResidRat,
     main="US Corn Production and Yield Residual Ratios",
     xlab="US Corn Yield Residual Ratio to Fitted",
     ylab="US Total Corn Production Residual Ratio to Fitted")
# Linear model of Yield residuals and Production Residuals
corn_prod_yield_residrat_lm <- lm(prod ~ yield, data = data.frame("prod"=corn_prod_resid$ResidRat, "yield"=corn_yield_loess_df$PercentageResidual))
lines(corn_yield_loess_df$PercentageResidual, 
      corn_prod_yield_residrat_lm$fitted, 
      col="red")
summary(corn_prod_yield_residrat_lm)
par(mfrow=c(2, 2))
plot(corn_prod_yield_residrat_lm)
par(mfrow = c(1, 1))

# Harvested Acreage and Production
plot(corn_harvest$Amount, corn_annual_mb_production$Amount)
#   Residuals
plot(corn_harvest_loess$residuals, corn_prod_loess$residuals)
#   Percentage Residuals
plot(corn_harvest_resid$ResidRat, corn_prod_resid$ResidRat)
par(mfrow=c(1, 1))



####

scale <- function (x) (x - min(x))/max(x)

# plot normalized values
corn_yield$ScaledAmount <- scale(corn_yield$Amount)
corn_annual_mb_production$ScaledAmount <- 
  scale(corn_annual_mb_production$Amount)
corn_plant <- corn %>% 
  filter(SC_Attribute_Desc == 'Planted acreage')
corn_plant$ScaledAmount <- scale(corn_plant$Amount)

ggplot(mapping = aes(Year_ID, ScaledAmount)) +
  geom_line(data = corn_yield, 
            aes(color = 'red')) +
  geom_line(data = corn_annual_mb_production, 
            aes(color = 'blue')) +
  geom_line(data = corn_plant,
            aes(color = 'green'))


# old two segment corn yield
attach(cy_b1940)
par(mfrow = c(1, 1))
#plot(Amount ~ Year_ID)
#corn_yield_linear_model_B1940 <- lm(Amount ~ Year_ID)
#abline(corn_yield_linear_model_B1940)
ggplot(corn_yield_linear_model_B1940, aes(Year_ID, Amount)) +
  geom_smooth(method='lm') +
  geom_point() +
  ggtitle("Corn Yield Residuals before 1940")
par(mfrow = c(2, 2))
plot(corn_yield_linear_model_B1940)
par(mfrow = c(1, 1))
detach(cy_b1940)
attach(cy_a1940)
#plot(Amount ~ Year_ID)
#corn_yield_linear_model_A1940 <- lm(Amount ~ Year_ID)
#abline(corn_yield_linear_model_A1940)
ggplot(corn_yield_linear_model_A1940, aes(Year_ID, Amount)) +
  geom_smooth(method='lm') +
  geom_line() +
  ggtitle("Corn Yields after 1940")
par(mfrow = c(2, 2))
plot(corn_yield_linear_model_A1940)
par(mfrow = c(1, 1))
detach(cy_a1940)

#
par(mfrow = c(2, 2))
plot(corn_yield_loess)
par(mfrow = c(1, 1))



# Comparing variance of yield
#  before 1940
anova(corn_yield_linear_model_B1940)
#  after 1940
anova(corn_yield_linear_model_A1940)

# Plots of important variables
par(mfrow = c(2, 2))
plot(corn_annual_mb_production$Year_ID, corn_annual_mb_production$Amount, xlab = 'Year', ylab = 'Million bushels', main = 'Total Corn Production US', type = 'l')
plot(corn_annual_bpa_yield$Year_ID, corn_annual_bpa_yield$Amount, main = 'Yield per acre', xlab = 'Commodity Market Year', ylab = 'Bushels per acre', type = 'l')
plot(corn_annual_mb_domesticuse$Year_ID, corn_annual_mb_domesticuse$Amount, main = 'Total Domestic Corn Usage', xlab = 'Year', ylab = 'Million bushels', type = 'l')


yellowcorn_prices <- yellowcorn_annual_dpb_mp %>%
  group_by(Year_ID) %>% summarise(Amount = mean(Amount))
plot(whitecorn_annual_dpb_mp$Year_ID, whitecorn_annual_dpb_mp$Amount, main = "Price of Corn (Yellow and White)", xlab = "Year", ylab = "Price", type = "l", col = "red")
lines(yellowcorn_prices$Year_ID, yellowcorn_prices$Amount, type = 'l', col = 'blue')
legend(1990, 7, legend=c("Yellow", "White"), col=c("red", "blue"), lty=1, cex=0.8)

par(mfrow=c(1, 1))

# Domestic corn usage
ggplot(data = corn_annual_mb_domesticuse, aes(Year_ID, Amount)) +
  geom_smooth(method="lm") +
  geom_point() +
  ggtitle("Annual Domestic Corn Usage") +
  labs(y = "Million bushels", x = "Year")

attach(corn_annual_mb_domesticuse)
corn_usage_lm <- lm(Amount ~ Year_ID)
par(mfrow=c(2, 2))
plot(corn_usage_lm)
par(mfrow=c(1, 1))
detach(corn_annual_mb_domesticuse)

# Explaining linear model plots
# https://data.library.virginia.edu/diagnostic-plots/

# Total corn production
ggplot(data = corn_annual_mb_production %>% filter(Year_ID > 1960), 
       aes(Year_ID, Amount)) +
  geom_point() +
  geom_smooth(method="lm") +
  ggtitle("Total Corn production in US") +
  labs(x = "Year", y = "Million bushels")

# Comparing production and domestic use
ggplot() +
  geom_smooth(data = corn_annual_mb_production %>% filter(Year_ID > 1980),
              method="lm", col="red", linetype="dotted",
              aes(Year_ID, Amount, fill = 'red')) +
  geom_line(data = corn_annual_mb_production %>% filter(Year_ID > 1980),
             col="red", aes(Year_ID, Amount)) +
  geom_point(data = corn_annual_mb_production %>% filter(Year_ID > 1980),
            col="red", aes(Year_ID, Amount)) +
  geom_smooth(data = corn_annual_mb_domesticuse %>% filter(Year_ID > 1980),
              method="lm", col="blue", linetype="dotted",
              aes(Year_ID, Amount)) +
  geom_line(data = corn_annual_mb_domesticuse %>% filter(Year_ID > 1980),
            col="blue", aes(Year_ID, Amount)) +
  geom_point(data = corn_annual_mb_domesticuse %>% filter(Year_ID > 1980),
             col="blue", aes(Year_ID, Amount)) +
  labs(x = "Year", y = "Million bushels") +
  ggtitle("Corn Production and Domestic Usage")
