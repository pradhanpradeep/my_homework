# [Section-1]
# Load petrolconsumption.csv and name columns
#
# source: http://people.sc.fsu.edu/~jburkardt/datasets/regression/
#
# There are 48 rows of data.  The data include:
#
# I,  the index;
# A1, the petrol tax;
# A2, the per capita income;
# A3, the miles of paved highway;
# A4, the proportion of drivers;
# B,  the consumption of petrol.
#
  
library(ggplot2)

data <- read.table('petrolconsumption.csv', sep=',', header=T)
head(data)
colnames(data) <- c("Index", "PetrolTax", "PerCapitaIncome", "PavedHighwayMiles", "LicensedDrivers", "PetrolConsumption")

# [Section-2]
# Creating scatter plots and look for any relationship
# between dependent and explanatory variables and
# save plots as png
set.seed(42)
png('plots/scatterplots_%03d.png', width=6, height=6, units='in', res=300)
plot(data, ask=FALSE)
ggplot(data, aes(x=PetrolTax, y=PetrolConsumption)) + geom_point(aes(colour = PetrolTax), ask=FALSE)
ggplot(data, aes(x=PerCapitaIncome, y=PetrolConsumption)) + geom_point(aes(colour = PerCapitaIncome), ask=FALSE)
ggplot(data, aes(x=PavedHighwayMiles, y=PetrolConsumption)) + geom_point(aes(colour = PavedHighwayMiles), ask=FALSE)
ggplot(data, aes(x=LicensedDrivers, y=PetrolConsumption)) + geom_point(aes(colour = LicensedDrivers), ask=FALSE)
dev.off()

# [Section-3]
# it appears perCapitaIncome & proportionOfLicensedDrivers 
# are positively influencing petrol consumption 
# applying linear regression function on each of them
png('plots/lm_percapitaincome_on_petrolconsumption.png', width=6, height=6, units='in', res=300)
layout(matrix(1:4, ncol=2))
plot(lm(PetrolConsumption ~ PerCapitaIncome, data))
dev.off()

png('plots/lm_licenseddrivers_on_petrolconsumption.png', width=6, height=6, units='in', res=300)
layout(matrix(1:4, ncol=2))
plot(lm(PetrolConsumption ~ LicensedDrivers, data))
dev.off()

# [Section-4]
# creating a regression model by 
# considering all independent variables
# start by dropping "Index" column
df <- subset(data, select = c("PetrolTax", "PerCapitaIncome", "PavedHighwayMiles", "LicensedDrivers", "PetrolConsumption"))
fit1 <- lm(PetrolConsumption ~.,df)
summary(fit1)

# eliminating "PavedHighwayMiles"
fit2 <- update(fit1,.~.-PavedHighwayMiles)
summary(fit2)

# eliminating "PetrolTax"
fit3 <- update(fit2,.~.-PetrolTax)
summary(fit3)

png('plots/lm_petrolconsumption.png', width=6, height=6, units='in', res=300)
layout(matrix(1:4, ncol=2))
plot(fit3)
dev.off()

# ---------------------------------------------------------------------------------



