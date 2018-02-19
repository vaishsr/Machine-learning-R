#loading the uswages dataset into the R studio with the package faraway
library(faraway)
data("uswages")

#Question 1
#descriptive statistics of the data
str(uswages)
summary(uswages)
?uswages
head(uswages)
View(uswages)
#graphical summary of the data
boxplot(uswages)
boxplot(uswages$exper)

#looks like this data seems to be irrevalent entry 
#as the person with the wage of 128.6 dollars weekly seems to have experience of -2.
oddexp <- uswages[which.min(uswages$exper),]
oddexp

#checking for missing values
uswages$exper[uswages$exper <0] <-NA
sum(is.na(uswages))

#so this row can be omitted considering it as the outlier.
uswages <- na.omit(uswages)
sum(is.na(uswages))

#Coverting the varibles into factor variables
uswages$race <- as.factor(uswages$race)
uswages$smsa <- as.factor(uswages$smsa)
uswages$pt <- as.factor(uswages$pt)

#setup the levels for the factor variables
levels(uswages$race) <- c("White","Black")
levels(uswages$smsa) <- c("No","Yes")
levels(uswages$pt) <- c("No","Yes")

#create a new factor variable for consolidating the 4 regions

uswages <- data.frame(uswages,region =
                        1*uswages$ne +
                        2*uswages$mw +
                        3*uswages$so +
                        4*uswages$we)

uswages$region <- factor(uswages$region)

#set their levels
levels(uswages$region) <- c("ne","mw","so","we")

#remove the 4 region variables as it is already consolidated into one
uswages_cleaned <- subset(uswages,select=-c(ne:we))
str(uswages_cleaned)
summary(uswages_cleaned)

#cleaning the outliers
uswages_cleaned <- subset(uswages_cleaned,select =wage < 5000)
summary(uswages_cleaned)

#linear regression 
model_wage <- lm(wage ~ ., data = uswages_cleaned)
summary(model_wage)

#Residual plot visualization
qplot(model_wage$fitted.value, wage, data=uswages_cleaned) +
  geom_abline(intercept = 0, slope = 1, color="blue") +
  ggtitle("Residual Plot")

# residuals vs race 
Resplot1 <- qplot(race, model_wage$resid, geom = "boxplot", data=uswages_cleaned) + geom_hline(yintercept=0)
# residuals vs smsa 
Resplot2 <- qplot(smsa, model_wage$resid, data=uswages_cleaned) + geom_hline(yintercept=0)
# residuals vs pt 
Resplot3 <- qplot(pt, model_wage$resid, data=uswages_cleaned) + geom_hline(yintercept=0)

library("gridExtra")
grid.arrange(Resplot1, Resplot2, Resplot3, nrow=3)

#*****************************************************************************************************
#Question 2 (a)
#Bivaraite regression model
bi_model <- lm(wage ~ educ , data= uswages_cleaned)
summary(bi_model)

#99% Confidence interval using t test on bivaraiate model
qt(p =0.005,df = 1965, lower.tail = TRUE)
qt(p =0.005,df = 1965, lower.tail = FALSE)

#Question 2 (b)
#%99 confidence interval for the regression coefficient using the F-test.

confint(bi_model, level = .99)

#*****************************************************************************************************

#Question 3
#Multivariate linear regression 

multi_model <- lm(wage ~ educ+exper, data = uswages_cleaned)
summary(multi_model)

plot(uswages$wage~uswages$educ)
abline(multi_model)

#****************************************************************************************************
#Question 4

null_model <- lm(wage ~ 1, data=uswages_cleaned)
summary(null_model)

full_model <- lm(wage ~ educ + exper , data =uswages_cleaned)
summary(full_model)

#to compare the two models
anova(null_model, full_model)

# model devience for both the models
null_dev <- deviance(null_model)
full_dev <- deviance(full_model)

#Degree of freedom from fitted models
degfree1 <-  df.residual(null_model)
degfree2 <-  df.residual(full_model)

#Calculation of F statistics and p value
Fstatistics <- ((null_dev - full_dev)/(degfree1 - degfree2))/(full_dev/degfree2)
1 - pf(Fstatistics, degfree1 - degfree2, degfree2) 

#Relating to the ttest and pvalue
sqrt (Fstatistics)
(tstat <- summary(full_model)$coef[2, 3])
2 * (1-pt (sqrt(Fstatistics), degfree2))


#*****************************************************************************************************
#Question 5
#prediction for 5 years of experience and 8 years of education with 90% confidence interval

predict(full_model ,data.frame(educ = 8, exper = 5),interval="confidence", level = 0.90)

#****************************************************************************************************
#Question 6

sum(is.na(uswages))

Modelexp <- lm(wage ~ educ+exper, data = uswages)
summary(Modelexp)

Modelreg <- lm(wage ~ educ+exper+ne+we, data = uswages)
summary(Modelreg)

Modelreg1 <- lm(wage ~ educ+exper+ne, data = uswages)
summary(Modelreg1)

Modelreg2 <- lm(wage ~ educ+exper+we, data = uswages)
summary(Modelreg2)
