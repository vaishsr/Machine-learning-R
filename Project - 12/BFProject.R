salesdata <- read.csv("D:/Vaishnavi_Acads/MIT/Business Forecasting/BF_PROJECT_DATA.csv")
nrow(salesdata)
head(salesdata)
ordered<- salesdata[order(as.Date(salesdata$Order.Date, "%m/%d/%Y"), decreasing  = FALSE),]

sum(is.na(ordered))
write.csv(ordered, "D:/Vaishnavi_Acads/MIT/Business Forecasting/sorted.csv")
  ordered$Sales
nullmodel <- lm(Sales ~ 1, data = auto)   # Nothing in the model
fullmodel <- lm(Sales ~., data = auto) 
stepself <- step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel),direction="forward")
summary(stepself)

subset<-regsubsets(Sales ~., data = ordered)


fullmodel <- lm(score ~., data=ordered)    # All variables in the model


head(ordered)

# OrderedDate <- as.Date(ordered$Order.Date)
# 
summary(ordered)
class(ordered$Product.Base.Margin)
# 
# df <- data.frame(date = OrderedDate,
#                  year = as.numeric(format(OrderedDate, format = "%Y")),
#                  month = as.numeric(format(OrderedDate, format = "%m")),
#                  day = as.numeric(format(OrderedDate, format = "%d")))
# df
# 
# A<-subset(ordered, format.Date(OrderedDate, "%m")=="11" & format.Date(OrderedDate, "%Y")=="11")
# A
str(salesdata)
fix(salesdata)
library("forecast")
library(fpp)
library(zoo)
Sales_ts <- ts(salesdata, frequency=12, start=c(2009,1))
names(salesdata)

plot.ts(Sales_ts)

Sales_ts








library("VIM")

sum(is.na(auto))


auto <- kNN(ordered, k=3)[, 1:21]



fit <-  lm(Sales ~. , data =  auto)

summary(fit)
str(ordered)
summary(auto)

