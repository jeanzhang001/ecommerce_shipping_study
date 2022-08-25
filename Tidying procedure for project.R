library(data.table)
library(DT)
library(dplyr)
library(mice)
library(ggcorrplot)
library(ggplot2)
library(psych)

setwd("/Users/jeAn/Desktop/5205 final project")
dat <- fread('raw dataset.csv')
head(dat)
dim(dat)

##change case for column names
for (i in 1:length(colnames(dat))) {
  colnames(dat)[i] = tolower(colnames(dat)[i])
}

##check NAs
unique(is.na(dat))

##check some vairables' property
unique(dat$reached.on.time_y.n)
unique(dat$warehouse_block)
unique(dat$mode_of_shipment)
unique(dat$customer_care_calls)
unique(dat$customer_rating)
unique(dat$product_importance)
unique(dat$prior_purchases)

##build functions
#round
round.numerics <- function(x, digits){
  if(is.numeric(x)){
    x <- round(x = x, digits = digits)
  }
  return(x)
}

#linear regression
lm_mod <- function(mod.lm){
  return(data.frame(variables= names(mod.lm$coefficients),
                    coefficients= mod.lm$coefficients, p=summary(mod.lm)$coef[, ncol(summary(mod.lm)$coef)]))
}

##getting familiar with the dataset
#correlation plot
train1<-dat[,c("customer_care_calls","customer_rating","cost_of_the_product","prior_purchases","discount_offered","weight_in_gms","reached.on.time_y.n")]
cor(train1)
ggcorrplot(cor(train1), type="lower", lab = TRUE, lab_size = 3)

#Four Chart Figures (warehouse block, mode of shipment, product importance and gender)

#warehouse block and reach on time
dat$reached.on.time_y.n<-factor(dat$reached.on.time_y.n)
ggplot(data = dat,aes(x=warehouse_block,fill=reached.on.time_y.n))+
  geom_bar(position = "fill")

#mode of shipment and reach on time
ggplot(data = dat,aes(x=mode_of_shipment,fill=reached.on.time_y.n))+
  geom_bar(position = "fill")

#product importance and reach on time
ggplot(data = dat,aes(x=product_importance,fill=reached.on.time_y.n))+
  geom_bar(position = "fill")

#gender and reach on time
ggplot(data = dat,aes(x=gender,fill=reached.on.time_y.n))+
  geom_bar(position = "fill")

#regression mode
fit<-lm(reached.on.time_y.n~.,data = train1)
summary(fit)

rating1<-lm(customer_rating~.,data = train1)
summary(fit)

##create new variable
#loyal customer
summary(dat$prior_purchases)
sum(dat$prior_purchases > 3)/nrow(dat)
plot(dat$prior_purchases)
dat <- dat[, "loyal_customer" := prior_purchases >3]

##prepare tables for analysis
low_price <- dat[cost_of_the_product <= 150,]
medium_price <- dat[cost_of_the_product > 150 & cost_of_the_product <250,]
high_price <- dat[cost_of_the_product >=250,]

#write.csv(x = dat, file = "Clean dataset for proposal.csv", row.names  =  FALSE)

View(dat)

