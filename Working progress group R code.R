rm(list=ls())
library(data.table)
library(hexbin)
library(IDPmisc)
library(rpart)
library(ggplot2)
library(rpart.plot)
library(rattle)
library(nnet)

####################################
#Question 1
###################################
clean_data<-read.csv("Clean dataset for proposal.csv",header = TRUE)
View(clean_data)
RNGversion(vstr=3.6)
clean_data<-clean_data[,2:12]

#Data Exploration

with(clean_data,{
  bin<-hexbin(prior_purchases,customer_care_calls)
  plot(bin,main="Hexagonal prior_purchases  customer_care_calls ")
})

with(clean_data,smoothScatter(prior_purchases,customer_care_calls,main="Hexagonal prior_purchases  customer_care_calls "))


with(clean_data,iplot(prior_purchases,customer_care_calls,main="Hexagonal prior_purchases  customer_care_calls "))

#Model 1 : logistic regression
set.seed(1234)
train<-sample(nrow(clean_data),0.7*nrow(clean_data))
clean_data.train<-clean_data[train,]
clean_data.test<-clean_data[-train,]
model1<-glm(reached.on.time_y.n~.,data = clean_data.train)
summary(model1)

#Stepwise logistic regression. A model is generated by fewer independent variables in order to get a 
#smaller AIC value by adding or removing insignificant independent variables.
model1.reduced<-step(model1)
options(scipen=10)
summary(model1.reduced)

#Since the on-time delivery is a binary outcome, logistic regression is the appropriate technique to apply. 
#In the model, we can see that independent variables including Customer_care_calls, Cost_of_the_Product, Prior_purchases, 
#Product_importance, Discount_offered, and weight_in_gms are significant factors. 

#Key Findings:
#1: The coefficient of Customer_care_calls is -0.0275 indicates that the on time delivery performance of products will 
#increase if the company gets more customer calls.

#2: The ciefficient of Prior_purchases is -0.013239895 indicates that the on time delivery performance of products will 
#increase if products prior purchases go up. Products with high frequency purchases are often arrived on schedule.

#3:With regard to the product importance, the products were categorized as low and medium level. 
#We found that those products with importance of low and medium level are more efficient to on-time delivery 
#than products with importance of high level.

#Understand the relationship between product_importance and prior_purchases.

clean_data$product_importance<-factor(clean_data$product_importance)
ggplot(data = clean_data,aes(x=prior_purchases,fill=product_importance))+
  geom_bar(position = "fill")
#Most prior purchases are from products with importance level of low and medium, so we assume that 
#low and medium level products have high frequency of purchases. 


#Checking the relation between customer calls and reached on time.
clean_data$reached.on.time_y.n<-factor(clean_data$reached.on.time_y.n)
ggplot(data = clean_data,aes(x=customer_care_calls,fill=reached.on.time_y.n))+
  geom_bar(position = "fill")
#It seems that when customers calls increase (>=4), the number of shipments reach on time would become higher. 
#It means that customer calls is playing important role for reaching shipment on time to customers. 


#Classify the training set(test) sample unit???model1.reduced)
#Predict Reached.on.Time_Y.N, and compare to the Reached.on.Time_Y.N in training set(test)
prob<-predict(model1.reduced,clean_data,type = "response")
logit.pred<-factor(prob>0.5,levels = c(FALSE,TRUE),labels = c(0,1))

#logit.pred
#logit.perf (Confusion Matrix)
logit.perf<-table(clean_data.test$reached.on.time_y.n,logit.pred,dnn = c("Actual","Predicted"))
logit.perf
accuracy=(776+1337)/(776+563+624+1337)
accuracy
#The accuracy of logistic regression model is 64.03%

#Model 2: Decision Tree

set.seed(1234)
d_tree<-rpart(reached.on.time_y.n~.,data = clean_data.train,method = "class",parms = list(split="information"))
d_tree$cptable
plotcp(d_tree)

#prune
d_tree.pruned<-prune(d_tree,cp=0.01000000)

prp(d_tree.pruned,type = 2,extra = 104,fallen.leaves = TRUE,main="DECISION TREE")

fancyRpartPlot(d_tree.pruned,main="DECISION TREE")

#Tree Notes:

#1:The decision tree model shows that if the discount_offered is greater than or equal to 11%, 
#the probability of on time arrival will be 0, and those observations account for 24% of the total sample. 
#If the discount_offered is less than 11%, the probability of products on-time arrival will be 53%. 

#2: If the weight of goods is less than 4,122 grams, the probability of products on-time arrival will be 44%. 
#Those observations with over 4,122 grams account for 25% of the total sample. 
#If the weight of goods is greater than or equal 4,122 grams, the probability of products on-time arrival will be 58%. 

#Conclusion:
#1.The shipment of certain discount products (over 11%) are mostly delayed. 
#Goods over 4,122 grams with small discounts are most likely reached on time, 
#while goods under than 4,122 grams with small accounts will not arrived on time at over 50% chances.  

#The accuracy of logistic regression model is 67.18%
d_tree.pred<-predict(d_tree.pruned,clean_data.test,type = "class")
d_tree.perf<-table(clean_data.test$reached.on.time_y.n,d_tree.pred,dnn = c("Actual","Predicted"))
d_tree.perf
accuracy1<-(972+1245)/(972+367+716+1245)
accuracy1

#Final Recommendations (Based on regression model and desicion tree model):

#1:The company should pay attention to customer services which will improve the delivery performance of products and 
#efficient communication with customers would fasten problem-solvings of products. 

#2: Maximize package efficiency for products with importance of low and medium level. 
#The company could improve package handling such as warehousing time/outbound time deduction and 
#streamline package process, in order to increase the inventory turnover and control the inventory carrying cost. 

#3. Apply a larger discount to some products that are expected to arrive late, 
#in order to reduce customers' compliant about the product shipment. 

#Netural Network(model inference)
#Prepare Data
clean_data<-read.csv("Clean dataset .csv",header = TRUE)
clean_data<-clean_data[,2:12]
clean_data$reached.on.time_y.n<-as.factor(clean_data$reached.on.time_y.n)
clean_data$warehouse_block<-ifelse(clean_data$warehouse_block=='A',1,
                                   ifelse(clean_data$warehouse_block=='B',2,
                                          ifelse(clean_data$warehouse_block=='C',3,
                                                 ifelse(clean_data$warehouse_block=='D',4,5))))

clean_data$mode_of_shipment<-ifelse(clean_data$mode_of_shipment=='Ship',1,
                                    ifelse(clean_data$mode_of_shipment=='Road',2,3))

clean_data$product_importance<-ifelse(clean_data$product_importance=='low',1,
                                      ifelse(clean_data$product_importancet=='medium',2,3))

clean_data$gender<-ifelse(clean_data$gender=='F',0,1)



set.seed(1031)
split<-sample(x =c('train','validation','test'),size = nrow(clean_data),
              replace = T,prob = c(0.4,0.4,0.2))
train =clean_data[split=='train',]
validation =clean_data[split=='validation',]
test=clean_data[split=='test',]
model3=nnet(reached.on.time_y.n~.,data =train,size=3,decay=0.1,MaxNWts=10000,maxit=100)
summary(model3)

####################################
#Question 2
###################################
rm(list=ls())
library(tidyverse)
library(stringr)
library(rpart)
library(rpart.plot)
library(corrplot)
library(caret)
library(gridExtra)
library(GGally)
library(data.table)
library(ggcorrplot)

#read the data
dat_clean <- fread('Clean dataset for proposal.csv',stringsAsFactors = T)

summary(dat_clean)
str(dat_clean)


#select numeric data
numeric_data <- dat_clean%>%keep(is.numeric)

#drop id
numeric_data <- numeric_data[, !"id"]

#check correlation
ggcorrplot(cor(numeric_data), type="lower", lab = TRUE, lab_size = 3)
ggpairs(numeric_data)

######
#encode string variables 
#check correlation
######
#one hot encoding
dat_clean2 <- dat_clean %>% 
  mutate(warehouse_num = case_when(
    warehouse_block == 'A' ~ 1,
    warehouse_block == 'B' ~ 2,
    warehouse_block == 'C' ~ 3,
    warehouse_block == 'D' ~ 4,
    warehouse_block == 'F' ~ 5,
  ))

dat_clean2 <- dat_clean2 %>% 
  mutate(shipment_num = case_when(
    warehouse_block == 'Ship' ~ 1,
    warehouse_block == 'Road' ~ 2,
    mode_of_shipment == 'Flight' ~ 3
  ))

dat_clean2 <- dat_clean2 %>% 
  mutate(importance_num = case_when(
    product_importance == 'low' ~ 1,
    product_importance == 'medium' ~ 2,
    product_importance == 'high' ~ 3
  ))

dat_clean2 <- dat_clean2 %>% 
  mutate(gender_num = case_when(
    gender == 'F' ~ 1,
    gender == 'M' ~ 2
  ))

numeric_clean <- dat_clean%>%keep(is.numeric)
numeric_clean <- numeric_clean[, !"id"]

ggcorrplot(cor(numeric_clean), type="lower", lab = TRUE, lab_size = 3)
ggpairs(numeric_clean)

#############################
#clustering preparation
#############################

#scale
data_cluster = scale(numeric_data)
head(data_cluster[,1:4])

###############################
#Hierarchical Cluster Analysis
##############################
#Define Similarity Measure
d = dist(x = data_cluster,method = 'euclidean') 
#Clustering Method
clusters = hclust(d = d,method='ward.D2')
#Examine Dendrogram
plot(clusters)
plot(cut(as.dendrogram(clusters),h=5)$upper)
rect.hclust(tree=clusters,k = 4,border='tomato')
#Goodness of fit
cor(cophenetic(clusters),d)
#0.5973518

#Selecting Clusters
h_segments = cutree(tree = clusters,k=4)
table(h_segments)

#Visual
#method 1
library(psych)
temp = data.frame(cluster = factor(h_segments),
                  factor1 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()
#method 2
library(cluster)
clusplot(data_cluster,
         h_segments,
         color=T,shade=T,labels=4,lines=0,main='Hierarchical Cluster Plot')

#####################
#K-means Clustering
####################
set.seed(1213)
km = kmeans(x = data_cluster,centers = 4,iter.max=10000,nstart=25)
table(km$cluster)
paste(km$totss,'=',km$betweenss,'+',km$tot.withinss,sep = ' ')
km$totss == km$betweenss + km$tot.withinss

#Total within sum of squares Plot aka Elbow plot
within_ss = sapply(1:10,FUN = function(x){
  set.seed(1213)
  kmeans(x = data_cluster,centers = x,iter.max = 1000,nstart = 25)$tot.withinss})

ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#Ratio Plot
ratio_ss = sapply(1:10,FUN = function(x) {
  set.seed(1213)
  km = kmeans(x = data_cluster,centers = x,iter.max = 1000,nstart = 25)
  km$betweenss/km$totss} )
ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#Silhouette Plot
pam(data_cluster,k = 4)$silinfo$avg.width #0.2237358
pam(data_cluster,k = 5)$silinfo$avg.width #0.2235857

silhoette_width = sapply(2:10,
                         FUN = function(x) pam(x = data_cluster,k = x)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1)) # silhouette supports 4 clusters

#Selected cluster
set.seed(1213)
km = kmeans(x = data_cluster,centers = 4,iter.max=10000,nstart=25)
k_segments = km$cluster
table(k_segments)

#visual
#method 1
temp = data.frame(cluster = factor(k_segments),
                  factor1 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

#method 2
clusplot(data_cluster,
         k_segments,
         color=T,shade=T,labels=4,lines=0,main='k-means Cluster Plot')


#################################
#Model-based clustering
################################
library(mclust)
clusters_mclust = Mclust(data_cluster)
summary(clusters_mclust)

#exam 4 clusters
clusters_mclust_4 = Mclust(data_cluster,G=4)
summary(clusters_mclust_4)
clusters_mclust_4$bic


#plot bic
mclust_bic = sapply(1:10,FUN = function(x) -Mclust(data_cluster,G=x)$bic)
mclust_bic
ggplot(data=data.frame(cluster = 1:10,bic = mclust_bic),aes(x=cluster,y=bic))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#Both hierarchical and k-means favored a four-cluster solution, 
#so we are going to go with a four-cluster solution.
m_clusters = Mclust(data = data_cluster,G = 4)
m_segments = m_clusters$classification
table(m_segments)

# visual
#method 1
temp = data.frame(cluster = factor(m_segments),
                  factor1 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

#method 2
clusplot(data_cluster,
         m_segments,
         color=T,shade=T,labels=4,lines=0,main='mclust Cluster Plot')

#Contrast Results
table(h_segments)
table(k_segments)
table(m_segments)


#Profile Clusters
data2 = cbind(dat_clean,h_segments, k_segments,m_segments)
data2 = data2[, c(4,5,6,7,10,11,12,2,3,8,9,13,14,15,16)]

#Profile Segments by numeric data
#table
library(dplyr)
data2 %>%
  select(customer_care_calls:reached.on.time_y.n,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()


#graph
library(tidyr)
data2 %>%
  select(c(customer_care_calls,customer_rating,prior_purchases,reached.on.time_y.n),k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,c(customer_care_calls,customer_rating,prior_purchases,reached.on.time_y.n))%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()

data2 %>%
  select(cost_of_the_product,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,cost_of_the_product)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()

data2 %>%
  select(discount_offered,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,discount_offered)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()

data2 %>%
  select(c(weight_in_gms),k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,weight_in_gms)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()

#Profile Segments by Demographics
#table
prop.table(table(data2$k_segments,data2$warehouse_block),1)
prop.table(table(data2$k_segments,data2$mode_of_shipment),1)
prop.table(table(data2$k_segments,data2$product_importance),1)
prop.table(table(data2$k_segments,data2$gender),1)
prop.table(table(data2$k_segments,data2$loyal_customer),1)

#graph
#warehouse_block distribution
library(RColorBrewer)
tab = prop.table(table(data2$k_segments,data2$warehouse_block),1)
tab2 = data.frame(round(tab,2))
ggplot(data=tab2,aes(x=Var2,y=Var1,fill=Freq))+
  geom_tile()+
  geom_text(aes(label=Freq),size=6)+
  xlab(label = '')+
  ylab(label = '')+
  scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Oranges'))

#mode of shipment distribution
tab = prop.table(table(data2$k_segments,data2$mode_of_shipment),1)
tab2 = data.frame(round(tab,2))
ggplot(data=tab2,aes(x=Var2,y=Var1,fill=Freq))+
  geom_tile()+
  geom_text(aes(label=Freq),size=6)+
  xlab(label = '')+
  ylab(label = '')+
  scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Oranges'))

#product importance distribution
tab = prop.table(table(data2$k_segments,data2$product_importance),1)
tab2 = data.frame(round(tab,2))
ggplot(data=tab2,aes(x=Var2,y=Var1,fill=Freq))+
  geom_tile()+
  geom_text(aes(label=Freq),size=6)+
  xlab(label = '')+
  ylab(label = '')+
  scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Oranges'))

#gender distribution
tab = prop.table(table(data2$k_segments,data2$gender),1)
tab2 = data.frame(round(tab,2))
ggplot(data=tab2,aes(x=Var2,y=Var1,fill=Freq))+
  geom_tile()+
  geom_text(aes(label=Freq),size=6)+
  xlab(label = '')+
  ylab(label = '')+
  scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Oranges'))


#loyal customer distribution
tab = prop.table(table(data2$k_segments,data2$loyal_customer),1)
tab2 = data.frame(round(tab,2))
ggplot(data=tab2,aes(x=Var2,y=Var1,fill=Freq))+
  geom_tile()+
  geom_text(aes(label=Freq),size=6)+
  xlab(label = '')+
  ylab(label = '')+
  scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Oranges'))

#compare sales of high/low discount rate of product with different importance in each segment
#to exam different segment cluster, change the segement number(1,2,3,4)
#to filter the various product importance: alter the product importance to low/medium/high

data2%>%
  filter(k_segments == 1) %>%
  filter(product_importance == 'low') %>%
  filter(discount_offered > mean(discount_offered)) %>%
  count()



####################################
#Question 3
###################################
rm(list=ls())
library(data.table)
library(DT)
library(dplyr)
library(psych)
library(caret)
library(nnet)
library(rpart)
library(rpart.plot)
library(ggcorrplot)

dat <- fread('Clean dataset for proposal.csv')


# Is there any product difference(price & weight) among the company's loyal customers and regular customers
#tables
tab1 <- dat[, .(price = mean(cost_of_the_product), weight = mean(weight_in_gms)), by = loyal_customer]
tab1

tab2 <- dat[, .(price = median(cost_of_the_product), weight = median(weight_in_gms)), by = loyal_customer]
tab2

tab3 <- dat[, .(price = sd(cost_of_the_product), weight = sd(weight_in_gms)), by = loyal_customer]
tab3

#correlation
plot1<-dat[,c("cost_of_the_product","weight_in_gms","loyal_customer")]
ggcorrplot(cor(plot1), type="lower", lab = TRUE, lab_size = 3)

#spilt data
split = sample(1:nrow(dat), 2000, replace = F)
test = dat[split,]
train = dat[!split,]

#glm models
glm.mod = glm(loyal_customer ~ cost_of_the_product + weight_in_gms, data = dat)
summary(glm.mod)
#model performance
glm.pred = predict(glm.mod, test, type = 'response')
ct = table(loyal_customer = test$loyal_customer,
           predictions = as.integer(glm.pred>0.5))
ct
glm.score = sum(ct[1,1],ct[2,2])/nrow(test)

#classification tree
classTree = rpart(loyal_customer ~ cost_of_the_product + weight_in_gms, data=dat)
rpart.plot(classTree)
#model performance
ct.pred = predict(classTree, test)
ct = table(loyal_customer = test$loyal_customer,
           predictions = as.integer(ct.pred>0.5))
ct
ct.score = sum(ct[1,1],ct[2,2])/nrow(test)

#Neural Network
set.seed(1031)
model1 = nnet(loyal_customer ~ cost_of_the_product + weight_in_gms,
              data = dat,
              size = 0,
              skip=T,
              lineout=T) 
summary(model1)
#model performance
nn.pred = predict(model1, newdata = test)
table(nn.pred, test$loyal_customer)
nn.score = mean(nn.pred==test$loyal_customer) # Accuracy

data.frame(glm.score,ct.score,nn.score)


####################################
#Question 4
###################################
rm(list=ls())
#Read CSV file
ecommerce <- read.csv('Clean dataset for proposal.csv')
library(dplyr)

#I used filter function to select three groups based on various product_importance
#I used mean function to calculate average customer rating among three groups
lowdata <- filter(ecommerce, product_importance == 'low')
ratinglow <- mean(lowdata$customer_rating)
ratinglow
mediumdata <- filter(ecommerce, product_importance == 'medium')
ratingmedium <- mean(mediumdata$customer_rating)
ratingmedium
highdata <- filter(ecommerce, product_importance == 'high')
ratinghigh<- mean(highdata$customer_rating)
ratinghigh

# Plot the bar chart 
H <- c(2.983576,2.997686,2.993671)
M <- c("Lowimportance","Mediumimportance","Highimportance")
barplot(H,names.arg=M,xlab="Groups",ylab="Customer rating",col="blue",
        main="Customerrating chart",border="red")


# I used filter function to select rows where delivery is on time
# and I calculated on-time delivery rate among three groups
deliverylow <- filter(lowdata, reached.on.time_y.n == 0)
a = nrow(lowdata)
b = nrow(deliverylow)
lowpercentage <- b/a
lowpercentage

deliverymedium <- filter(mediumdata, reached.on.time_y.n == 0)
c = nrow(mediumdata)
d = nrow(deliverymedium)
mediumpercentage <- d/c
mediumpercentage

deliveryhigh <- filter(highdata, reached.on.time_y.n == 0)
e = nrow(highdata)
f = nrow(deliveryhigh)
highpercentage <- f/e
highpercentage

# Plot the bar chart 
H <- c(0.4072116,0.4095499,0.350211)
M <- c("Lowimportance","Mediumimportance","Highimportance")
barplot(H,names.arg=M,xlab="Groups",ylab="On-time Delivery Rate",col="red",
        main="On-time Delivery Rate Chart",border="blue")










