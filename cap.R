install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(ggplot2)
install.packages("corrplot")
library("corrplot")
library(gridExtra) 
library(RColorBrewer)
library(rJava)
library(FSelector)



bank_additional <- read_delim("Downloads/bank-additional/bank-additional.csv", ";", escape_double = FALSE, trim_ws = TRUE)
head(bank_additional) 
data<- bank_additional
view(data)


dim(data)    # dimension of the data set

attach(data)
str(data) #structure of data

#converting character variables to factor
data<-data %>%
  mutate(y = as.factor(y),job= as.factor(job),education = as.factor(education),
         marital= as.factor(marital),default = as.factor(default),
         housing = as.factor(housing),loan = as.factor(loan),
         contact = as.factor(contact),poutcome = as.factor(poutcome),
         day_of_week = as.factor(day_of_week),month = as.factor(month),
         previous = as.factor(previous))

#Feature engineering age ,duration variable for visualization
#age
data<-data %>%
      mutate(age_group = case_when(
                           age >= 0 & age <= 12 ~ 'child',
                           age >= 13 & age <= 19 ~'Teen',
                           age >= 20 & age <=35 ~'younger adult',
                           age >= 36 & age <= 45 ~ "middle aged",
                           age >= 46 & age <= 60 ~"older adult",
                             age >= 61 ~ "retired "))
data$age_group<-as.factor(data$age_group)

#duration from seconds to minutes
data<- data %>%
         mutate(duration_min = round(duration/60,2))

# searching for missing values 
sum(is.na(data)) 
table(as.factor(previous))

 #fixing pdays attribute : are marked as 999 
data$pdays[data$pdays == '999']<- -1
pdays = as.factor(pdays)
table(data$pdays)

#checking unknown category variable 
sum(apply(data, 1, function(x) any(x %in% c("unknown")))) 

#final summary of the data 
summary(data)
str(data)

#-----------------------------------------------Analysis --------------------------------------------------------------

table(data$y) # checking the class variable (target)


ggplot(data,aes(y,fill = y))+geom_bar(stat="count", width=0.8)+
         ggtitle("Class variable distribution")+xlab("Customer response ")+
   ylab(" Customer Count")+
   theme(plot.title=element_text(face ="bold") )+labs(fill = "Customer\nResponse")+
   scale_fill_manual(values = c("Red","Steelblue"))
 

 #----------------------------------------- analysis-----------------------------------------------
#marital
mar<-ggplot(data,aes(marital,fill = y))+geom_bar(stat="count", width=0.5)+
   ggtitle("Marital status vs Customer Response")+xlab("Marital Status")+
   ylab(" Customer Count")+
   theme(plot.title=element_text(face ="bold")) +labs(fill = "Customer\nResponse")+
   scale_fill_manual(values = c("Red","Steelblue"))
   
mar

#job
j<-ggplot(data,aes(job,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle("Job vs Customer response distribution")+xlab(" Age ")+
   ylab("Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold"),axis.text.x = element_text(angle = 45)) +
   scale_fill_manual(values = c("Red","Steelblue"))
j

#education

e<-ggplot(data,aes(education,fill = y))+geom_bar(stat="count", width=0.8,)+
   ggtitle("Education vs Customer response distribution")+xlab("Education ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold"),axis.text.x = element_text(angle = 45)) +
   scale_fill_manual(values = c("Red","Steelblue"))
e

#default
d<-ggplot(data,aes(default,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle(" Default in credit vs Customer Response Distribution")+xlab("Credit default ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
d
#housing loan
h<-ggplot(data,aes(housing,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle("Housing loan vs Customer Response distribution")+xlab("Housing Loan ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
h

#loan
l<-ggplot(data,aes(loan,fill = y))+geom_bar(stat="count", width=0.8)+
ggtitle("Personal Loan vs Customer Response ")+xlab("Personal Loan  ")+
   ylab(" Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))#
l
#contact
c<-ggplot(data,aes(contact,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle("Contact type vs Customer Response distribution")+xlab("Contact ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
c
#month
mnth<-ggplot(data,aes(month,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle("Month vs customer response distribution")+xlab("Month ")+
   ylab(" Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))#
mnth
#day_of_week
d<-ggplot(data,aes(day_of_week,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle("Day of the week vs Customer response distribution")+xlab("Days of the week ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
d
#age
ag<-ggplot(data,aes(age_group,fill = y))+geom_bar(stat="count", width=0.8)+
ggtitle("Age vs customer response distribution")+
   xlab("Age\nChild = 0-12\nTeen = 13-19\nYounder adult = 20-35\nMiddle age = 36-45\nOlder adult = 46-60\nRetired = above 60 ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
ag
#duration
dur<-ggplot(data,aes(duration_min,fill = y))+geom_histogram(stat="count", width=0.8,bins = 3)+
   ggtitle("Duration of the call made Vs Customer response distribution")+xlab("Duration of call ")+
   ylab("Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
dur

#campaign
camp<-ggplot(data,aes(campaign,fill = y))+geom_histogram(stat="count", width=0.8)+
   ggtitle("Call count for the current campaign Vs customer response distribution")+
   xlab("Customer Campaign ")+ylab("Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
camp

#pday
pday<-ggplot(data,aes(pdays,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle("Days passed since previous campaign calls Vs customer response distribution")+
   xlab("pdays")+ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
pday

# previous
prev<-ggplot(data,aes(previous,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle("Previous vs customer response distribution")+xlab("Previous  ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))#

prev
#poutcome
pout<-ggplot(data,aes(poutcome,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle("Outcome of previous campaign vs customer response distribution")+xlab("poutcome ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
 scale_fill_manual(values = c("Red","Steelblue"))
pout

#emp_var_rate
evar<-ggplot(data,aes(emp.var.rate,fill = y))+geom_histogram(stat="count", width=0.8)+
   ggtitle("Employment Variation Rate vs customer response distribution")+xlab("Emp var rate ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))#
evar
#cons.price.idx
price.idx<-ggplot(data,aes(cons.price.idx,fill = y))+geom_histogram(stat="count", width=0.8,binwidth = 500)+
   ggtitle("consumer price index vs customer response distribution")+
   xlab("consumer price index ")+ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
price.idx

#cons.conf.indx
conf.indx<-ggplot(data,aes(cons.conf.idx,fill = y))+geom_histogram(stat="count", width=0.8)+
   ggtitle("Consumer confidence index vs customer response distribution")+
   xlab("consumer confidence index ")+ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
conf.indx

#euribor3m
eur<-ggplot(data,aes(euribor3m,fill = y))+geom_histogram(stat="count", width=0.8)+
   ggtitle("Euribor 3 month rate vs customer response distribution")+xlab("Euribor3m ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
eur
#nr.emp
nr.emp<-ggplot(data,aes(nr.employed,fill = y))+geom_histogram(stat="count", width=0.8,bins = 3,binswidth = 0.05)+
   ggtitle("Number of Employees vs Customer Response Distribution")+xlab("nr.employed ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
nr.emp


#Analysis of outliers with box plot

num_variable = list("duration_min","campaign","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed")
b= list()
for (value  in num_variable){
   b[[value]]<-ggplot(data,aes_string(x = value,fill = data$y)) +geom_boxplot(alpha = 0.8)+ scale_fill_discrete(name = "y")+scale_fill_manual(values = c("Red","Steelblue"))
   
}
do.call(grid.arrange, c(b, ncol=2))

#---------------------------------------------Multivariable analysis------------------------------------------
#age,education with marital status
ggplot(data, aes(x = age, y = education))+
   geom_bar(aes(fill = y), stat = "identity",
      position = position_dodge(0.9))+facet_wrap(~marital)+
   scale_fill_manual(values = c("Red","Steelblue"))


table(data$job,data$month,data$y)

#------------------------------------------pre processing-------------------------------------------------
data_prep<- data %>% 
   mutate(job = as.numeric(job),
          marital = as.numeric(marital),
          education = as.numeric(education),
          default = as.numeric(default),
          housing = as.numeric(housing),
          loan = as.numeric(loan),
          contact = as.numeric(contact),
          month = as.numeric(month),
          previous = as.numeric(previous),
          day_of_week = as.numeric(day_of_week),
          poutcome = as.numeric(poutcome),
          age_group = as.numeric(age_group),
          y = as.numeric(y)
          )
str(data_prep)

dim(data_prep)

# dropping age and duration  from dataset since it has been grouped and converted to min respectively
data_prep1<- data_prep[,c(2:10,12:23)]
str(data_prep1)
view(data_prep1)
#------------------------------------------------------Feature selection------------------------------- 
# feature selection of all numeric attributes can be done by correlation matrix
cor_data<-data_prep1
cor_test<- round(cor(cor_data),2)
corrplot(cor_test,type = "upper",is.corr = F,method = "number",
         col = brewer.pal(n = 8,name = "RdBu"),
         tl.col = "black",
         tl.srt = 45,diag = FALSE)

#dropping euribor3m(14,17,18,21),emp_var_rate,nr_employed,duration from analysis
data_clean<-data_prep1[,c(1:13,15:16,19:20)]
str(data_clean)

#normalizing 
normalize <- function(x) {
   return ((x - min(x)) / (max(x) - min(x)))
}
# normalize the data
data_clean <- as.data.frame(lapply(data_clean, normalize))     #(all set)




