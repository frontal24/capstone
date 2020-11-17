library(tidyverse)
library(ggplot2)                  # Visualzation 
library("corrplot")
library(gridExtra)            # grid view of the graph
library(RColorBrewer)
library('caret')             # splitting the data 
#library(DMwR)         # Smote balancing 

bank_additional <- read_delim("C:/Users/User/Downloads/bank-additional.csv", ";", escape_double = FALSE, trim_ws = TRUE)
head(bank_additional) 
data<- bank_additional
#View(data)


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



 # reducing the levels in job by combining self-employed and entreprenuer
# services and house maid 

levels(data$job) <- c("admin", "blue-collar", "self-employed",'services','management',
                      'unemployed','self-employed','services','student','technician','unemployed',
                      'unknown')

 # combining 3 basic 4y,6y,9y education as one 
levels(data$education)<- c('Basic','Basic','Basic','high.school',
                           'uneducated','professional.course','university.degree','unknown')
table(education)




#duration from seconds to minutes
data<- data %>%
         mutate(duration_min = round(duration/60,2))

# searching for missing values 
sum(is.na(data))


 #fixing pdays attribute : are marked as 999 
data$pdays[data$pdays == '999']<- -1
data$pdays = as.factor(data$pdays)
table(data$pdays)

#checking unknown category variable 
sum(apply(data, 1, function(x) any(x %in% c("unknown")))) 


table(age_group,data$education)

data$default[data$default =='unknown']<- 'no' # imputation of default 'unknown ' to no :803
data$default<- as.factor(data$default)
table(data$default)

# loan
data$loan[data$loan =='unknown']<- 'no' # imputation of default 'unknown ' to no :105
data$loan<- as.factor(data$loan)
table(data$loan)
dim(data)

unk_mar<-(which(data$marital == 'unknown'))
# marital status
data<- data[-unk_mar,]                     # deleted 11 rows with unknown marital status
dim(data)

# education
unk_edu<- (which(data$education == 'unknown')) 
unk_edu# deleting the unknown values from data :167
data<-data[-unk_edu,]
dim(data)

unk_house<-(which(data$housing == 'unknown')) # housing loan status
data<- data[-unk_house,]
dim(data)


View(data)
data<-data[,c(2:10,12:23)]
#final summary of the data 
summary(data)
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
   ggtitle("Job vs Customer response ")+xlab(" job \nself-employed = entreprenuer and  self-employed\n services = services and house maid" )+
   ylab("Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold"),axis.text.x = element_text(angle = 45)) +
   scale_fill_manual(values = c("Red","Steelblue"))
j

#education

e<-ggplot(data,aes(data$education,fill = y))+geom_bar(stat="count", width=0.8,)+
   ggtitle("Education vs Customer response ")+xlab("Education\n basic includes 4y,6y,9y education ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold"),axis.text.x = element_text(angle = 45)) +
   scale_fill_manual(values = c("Red","Steelblue"))
e

#default
d<-ggplot(data,aes(default,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle(" Default in credit vs \nCustomer Response ")+xlab("Credit default ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
d

#housing loan
h<-ggplot(data,aes(housing,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle("Housing loan vs Customer Response ")+xlab("Housing Loan ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
h

#loan
l<-ggplot(data,aes(loan,fill = y))+geom_bar(stat="count", width=0.8)+
ggtitle("Personal Loan vs Customer Response ")+xlab("Personal Loan  ")+
   ylab(" Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
l
#contact
c<-ggplot(data,aes(contact,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle("Contact type vs Customer Response ")+xlab("Contact ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
c
#month
mnth<-ggplot(data,aes(month,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle("Month vs customer response ")+xlab("Month ")+
   ylab(" Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))#
mnth

#day_of_week
d<-ggplot(data,aes(day_of_week,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle("Day of the week vs \nCustomer response ")+xlab("Days of the week ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
d

#age
ag<-ggplot(data,aes(age_group,fill = y))+geom_bar(stat="count", width=0.8)+
ggtitle("Age vs customer response ")+
   xlab("Age\nTeen = 13-19\nYounder adult = 20-35\nMiddle age = 36-45\nOlder adult = 46-60\nRetired = above 60 ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
ag

#duration
dur<-ggplot(data,aes(duration_min,fill = y))+geom_histogram(stat="count", width=0.8,bins = 3)+
   ggtitle("Duration of the call made 
           Vs Customer response ")+xlab("Duration of call ")+
   ylab("Frequency")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
dur

#campaign
camp<-ggplot(data,aes(campaign,fill = y))+geom_histogram(stat="count", width=0.8)+
   ggtitle("Call count for the current campaign
           Vs customer response ")+
   xlab("Customer Campaign ")+ylab("Frequency")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
camp

#pday
pday<-ggplot(data,aes(pdays,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle("Days passed since previous campaign calls 
           Vs customer response ")+
   xlab("pdays")+ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
pday

# previous
prev<-ggplot(data,aes(previous,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle("Previous vs customer response ")+xlab("Previous  ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))#

prev
#poutcome
pout<-ggplot(data,aes(poutcome,fill = y))+geom_bar(stat="count", width=0.8)+
   ggtitle("Outcome of previous campaign 
           vs customer response ")+xlab("poutcome ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
 scale_fill_manual(values = c("Red","Steelblue"))
pout

#emp_var_rate
evar<-ggplot(data,aes(emp.var.rate,fill = y))+geom_histogram(stat="count", width=0.8)+
   ggtitle("Employment Variation Rate vs customer response ")+xlab("Emp var rate ")+
   ylab("frequency")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))#
evar
#cons.price.idx
price.idx<-ggplot(data,aes(cons.price.idx,fill = y))+geom_histogram(stat="count", width=0.8,binwidth = 500)+
   ggtitle("consumer price index vs \ncustomer response ")+
   xlab("consumer price index ")+ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
price.idx

#cons.conf.indx
conf.indx<-ggplot(data,aes(cons.conf.idx,fill = y))+geom_histogram(stat="count", width=0.8)+
   ggtitle("Consumer confidence index vs\ncustomer response ")+
   xlab("consumer confidence index ")+ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
conf.indx

#euribor3m
eur<-ggplot(data,aes(euribor3m,fill = y))+geom_histogram(stat="count", width=0.8)+
   ggtitle("Euribor 3 month rate vs customer response ")+xlab("Euribor3m ")+
   ylab("Customer Count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
eur
#nr.emp
nr.emp<-ggplot(data,aes(nr.employed,fill = y))+geom_histogram(stat="count", width=0.8,bins = 3,binswidth = 0.05)+
   ggtitle("Number of Employees vs Customer Response ")+xlab("nr.employed ")+
   ylab("Frequency")+labs(fill = "Customer\nResponse")+
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


#------------------------------------------pre processing-------------------------------------------------

data_prep<- data %>% 
   mutate(job = as.numeric(job),
          marital = as.numeric(marital),
          education = as.numeric(education),
          default= as.numeric(data$default),
          housing = as.numeric(data$housing),
          loan= as.numeric(data$loan),
          contact = as.numeric(contact),
          month = as.numeric(month),
          previous = as.numeric(previous),
          day_of_week = as.numeric(day_of_week),
          poutcome = as.numeric(poutcome),
          age_group = as.numeric(age_group),
          y = as.numeric(data$y),
          pdays = as.numeric(pdays)
          )

# dropping age and duration  from dataset since it has been grouped and converted to min respectively
data_prep<- data
summary(data_prep)

dim(data_prep)

#correlation analysis
cor_data<-data_prep
cor_test<- round(cor(cor_data),2)
corrplot(cor_test,type = "upper",is.corr = F,method = "number",
         col = brewer.pal(n = 8,name = "RdBu"),
         tl.col = "black",
         tl.srt = 45,diag = FALSE)


#--------------------------------------Class balancing --------------------------------
#normalizing 
normalize <- function(x) {
   return ((x - min(x)) / (max(x) - min(x)))
}

# normalize the data

data_set <- as.data.frame(lapply(data_prep, normalize))     

# Here we ue SMOTE sampling for balancing the data s class varaible.
#First we should split the data as train and test set for doing the same 
#and SMOTE should be applied on train set 

set.seed(123)
trainIndex <- createDataPartition(data_prep$y, 
                                  p = .75, 
                                  list = FALSE, 
                                  times = 1)

train1 <- data_prep[ trainIndex,]
test1  <- data_prep[-trainIndex,]


dim(test1)
dim(train1)


data_set$y <- as.factor(data_set$y) # class variable must be in factors to use smote for balancing
balanced_train<- SMOTE(y~.,data_set, perc.over = 950, perc.under = 110,k = 5)

as.data.frame(table(balanced_train$y))

str(balanced_train)






