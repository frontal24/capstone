
 #------------------------------------------------------- Bank Marketing dataset Analysis ---------------------------------------------

library(tidyverse)            #  manipulation 
library(ggplot2)              # Visualzation 
library("corrplot")            # correlation
library(gridExtra)            # grid view of the plot
library('DMwR')             # SMOTE the data - balancing 
library(caret)              # confusionMatrix 
library(rpart)              #decision tree 
library(rpart.plot)         # decision tree plot 
library(Boruta)            # features  selection 
library(e1071)              #naive bayes
library(ROCR)                #Prediction 

#importing the data
bank_additional <- read_delim("/Users/ashlyabraham/Downloads/bank-additional/bank-additional.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)
data<- bank_additional
dim(data)                # dimension of the data set

str(data)                #structure of data
summary(data)


#Feature engineering age ,duration variable for visualization
#age
data<-data %>%
      mutate(age_group = case_when(
         age >= 13 & age <= 19 ~'Teen',
         age >= 20 & age <=35 ~'younger adult',
         age >= 36 & age <= 45 ~ "middle aged",
         age >= 46 & age <= 60 ~"older adult",
         age >= 61 ~ "retired "))

data$age_group<-as.factor(data$age_group)



#duration from seconds to minutes
data<- data %>%
   mutate(duration_min = round(duration/60,2))



#fixing pdays attribute : are marked as 999 

data$pdays[data$pdays == '999']<- -1
table(data$pdays)
which(data$job =='unknown')
#------------------------------------------------- Missing and unknown values---------------------------------
sum(is.na(data)) # searching for missing values 



sum(apply(data, 1, function(x) any(x %in% c("unknown"))))  #checking unknown category variable 



#------------------- loan-------------------
length(which(data$loan == 'unknown'))
table(data$loan)
data$loan[data$loan =='unknown']<- 'no' # imputation of default 'unknown ' to 'No'
table(data$loan)


# ------------------marital status---------------
length(which(data$marital == 'unknown'))
table(data$marital)
data$marital[data$marital == 'unknown'] <- 'married'    #imputation of default 'unknown' to 'married'
table(data$marital)



#-------------------------------------job----------------------------
length(which(data$job == 'unknown'))
table(data$job)

unknown_2<-which(data$job == 'unknown' & data$default == 'unknown'  )              # cross checking 2 attributes - Job - default
unknown_ed.job<-which(data$job == 'unknown' & data$education == 'unknown'  )       # cross checking 2 attribute - job - education 

data<-data[-unknown_2,]                     # deleting the rows with  unknown values in job and default
data<-data[-unknown_ed.job,]                # deleting the rows with unknown values in job and education


data$job[data$job == 'unknown'] <- 'admin.'     #imputation of default 'unknown' to 'married

data$job <- as.factor(data$job)

#  combining 
#self-employed and entrepreneur
# services and house maid 
#unemployed and retired 

levels(data$job) <- c("admin", "blue-collar", "self-employed",'services','management',
                      'unemployed','self-employed','services','student','technician','unemployed')

table(data$job)



#----------------------------default--------------------------------
table(data$default)
data$default[data$default =='unknown']<- 'no'                     # imputation of default 'unknown ' to 'No'
data$default<- as.factor(data$default)                            # as factor 
table(data$default)




#---------------------------------housing loan---------------------
table(data$housing)
unk_house<-which(data$housing == 'unknown')           # housing loan status
table(data$housing)
mid_var<- round(length(unk_house)/2)                 # splitting the unknown values among Y/N
for (v in unk_house){
   if (v <=unk_house[mid_var]){
      data$housing[v] ='no'
      
   }
   else{  
      data$housing[v] ='yes'
   }
}
   
table(data$housing)

# ---------------------------education-----------------------

table(data$education)

data$education[data$education == 'basic.4y'] <- 'basic'      # combining 3 basic 4y,6y,9y education as 'one'basic' 
data$education[data$education == 'basic.6y'] <- 'basic'
data$education[data$education == 'basic.9y'] <- 'basic'

unk_edu<- which(data$education =='unknown')     
mid_val<-round(length(unk_edu)/2)                 # splitting unknown values among 'basic and 'university degree'

for (v in unk_edu){
   if (v <=unk_edu[mid_val]){
      data$education[v] ='basic'
      
   }
   else{  
      data$education[v] ='university.degree'
   }
}

table(data$education)                        # table to check for levels 


#converting character variables to factor
data<-data %>%
   mutate(y = as.factor(y),education = as.factor(education),
          marital= as.factor(marital),default = as.factor(default),
          housing = as.factor(housing),loan = as.factor(loan),
          contact = as.factor(contact),poutcome = as.factor(poutcome),
          day_of_week = as.factor(day_of_week),month = as.factor(month),
          previous = as.factor(previous))

#Final check for any other unknown 
which(data == 'unknown')



#-----------deleting duration and age ---------------
data<-data[,c(2:10,12:23)]

#final summary of the data 
summary(data)
#-----------------------------------------------Analysis --------------------------------------------------------------

table(data$y) # checking the class variable (target)

# plot 
ggplot(data,aes(y,fill = y))+geom_bar(stat="count", width=0.8)+
         ggtitle("Class variable distribution")+xlab("Customer response ")+
   ylab(" Customer Count")+
   theme(plot.title=element_text(face ="bold") )+labs(fill = "Customer\nResponse")+
   scale_fill_manual(values = c("Red","Steelblue"))
 


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
   ylab("count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
dur



#campaign
camp<-ggplot(data,aes(campaign,fill = y))+geom_histogram(stat="count", width=0.8)+
   ggtitle("Call count for the current campaign
           Vs customer response ")+
   xlab("Customer Campaign ")+ylab("count")+labs(fill = "Customer\nResponse")+
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
   ylab("count")+labs(fill = "Customer\nResponse")+
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
   ylab("count")+labs(fill = "Customer\nResponse")+
   theme(plot.title=element_text(face ="bold")) +
   scale_fill_manual(values = c("Red","Steelblue"))
nr.emp




#--------------------------------correlation analysis--------------------------------------
cor_data<-data %>%
         mutate(job = as.numeric(job),
                        marital = as.numeric(marital),
                        education = as.numeric(education),
                        contact = as.numeric(contact),
                        month = as.numeric(month),
                        previous = as.numeric(previous),
                        poutcome = as.numeric(poutcome),
                        y = as.numeric(y),
                        pdays = as.numeric(pdays),
                        default = as.numeric(default),
                        housing = as.numeric(housing),
                        loan = as.numeric(loan),
                        month = as.numeric(month),
                        day_of_week = as.numeric(day_of_week),
                        campaign = as.numeric(campaign),
                        age_group = as.numeric(age_group),
                        y = as.numeric(y)
                        
)
 cor_test<- round(cor(cor_data),2)
 
 col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
 #plot 
 corrplot(cor_test, method="color", col=col(200),  
          type="upper", order="hclust", 
          tl.col="black", tl.srt=45, #Text label color and rotation
          diag=TRUE 
 )
 

(findCorrelation(cor_test,cutoff = 0.7)) # finding attributes with more than 0.7 correlation coefficient 

 # (emp.var.rate - euribor3m) , (emp.var.rate-nr.employed),(euribor3m - nr.employed) are highly correlated 


data<-data[,-c(14,17,21)]    # emp.var.rate and euribor3m are  removed along with duration_min(based on UCI repository description)

str(data) 
dim(data)
#--------------------------------Analysis of outliers with box plot------------------------

num_variable = list("campaign","cons.price.idx","cons.conf.idx","nr.employed")        # list of numerical variables 
b= list()
for (value  in num_variable){
   b[[value]]<-ggplot(data,aes_string(x = value,fill = data$y)) +
      geom_boxplot(alpha = 0.8) + 
      scale_fill_manual(values = c("Red","Steelblue"),name = 'customer\n response')
   
}

do.call(grid.arrange, c(b, ncol=2))

table(data$campaign)
(length(boxplot.stats(data$campaign)$out))    # total outliers in campaign attribute 
quant<-quantile(data$campaign,0.98) # checking for outliers above 98th percentile 
quant
out_val<-(which(data$campaign >=quant))
data<- data[-out_val,]   # removing the outliers beyond 98 %

dim(data)

#------------------------------------ Feature selection -------------------------------
bor<-Boruta(y~.,data = data,doTrace = 2)
plot(bor,cex = 1,las = 3,xlab = "",cex.axis = 0.7) # plot 

(getSelectedAttributes(bor, withTentative = F)) # not rejected attributes 

data_bor<-data[,c(1:3,7:8,11:17)] # eliminating rejected attributes 

summary(data_bor) 



#--------------------------------Class Balancing ---------------------------------------
data_model<- as.data.frame(data_bor)

# balancing the data set

data_model$y<- as.factor(data_model$y) # before balancing in SMOTE class variable should be factor

balanced_data_base<- SMOTE(y~.,data_model, perc.over = 920, perc.under = 111,k = 5) # balancing the class variable 
as.data.frame(table(balanced_data_base$y))


dim(balanced_data_base)

#--------------------------------------------- normalizing the dataset----------------------------------------------------

balanced_data_base<- balanced_data_base %>%
   mutate(job = as.numeric(job),
          marital = as.numeric(marital),
          education = as.numeric(education),
          contact = as.numeric(contact),
          month = as.numeric(month),
          previous = as.numeric(previous),
          poutcome = as.numeric(poutcome),
          y = as.numeric(y),
          pdays = as.numeric(pdays)
   )

normalize <- function(x) {
   return ((x - min(x)) / (max(x) - min(x))) # Normalizing function 
}
data_normalize <- as.data.frame(lapply(balanced_data_base, normalize)) # normalizing      


as.data.frame(table(data_normalize$y))

#---------------------------- Splitting the dataset -----------------------------------------------------
set.seed(111)

data_normalize$y<- as.factor(data_normalize$y)
trainIndex <- createDataPartition(data_normalize$y, 
p = .75, 
list = FALSE, 
times = 1)

train_norm <- data_normalize[ trainIndex,] # train set 
test_norm  <- data_normalize[-trainIndex,] # test set 

#-------------------------------- Modelling 1 : Decision tree ---------------------------

model_norm<- rpart(train_norm$y~., data = train_norm, method = 'class') # model 

rpart.plot(model_norm) # plot
# performance evaluation 
predict_norm <-predict(model_norm, test_norm, type = 'class') # on test set 
   
table_norm <- confusionMatrix(predict_norm,test_norm$y,positive =  '1') # confusion matrix for class '1'
   (table_norm)
   predict_norm1 <-predict(model_norm, test_norm, type = 'prob')
   pred_norm<- prediction(predict_norm1[,2],test_norm$y)
   
# precision,recall,f1 score and accuracy
   table_tree<- as.matrix(table(predict_norm,test_norm$y))
   dia_t = diag(table_tree) # correctly classified instances per class 
   rows_t = apply(table_tree, 1, sum) #  instances per class
   cols_t= apply(table_tree, 2, sum) # prediction per class 
   
   recall_t = dia_t /cols_t 
   precision_t = dia_t / rows_t 
   f1_t = 2 * precision_t * recall_t / (precision_t + recall_t) 
   accuracy_t = sum(dia_t)/sum(table_tree)
   
  data.frame(precision_t,recall_t,accuracy_t,f1_t) # precision,recall,accuracy,F1 score  of Decision tree 
  
  
   # AUC
   Pred_auc_norm <- performance(pred_norm, "auc")
   auc_t<-round(as.numeric(Pred_auc_norm@y.values), 2)
   auc_t # decision tree AUC 


#-------------------------------------------Naive Bayes ---------------------------
naive_Bayes_norm<-naiveBayes(y~.,data = train_norm) #model
NB_Predict_norm=predict(naive_Bayes_norm,test_norm) # on test set
nb_table_norm <- confusionMatrix(NB_Predict_norm,test_norm$y,positive = '1') #confusion matrix
nb_table_norm

# performance evaluation 
predict_norm_nb <-predict(naive_Bayes_norm, test_norm, type = 'raw')
pred_nb<- prediction(predict_norm_nb[,2],test_norm$y)

# precision, recall,accuracy  and F1 score 
table_nb<- as.matrix(table(NB_Predict_norm,test_norm$y))
dia = diag(table_nb) # correctly classified instances per class 
rows = apply(table_nb, 1, sum) #  instances per class
cols = apply(table_nb, 2, sum) # prediction per class 

precision = dia /rows
recall = dia / cols 
f1 = 2 * precision * recall / (precision + recall) 
accuracy = sum(dia)/sum(table_nb)

data.frame(precision,recall,accuracy,f1) # precision,recall,accuracy,F1 score for Naive bayes

#AUC 
Pred_auc_nbnor <- performance(pred_nb, "auc")
auc_nb<-round(as.numeric(Pred_auc_nbnor@y.values), 2)
auc_nb  # auc for Naive bayes 

# ROC   curve of both models 

#Decision tree
ROC_tree<-performance(pred_norm,'tpr','fpr')
plot(ROC_tree,
     col = 'red',xlab = "sensitivity",ylab = '1-specificity',main = "ROC of both classifers ",print.auc = T)
abline(a = 0,b = 1)

#naive bayes
ROC<-performance(pred_nb,'tpr','fpr')
plot(ROC,add = T,
     col = 'green',xlab = "sensitivity",ylab = '1-specificity',print.auc = T)
legend("right", c("Decision tree", "Naive bayes"), lty=1, 
       col = c("red", "green"), bty="o", inset=c(0,-0.15))



