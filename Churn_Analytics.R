# ML PROJECT: Customer Churn Behaviour Analytics . 
# Data used: From USCI Repository, Kaggle 
# Written By Mr. Murera Gisa.
# _________________________________


install.packages("patchwork")
install.packages("caret")
install.packages("vcd")
install.packages("lme4")
install.packages(c("corrplot","DMwR","InformationValue","ROCR","randomForest",
                   "xgboost","xgboostExplainer","ggmosaic","e1071","ranger",
                   "penalized","ggcorrplot","caTools","doMC"))
install.packages("ggthemes")
install.packages("vip")
library(tidyverse)
library(ggthemes)
library(patchwork)
library(caret)
library(gridExtra)
library(grid)
library(vcd)
library(knitr)
library(corrplot)
library(scales)
library(lme4)
library(DMwR)
library(InformationValue)
library(ROCR)
library(rpart)
library(randomForest)
library(xgboost)
library(xgboostExplainer)
library(MASS)
library(ggmosaic)
library(e1071)
library(ranger)
library(penalized)
library(rpart.plot)
library(ggcorrplot)
library(caTools)
library(doSNOW)
library(doMC)
library(DT)
library(plotly)
registerDoMC(cores=4)
#____________________________________________
#Setting working directory
setwd("C:/Users/Murera Gisa/Desktop/TWETTER&CHURN")
#_____________________________________
#Importing data
bankChurn <- read_csv("C:/Users/Murera Gisa/Desktop/TWEETER&CHURN/Churn_Modelling.csv")
View(bankChurn)
dim(bankChurn)
glimpse(bankChurn)
str(bankChurn)
#Tabulating (DT library for interactive and flexible table)
#_____________________________
xtable::xtable(bankChurn)# Generating latex script to tabulate the data
datatable(bankChurn[,-1], colnames = c('ID' = 1),class = 'cell-border stripe',
          caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 'Table 1: ',
                                            htmltools::em("Source: Mgisa Churn Analytics."))
          #setting header black
          ,options = list(autoWidth = FALSE, initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': 'blue'});",
            "}")))
#Data Munging&Wrangling (CLeaning)
#______________________________
#1. Removing unwanted variables and converting other as factors for classification
bankChurn <- bankChurn %>% 
  dplyr::select(-RowNumber, -CustomerId, -Surname) %>% #remove unwanted column 
  mutate(Geography = as.factor(Geography),
         Gender = as.factor(Gender),
         HasCrCard = as.factor(HasCrCard),
         IsActiveMember = as.factor(IsActiveMember),
         Exited = as.factor(Exited),
         Tenure = as.factor(Tenure),
         NumOfProducts = as.factor(NumOfProducts))
#2. DESCRIPTIVE STATISTICS
#____________________________
library(skimr)
skimmed <- skim(bankChurn)
print(skimmed)
# Checking NA
sapply(bankChurn, function(x) sum(is.na(x)))
sum(is.na(bankChurn)) #Checking the total missing values in data
# Function for detecting NA observations: 
na_rate <- function(x) {x %>% is.na() %>% sum() / length(x)}

sapply(bankChurn, na_rate)
#No missing values obtained

#EXPLORATION OF VARIABLES (FEATURES ENGINEERING)
#______________________________
#Overview of data
summary(bankChurn)
#________________________________________VISUALIZATION OF LABELS______________________
         #CREATION OF THEME

blank_theme <- theme_bw() +
  theme(
    axis.title.x = element_text(angle = 360, face = "bold", colour = "red", size = 15),
    axis.text.x = element_text(angle = 360, face = "bold", colour = "black", size = 12),
    axis.title.y = element_text(angle = 90, face = "bold", colour = "red", size = 15),
    axis.text.y = element_text(angle = 45, face = "bold", colour = "black", size = 12),
    legend.position= "none",
    #panel.border = element_blank(),
    #panel.grid= element_blank(),
    #axis.ticks = element_blank(),
    plot.title = element_text(size=16, face="bold", color="forest green")
  )
#Visualization of Output variables (Counting the customer status in data)

counting_cust_status <- bankChurn %>% count(Exited) %>% 
  mutate(Rate=round(prop.table(table(bankChurn$Exited))*100, digits = 2)) %>% 
  mutate(Churnlabels= as.factor(fct_recode(Exited,  Non_Churned= "0",Churned = "1"))) %>%
  ggplot(aes(x=Churnlabels , y= n, fill = Churnlabels)) + 
  geom_bar(stat = "identity", width = 0.8, show.legend = FALSE) + theme_bw() + blank_theme+
  labs(x= "Customer Churn Status", y= "Customers' total number", caption =  "Source: Mgisa Churn Analytics") +
  scale_fill_manual(values = c("Non_Churned" = "blue", "Churned"=  "cyan"), aesthetics = "fill")+
  scale_x_discrete(limits = c("Non_Churned","Churned")) + ggtitle("BANK'S CUSTOMER STATUS") + 
  geom_text(aes(label=  str_c(Rate,"%")),vjust= 4.5,size= 6, color= "black")
#+theme(legend.position= "top", axis.text.x = element_text(angle = 45, face = "bold", colour = "black", size = 15),axis.title.x= element_text(size = 12,face = "bold"), axis.title.y = element_text(angle = 90,vjust = 0.3,face = "bold")) 

plotly::plotly_build(counting_cust_status)
#______________________________________________________

          #TABULATING DATA

datatable(bankChurn, colnames = c('ID' = 1),class = 'cell-border stripe',
          caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', 
                                            'Table 2: ',
                                            htmltools::em("Source: Mgisa Churn Analytics."))
          #setting header black
          ,options = list(autoWidth = FALSE, initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")))
#______________________________________________________________
#Tabulating the output (And compute the percentage of customers status)
table(bankChurn$Exited)
round(prop.table(table(bankChurn$Exited)),3) #Most of our customers did not churn
#Overall distribution of all variables
#_______________________________________________
              #1. Histogram for continuous variables

HistCont<-bankChurn %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x=value,fill=key), color="black") +theme_bw()+
  facet_wrap(~ key, scales = "free") +
  theme_minimal() + labs(x="Corresponding Value",y= "Total Numbers", 
                         caption =  "Source: Mgisa Churn Analytics")+ 
  ggtitle("CONTINUOUS VARIABLE DISTRIBUTION")+
  theme(legend.position = 'none',
        #axis.text.x = element_text(angle = 45, face = "bold", colour = "black",size = 15),
        axis.title.x= element_text(size = 18,face = "bold",color="red"), 
        axis.title.y = element_text(size=18, angle = 90,vjust = 0.3,face = "bold",color = "red"),
        plot.title = element_text(size=16, face="bold", color="forest green")) 
HistCont
plotly_build(HistCont)
#_________________________________
         #Correlation Matrix (Checking multicollineality)

numericVarName <- names(which(sapply(bankChurn, is.numeric)))
corr <- cor(bankChurn[,numericVarName], use = 'pairwise.complete.obs')
ggcorrplot(corr, lab = TRUE, title = "CORRELATION AMONG CONT. VARIABLES")
# Any high correlation between the continuous variables 
#(i.e. no multicollinearity). So I'll keep all this continuous variables.
#_________________________________________

  #2. Categorical Variables Distribution

HistCateg<- bankChurn %>%
  dplyr::select(-Exited) %>% 
  keep(is.factor) %>%
  gather() %>%
  group_by(key, value) %>% 
  summarize(n = n()) %>% 
  ggplot() +
  geom_bar(mapping=aes(x = value, y = n, fill=key), color="black", stat='identity') + 
  coord_flip() + facet_wrap(~ key, scales = "free") +
  theme_minimal() +  theme_minimal() + labs(y="Counted Value",x= "Value", 
                         caption =  "Source: Mgisa Churn Analytics")+ 
  ggtitle("CATEGORICAL VARIABLE DISTRIBUTION")+
  theme(legend.position = 'none',
        #axis.text.x = element_text(angle = 45, face = "bold", colour = "black",size = 15),
        axis.title.x= element_text(size = 18,face = "bold",color="red"), 
        axis.title.y = element_text(size=18, angle = 90,vjust = 0.3,face = "bold",color = "red"),
        plot.title = element_text(size=16, face="bold", color="forest green"))
HistCateg
plotly_build(HistCateg)
#_____________________________________________________
#Continuous Variables Exploration
#1. Age

age_hist <- ggplot(bankChurn, aes(x = Age, fill = Exited)) +
  geom_histogram(binwidth = 5) + theme_bw()+
  theme_minimal() + labs(y="Counted Customers", x= "Customer's Age", 
                        caption =  "Source: Mgisa Churn Analytics")+ 
  ggtitle("AGE VARIABLE EXPLORATION")+
  theme(legend.position = 'none',
        axis.title.x= element_text(size = 18,face = "bold",color="red"), 
        axis.title.y = element_text(size=18, angle = 90,vjust = 0.3,face = "bold",color = "red"),
        plot.title = element_text(size=16, face="bold", color="forest green"))+
  scale_x_continuous(breaks = seq(0,100,by=10), labels = comma)

age_boxplot <- ggplot(bankChurn, aes(x = Exited, y = Age, fill = Exited)) +
  geom_boxplot() + theme_minimal() +labs(y="Customer's Age", x= "Churn's Status", 
                          caption =  "Source: Mgisa Churn Analytics")+ 
  ggtitle("AGE VARIABLE EXPLORATION")+
  theme(legend.position = 'right',
        axis.text.x = element_text(size=18, angle = 360, vjust = 0.5, face="bold",color = "black"),
        axis.title.x= element_text(size = 18,face = "bold",color="red"), 
        axis.title.y = element_text(size=18, angle = 90,vjust = 0.3,face = "bold",color = "red"),
        plot.title = element_text(size=16, face="bold", color="forest green"))
  

age_hist | age_boxplot
#Comments:
#Outliers above 60 years old maybe our stable customers.
#Churned customers are mostly around 40 to 50. 
#They might need to switch to other banking service for retirement purpose or whole family issue.
#___________________________
#2. Balance
balance_hist <- ggplot(bankChurn, aes(x = Balance, fill = Exited)) +
  geom_histogram() +
  theme_minimal() + labs(y="Counted Customers", x= "Customer's Balance", 
                            caption =  "Source: Mgisa Churn Analytics")+ 
  ggtitle("BALANCE VARIABLE EXPLORATION")+
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, face="bold"),
        axis.text.y = element_text(angle = 45, vjust = 0.5, face="bold"),
        axis.title.x= element_text(size = 18,face = "bold",color="red"), 
        axis.title.y = element_text(size=18, angle = 90,vjust = 0.3,face = "bold",color = "red"),
        plot.title = element_text(size=16, face="bold", color="forest green"))+
  scale_x_continuous(breaks = seq(0,255000,by=30000), labels = comma) 
  

balance_box <- ggplot(bankChurn, aes(x = Exited, y = Balance, fill = Exited)) +
  geom_boxplot() +    
  theme_minimal() + labs(y="Customer's Total Balance", x= "Churn's Status", 
                         caption =  "Source: Mgisa Churn Analytics")+ 
  ggtitle("BALANCE VARIABLE EXPLORATION")+
  theme(legend.position = 'right',
        axis.text.x = element_text(size=18, angle = 360, vjust = 0.5, face="bold",color = "black"),
        axis.text.y = element_text(angle = 45, vjust = 0.5, face="bold"),
        axis.title.x= element_text(size = 18,face = "bold",color="red"), 
        axis.title.y = element_text(size=18, angle = 90,vjust = 0.3,face = "bold",color = "red"),
        plot.title = element_text(size=16, face="bold", color="forest green"))

balance_hist | balance_box
#Comments:
#We can see the distribution of these two groups are quite similar.
#Surprisingly some non-churned customers have lower balance than churned customers.
#_________________________
#3.Credit Score
credit_hist <- ggplot(bankChurn, aes(x = CreditScore, fill = Exited)) +
  geom_histogram() + theme_minimal() + labs(y="Counted Customers", x= "Customer's Credit Score", 
                                            caption =  "Source: Mgisa Churn Analytics")+ 
  ggtitle("CREDIT SCORE VARIABLE EXPLORATION")+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=8, angle = 360, vjust = 0.5, face="bold"),
        axis.text.y = element_text(size=8, angle = 45, vjust = 0.5, face="bold"),
        axis.title.x= element_text(size = 18,face = "bold",color="red"), 
        axis.title.y = element_text(size=18, angle = 90,vjust = 0.3,face = "bold",color = "red"),
        plot.title = element_text(size=16, face="bold", color="forest green"))
  

credit_box <- ggplot(bankChurn, aes(x = Exited, y = CreditScore, fill = Exited)) +
  geom_boxplot() + 
  theme_minimal() + labs(y="Customer's Credit Score", x= "Churn's Status", 
                         caption =  "Source: Mgisa Churn Analytics")+ 
  ggtitle("CREDIT SCORE VARIABLE EXPLORATION")+
  theme(legend.position = 'right',
        axis.text.x = element_text(size=18, angle = 360, vjust = 0.5, face="bold",color = "black"),
        axis.text.y = element_text(size=8, angle = 45, vjust = 0.5, face="bold"),
        axis.title.x= element_text(size = 18,face = "bold",color="red"), 
        axis.title.y = element_text(size=18, angle = 90,vjust = 0.3,face = "bold",color = "red"),
        plot.title = element_text(size=16, face="bold", color="forest green"))

credit_hist | credit_box
#Comments:
#Some customers with extremely low credit score (on the left tail) as well as 
#with high credit score also churned, it indicates that really low and high
#quality customer are easily churn than the average quality customer.
#_________________________________________
#4. Estimated Salary
estimated_hist <- ggplot(bankChurn, aes(x = EstimatedSalary, fill = Exited)) +
  geom_histogram() + theme_minimal() + labs(y="Counted Customers", x= "Customer's Estimated Salary", 
                                            caption =  "Source: Mgisa Churn Analytics")+ 
  ggtitle("ESTIMATED SALARY VARIABLE EXPLORATION")+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=8, angle = 90, vjust = 0.5, face="bold"),
        axis.text.y = element_text(size=8, angle = 45, vjust = 0.5, face="bold"),
        axis.title.x= element_text(size = 18,face = "bold",color="red"), 
        axis.title.y = element_text(size=18, angle = 90,vjust = 0.3,face = "bold",color = "red"),
        plot.title = element_text(size=16, face="bold", color="forest green"))

estimated_box <- ggplot(bankChurn, aes(x = Exited, y = EstimatedSalary, fill = Exited)) +
  geom_boxplot() + 
  theme_minimal() + labs(y="Customer's Estimated Salary", x= "Churn's Status", 
                         caption =  "Source: Mgisa Churn Analytics")+ 
  ggtitle("ESTIMATED SALARY VARIABLE EXPLORATION")+
  theme(legend.position = 'right',
        axis.text.x = element_text(size=18, angle = 360, vjust = 0.5, face="bold",color = "black"),
        axis.text.y = element_text(size=8, angle = 45, vjust = 0.5, face="bold"),
        axis.title.x= element_text(size = 18,face = "bold",color="red"), 
        axis.title.y = element_text(size=18, angle = 90,vjust = 0.3,face = "bold",color = "red"),
        plot.title = element_text(size=16, face="bold", color="forest green"))

estimated_hist | estimated_box
#Comments:
#Both groups have a very similar distribution.
#Esimated Salary might not be a very important infomation to decide if a customer will churn or not.
#_______________________________________
#Categorical Variables Exploration
gender_graph <- bankChurn %>%
  dplyr::select(Gender, Exited) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(Gender), fill = Exited)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'Gender')

geography_graph <- bankChurn %>%
  dplyr::select(Geography, Exited) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(Geography), fill = Exited)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'Geography')

tenure_graph <- bankChurn %>%
  dplyr::select(Tenure, Exited) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(Tenure), fill = Exited)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'Tenure')

HasCrCard_graph <- bankChurn %>%
  dplyr::select(HasCrCard, Exited) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(HasCrCard), fill = Exited)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'HasCrCard')

IsActiveMember_graph <- bankChurn %>%
  dplyr::select(IsActiveMember, Exited) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(IsActiveMember), fill = Exited)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'IsActiveMember')

NumOfProducts_graph <- bankChurn %>%
  dplyr::select(NumOfProducts, Exited) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(NumOfProducts), fill = Exited)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'NumOfProducts')
(gender_graph | geography_graph) / (IsActiveMember_graph | HasCrCard_graph ) / (tenure_graph | NumOfProducts_graph)
#Comments:
#1.Female are more likely to churn than male
#2.Customers in Germany are more likely to churn than customers in France and Spain
#3.In-active customers are more likely to churn than active (very reasonable)
#4.HasCrCard may not be a useful feature as we cannot really tell if a customer has credit card will churn or not
#5.Customers in different tenure groups don't have an apparent tendency to churn or stay
#6.Customers who use 3 or 4 product are extremely likely to churn
#___________________________________________________________________
       #FEATURES SELECTION BY CHI-SQUARE TEST METHOD

chi.square <- vector()
p.value <- vector()
cateVar <- bankChurn %>% 
  dplyr::select(-Exited) %>% 
  keep(is.factor)

for (i in 1:length(cateVar)) {
  p.value[i] <- chisq.test(bankChurn$Exited, unname(unlist(cateVar[i])), correct = FALSE)[3]$p.value
  chi.square[i] <- unname(chisq.test(bankChurn$Exited, unname(unlist(cateVar[i])), correct = FALSE)[1]$statistic)
}

chi_sqaure_test <- tibble(variable = names(cateVar)) %>% 
  add_column(chi.square = chi.square) %>% 
  add_column(p.value = p.value)
knitr::kable(chi_sqaure_test)
#Comments:
#The chi-square for Tenure and HasCrCard are pretty small, at the same time, 
#their p-values are greater than 0.05, so it confirms our hypothesis that these
#two features will not provide useful information on the reponse (target) variable. 
#Thus I decided to drop these two variables.
#_____________________________________________
  #FEATURE SELECTION USING CARET
#________________________________________
#Feature selection using rfe in caret (Recursive Feature Elimination)
control <- rfeControl(functions = rfFuncs,
                      method = "cv",
                      repeats = 3,
                      verbose = FALSE)
outcomeName<-'Exited'
predictors<-names(bankTrain)[!names(bankTrain) %in% outcomeName]
Loan_Pred_Profile <- rfe(bankTrain[,predictors], bankTrain[,outcomeName],
                         rfeControl = control)
Loan_Pred_Profile # The top variables are 5 (age,#of products, Balance and Activeness of customers)

#Taking only the top 5 predictors
predictors<-c("Age","NumOfProducts","Balance","Geography","IsActiveMember")
#__________________________________________________
#Dropping the unwanted variables
bankChurn <- bankChurn %>% 
  dplyr::select(-Tenure, -HasCrCard)
#______________________________________________
        #Build Predictive Models
        #_______________________#

  #1. PREPROCESSING DATA
#1.1 Splitting data into test and training set, 
#I'll split the data using a stratified sampling approach.
# Set the seed for reproducibility
set.seed(1234)
sample_set <- bankChurn %>%
  pull(.) %>% 
  sample.split(SplitRatio = .75)

bankTrain <- subset(bankChurn, sample_set == TRUE)
dim(bankTrain)
bankTest <- subset(bankChurn, sample_set == FALSE)
dim(bankTest)
#View(bankTest)
#______________________________________
  #1.2. BALANCING THE UNBALANCED CLASS
#Let's look at the class distribution again.
round(prop.table(table(bankChurn$Exited)),3) # Entire dataset
round(prop.table(table(bankTrain$Exited)),3) # Training dataset
round(prop.table(table(bankTest$Exited)),3) #Test dataset
#Comments: The class labels are not balanced, 79.6% and 20.4% for non-churned and churned respectively

#We will use SMOTE function from DMwR package to balance them (assign them the same weight)
bankTrain <- SMOTE(Exited~., data.frame(bankTrain), perc.over = 100, perc.under = 200)
#Make a look at the data
round(prop.table(table(dplyr::select(bankTrain, Exited), exclude = NULL)),4)
#Comments: The class labels are balanced 50% for churned and non-churned.

cat("The dimension of the training set is (", dim(bankTrain), ")")
# Step 3: # Create the test sample

cat("The dimension of test set is (", dim(bankTest), ")")

# SELECTING IMPORTANCE VARIABLES IN TRAINING SET 
featurePlot(x = bankTrain[,1:9], 
            y = bankTrain$Exited, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

#Note: Works after on-hot coding (Coverting tha categorical variables in dummies)
           ##__________ PREDICTIVE ML MODELS#____________
#______________________
# Define the training control
fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 10,                      # number of folds
  savePredictions = 'final',       # saves predictions for optimal tuning parameter
  selectionFunction = "best",       
  allowParallel = TRUE
  ) 
#__________________________________
#Model1. Logit Model
logit.mod <- glm(Exited ~., family = binomial(link = 'logit'), data = bankTrain)
summary(logit.mod)
## Predict the outcomes against our test data
logit.pred.prob <- predict(logit.mod, bankTest, type = 'response')
logit.pred <- as.factor(ifelse(logit.pred.prob > 0.5, 1, 0))
head(bankTest,10)
Confmatrix<-caret::confusionMatrix(logit.pred, bankTest$Exited, positive = "1")
#________________________
#Model2 CART, Classification and Regression Trees
ctrl <-  trainControl(method = "cv", #cross-validation
               number = 10, #10-fold
               selectionFunction = "best",
               allowParallel = TRUE
               )

grid <-  expand.grid(
    .cp = seq(from=0.0001, to=0.005, by=0.0001)
  )
set.seed(1234)
tree.mod <-
  train(
    Exited ~.,
    data = bankTrain,
    method = "rpart",
    metric = "Kappa",
    trControl = ctrl,
    tuneGrid = grid
  )
## Make predictions based on our candidate model
tree.pred.prob <- predict(tree.mod, bankTest, type = "prob")
tree.pred <- predict(tree.mod, bankTest, type = "raw")
###View Confusion Matrix
ConfMatrixTree<-caret::confusionMatrix(tree.pred, bankTest$Exited, positive = "1")
#________________
#Model 3 Random Forest
#Create a control object.
ctrl <- trainControl(method = "cv",
                     number = 10,
                     selectionFunction = "best",
                     allowParallel = TRUE
                     )

## Create a grid search based on the available parameters.
grid <- expand.grid(.mtry = c(1:8))

## Build the random forest model
rf.mod <- 
  train(Exited ~.,
        data = bankTrain,
        method = 'rf',
        metric = 'Kappa',
        trControl = ctrl,
        tuneGrid = grid)
## Make the predictions
rf.pred <- predict(rf.mod, bankTest, type = "raw")
rf.pred.prob <- predict(rf.mod, bankTest, type = "prob")

#View Confusion Matrix
ConfMatix_rf<- caret::confusionMatrix(rf.pred, bankTest$Exited, positive = "1")

#Model4: XGBTREE Model
## Create a control object
ctrl <-
  trainControl(method = "cv",
               number = 10,
               selectionFunction = "best",
               allowParallel = TRUE
               )

#modelLookup("xgbTree")
## Grid Search
grid <- expand.grid(
  nrounds = 40,
  max_depth = c(4,5,6,7,8),
  eta =  c(0.1,0.2,0.3,0.4,0.5),
  gamma = 0.01,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = c(0.5, 1)
)

## Build XGBoost
set.seed(1234)
xgb.mod <-
  train(
    Exited ~ .,
    data = bankTrain,
    method = "xgbTree",
    metric = "Kappa",
    trControl = ctrl,
    tuneGrid = grid
  )
## Make the prediction
xgb.pred <- predict(xgb.mod, bankTest, type = "raw")
xgb.pred.prob <- predict(xgb.mod, bankTest, type = "prob")
#View Confusion Matrix
ConfMatrix_xgb<-caret::confusionMatrix(xgb.pred, bankTest$Exited, positive = "1")
#________________________________
#Model 5 sda model
model_sda <- train(Exited~., data = bankTrain, method = 'sda' ,
                   metric= "Kappa",
                   trControl = fitControl)
#Make prediction
sda.pred.prob <- predict(model_sda, bankTest, type = "prob")
sda.pred <- predict(model_sda, bankTest, type = "raw")
#View Confusion Matrix
ConfMatrix_sda<-caret::confusionMatrix(sda.pred, bankTest$Exited, positive = "1")

#Model 6 C5.0Tree (Single C5.0 Tree) model
model_c5.0 <- train(Exited~., data = bankTrain, method = 'C5.0Tree' ,
                    trControl = fitControl, 
                    metric = "Kappa")
# predict the outcome on a test set (Testing model or model validation)
c5.0.pred.prob <- predict(model_c5.0, bankTest, type = "prob")
c5.0.pred <- predict(model_c5.0, bankTest, type = "raw")
#View Confusion Matric
ConfMatrix_c5.0<-caret::confusionMatrix(c5.0.pred, bankTest$Exited, positive = "1")

#Model 7 Naive Bayes
model.naiveb <- train(Exited~., data = bankTrain, method = 'naive_bayes' ,
                    trControl = fitControl, 
                    metric = "Kappa")
# predict the outcome on a test set (Testing model or model validation)
naiveb.pred.prob <- predict(model.naiveb, bankTest, type = "prob")
naiveb.pred <- predict(model.naiveb, bankTest, type = "raw")
#View Confusion Matric
ConfMatrix_naive<-caret::confusionMatrix(naiveb.pred, bankTest$Exited, positive = "1")

#Model 8 Multivariate Adaptive Regression Spline
model.mars <- train(Exited~., data = bankTrain, method = 'earth' ,
                      trControl = fitControl, 
                      metric = "Kappa")
# predict the outcome on a test set (Testing model or model validation)
mars.pred.prob <- predict(model.mars, bankTest, type = "prob")
mars.pred <- predict(model.mars, bankTest, type = "raw")
#View Confusion Matric
ConfMatrix_mars<-caret::confusionMatrix(mars.pred, bankTest$Exited, positive = "1")

#Model 9 Adaptive Boosting Machine

model.adaboost <- train(Exited~., data = bankTrain, method = 'adaboost' ,
                      trControl = fitControl, 
                      metric = "Kappa")
# predict the outcome on a test set (Testing model or model validation)
adaboost.pred.prob <- predict(model.adaboost, bankTest, type = "prob")
adaboost.pred <- predict(model.adaboost, bankTest, type = "raw")

#View Confusion Matric
ConfMatrix_adaboost<-caret::confusionMatrix(adaboost.pred, bankTest$Exited, positive = "1")

#Model 10. Ctree (Conditional Inference Tree) Model 
model.ctree <- train(Exited~., data = bankTrain, method = 'ctree' ,
                     trControl = fitControl,
                     metric = "Kappa")
ctree.pred.prob <- predict(model.ctree, bankTest, type = "prob")
ctree.pred <- predict(model.ctree, bankTest, type = "raw")
#View Confusion Matric
ConfMatrix_ctree<-caret::confusionMatrix(ctree.pred, bankTest$Exited, positive = "1")

#Model 11. Linda(Robust Linear Discriminant Analysis aka Constructor) model
model.linda <- train(Exited~., data = bankTrain, method = 'Linda' ,
                     trControl = fitControl,
                     metric = "Kappa")
linda.pred.prob <- predict(model.linda, bankTest, type = "prob")
linda.pred <- predict(model.linda, bankTest, type = "raw")
#View Confusion Matric
ConfMatrix_linda<-caret::confusionMatrix(linda.pred, bankTest$Exited, positive = "1")

#Model 12. AdaBag model
model.adaBag <- train(Exited~., data = bankTrain, method = "AdaBag" ,
                     trControl = fitControl,
                     metric = "Kappa")
adaBag.pred.prob <- predict(model.adaBag, bankTest, type = "prob")
adaBag.pred <- predict(model.adaBag, bankTest, type = "raw")
#View Confusion Matric
ConfMatrix_adaBag<-caret::confusionMatrix(adaBag.pred, bankTest$Exited, positive = "1")
#___________________________

       # RUN resamples() TO COMPARE THE MODELS
        #____________________________________
# 1. Compare model performances using resample()
models_compare<- resamples(list(XGBTREE = xgb.mod,NBayes=model.naiveb, MARS=model.mars,RForest = rf.mod))
                                
#,XGBTREE = xgb.mod,LINDA = model.linda,GLM=logit.mod,ADABOOST = model.adaboost,RForest = rf.mod, CART = tree.mod,C5.0Tree = model_c5.0,CITree=model.ctree,MARS=model.mars, SDA=model_sda))

# Summary of the models performances
summary(models_compare)

#Let's plot the resamples summary output.
# Draw box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales, main='The Comparative Performance of Learning Algorithms')
 
#2. USING DIFFERENT ML METRICS
      
                    #1 LOGISTIC REGLESSION.
## Logistic Regression
test <- bankTest$Exited
pred <- logit.pred
prob <- logit.pred.prob

# Logistic Regression ROC curve
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, main = "ROC Curve for Bank Churn Prediction Learning Machine", col = 2, lwd = 2)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)

## Logistic Regression Performance Metrics
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- tibble(Learning_Machine="Logistic Regression", accuracy = accuracy, fmeasure = fmeasure,kappa = kappa, auc = auc)
     
        # CLASSIFICATION TREE
test <- bankTest$Exited
pred <- tree.pred
prob <- tree.pred.prob[,2]

## Classification Tree ROC Curve
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, col=3, lwd = 2, add=TRUE)

## Classification Tree Performance Metrics
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- comparisons %>%
  add_row(Learning_Machine="Classification Tree", accuracy = accuracy, fmeasure = fmeasure, kappa = kappa, auc = auc) 
 
        # RANDOM FOREST

test <- bankTest$Exited
pred <- rf.pred
prob <- rf.pred.prob[,2]

## Random Forest ROC Curve
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, col=4, lwd = 2, add=TRUE)

## Random Forest Performance Metrics
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- comparisons %>%
  add_row(Learning_Machine="Random Forest", accuracy = accuracy, fmeasure = fmeasure, kappa = kappa, auc = auc) 
 
      # EXTREME GRADIENT BOOSTING MACHINE
## XGBoost
test <- bankTest$Exited
pred <- xgb.pred
prob <- xgb.pred.prob[,2]

# Plot ROC Curve.
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, col=5, lwd = 2, add=TRUE)

# Get performance metrics.
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- comparisons %>%
  add_row(Learning_Machine="eXtreme Gradient Boosting", accuracy = accuracy, fmeasure = fmeasure, kappa = kappa, auc = auc) 
#____________________________________________________________
 
        # Statistical Discriminant Analytics
## sda
test <- bankTest$Exited
pred <- sda.pred
prob <- sda.pred.prob[,2]

# Plot ROC Curve.
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, col=6, lwd = 2, add=TRUE)

# Get performance metrics.
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- comparisons %>%
  add_row(Learning_Machine="Stat. Discriminant Analysis", accuracy = accuracy, fmeasure = fmeasure, kappa = kappa, auc = auc) 
#___________________________________________
         # Single C5.0 Tree
## C5.0Tree
test <- bankTest$Exited
pred <- c5.0.pred
prob <- c5.0.pred.prob[,2]

# Plot ROC Curve.
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, col=7, lwd = 2, add=TRUE)

# Get performance metrics.
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- comparisons %>%
  add_row(Learning_Machine="Single C5.0Tree", accuracy = accuracy, fmeasure = fmeasure, kappa = kappa, auc = auc) 
#____________________________________
                # Naive Bayes
## naiveb
test <- bankTest$Exited
pred <- naiveb.pred
prob <- naiveb.pred.prob[,2]

# Plot ROC Curve.
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, col=8, lwd = 2, add=TRUE)

# Get performance metrics.
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- comparisons %>%
  add_row(Learning_Machine="Naive Bayes", accuracy = accuracy, fmeasure = fmeasure, kappa = kappa, auc = auc) 
#__________________________________
  #Multivariate Adaptive Regression Spline  
## mars
test <- bankTest$Exited
pred <- mars.pred
prob <- mars.pred.prob[,2]

# Plot ROC Curve.
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, col=9, lwd = 2, add=TRUE)

# Get performance metrics.
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- comparisons %>%
  add_row(Learning_Machine="Multivariate Adaptive Regression Spline", accuracy = accuracy, fmeasure = fmeasure, kappa = kappa, auc = auc) 
#_________________________________________
           # Adaptive Boosting Machine
## adaboost
test <- bankTest$Exited
pred <- adaboost.pred
prob <- adaboost.pred.prob[,2]

# Plot ROC Curve.
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, col=10, lwd = 2, add=TRUE)

# Get performance metrics.
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- comparisons %>%
  add_row(Learning_Machine="Adaptive Boosting Machine", accuracy = accuracy, fmeasure = fmeasure, kappa = kappa, auc = auc) 
#_______________________________________
            # CONDITIONAL INFERENCE TREE
## CTree
test <- bankTest$Exited
pred <- ctree.pred
prob <- ctree.pred.prob[,2]

# Plot ROC Curve.
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, col=11, lwd = 2, add=TRUE)

# Get performance metrics.
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- comparisons %>%
  add_row(Learning_Machine="Conditional Inference Tree", accuracy = accuracy, fmeasure = fmeasure, kappa = kappa, auc = auc) 
#___________________________________
# Constructor Model
## linda
test <- bankTest$Exited
pred <- linda.pred
prob <- linda.pred.prob[,2]

# Plot ROC Curve.
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, col=12, lwd = 2, add=TRUE)

# Get performance metrics.
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- comparisons %>%
  add_row(Learning_Machine="Constructor Learning Machine", accuracy = accuracy, fmeasure = fmeasure, kappa = kappa, auc = auc) 
#______________________________________
# Adaptive Learning Machine with Bagging
## adaBag
test <- bankTest$Exited
pred <- adaBag.pred
prob <- adaBag.pred.prob[,2]

# Plot ROC Curve.
roc.pred <- prediction(predictions = prob, labels = test)
roc.perf <- performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf, col=13, lwd = 2, add=TRUE)

# Get performance metrics.
accuracy <- mean(test == pred)
precision <- posPredValue(pred, test, positive = "1")
recall <- caret::sensitivity(pred, test, positive = "1")
fmeasure <- (2 * precision * recall)/(precision + recall)
confmat <- caret::confusionMatrix(pred, test, positive = "1")
kappa <- as.numeric(confmat$overall["Kappa"])
auc <- as.numeric(performance(roc.pred, measure = "auc")@y.values)
comparisons <- comparisons %>%
  add_row(Learning_Machine="Adaptive Learning with Bagging", accuracy = accuracy, fmeasure = fmeasure, kappa = kappa, auc = auc) 

# Draw ROC legend.
legend(1.2, 1.2, c('Logistic Regression', 
                   'Classification Tree', 
                   'Random Forest', 
                   'eXtreme Gradient Boosting', 
                   "Discriminant Analysis", 
                  "Single C5.0Tree",    
                  "Naive Bayes",
                  "Multivariate Adap. Reg.Spline",
                  "Adaptive Boosting Machine",
                  "Conditional Inference Tree",
                  "Constructor Learning Machine",
                  "Adaptive Learning with Bagging"), 2:13)

#OUTPUT COMPARISON TABLE
knitr::kable(comparisons)

#Since the response class are quite unmblanced so we will not use the prediction accuracy,
# we prefer to use other listed measures, for ex. from ROC Multivariate Adaptive Regression Spline
#achieves a better performance. I'll go with Mars as our final model.

# FEATURE IMPORTANCE OF WINNING MODEL
#Winner 1. MARS
varimp_mars <- varImp(model.mars)
plot(varimp_mars, main="Variable Importance with Multivariate Adaptive Regression Spline (MARS)")
#Other way to plpt the varimpo.
vip::vip(model.mars, color="black", fill="yellow")
varimp_rForest <- varImp(rf.mod)
plot(varimp_rForest, main="Variable Importance with Random Forest")
#Other way to plpt the varimpo.
vip::vip(rf.mod, color="black", fill="red")
#_____________________________________________________

         #ENSEMBLE PREDICTION
install.packages("caretEnsemble")
library(caretEnsemble)
# Example of Stacking algorithms
# create submodels
control <- trainControl(method="repeatedcv", 
                        number=10, repeats=3, 
                        savePredictions=TRUE, 
                        classProbs=TRUE,
                        allowParallel = TRUE
                        )
seed=1000
algorithmList <- c('earth', 'xgbTree', 'glm', 'naive_bayes', 'rf')
set.seed(seed)
models <- caretList(Exited~., data=bankTrain, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)
stackControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE,
                             allowParallel = TRUE)

# Ensemble the predictions of `models` to form a new combined prediction based on glm
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)
# Predict on testData
stack_predicteds <- predict(stack.glm, newdata=bankTest)
head(stack_predicteds)
#Estimating Probability
summary(stack.glm)
#___________________________END
