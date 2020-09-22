            
    
     #BNR RECRUITMENT CHALLENGE FOR SENIOR OFFICER, DATA SCIENTIST
     #____________________________________________________________

# CHALLENGE TITLE: Recession Prediction. 
# Data used: Monthly Real Country Data   
# Source: Given from BNR.
# Written By Mr. Murera Gisa.
#--------------------------------------------------------------

#STEP 1: Set Working Directory
#
setwd("C:/Users/Murera Gisa/Desktop/BNR_ASSIGNMENT")

#STEP 2: Load the R Packages and Libraries.
#_________________________________________


data_cleaning_packages <- c("tidyverse", "ggplot2", "xtable", "knitr","kableExtra", "openxlsx", "vtable")
machine_learning_packages <- c("MASS", "caret", "kernlab","rpart","earth","gbm","xgboost","fastAdaboost",
                               "c50","randomForest","class","ada", "rda","e1071", "nnet","ipred",
                               "dbarts", "klaR", "glmn","caretEnsemble")
data_visualization<-c("gridExtra","skimr","plotly","ggthemes","openair")

install.packages("vtable")
install.packages("purrr")
install.packages("lattice")
install.packages("devtools")
install.packages("caretEnsembe", dependencies = TRUE)
#Load all packages listed above
#___________________________________
if(!require(install.load)){
  install.packages("install.load")
}
install.load::install_load(c(data_cleaning_packages, machine_learning_packages,data_visualization))

#Loading Libraries
#____________________

library(ggthemes)
library(tidyverse)
library(skimr)
library(plotly)
library(gridExtra)
library(DT)
library(vtable)
library(openair)
library(install.load) 
library(purrr)
library(caret)
library(lattice)
library(kableExtra)
library(tidyverse)
library(caret)
library(e1071)
#_______________________________________

#STEP 3: Import Dataset.

Recession_data<- read.csv("C:/Users/Murera Gisa/Desktop/BNR_ASSIGNMENT/Recession_Prediction.csv",
                          stringsAsFactors = FALSE, header = TRUE)

# Checking Features Multicollinearity by PCA and select the PCs to be used
#_________________________________________________________________
#Since the skewness and magnitude of variables influence PCA we set center and scale True
prin_comp <- prcomp(Recession_data[,2:25], scale. = TRUE, center = TRUE)
#SELECTING AUTOMATICALLY THE PCS NEEDED FOR ANALYTICS

Retained_PCs = preProcess(Recession_data[,2:24],
                          method=c("BoxCox", "center", 
                            "scale", "pca"))
Retained_PCs #Only 9 PCs retained since they are contributing 95% of total variability (variance)in data set.
PCs <-predict(Retained_PCs, Recession_data[,2:24])
# Retained PCs 
head(PCs, 3)
#Creating Data from 9 PCs
data<-cbind(PCs,Recession_data$Recession)
View(data)
Recess_data<- data %>% as.data.frame()%>%mutate(Recession=data$`Recession_data$Recession`)
dim(Recess_data)
View(Recess_data)
Recess_data<- Recess_data[,-10]
dim(Recess_data)
View(Recess_data)
colnames(Recess_data)
#___________________________________

#STEP 4: Mining Insight Hidden from Data.
 
dim(Recession_data) #Dimension of data
vtable(Recession_data, factor.limit =0) #Interactive descriptive variable data table 
datatable(Recession_data) # Interactive data table
View(Recession_data) #Viewing data
str(Recession_data) #Structure of data
summary(Recession_data) #Summary statistics of data
colnames(Recession_data) # Checking the columns names

      #4.1 Descriptive Statistics.
      #_____________________________

table1::label(Recession_data$level_yield_curve) <- "Level yield curve"
table1::label(Recession_data$slope_yield_curve) <- "Slope yield curve"
table1::label(Recession_data$curvature_yield_curve) <- "Curvature yield curve"
table1::label(Recession_data$BAA_spread) <- "BBA Spread"
table1::label(Recession_data$AAA_spread) <- "AAA Spread "
table1::label(Recession_data$real_M2) <- "Real Money Supply "
table1::label(Recession_data$credit_spread) <- "Spread of Credits"
table1::label(Recession_data$M2) <- "Money Supply"
table1::label(Recession_data$Dollar_index) <- "Dollar Index"
table1::label(Recession_data$Ouput) <- "Output "
table1::label(Recession_data$Income) <- "National Income"
table1::label(Recession_data$permits) <- "Permits "
table1::label(Recession_data$Civilian_Unemployment_Rate) <- "Unemployment Rate"
table1::label(Recession_data$non_farm_payrol) <- "Non Farm Payrol "
table1::label(Recession_data$all_employ.payrol) <- "Tot. Employment Payrol"
table1::label(Recession_data$Insurance_claim_unempl) <- "Insurence Claims "
table1::label(Recession_data$S.P_500_Index) <- "SP_500 Index "
table1::label(Recession_data$SWISS_EX) <- " Swiss Exchange"
table1::label(Recession_data$FEDFUNDS) <- "FED Funds "
table1::label(Recession_data$confidence_ind_manufac) <- "Confidence IND_Man "
table1::label(Recession_data$IP_manafac) <- "IP Manafac. "
table1::label(Recession_data$Yen) <- "Yen Currency "

# Tuning Descriptive statistics table
table1::table1(~level_yield_curve + slope_yield_curve + curvature_yield_curve + BAA_spread +AAA_spread +real_M2 +credit_spread +M2+Dollar_index+Ouput+Income+permits+Civilian_Unemployment_Rate+non_farm_payrol+all_employ.payrol+Insurance_claim_unempl+S.P_500_Index+SWISS_EX+FEDFUNDS+confidence_ind_manufac+IP_manafac+Yen, data = Recession_data)

#Count the class labels
Recession_occurence<-Recession_data%>%count(Recession)
Percentage_of_Recession_occurence <- round(prop.table(table(Recession_data$Recession))*100, digits = 2)
#_____________________________________________
      #4.2 Data Visualization (STATIC PLOTS).

#Removing the first column 

Recession_data1<-Recession_data[,-1]

#PLOT1. Correlation matrix of numerical variables.
res<-cor(Recession_data1)
corrplot::corrplot(res, type = "upper", order = "hclust", 
  tl.col = "black", tl.srt = 45) # Displaying worse due to large variables((It requires a large screen or selection of variables))

#PLOT2. Correlation of variables together with:
       #(i) Histogram distribution of each variables
       #(ii) Scatter Plot with fitted lines
       #(iii) Value of the correlation
       #(iv) Significance level of the variables correlation.
PerformanceAnalytics::chart.Correlation(Recession_data1, histogram=TRUE, pch=19) #Displaying worse due to the large variables(It requires a large screen or selection of variables)

# Random Selection of variables

PerformanceAnalytics::chart.Correlation(Recession_data1[,c(2,4,5,6,8)], histogram=TRUE, pch=19)

#PLOT3. Recession Occurence

factor_variable_position <- c(26)
Recession_data<- Recession_data %>% mutate_at(.,vars(factor_variable_position),~as.factor(.)) # Convert output into factor variable

Recession_Occurs <- Recession_data[,-1] %>% mutate(Recession= as.factor(fct_recode(Recession, Non_Recession= "0",Recession = "1"))) # Re-naming the output

#Plotting 
Graph <- Recession_Occurs%>% count(Recession) %>% mutate(Rate=round(prop.table(table(Recession_Occurs$Recession))*100, digits = 2)) %>% ggplot(aes(x= Recession, y= n, fill = Recession)) + geom_bar(stat = "identity", width = .8, show.legend = FALSE) + theme_bw() + labs(x= "Recession Occurence", y= "Recession Counts", caption =  "Source: BNR_mgisa") + scale_fill_manual(values = c("Non_Recession" = "green", "Recession"=  "red"), aesthetics = "fill") + geom_text(aes(label=  str_c(Rate,"%")),vjust= 4.5,size= 8,angle=45, color= "black")+ggtitle("Rate of Rwandan Economic Recession Occurence (1969-2019) ")+theme(axis.text.x = element_text(angle = 360, face = "bold", colour = "black", size = 15),axis.title.x= element_text(size = 12,face = "bold"), axis.title.y = element_text(angle = 90,vjust = 0.3,face = "bold"),plot.title = element_text(size = 15, face="bold", colour="red"),legend.position="none") 

Graph
ggsave("Graph.png", width = 10, height = 7) # Saving graph in working directory

#PLOT4 Separation Quality of features (Box Plots of all variables Vs Recession)
#_______________________________________
plot_box <- function(df, cols, col_x = "Recession") {
  for (col in cols) {
    p <- ggplot(df, aes(x = .data[[col_x]], y = .data[[col]], fill = .data[[col_x]])) +
      geom_boxplot(show.legend = FALSE) +
      scale_fill_manual(values = c("Non_Recession" = "green", "Recession" = "red"), aesthetics = "fill") +
      labs(
        x = "Recession Occurence", y = str_c(col),
        title = str_c("Box plot of", col, "vs", col_x, sep = " "),
        caption = "Source: BNR_mgisa"
      ) +theme(
        axis.text.x = element_text(face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold")
      )
    
    print(p)
  }
}

num_cols <-
  Recession_Occurs %>%
  select_if(is.numeric) %>%
  colnames()

boxplt<-plot_box(Recession_Occurs, num_cols)
#_____________________________________________________
#STEP 5: Data Preparation For Machine Learning (ML) Modelling.

#For Binary Classification we need to convert output into factor

factor_variable_position <- c(10)
Recession_data<- Recess_data %>% mutate_at(.,vars(factor_variable_position),~as.factor(.)) # Convert output into factor variable

# Create the training and test datasets for Recession data
#
   # 5.1: Get row numbers for the training data
partition <- createDataPartition(Recession_data$Recession, p = 0.8, list = FALSE)

   # 5.2: Create the training sample
train_RecessionData <- Recession_data[partition, ]

cat("The dimension of the training set is (", dim(train_RecessionData), ")")

   # 5.3: Create the test sample
test_RecessionData <- Recession_data[-partition, ]

cat("The dimension of test set is (", dim(test_RecessionData), ")")
  
    # 5.4 Scaling the continuous variables

preProcess_scale_model <- preProcess(train_RecessionData, method = c("center", "scale"))
# Here is what preProcess_scale_model does
# It only normalized the continuous variables and ignore none continuous one.
print(preProcess_scale_model)

   #5.5 Creation of Training and Test data set 

train_RecessionData <- predict(preProcess_scale_model, train_RecessionData) # rescaled train Recession data (xytrain)
xytrain<-train_RecessionData

test_RecessionData <- predict(preProcess_scale_model, test_RecessionData) # rescaled test Recession data (xytest)
xytest<-test_RecessionData
# Removing the output column to create the xtrain and xtest

xtrain <- train_RecessionData[-length(train_RecessionData)] 
xtest <- test_RecessionData[-length(test_RecessionData)]

  #5.6 Feature Selection to include relevant variables in the model
   
# Eliminate low variance variables

near_zero <- nearZeroVar(xtrain, freqCut = 95 / 5, uniqueCut = 10, saveMetrics = TRUE)

low_variance_cols <- near_zero[(near_zero$zeroVar == TRUE) | (near_zero$nzv == TRUE), ]

print(low_variance_cols) # This has shown that no feature has been qualified as low variance feature. Then no variable dropped out for both test and train datasets

  # Store X and Y for later use.

xtrain <- xtrain
ytrain <- xytrain$Recession

xtest <- xtest
ytest <- xytest$Recession

ntr <- nrow(xytrain)
nte <- nrow(xytest)

#__________________________________________

#STEP 6: Train and Tuning the ML Algorithms Models.

   #6.1 Model1: Linear Discriminant Analysis (LDA)
   
set.seed(12345)
lda.model <- train(Recession~., data = xytrain, method = "lda",trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid"),  metric = "Accuracy") 
yhat.lda <- predict(lda.model, xytest)
Conf_Mat <- table(ytest, yhat.lda)
#__________________

   #6.2 Model2: Naive_bayes model
set.seed(12345)
naiveb.model <- train(Recession~., data = xytrain, method = "naive_bayes",trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid"),  metric = "Accuracy") 
yhat.naiveb <- predict(naiveb.model, xytest) # Model Prediction
Conf_Mat <- confusionMatrix(ytest, yhat.naiveb)
#___________________________
    
     #6.3 Model3: Kernel k Nearest Neabors (kkNN) Model
     
set.seed(12345)
kknn.model <- train(Recession~., data = xytrain, method = "kknn",trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid"),  metric = "Accuracy") 
yhat.kknn <- predict(kknn.model, xytest)
Conf_Mat <- confusionMatrix(ytest, yhat.kknn)
#______________________________

    #6.4 Model4: Classification and Regression Tree (CART) Model
set.seed(12345)
caret_model <- train(Recession~., data = xytrain, method= "rpart", trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid"),  metric = "Accuracy") 

yhat.caret <- predict(caret_model, xytest)
Conf_mat <- confusionMatrix(ytest, yhat.caret)
#____________________________________

   #6.5 Model5: Adaptive Boosting Learning Machine (adaboost)
   
set.seed(12345)
adaboost_model <- train(Recession~., data = xytrain, method = "adaboost", trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid"),  metric = "Accuracy") 
yhat.adaboost <- predict(adaboost_model, xytest)
Conf_Mat <- confusionMatrix(ytest, yhat.adaboost)
#__________________________

    # 6.6. Model6: Neural Network Model (Nnet)
set.seed(12345)
nnet.model <- train(Recession~., data = xytrain, method = "nnet",trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid"),  metric = "Accuracy",trace=FALSE) 
yhat.nnet <- predict(nnet.model, xytest)
Conf_Mat <- confusionMatrix(ytest, yhat.nnet)
#----------------------------------------

     #6.7 Model7: RFlda (High-Dimensional Factor-Based Linear Discriminant Analysis) Model
     #
RFlda.model <- train(Recession~., data = xytrain, method = "RFlda",trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid"),  metric = "Accuracy") 
yhat.RFlda <- predict(RFlda.model, xytest)
Conf_Mat <- confusionMatrix(ytest, yhat.RFlda)
#________________________
     #6.8 Model8: Random Forest Model (rForest)
set.seed(12345)
rforest.model <- train(Recession~.,data = xytrain,
  method = "rf", trControl = trainControl(
    method = "repeatedcv", number = 10,repeats=3), metric = "Accuracy", ntree = 20, importance = TRUE)
yhat.rforest <- predict(rforest.model, xytest)
Conf_Mat <- confusionMatrix(ytest, yhat.rforest)
#_____________________________
    
     #6.9 Model9: Gradient Boosting Machine (GBM)
set.seed(12345)
gbm.model <- train(Recession~.,data = xytrain,
  method = "gbm", trControl = trainControl(
    method = "repeatedcv", number = 10,repeats=3), metric = "Accuracy", tuneGrid=expand.grid(interaction.depth=1:2,shrinkage=0.1,n.trees=c(10,50,100),n.minobsinnode=10),verbose=FALSE)

yhat.gbm <- predict(gbm.model, xytest)
Conf_Mat <- confusionMatrix(ytest, yhat.gbm)
#_____________________
    #6.10 Model 10: Multivariate Adaptive Regression Splines (MARS)
    
set.seed(12345)

mars.model <- train(Recession~.,data = xytrain, method = "earth",
  trControl = trainControl(method = "repeatedcv", number = 10,repeats = 3),tuneGrid = data.frame(degree = 1, nprune = (2:4) * 2),metric= "Accuracy")
yhat.mars <- predict(mars.model, xytest)
Conf_Mat <- confusionMatrix(ytest, yhat.mars)
#______________________________________

#STEP 7: Model Performance Evaluation.

  #7.1 EVALUATION METRICS :Matthews Correlation Coefficients (MCC)

mcc <- function (actual, predicted)
{
  TP <- sum(actual == 1 & predicted == 1)
  TN <- sum(actual == 0 & predicted == 0)
  FP <- sum(actual == 0 & predicted == 1)
  FN <- sum(actual == 1 & predicted == 0)
  #TP;TN;FP;FN # for debugging
  sum1 <- TP+FP
  sum2 <- TP+FN
  sum3 <- TN+FP
  sum4 <- TN+FN
  
  denom <- as.double(sum1)*sum2*sum3*sum4 # as.double to avoid overflow error on large products
  if (any(sum1==0, sum2==0, sum3==0, sum4==0)) {
    denom <- 1
  }
  mcc <- ((TP*TN)-(FP*FN)) / sqrt(denom)
  return(mcc)
}

#Model Performance Evaluation By Computing mcc of each model
mcc_lda <- mcc(ytest, yhat.lda)
mcc_naive <- mcc(ytest, yhat.naiveb)
mcc_kknn <- mcc(ytest, yhat.kknn)
mcc_cart <- mcc(ytest, yhat.caret)
mcc_adaboost <- mcc(ytest, yhat.adaboost)
mcc_nnet <- mcc(ytest, yhat.nnet)
mcc_RFlda <- mcc(ytest, yhat.RFlda)
mcc_rforest <- mcc(ytest, yhat.rforest)
mcc_gbm <- mcc(ytest, yhat.gbm)
mcc_mars <- mcc(ytest, yhat.mars)

#Comparing MLA by their mcc to select the optimal one with high mcc

Model_evaluation <- tibble(LDA = mcc_lda, Naive_Bayes = mcc_naive, KernelKnn = mcc_kknn,CART = mcc_cart, Adaboost = mcc_adaboost, Nnet = mcc_nnet,RFlda= mcc_RFlda, rForest = mcc_rforest, GBM = mcc_gbm, MARS = mcc_mars)
mcc <- t(Model_evaluation)
Model <- rownames(mcc)
comparision_table <- as_tibble(mcc) %>% add_column(Model) %>% rename(mcc = "V1") %>%  arrange(mcc) %>% add_column(SN = 1:length(Model),.after = 0) %>% dplyr::select(SN, Model, mcc)

#Interactive presentation of Model Performance
#_______________________________________________
write.table(comparision_table, "ML_Performance.csv", sep=",", row.names = F)
# Visualize the comparative table
# ___________________________________
Compadata <- read.csv("C:/Users/GISA/Desktop/BNR_ASSIGNMENT/ML_Performance.csv")

Plot<- Compadata %>% ggplot(aes(x=Model, y=mcc, fill= mcc)) +  geom_bar(stat = "identity") + coord_polar() + labs(title = "MLA Mathews Correlation Coefficient", caption=  "Source: BNR_mgisa")+ theme(legend.position = "right", axis.title = element_blank(),axis.line = element_blank(),axis.text.y= element_blank(),axis.text.x = element_text(angle = 45, face = "bold", colour = "purple", size = 15)) +theme(legend.text = element_text(colour="blue", size=10,face= "bold")) + theme(legend.title = element_text(colour="red", size = 20,face="bold"))
Plot
ggsave("Plot.png", width = 10, height = 7) # Saving graph in working directory

#STEP 8: Optimal Model Selection.

#The model with the maximum Matthews Correlation Coefficient is an accurate optimal model to select and later used to predict the economic recession period.

# Model selection with maximum Mathews Correlation Coefficient.

maxmcc <- comparision_table %>% filter(mcc == max(mcc))
kable(maxmcc, "latex", caption = "Optimal model", align = c("c", "c")) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
#Worse Model 
minmcc <- comparision_table %>% filter(mcc == min(mcc))
#_______________________________________

#STEP 9: Finalize ML Modeling (Recession period  Prediction).
# Generalization of optimal model on validation data set 

validation_data<- xytest
#Predicting of the Recession Occurence by Adaboost Machine
Recession_Prediction <- predict(adaboost_model, validation_data)
#Convert to the data frame

Predicted_Recession <- tibble(Recession = Recession_Prediction)
Predicted_Recession_table<-Predicted_Recession %>%
count(Recession) %>% mutate(Percent = round(n / sum(n) * 100, 2))

#PREDICTION OF RECESSION LABEL(OCCURENCE) IN NEXT 6 MONTHS
#______________________________________

Recession_label <- read.csv("C:/Users/GISA/Desktop/BNR_ASSIGNMENT/Pred_Recession.csv",stringsAsFactors = FALSE, header = TRUE)

Recession_label<- cbind(Recession_label,Predicted_Recession[1:6,])

Sumbission_Format<-as.data.frame(Recession_label)

kable(Sumbission_Format, "latex","latex", caption = "Next six month ML Predicted Recession Labels") # Turn Latex Code
#__________________
                   #END OF SCRIPT____________________
      
#Author: Mr. Murera Gisa
#Email: elgisamur@gmail.com/mgisa@aims.ac.rw