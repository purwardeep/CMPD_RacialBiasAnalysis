mydata <- read.table("CMPD_binary.csv", sep=",", 
                     header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
nrow(mydata)

colnames(mydata)
mydata <- mydata[complete.cases(mydata),]

write.table(mydata, file="CMPD_Stops.csv", sep=",", row.names=F)
# H2o is an R package
install.packages("h2o")
library(h2o)
localH2O <- h2o.init() # Create an H2O cloud

#h2o.removeAll() # Clean slate - just in case the cluster was already running
getwd()

trafficStop.hex = h2o.importFile(path="CMPD_Stops.csv", 
                                 na.strings = c("?"," ?","? "," ? ",""," ","NA","NaN"))


head(trafficStop.hex)
str(trafficStop.hex)
summary(trafficStop.hex)
colnames(trafficStop.hex)

# Dropping variables
trafficStop.hex$ObjectID <- NULL
trafficStop.hex$CreationDate <- NULL
trafficStop.hex$Creator <- NULL
trafficStop.hex$EditDate <- NULL
trafficStop.hex$Editor <- NULL
nrow(trafficStop.hex)

colnames(trafficStop.hex)

trafficStop.hex <- trafficStop.hex[,c(-2,-13,-24,-34,-40)]
colnames(trafficStop.hex)

#--------------------Converting to factor variables----------------------------#
trafficStop.hex$CMPD_Westover <- as.factor(trafficStop.hex$CMPD_Westover)
trafficStop.hex$CMPD_UniversityCity <- as.factor(trafficStop.hex$CMPD_UniversityCity)
trafficStop.hex$CMPD_SteeleCreek <- as.factor(trafficStop.hex$CMPD_SteeleCreek)
trafficStop.hex$CMPD_South <- as.factor(trafficStop.hex$CMPD_South)
trafficStop.hex$CMPD_Providence <- as.factor(trafficStop.hex$CMPD_Providence)
trafficStop.hex$CMPD_NorthTryon <- as.factor(trafficStop.hex$CMPD_NorthTryon)
trafficStop.hex$CMPD_North <- as.factor(trafficStop.hex$CMPD_North)
trafficStop.hex$CMPD_Metro <- as.factor(trafficStop.hex$CMPD_Metro)
trafficStop.hex$CMPD_Independence <- as.factor(trafficStop.hex$CMPD_Independence)
trafficStop.hex$CMPD_HickoryGrove <- as.factor(trafficStop.hex$CMPD_HickoryGrove)
trafficStop.hex$CMPD_Freedom <- as.factor(trafficStop.hex$CMPD_Freedom)
trafficStop.hex$CMPD_Eastway <- as.factor(trafficStop.hex$CMPD_Eastway)
trafficStop.hex$CMPD_Central <- as.factor(trafficStop.hex$CMPD_Central)

trafficStop.hex$RS_NoAction <- as.factor(trafficStop.hex$RS_NoAction)
trafficStop.hex$RS_Citation <- as.factor(trafficStop.hex$RS_Citation)
trafficStop.hex$RS_Arrest <- as.factor(trafficStop.hex$RS_Arrest)
trafficStop.hex$RS_Written <- as.factor(trafficStop.hex$RS_Written)
trafficStop.hex$RS_Verbal <- as.factor(trafficStop.hex$RS_Verbal)

trafficStop.hex$Was_a_Search_Conducted <- as.factor(trafficStop.hex$Was_a_Search_Conducted)
trafficStop.hex$Driver_Gender <- as.factor(trafficStop.hex$Driver_Gender)
trafficStop.hex$Driver_Ethnicity <- as.factor(trafficStop.hex$Driver_Ethnicity)

trafficStop.hex$DR_Asian <- as.factor(trafficStop.hex$DR_Asian)
trafficStop.hex$DR_Other <- as.factor(trafficStop.hex$DR_Other)
trafficStop.hex$DR_White <- as.factor(trafficStop.hex$DR_White)
trafficStop.hex$DR_Black <- as.factor(trafficStop.hex$DR_Black)
trafficStop.hex$DR_Native <- as.factor(trafficStop.hex$DR_Native)

trafficStop.hex$ROS_CheckPoint <- as.factor(trafficStop.hex$ROS_CheckPoint)
trafficStop.hex$ROS_Investigation <- as.factor(trafficStop.hex$ROS_Investigation)
trafficStop.hex$ROS_Safe_Movement <- as.factor(trafficStop.hex$ROS_Safe_Movement)
trafficStop.hex$ROS_Speeding <- as.factor(trafficStop.hex$ROS_Speeding)
trafficStop.hex$ROS_Vehicle_Equipment <- as.factor(trafficStop.hex$ROS_Vehicle_Equipment)
trafficStop.hex$OR_2orMore <- as.factor(trafficStop.hex$OR_2orMore)
trafficStop.hex$OR_Asian <- as.factor(trafficStop.hex$OR_Asian)
trafficStop.hex$OR_HispanicLatino <- as.factor(trafficStop.hex$OR_HispanicLatino)
trafficStop.hex$OR_NotSpecified <- as.factor(trafficStop.hex$OR_NotSpecified)
trafficStop.hex$Officer_Gender <- as.factor(trafficStop.hex$Officer_Gender)
trafficStop.hex$ROS_Driving_while_impaired <- as.factor(trafficStop.hex$ROS_Driving_while_impaired)
trafficStop.hex$ROS_Other <- as.factor(trafficStop.hex$ROS_Other)
trafficStop.hex$ROS_SeatBelt <- as.factor(trafficStop.hex$ROS_SeatBelt)
trafficStop.hex$ROS_Stop_LightSign <- as.factor(trafficStop.hex$ROS_Stop_LightSign)
trafficStop.hex$ROS_Vehicle_Regulatory <- as.factor(trafficStop.hex$ROS_Vehicle_Regulatory)
trafficStop.hex$OR_AmericanIndian <- as.factor(trafficStop.hex$OR_AmericanIndian)
trafficStop.hex$OR_Black <- as.factor(trafficStop.hex$OR_Black)
trafficStop.hex$OR_NativeHawaiian <- as.factor(trafficStop.hex$OR_NativeHawaiian)
trafficStop.hex$OR_White <- as.factor(trafficStop.hex$OR_White)
trafficStop.hex$Month_of_Stop <- as.factor(trafficStop.hex$Month_of_Stop)

summary(trafficStop.hex,exact_quantiles=TRUE)
colnames(trafficStop.hex)

trafficStop.hex$RS_NoAction <- NULL
trafficStop.hex$RS_Citation <- NULL
trafficStop.hex$RS_Arrest <- NULL
trafficStop.hex$RS_Written <- NULL
trafficStop.hex$RS_Verbal <- NULL

library(randomForest)
library(caret)

# Split dataset giving the training dataset 63.2% of the data
trafficstop.splitb <- h2o.splitFrame(data=trafficStop.hex, ratios= 0.632)

# Create a training set from the 1st dataset in the split
trafficstop.trainb <- trafficstop.splitb[[1]]

# Create a testing set from the 2nd dataset in the split
trafficstop.testb <- trafficstop.splitb[[2]]

colnames(trafficStop.hex)

y = colnames(trafficstop.trainb[c(30)])
x = colnames(trafficstop.trainb[c(-30)])
rf_b1 = h2o.randomForest(
  x=x, 
  y=y, 
  training_frame = trafficstop.trainb, 
  ntrees = 100, 
  max_depth = 4, 
  model_id="rf_tb1", 
  stopping_rounds = 2)

summary(rf_b1)


# Extracting Variable Importance
h2o.varimp(rf_b1)
h2o.varimp_plot(rf_b1)
#Variable Importances: 
#  variable relative_importance scaled_importance percentage
#1                RS_Arrest          963.094055          1.000000   0.676191
#2               Driver_Age           96.055656          0.099737   0.067441
#3 Officer_Years_of_Service           74.680664          0.077542   0.052434
#4            Driver_Gender           74.249809          0.077095   0.052131
#5                 DR_Black           60.196457          0.062503   0.042264

rf_b2 = h2o.randomForest(
  x=x, 
  y=y, 
  training_frame = trafficstop.trainb, 
  ntrees = 100, 
  max_depth = 4, 
  mtries = 6,
  model_id="rf_tb2", 
  stopping_rounds = 2)

summary(rf_b2)
h2o.varimp(rf_b2)
h2o.varimp_plot(rf_b2)

#-----------FREQUENCY TABLE---------------------#
h2o.table(trafficStop.hex$Was_a_Search_Conducted)
ft_thing <- h2o.table(trafficStop.hex[,c(1,9)])

df2 <- h2o.pivot(trafficStop.hex,index="Reason_for_Stop",column="Was_a_Search_Conducted",value="Was_a_Search_Conducted")
df2
options(max.print=100)
print(ft_thing, echo=TRUE)
colnames(trafficStop.hex)

#---------------PREDICTED DATA--------------------#
finalRF_predictions_b1 <-h2o.predict(object=rf_b1,newdata = trafficstop.testb)
mean(finalRF_predictions_b1$predict == trafficstop.testb$Was_a_Search_Conducted)


finalRF_predictions_b2 <-h2o.predict(object=rf_b2,newdata = trafficstop.testb)
mean(finalRF_predictions_b2$predict == trafficstop.testb$Was_a_Search_Conducted)

# Performance of the model

traffic.perf_b1 <- h2o.performance(rf_b1,newdata = trafficstop.testb)
h2o.confusionMatrix(rf_b1,trafficstop.testb, valid = FALSE)
h2o.auc(traffic.perf_b1)
plot(traffic.perf_b1, type = "roc")



traffic.perf_b2 <- h2o.performance(rf_b2,newdata = trafficstop.testb)
h2o.confusionMatrix(rf_b2,trafficstop.testb, valid = FALSE)
h2o.auc(traffic.perf_b2)
plot(traffic.perf_b2, type = "roc")


#-------------GRID SEARCH------------------#

x
y

gbm_params1 <- list(learn_rate = c(0.01, 0.1),
                    max_depth = c(3, 4, 5, 6)
)

# Train a cartesian grid of GBMs
gbm_grid1 <- h2o.grid("gbm",
                      x = x, 
                      y = y,
                      grid_id = "gbm_grid1",
                      training_frame = trafficstop.trainb, 
                      ntrees = 100,
                      seed = 10000000,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by validation AUC
gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                             sort_by = "auc",
                             decreasing = TRUE)
print(gbm_gridperf1)

# Grab the top GBM model, chosen by validation AUC
best_gbm1 <- h2o.getModel(gbm_gridperf1@model_ids[[1]])

# Now let's evaluate the model performance on a test set
# so we get an honest estimate of top model performance
best_gbm_perf1 <- h2o.performance(model = best_gbm1,
                                  newdata = trafficstop.testb)
h2o.auc(best_gbm_perf1)  # 0.7781932

# Look at the hyperparamters for the best model
print(best_gbm1@model[["model_summary"]])













#-------------Random forest using original dataset--------------------#

# H2o is an R package
install.packages("h2o")
library(h2o)
localH2O <- h2o.init() # Create an H2O cloud

setwd("C:\\Users\\purwa\\Downloads\\Business_Analytics_UNCC\\Second_Semester\\Advanced_BI\\PROJECT_CMPD")
mydata <- read.table("Officer_Traffic_Stops.csv", sep=",", 
                     header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
nrow(mydata)

colnames(mydata)
mydata <- mydata[complete.cases(mydata),]

write.table(mydata, file="Traffic_Stops.csv", sep=",", row.names=F)
# write the csv file of the output



#h2o.removeAll() # Clean slate - just in case the cluster was already running
getwd()
setwd("C:\\Users\\purwa\\Downloads\\Business_Analytics_UNCC\\Second_Semester\\Advanced_BI\\PROJECT_CMPD")

trafficStop.hex = h2o.importFile(path="Traffic_Stops.csv", 
                                 na.strings = c("?"," ?","? "," ? ",""," ","NA","NaN"))


head(trafficStop.hex)
str(trafficStop.hex)
summary(trafficStop.hex)
colnames(trafficStop.hex)
nrow(trafficStop.hex)

# Dropping variables
trafficStop.hex$ObjectID <- NULL
trafficStop.hex$CreationDate <- NULL
trafficStop.hex$Creator <- NULL
trafficStop.hex$EditDate <- NULL
trafficStop.hex$Editor <- NULL
trafficStop.hex$Editor <- NULL
#trafficStop.hex$Month_of_Stop <- NULL
trafficStop.hex$Result_of_Stop <- NULL
nrow(trafficStop.hex)

colnames(trafficStop.hex)

#--------------------Converting to factor variables----------------------------#
trafficStop.hex$Month_of_Stop <- as.factor(trafficStop.hex$Month_of_Stop)
trafficStop.hex$Officer_Gender <- as.factor(trafficStop.hex$Officer_Gender)
trafficStop.hex$Driver_Ethnicity <- as.factor(trafficStop.hex$Driver_Ethnicity)
trafficStop.hex$Was_a_Search_Conducted <- as.factor(trafficStop.hex$Was_a_Search_Conducted)
trafficStop.hex$Reason_for_Stop <- as.factor(trafficStop.hex$Reason_for_Stop)
trafficStop.hex$Driver_Gender <- as.factor(trafficStop.hex$Driver_Gender)
#trafficStop.hex$Result_of_Stop <- as.factor(trafficStop.hex$Result_of_Stop)
trafficStop.hex$Officer_Race <- as.factor(trafficStop.hex$Officer_Race)
trafficStop.hex$Driver_Race <- as.factor(trafficStop.hex$Driver_Race)
trafficStop.hex$CMPD_Division <- as.factor(trafficStop.hex$CMPD_Division)


#trafficStop.hex <- trafficStop.hex[c(1:10,12)]
#h2o.describe(trafficStop.hex)

#str(trafficStop.hex)
summary(trafficStop.hex,exact_quantiles=TRUE)
colnames(trafficStop.hex)

library(randomForest)
library(caret)

# Split dataset giving the training dataset 63.2% of the data
trafficstop.split <- h2o.splitFrame(data=trafficStop.hex, ratios= 0.632)

# Create a training set from the 1st dataset in the split
trafficstop.train <- trafficstop.split[[1]]

# Create a testing set from the 2nd dataset in the split
trafficstop.test <- trafficstop.split[[2]]


y = colnames(trafficstop.train[c(10)])
x = colnames(trafficstop.train[c(-10)])
rf = h2o.randomForest(
  x=x, 
  y=y, 
  training_frame = trafficstop.train, 
  ntrees = 100, #use a maximum of 100 trees to create the random tree model. 
  #Default is 50. I have increased it because i will let the 
  #early stopping criteria decide when the random forest is sufficiently accurate
  max_depth = 3, 
  model_id="rf_traffic1", 
  stopping_rounds = 2# stop fitting new trees when the 2-tree average is within 0.001 
  #(default) of the prior two  2-tree averages
  # can be though of as a convergence setting
)

summary(rf)


# Extracting Variable Importance
h2o.varimp(rf)
h2o.varimp_plot(rf)
#Variable Importances: 
#  variable relative_importance scaled_importance percentage
#1            Driver_Gender          300.190369          1.000000   0.225713
#2          Reason_for_Stop          265.780090          0.885372   0.199840
#3 Officer_Years_of_Service          236.035629          0.786286   0.177475
#4            CMPD_Division          233.599640          0.778172   0.175643
#5              Driver_Race          180.141632          0.600091   0.135448
#6               Driver_Age          104.928566          0.349540   0.078896
#7             Officer_Race            7.286146          0.024272   0.005478
#8         Driver_Ethnicity            2.004200          0.006676   0.001507
#9           Officer_Gender            0.000000          0.000000   0.000000



rf2 = h2o.randomForest(
  x=x, 
  y=y, 
  training_frame = trafficstop.train, 
  ntrees = 100, #use a maximum of 100 trees to create the random tree model. 
  #Default is 50. I have increased it because i will let the 
  #early stopping criteria decide when the random forest is sufficiently accurate
  max_depth = 3, 
  mtries = 6, # mtries using variable importance plot
  model_id="rf_traffic2", 
  stopping_rounds = 2# stop fitting new trees when the 2-tree average is within 0.001 
  #(default) of the prior two  2-tree averages
  # can be though of as a convergence setting
)

summary(rf2)
h2o.varimp(rf2)
h2o.varimp_plot(rf2)


finalRF_predictions <-h2o.predict(object=rf,newdata = trafficstop.test)
mean(finalRF_predictions$predict == trafficstop.test$Was_a_Search_Conducted)


finalRF_predictions2 <-h2o.predict(object=rf2,newdata = trafficstop.test)
mean(finalRF_predictions2$predict == trafficstop.test$Was_a_Search_Conducted)

nrow(trafficstop.test)



traffic.perf <- h2o.performance(rf,newdata = trafficstop.test)
h2o.confusionMatrix(rf,trafficstop.test, valid = FALSE)
h2o.auc(traffic.perf)
plot(traffic.perf, type = "roc") 


traffic.perf2 <- h2o.performance(rf2,newdata = trafficstop.test)
h2o.confusionMatrix(rf2,trafficstop.test, valid = FALSE)
h2o.auc(traffic.perf2)
plot(traffic.perf2, type = "roc") 


h2o.shutdown(prompt = F)















