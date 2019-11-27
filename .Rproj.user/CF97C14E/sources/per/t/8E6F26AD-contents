rm(list=ls())
library(tidyverse)
library(corrplot)
library(cowplot)
library(VIM)
library(missForest)
library(Boruta)
library(caret)
library(glmnet)
library(rsample)
library(Metrics)
library(randomForest)
library(scales)
library(viridis)

df_origin<- read_csv("data/data.csv")

######################################
###### Missing Values ################
######################################
# check missing value
df_origin %>%
  map_df(function(x) sum(is.na(x))) %>%
  gather(feature, num_nulls) %>%
  mutate(percentage= num_nulls/1460) %>%
  arrange(desc(percentage))

#fill missing value
data <- 
  df_origin %>% 
  mutate(PoolQC = as.factor(ifelse(is.na(PoolQC),"No_Pool",PoolQC))) %>%
  mutate(MiscFeature = as.factor(ifelse(is.na(MiscFeature),"None",MiscFeature))) %>%
  mutate(Alley = as.factor(ifelse(is.na(Alley),"No_Alley_Access",Alley))) %>%
  mutate(Fence = as.factor(ifelse(is.na(Fence),"No_Fence",Fence))) %>%
  mutate(FireplaceQu = as.factor(ifelse(is.na(FireplaceQu),"No_Fireplace",FireplaceQu))) %>%
  mutate(GarageType = as.factor(ifelse(is.na(GarageType),"No_Garage",GarageType))) %>%
  mutate(GarageYrBlt = ifelse(is.na(GarageYrBlt),0,GarageYrBlt)) %>%
  mutate(GarageFinish = as.factor(ifelse(is.na(GarageFinish),"No_Garage",GarageFinish))) %>%
  mutate(GarageQual = as.factor(ifelse(is.na(GarageQual),"No_Garage",GarageQual))) %>%
  mutate(GarageCond = as.factor(ifelse(is.na(GarageCond),"No_Garage",GarageCond))) %>%
  mutate(BsmtExposure = as.factor(ifelse(is.na(BsmtExposure),"No_Basement",BsmtExposure))) %>%
  mutate(BsmtFinType2 = as.factor(ifelse(is.na(BsmtFinType2),"No_Basement",BsmtFinType2))) %>%
  mutate(BsmtQual = as.factor(ifelse(is.na(BsmtQual),"No_Basement",BsmtQual))) %>%
  mutate(BsmtCond = as.factor(ifelse(is.na(BsmtCond),"No_Basement",BsmtCond))) %>%
  mutate(BsmtFinType1 = as.factor(ifelse(is.na(BsmtFinType1),"No_Basement",BsmtFinType1))) %>%
  mutate(MasVnrType = as.factor(ifelse(is.na(MasVnrType),"None",MasVnrType))) %>%
  mutate(MasVnrArea=ifelse(is.na(MasVnrArea),mean(MasVnrArea,na.rm = TRUE),MasVnrArea )) %>%
  mutate(LotFrontage=ifelse(is.na(LotFrontage),mean(LotFrontage,na.rm = TRUE),LotFrontage )) %>%
  mutate(Electrical = as.factor(ifelse(is.na(Electrical),"Unknown",Electrical))) %>%
  mutate_if( is.character, as.factor) %>%
  filter(GrLivArea < 4000) %>%
  select(-Id)%>%
  rename(X1stFlrSF = `1stFlrSF`, X2ndFlrSF = `2ndFlrSF`, X3SsnPorch=`3SsnPorch`)


#recheck missing value
  data %>%
  map_df(function(x) sum(is.na(x))) %>%
  gather(feature, num_nulls) %>%
  mutate(percentage= num_nulls/1022) %>%
  arrange(desc(percentage))

#####################################
########  Outliers ##################
######################################
  df_origin %>%
    ggplot(aes(x=GrLivArea, y=SalePrice, color=SalePrice)) +
    geom_point() +
    scale_color_viridis_c(option = "plasma", direction=-1)+
    scale_y_continuous(labels = comma)
  
######################################
###### Splitting Data ################
######################################
  
set.seed(55)
ames_split <- initial_split(data, prop = .75, strata = "SalePrice")
train <- training(ames_split)
test  <- testing(ames_split) 
  
save(train, file = "data/train.rda")
save(test, file = "data/test.rda")

######################################
###### Categorical Data ##############
######################################
cat_df<-
  train %>%
  select_if(is.factor)%>% 
  mutate(SalePrice= train$SalePrice) 

names(cat_df)
colNames <- names(cat_df)[1:43]
colNames

plt_list=list()
for(i in colNames){
  plt_list[[i]] <- ggplot(cat_df, aes_string(x=i, y = cat_df$SalePrice, fill= i)) +
    geom_boxplot() 
}

for (i in 1:length(plt_list)){
  plot(plt_list[[i]])
}

######################################
###### Continuous Data ##############
######################################

correlations <- cor(train%>%select_if(is.numeric), use="everything")
corrplot(correlations, method="circle", type="lower",  sig.level = 0.01, insig = "blank",tl.col="black")


######################################
############ Boruta ##################
######################################

bor.results <- Boruta(train %>% select(-SalePrice),train$SalePrice, maxRuns=101, doTrace=0)
bor.results

plot(bor.results, las=2, cex.axis=0.7, xlab='')
title("Feature Importance Using Boruta Algo")
######################################
############ RFE  ####################
######################################
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
rfe.train <- rfe(train %>% select(-SalePrice), train$SalePrice,  rfeControl=control)
rfe.train

rfe.train$optVariables

######################################################
############ Feature engineering  ###################
#########################################################

# Create training and testing feature model matrices and response vectors.
# we use model.matrix(...)[, -1] to discard the intercept
train_x <- model.matrix(SalePrice ~ ., train)[, -1]
train_y <- log(train$SalePrice)

test_x <- model.matrix(SalePrice ~ ., test)[, -1]
test_y <- log(test$SalePrice)


# Apply CV Ridge regression to ames data
cv_ridge <- cv.glmnet(x = train_x,y = train_y, alpha = 0)
# plot results
#plot(ames_ridge)

#ames_ridge_min <- glmnet(x = train_x, y = train_y, alpha = 0)
#plot(ames_ridge_min, xvar = "lambda")
#abline(v = log(ames_ridge$lambda.1se), col = "red", lty = "dashed")




ridge_coefs <- coef(cv_ridge) 

ridge_coef_frame <- 
  data.frame(ridge_coefs = rownames(ridge_coefs)[-1],value = ridge_coefs[-1,1]) %>%
  filter(value != 0) %>%
  top_n(30, wt = abs(value)) 


ggplot(ridge_coef_frame, aes(x=reorder(ridge_coefs,value), y=value, color= value > 0)) + 
  geom_pointrange(aes(ymin=0, ymax=value),show.legend = FALSE) + 
  ggtitle("Coefficients of ridge model") + 
  coord_flip() +
  ylab("Coefficient") +
  xlab(NULL) 


# Apply CV lasso regression to ames data
cv_lasso <- cv.glmnet(x = train_x, y = train_y,alpha = 1)
# plot results
#plot(cv_lasso)


lasso_coefs <- coef(cv_lasso) 

lasso_coef_frame <- 
  data.frame(lasso_coefs = rownames(lasso_coefs)[-1],value = lasso_coefs[-1,1]) %>%
  filter(value != 0)
  

ggplot(lasso_coef_frame, aes(x=reorder(lasso_coefs,value), y=value, color= value > 0)) + 
  geom_pointrange(aes(ymin=0, ymax=value),show.legend = FALSE) + 
  ggtitle("Coefficients of lasso model") + 
  coord_flip() +
  ylab("Coefficient") +
  xlab(NULL) 


# minimum Ridge MSE
min(cv_ridge$cvm)
# minimum Lasso MSE
min(cv_lasso$cvm)

# predict
lasso_pred <- predict(cv_lasso, s = cv_lasso$lambda.min, test_x)
ridge_pred <- predict(cv_ridge, s = cv_ridge$lambda.min, test_x)

#####################################################
############ Random Forest Model  ###################
#####################################################


RF_model <- randomForest(SalePrice ~ ., data=train)

# Predict using the test set
rf_prediction <- predict(RF_model, test)
rf_prediction

#model_output <- cbind(test, rf_prediction)
#model_output$log_prediction <- log(model_output$rf_prediction)
#model_output$log_SalePrice <- log(model_output$SalePrice)
#Test with RMSE
#rmse(model_output$log_SalePrice,model_output$log_prediction)

RF_model$importance


#####################################################
############ prediction  ############################
#####################################################

#predcition dataframe
prediction_df<- 
  cbind (test_y,lasso_pred,ridge_pred, log(rf_prediction)) %>%
  as.data.frame() %>%
  setnames(c("Actual_sale_price", "Lasso","Ridge", "Random_Forest")) %>%
  mutate(Actual_sale_price=exp(Actual_sale_price)) %>%
  mutate(Lasso=exp(Lasso)) %>%
  mutate(Ridge=exp(Ridge)) %>%
  mutate(Random_Forest=exp(Random_Forest)) 
  

P<- 
  prediction_df %>%
  gather('prediction_type', 'predicted_sale_price', Lasso,Ridge,Random_Forest) %>%
  ggplot(aes(x= Actual_sale_price, y=predicted_sale_price , color= Actual_sale_price))+
  geom_point(shape = 19,alpha= 0.5, size= 2) +
  geom_abline(aes(intercept = 0 , slope= 1 ), linetype= "dashed") +
  scale_size(range = c(0.5, 5))  +
  scale_x_log10(labels = comma)+
  scale_y_log10(labels = comma)+
  scale_color_viridis_c("Sale price ($)",option="plasma",labels = comma) +
  facet_wrap(~prediction_type,ncol = 3)+
  theme_bw() +
  xlab("Actual Sale Price (dollars)") +
  ylab("Predicted Sale Price (dollars)")

P

rmse(prediction_df$Actual_sale_price, prediction_df$Lasso)
rmse(prediction_df$Actual_sale_price, prediction_df$Ridge)
rmse(prediction_df$Actual_sale_price, prediction_df$Random_Forest)

dat_text <- data.frame(
  label = c("RMSE= 21612", "RMSE=21873", "RMSE=28519"),
  prediction_type = c('Lasso', 'Ridge', 
                      'Random_Forest'),
  Actual_sale_price     = c(300000, 300000, 300000),
  predicted_price    = c(50000, 50000, 50000))


P2<- P + geom_text(
  data    = dat_text,
  mapping = aes(x = Actual_sale_price, y = predicted_price, label = label), color= "black") 

P2


#####################################################
############ lASSO PREDICTION  ######################
#####################################################
lasso_coef_frame
save(lasso_coef_frame,file= "data/lasso_coef_frame.rda")

ggplot(lasso_coef_frame, aes(x=reorder(lasso_coefs,value), y=value, color= value > 0)) + 
  geom_pointrange(aes(ymin=0, ymax=value),show.legend = FALSE) + 
  ggtitle("Coefficients of lasso model") + 
  coord_flip() +
  ylab("Coefficient") +
  xlab(NULL) +
  theme(plot.title = element_text(hjust = 0.5))


fake_pred_df<- 
  data %>%
  mutate(BedroomAbvGr=3) %>%
  mutate(FullBath=2) %>%
  mutate(GrLivArea=1500) 


fake_pred_df_x <- model.matrix(SalePrice ~ ., fake_pred_df)[, -1]

baseline_predict <- predict(cv_lasso, s = cv_lasso$lambda.min, fake_pred_df_x)

#sceanrio1
scenario_1<- 
  data %>%
  mutate(BedroomAbvGr=4) %>%
  mutate(FullBath=2) %>%
  mutate(GrLivArea=1630) 


scenario_1_x <- model.matrix(SalePrice ~ ., scenario_1)[, -1]
scenario_1_predict <- predict(cv_lasso, s = cv_lasso$lambda.min, scenario_1_x)

#sceanrio2
scenario_2<- 
  data %>%
  mutate(BedroomAbvGr=3) %>%
  mutate(FullBath=2) %>%
  mutate(HalfBath=HalfBath+1) %>%
  mutate(GrLivArea=1580) 


scenario_2_x <- model.matrix(SalePrice ~ ., scenario_2)[, -1]
scenario_2_predict <- predict(cv_lasso, s = cv_lasso$lambda.min, scenario_2_x)



#sceanrio3
scenario_3<- 
  data %>%
  mutate(BedroomAbvGr=3) %>%
  mutate(FullBath=2) %>%
  mutate(GrLivArea=1900) 

scenario_3_x <- model.matrix(SalePrice ~ ., scenario_3)[, -1]
scenario_3_predict <- predict(cv_lasso, s = cv_lasso$lambda.min, scenario_3_x)

##############
###############



(exp(scenario_1_predict[10]) - exp(  baseline_predict[10] ))/exp( baseline_predict[10])
(exp(scenario_2_predict[10]) - exp(  baseline_predict[10] ))/exp( baseline_predict[10])
(exp(scenario_3_predict[10]) - exp(  baseline_predict[10] ))/exp( baseline_predict[10])


##############
###############
sample_data <- 
  data %>%
  filter(BedroomAbvGr==3) %>%
  filter(FullBath==2) %>%
  filter(GrLivArea==1500) 

sample_data_x <- model.matrix(SalePrice ~ ., sample_data)[, -1]

baseline_predict <- predict(cv_lasso, s = cv_lasso$lambda.min, sample_data_x)



scenario_1<- 
  sample_data %>%
  mutate(BedroomAbvGr=4) %>%
  mutate(GrLivArea=1630) 

scenario_1_x <- model.matrix(SalePrice ~ ., scenario_1)[, -1]
scenario_1_predict <- predict(cv_lasso, s = cv_lasso$lambda.min, scenario_1_x)

scenario_2<- 
  sample_data %>%
  mutate(BedroomAbvGr=3) %>%
  mutate(HalfBath=HalfBath+1) %>%
  mutate(GrLivArea=1580) 


scenario_2_x <- model.matrix(SalePrice ~ ., scenario_2)[, -1]
scenario_2_predict <- predict(cv_lasso, s = cv_lasso$lambda.min, scenario_2_x)


scenario_3<- 
  sample_data %>%
  mutate(GrLivArea=1900) 

scenario_3_x <- model.matrix(SalePrice ~ ., scenario_3)[, -1]
scenario_3_predict <- predict(cv_lasso, s = cv_lasso$lambda.min, scenario_3_x)


(exp(scenario_1_predict[1]) - sample_data$SalePrice[1])/sample_data$SalePrice[1]
(exp(scenario_2_predict[1]) - sample_data$SalePrice[1])/sample_data$SalePrice[1]
(exp(scenario_3_predict[1]) - sample_data$SalePrice[1])/sample_data$SalePrice[1]
(exp(scenario_1_predict[2]) - sample_data$SalePrice[2])/sample_data$SalePrice[2]
(exp(scenario_2_predict[2]) - sample_data$SalePrice[2])/sample_data$SalePrice[2]
(exp(scenario_3_predict[2]) - sample_data$SalePrice[2])/sample_data$SalePrice[2]


scenario_1_predict[1] - baseline_predict[1]
scenario_2_predict[1] - baseline_predict[1]
scenario_3_predict[1] - baseline_predict[1]
scenario_1_predict[2] - baseline_predict[2]
scenario_2_predict[2] - baseline_predict[2]
scenario_3_predict[2] - baseline_predict[2]



Option_A <- (exp(scenario_1_predict[1]) - exp(baseline_predict[1]))/exp(baseline_predict[1])
Option_B <- (exp(scenario_2_predict[1]) - exp(baseline_predict[1]))/exp(baseline_predict[1])
Option_C <- (exp(scenario_3_predict[1]) - exp(baseline_predict[1]))/exp(baseline_predict[1])

value <- c(Option_A, Option_B,Option_C) 
name <- c('Option_A', 'Option_B','Option_C')

Q3 <- data.frame(name, value)

save(Q3, file = "data/Q3.rda")

Q3 %>%
  mutate(value=value*100) %>%
  ggplot(aes(x=name, y=value, fill= name)) +
  geom_bar(stat="identity", show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels=c("Option_A" = "Add 130 sq ft bedroom", 
                            "Option_B" = "Add 80 sq ft half bath", "Option_C" = "Expand 400 sq ft livingroom")) + 
  coord_flip() +
  theme_cowplot()+
  xlab("") +
  ylab("Percentage increase in sale price")







#####################################################
############ Question 4  ############################
#####################################################
levels(data$BldgType)

sample_data_q4 <- 
  data %>%
  mutate(BldgType_1 = as.factor(ifelse(BldgType=='1Fam',"Duplex",BldgType))) %>%
  filter(BldgType=='1Fam') %>%
  filter(BedroomAbvGr==4) 
levels(sample_data_q4$BldgType_1)   
levels(sample_data_q4$BldgType_1) <- c("1Fam","2fmCon","Twnhs","TwnhsE","Duplex")
levels(sample_data_q4$BldgType_1)   


names(sample_data_q4 %>%select_if(is.numeric))

sample_data_q4_r<- 
  sample_data_q4 %>%
  select(-BldgType) %>% 
  rename("BldgType"="BldgType_1") %>%
  mutate(MSSubClass=90) %>%
  mutate(LotFrontage= LotFrontage/2) %>%
  mutate(LotArea= LotArea/2) %>%
  mutate(MasVnrArea= MasVnrArea/2) %>%
  mutate(BsmtFinSF1= BsmtFinSF1/2) %>%
  mutate(BsmtFinSF2= BsmtFinSF2/2) %>%
  mutate(BsmtUnfSF= BsmtUnfSF/2) %>%
  mutate(BsmtFullBath= BsmtFullBath/2) %>%
  mutate(BsmtHalfBath= BsmtHalfBath/2) %>%
  mutate(FullBath= FullBath/2) %>%
  mutate(HalfBath= HalfBath/2) %>%
  mutate(BedroomAbvGr= BedroomAbvGr/2) %>%
  mutate(TotalBsmtSF= TotalBsmtSF/2) %>%
  mutate(LowQualFinSF= LowQualFinSF/2) %>%
  mutate(GrLivArea= GrLivArea/2) %>%
  mutate(TotRmsAbvGrd= TotRmsAbvGrd/2) %>%
  mutate(GarageCars= GarageCars/2) %>%
  mutate(GarageArea= GarageArea/2) %>%
  mutate(WoodDeckSF= WoodDeckSF/2) %>%
  mutate(OpenPorchSF= OpenPorchSF/2) %>%
  mutate(EnclosedPorch= EnclosedPorch/2) %>%
  mutate(X3SsnPorch= X3SsnPorch/2) %>%
  mutate(ScreenPorch= ScreenPorch/2) %>%
  mutate(PoolArea= PoolArea/2) %>%
  mutate(MiscVal= MiscVal/2) 
  
sample_data_q4_x <- model.matrix(SalePrice ~ ., sample_data_q4_r)[, -1]
sample_data_q4_predict <- predict(cv_lasso, s = cv_lasso$lambda.min, sample_data_q4_x)
sample_data_q4_predict


q4<- 
  sample_data_q4 %>%
  mutate(duplex_sale_price = exp(sample_data_q4_predict)) %>%
  mutate(duplex_yearly_rent = 0.1*duplex_sale_price*2) %>%
  mutate(fam_yearly_rent = 0.1*SalePrice) %>%
  select(SalePrice,fam_yearly_rent,duplex_yearly_rent) %>%
  filter(duplex_yearly_rent <2.869787e+04) %>%
  gather(type, `At Present`, SalePrice,fam_yearly_rent,duplex_yearly_rent) %>%
  mutate(`In 5 Years`= ifelse(type != 'SalePrice', `At Present`*5, `At Present`)) %>%
  mutate(`In 10 Years`= ifelse(type != 'SalePrice', `At Present`*10, `At Present`)) %>%
  mutate(`In 15 Years`= ifelse(type != 'SalePrice', `At Present`*15, `At Present`)) %>%
  gather(time, value, -type) %>%
  mutate(time=as.factor(time))
  
  
q4$time <- ordered(q4$time, levels= c('At Present', 'In 5 Years', 'In 10 Years','In 15 Years'))

save(q4, file= "data/Q4.rda")

q4 %>%
  ggplot(aes(type, value, fill= type)) +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(labels=c("duplex_yearly_rent" = "Convert to duplex", "fam_yearly_rent" = "Rent the house", "SalePrice" = "Sell the house")) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(~time) +
  xlab("")
  





