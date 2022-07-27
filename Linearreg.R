library(MASS)
library(dplyr)
library(graphics)
vif=function(object) {
  X = object$x[,-1]
  V = vcov(object)[-1,-1]
  n = dim(X)[1]
  sigma=summary(object)$sigma
  v = diag(V)
  S = diag(var(X))*(n-1)
  vif = v*S/sigma^2
  vif
}
BoxCox_convert <- function(x, lambda) {
  if (lambda == 0) ln(x) else (x^lambda - 1)/lambda
}
invBoxCox <- function(x, lambda) {
  if (lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda)
}
# Checking assumptions of linear regression
df = read.csv("df2001.csv")
df1 = df%>%
  select(-lease_commence_date, -X, -address)
df6 = df1%>%
  filter(flat_model=="Multi Generation") 
df1$town = factor(df1$town)
df1$flat_type = relevel(factor(df1$flat_type), "3 ROOM")
df1$flat_model = relevel(factor(df1$flat_model), "Model A")
#Deciding which regressors to use
df1.fit = lm(resale_price~month+year+years+quarter+town+Lat+Lon+remaining_lease+flat_model+floor_area_sqm+
               flat_type+index+dist_presch+dist_park+dist_pharm+dist_mrt+dist_gym+
               dist_hawker+storey_index,x=TRUE,data=df1)
# Variance Inflation Factor shows strong multi-collinearity
vif(df1.fit)
# Remove month, year, years and quarter as they are correlated with index, lat and lon as they are correlated to town
df1.fit2 = lm(resale_price~town+remaining_lease+flat_type+floor_area_sqm+flat_model
               +index+dist_presch+dist_park+dist_pharm+dist_mrt+dist_gym+
               dist_hawker+storey_index,x=TRUE,data=df1)
# Notice that flat_type Multi-generation and flat_model Multi-generation are perfectly correlated. 
# Moderate correlation between flat_type and floor_area_sqm
vif(df1.fit2)
summary(df1.fit2)

#Remove flat_type to solve the 2 issues
df1.fit3 = lm(resale_price~town+remaining_lease+floor_area_sqm+flat_model
              +index+dist_presch+dist_park+dist_pharm+dist_mrt+dist_gym+
                dist_hawker+storey_index,x=TRUE,data=df1)
# No more indication of multi-collinearity
vif(df1.fit3)
# Initial fit shows all regressors (except flat_modelAdjoined flat) are significant
summary(df1.fit3)

# Diagnostic plots
png("before_diagnostic.png")
  par(mfrow=c(2,2))
  plot(df1.fit3)
dev.off()

# Top_left graph indicates non-constant variance of errors and non-linearity. 
# top_right graph indicates non-normality of errors
# Solution: Use boxcox to transform response variable
bc <- boxcox(resale_price~town+remaining_lease+floor_area_sqm+flat_model
             +index+dist_presch+dist_park+dist_pharm+dist_mrt+dist_gym+
               dist_hawker+storey_index, data=df1)
lambda <- bc$x[which.max(bc$y)]

df1$resale_b = BoxCox_convert(df1$resale_price, lambda)

df1.fit4 = glm(resale_b~town+remaining_lease+floor_area_sqm+flat_model
              +index+dist_presch+dist_park+dist_pharm+dist_mrt+dist_gym+
                dist_hawker+storey_index,x=TRUE,data=df1)
# All regressors now significant
summary(df1.fit4)

# New diagnostic plots
png("after_diagnostic.png")
  par(mfrow=c(2,2))
  plot(df1.fit4)
dev.off()

# Finding outliers
infl = influence(df1.fit4, do.coef = FALSE)
cook =cooks.distance(df1.fit4, infl,res = infl$pear.res,
                     dispersion = summary(df1.fit4)$dispersion, hat = infl$hat)
# Max is very far away from the median causing a right-skew of the mean
summary(cook)
# We define outliers as points with cook's distance > 4/n, where n is the total number of data points
# https://www.statology.org/how-to-identify-influential-data-points-using-cooks-distance/#:~:text=A%20data%20point%20that%20has,considered%20to%20be%20an%20outlier.
threshold = 4/nrow(df1)
influential <- as.numeric(names(cook)[(cook > threshold)])
outliers <- df1[influential,]
# Percentage outliers
nrow(outliers)/nrow(df1)
# We choose not remove outliers as it results in overfitting

# df2 = df1[-influential,]
# df1.fit5 = lm(resale_b~town+remaining_lease+floor_area_sqm+flat_model
#              +index+dist_presch+dist_park+dist_pharm+dist_mrt+dist_gym+
#                dist_hawker+storey_index,x=TRUE,data=df1)
# df2.fit = lm(resale_b~town+remaining_lease+floor_area_sqm+flat_model
#              +index+dist_presch+dist_park+dist_pharm+dist_mrt+dist_gym+
#                dist_hawker+storey_index,x=TRUE,data=df2)
# summary(df1.fit5)
# summary(df2.fit)
# temp = df2 %>%
#   select(flat_model)
# table(temp)
# table(df1$flat_model)
# test = read.csv("s1.csv")
# test2 = test %>%
#   filter(flat_model != "2-room")
# test_y = test %>%
#   select(resale_price)
# test2_y = test2 %>%
#   select(resale_price)
# y_new_b = predict(df1.fit5, test)
# y_new = invBoxCox(y_new_b, lambda)
# y_new2_b = predict(df2.fit, test_x2)
# y_new2 = invBoxCox(y_new2_b, lambda)
# y = cbind(test_y, y_new, y_new2)
# sum((test_y - y_new)**2)
# sum((test_y-y_new2)**2)

# k-fold cross-validation
preprocess <- function(file) {
  df = read.csv(file)
  df1 = df%>%
    select(-lease_commence_date, -X, -address)
  df6 = df1%>%
    filter(flat_model=="Multi Generation") 
  df1$town = factor(df1$town)
  df1$flat_type = relevel(factor(df1$flat_type), "3 ROOM")
  df1$flat_model = relevel(factor(df1$flat_model), "Model A")
  return(df1)
}
df2001 = preprocess("df2001.csv")
df2003 = preprocess("df2003.csv")
df2005 = preprocess("df2005.csv")
df2007 = preprocess("df2007.csv")
df2009 = preprocess("df2009.csv")
df2011 = preprocess("df2011.csv")
df2013 = preprocess("df2013.csv")
df2015 = preprocess("df2015.csv")
df2017 = preprocess("df2017.csv")
df2019 = preprocess("df2019.csv")
df2020 = preprocess("df2020.csv")
test = read.csv("s1.csv")
s1 = test %>%
  filter(k_fold==1)
y_s1 = s1$resale_price
# Define k_fold function
k_fold = function(dataset, year, k=10) {
  SSE_valid_acc = 0
  SSE_test_acc = 0
  for (fold_num in 1:k) {
    print(paste("Fold Number ", fold_num))
    validation = dataset[dataset['k_fold']==fold_num,]
    y_validation = validation$resale_price
    train = dataset[dataset['k_fold']!=fold_num,]
    train$resale_b = BoxCox_convert(train$resale_price, lambda)
    fit <- lm(resale_b~town+remaining_lease+floor_area_sqm+flat_model
                            +index+dist_presch+dist_park+dist_pharm+dist_mrt+dist_gym+
                              dist_hawker+storey_index,x=TRUE,data=train)
    y_validation_pred_b = predict(fit, validation)
    
    y_validation_pred = invBoxCox(y_validation_pred_b, lambda)
    SSE_valid = sum((y_validation - y_validation_pred)^2)
    SSE_valid_acc = SSE_valid_acc + SSE_valid
    
    y_test_pred_b = predict(fit, s1)
    y_test_pred = invBoxCox(y_test_pred_b, lambda)
    
    
    SSE_test = sum((y_s1 - y_test_pred)^2)
    SSE_test_acc = SSE_test_acc + SSE_test
  }
  SSE_valid_avg = SSE_valid_acc / k
  SSE_test_avg = SSE_test_acc / k
  return (c(year, SSE_valid_avg, SSE_test_avg))
}

avg_results= k_fold(df2001, 2001)
avg_results = rbind(avg_results, k_fold(df2003, 2003))
avg_results = rbind(avg_results, k_fold(df2005, 2005))
avg_results = rbind(avg_results, k_fold(df2007, 2007))
avg_results = rbind(avg_results, k_fold(df2009, 2009))
avg_results = rbind(avg_results, k_fold(df2011, 2011))
avg_results = rbind(avg_results, k_fold(df2013, 2013))
avg_results = rbind(avg_results, k_fold(df2015, 2015))
avg_results = rbind(avg_results, k_fold(df2017, 2017))
avg_results = rbind(avg_results, k_fold(df2019, 2019))
avg_results = rbind(avg_results, k_fold(df2020, 2020))
avg_results = data.frame(avg_results)
row.names(avg_results) <- avg_results$X1
avg_results = avg_results %>%
  rename(SSE_valid = X2, SSE_test = X3) %>%
  select(-X1)
write.csv(avg_results, file="Learner/results-LR.csv")

