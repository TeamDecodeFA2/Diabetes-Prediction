library(readxl)
Diabetes_Dataset <- read_excel("C:/Users/SHRUTI SHARMA/Desktop/Diabetes Dataset.xlsx")
View(Diabetes_Dataset)

#Logistic Regression Model

L1 <- lm(Diabetes_Dataset$Outcome ~ Diabetes_Dataset$Glucose 
         + Diabetes_Dataset$BloodPressure
         + Diabetes_Dataset$BMI)
L1
summary(L1)

#Logit Model 

L2 <- glm(Diabetes_Dataset$Outcome ~ Diabetes_Dataset$Glucose 
          + Diabetes_Dataset$BloodPressure
          + Diabetes_Dataset$BMI,
          family = binomial(link = "logit"))
L2
summary(L2)

exp(L2$coefficients)  #Odds Ratio

#Average marginal effect of the model:

ME1 <- mean(dlogis(predict(L2, 
                           type = "link")))

ME1   # logit scaler

#Average marginal effect
AME <- ME1*coef(L2)
AME

# In percentage

AME_percent <- AME*100
AME_percent

# Predicting probabilities

P1 <- predict(L2, type = "response")
P1
summary(P1)
head(P1)

T1 <- table(P1 > 0.6,Diabetes_Dataset$Outcome )
T1

# 0 and 1  actual values

# False and true are the predicted values from the model

# 0 means the event has not occurred.
# False means that the model has predicted that the
# event has not occurred


# 1 means the event has occurred
# True means that the model has predicted that the
# event has occurred

# 467 implies that the event did not happen and 
# and the model also predicted 467 times that the
# event did not occur. This is also called True negative

# 150 implies that the 150 times the model predicted 
# that the event did not occur when it actually occurred.
# This is known as false positive


# 33 implies that 33 times the model predicted that the 
# event happened when it did not occur actually
# This is known as false negative.


# 118 implies that 118 times the model predicted that
# the event happen when it actually happened.
# This is called true positive




# Accuracy of the model is True Negative (467)
# and True Positive (118) of the total

# Accuracy = (TP + TN)/Total
# Sensitivity = TP/(TP + FN)
# Specificity = TN/(TN + FP)
# False Positive rate = FP/(FP+TN)


# To find out accuracy

Acc <- sum(diag(T1))/sum(T1)*100
Acc

sens <- T1[2,2]/(sum(T1[2,2],T1[1,2]))
sens

spec <- T1[1,1]/sum(T1[1,1],T1[2,1])
spec


fp <- T1[2,1]/sum(T1[2,1],T1[1,1])
fp






