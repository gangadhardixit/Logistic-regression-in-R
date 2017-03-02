hrdata=read.csv(file="c://Users/gandixit/Desktop/WA_Fn-UseC_-HR-Employee-Attrition.csv")
View(hrdata)
str(hrdata)

set.seed(111)
rownumbers=sample(1:nrow(hrdata),0.7*nrow(hrdata))
traindata1=hrdata[rownumbers,]
testdata1=hrdata[-rownumbers,]
attach(hrdata)
nrow(traindata1)
nrow(testdata1)
colnames(traindata1)
sapply(hrdata, function(x) sum(is.na(x)))
logisticmodel=glm(formula = Attrition~Age+DailyRate+DistanceFromHome+Education
                  +EnvironmentSatisfaction+HourlyRate+JobInvolvement+JobLevel
                  +JobSatisfaction+MonthlyIncome+PercentSalaryHike+PerformanceRating+
                  +RelationshipSatisfaction+StandardHours+StockOptionLevel+TotalWorkingYears
                  +TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole
                  +YearsSinceLastPromotion+YearsWithCurrManager,
                  family = "binomial",data = traindata1)



logisticmodel1=glm(formula = Attrition~Age+DistanceFromHome+EnvironmentSatisfaction+JobInvolvement
                  +JobSatisfaction+RelationshipSatisfaction+StockOptionLevel
                  +TrainingTimesLastYear+WorkLifeBalance+YearsInCurrentRole
                  +YearsSinceLastPromotion+YearsWithCurrManager,
                  family = "binomial",data = traindata1)


summary(logisticmodel)
summary(logisticmodel1)

attritiontest_prob=predict(object=logisticmodel1,newdata = testdata1,type = "response")
assignprb=ifelse(attritiontest_prob>=0.45,1,0)

table(prediction=assignprb,actual=testdata1$Attrition)

((357+15)/441)*100
