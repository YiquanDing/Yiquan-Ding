emp<-read_csv("train.csv")
emp
library(ggplot2)
## Attrition
plot_1=ggplot(data=emp)+
  geom_bar(aes(x=Attrition,fill=Attrition),position="dodge")
plot_1
## Gender
plot_2=ggplot(data=emp)+
  geom_bar(aes(x=Gender,fill=Attrition),position="dodge")
plot_2
plot_3=ggplot(data=emp)+
  geom_bar(aes(x=Gender,fill=Attrition),position="fill")
plot_3
## #Change Education to 1-5 factor variable
emp$Education=factor(emp$Education,levels=1:5)  
# education vs attrition
plot_4=ggplot(data=emp)+
  geom_bar(aes(x=Attrition,fill=Education),position="dodge")
plot_4
plot_5=ggplot(data=emp)+
  geom_bar(aes(x=Education,fill=Attrition),position="fill")
plot_5
# EducationField vs attrition
plot_6=ggplot(data=emp)+
  geom_bar(aes(x=Attrition,fill=EducationField),position="dodge")
plot_6
plot_7=ggplot(data=emp)+
  geom_bar(aes(x=EducationField,fill=Attrition),position="fill")+
  scale_x_discrete(guide=guide_axis(n.dodge=2))
plot_7
## Convert last year's training time to a factor from 0 to 6
emp$TrainingTimesLastYear=factor(emp$TrainingTimesLastYear,levels=0:6)
## TrainingTimesLastYear vs attrition
plot_8=ggplot(data=emp)+
  geom_bar(aes(x=Attrition,fill=TrainingTimesLastYear),position="dodge")
plot_8
plot_9=ggplot(data=emp)+
  geom_bar(aes(x=TrainingTimesLastYear,fill=Attrition),position="fill")
plot_9
## Integrate 6 tables together,use grid.arrange()
library(gridExtra)
grid.arrange(plot_4,plot_5,plot_6,plot_7,plot_8,plot_9,nrow=3,ncol=2)

## The Relationship between Living Conditions and Turnover
## gender vs attrition
plot_10=ggplot(data=emp)+
  geom_bar(aes(x=Attrition,fill=Gender),position="dodge")
plot_10
plot_11=ggplot(data=emp)+
  geom_bar(aes(x=Gender,fill=Attrition),position="fill")
plot_11
## MaritalStatus vs attrition
plot_12=ggplot(data=emp)+
  geom_bar(aes(x=Attrition,fill=MaritalStatus),position="dodge")
plot_12
plot_13=ggplot(data=emp)+
  geom_bar(aes(x=MaritalStatus,fill=Attrition),position="fill")
plot_13

plot_14=ggplot(data=emp,aes(x=Attrition,y=DistanceFromHome))+
  geom_boxplot()
plot_14
## Integrate 5 tables together,use grid.arrange()
grid.arrange(plot_10,plot_11,plot_12,plot_13,plot_14,nrow=3,ncol=2)
## there is almost no difference in turnover rates between different genders.
##There is not much difference in the turnover rate between divorced and married people, 
###while the turnover rate for singles is three times that of divorce.
##The turnover rate of people with a large distance from home to company 
#is significantly higher than that of people with a small distance from home to company,

###The relationship between job engagement and turnover
plot_15=ggplot(data=emp)+
  geom_bar(aes(x=Attrition,fill=BusinessTravel),position="dodge")
plot_15
plot_16=ggplot(data=emp)+
  geom_bar(aes(x=BusinessTravel,fill=Attrition),position="fill")
plot_16
## Convert last year's training time to a factor from 1 to 4
emp$JobInvolvement=factor(emp$JobInvolvement,levels=1:4)
## job involvement vs attrition
plot_17=ggplot(data=emp)+
  geom_bar(aes(x=Attrition,fill=JobInvolvement),position="dodge")
plot_17
plot_18=ggplot(data=emp)+
  geom_bar(aes(x=JobInvolvement,fill=Attrition),position="fill")
plot_18
## Integrate 4 tables together,use grid.arrange()
grid.arrange(plot_15,plot_16,plot_17,plot_18,nrow=2,ncol=2)
### the employees of this company travel frequently
### more people leave their jobs among the people who travel more frequently.
## the turnover rate of people with lower work engagement is also higher.

# The relationship between company status and exit status
## Department vs attrition
plot_19=ggplot(data=emp)+
  geom_bar(aes(x=Attrition,fill=Department),position="dodge")
plot_19
plot_20=ggplot(data=emp)+
  geom_bar(aes(x=Department,fill=Attrition),position="fill")
plot_20

## JobLevel vs attrition
## Convert job role level to a factor from 1 to 5
emp$JobLevel=factor(emp$JobLevel,levels=1:5)
## JobLevel vs attrition
plot_21=ggplot(data=emp)+
  geom_bar(aes(x=Attrition,fill=JobLevel),position="dodge")
plot_21
plot_22=ggplot(data=emp)+
  geom_bar(aes(x=JobLevel,fill=Attrition),position="fill")
plot_22
## JobRole vs attrition
plot_23=ggplot(data=emp)+
  geom_bar(aes(x=Attrition,fill=JobRole),position="dodge")
plot_23
plot_24=ggplot(data=emp)+
  geom_bar(aes(x=JobRole,fill=Attrition),position="fill")+
  scale_x_discrete(guide=guide_axis(n.dodge=2))
plot_24
grid.arrange(plot_19,plot_20,plot_21,plot_22,nrow=2,ncol=2)
grid.arrange(plot_23,plot_24,nrow=1,ncol=2)
## the human resources department and the sales department have the highest turnover rate
## Employees with a job level of 1 have the highest turnover rate; 
## employees with a job level of 4 and above have the lowest turnover rate. 
## This shows that the lower the job level, the easier it is for employees to lose.

# The relationship between work experience and turnover
## Age vs attrition
plot_25=ggplot(data=emp)+
  geom_boxplot(aes(x=Attrition,y=Age))
plot_25
## Number of companies worked for vs attrition
plot_26=ggplot(data=emp)+
  geom_boxplot(aes(x=Attrition,y=NumCompaniesWorked))
plot_26
## total number of years worked vs attrition
plot_27=ggplot(data=emp)+
  geom_boxplot(aes(x=Attrition,y=TotalWorkingYears))
plot_27
## Years in the company vs attrition
plot_28=ggplot(data=emp)+
  geom_boxplot(aes(x=Attrition,y=YearsAtCompany))
plot_28
## Number of years under this manager vs attrition
plot_29=ggplot(data=emp,aes(x=Attrition,y=YearsWithCurrManager))+
  geom_boxplot()
plot_29

grid.arrange(plot_25,plot_26,plot_27,plot_28,plot_29,nrow=3,ncol=2)
## people in their 30s are the easiest to leave. 
## The average age of non-leave employees is over 35 years old. 
## employees with less work experience have a higher turnover rate.

## The relationship between income and turnover
## MonthlyIncome vs attrition
plot_30=ggplot(data=emp)+
  geom_boxplot(aes(x=Attrition,y=MonthlyIncome))
plot_30
## Percentage increase in wages compared to last year vs attrition
plot_31=ggplot(data=emp)+
  geom_boxplot(aes(x=Attrition,y=PercentSalaryHike))
plot_31

## Convert Stock option level to a factor from 0 to 3
emp$StockOptionLevel=factor(emp$StockOptionLevel,levels=0:3)
## Stock option level vs attrition
plot_32=ggplot(data=emp)+
  geom_bar(aes(x=StockOptionLevel,fill=Attrition),position="fill")
plot_32
## Number of years since last promotion vs attrition
plot_33=ggplot(data=emp,aes(x=Attrition,y=YearsSinceLastPromotion))+
  geom_boxplot()
plot_33
grid.arrange(plot_30,plot_31,plot_32,plot_33,nrow=2,ncol=2)
## People with lower monthly income are more inclined to leave.
##The percentage of salary increases last year, 
##the level of employee stock options
##the number of years since the last promotion have little to do with whether an employee leaves or not.

### The Overall Distribution of Employee Income
plot_34=ggplot(data=emp,aes(x=MonthlyIncome))+
  geom_histogram(binwidth=400)
plot_34
## the income distribution of the company's employees presents multiple peaks.
## The employees of the company can be classified according to their peak income levels, which are: 
# low-income groups, low-middle-income groups, middle-income groups, middle-high-income groups, and high-income groups.
## The income of people in each income group shows an approximately normal distribution.


## Distribution of revenue across sectors
plot_35=ggplot(data=emp,aes(x=Department,y=MonthlyIncome))+
  geom_boxplot()
plot_35 
## the salary of the Research & Development department is higher than that of the other two departments
# its outliers are also more, that is, there are more high-income groups.
## The lower wages in the human resources department
# the staff turnover rate in the human resources department is the highest.

## Turn the ordinal variable BusinessTravel into a numeric variable, ‘1’, ‘2’, and ‘3’
# respectively represent ‘Non-Travel’,’ Travel_Rarely’, ‘Travel_Frequently’
## turn the ordinal variable MaritalStatus into a numeric variable, where ‘1’, ‘2’, ‘3’ 
# represent ‘Divorced’, ‘Married’, ‘Single’, respectively
##Turn Department into a numeric variable, where ‘1’, ‘2’, ‘3’, 
# respectively represent ‘Human Resources’, ‘Research & Development’, ‘Sales’.
# Delete 6 single variables: Over18, StandardHours, EmployeeCount, OverTime, etc.

library(magrittr)
library(dplyr)
emp2=emp%>%
  mutate(Attrition=model.matrix(user_id~Attrition,data=emp)[,2])
emp2=mutate(emp2,Gender=model.matrix(user_id~Gender,data=emp)[,2])
emp2=mutate(emp2,BusinessTravel=as.numeric(as.factor(BusinessTravel)))
emp2=mutate(emp2,MaritalStatus=as.numeric(as.factor(MaritalStatus)))
emp2=mutate(emp2,Department=as.numeric(as.factor(Department)))
emp2=emp2%>%
  select(-EducationField,-EmployeeCount,-JobRole,-Over18,-StandardHours,-OverTime)
emp2=emp2%>%
select(-user_id)












