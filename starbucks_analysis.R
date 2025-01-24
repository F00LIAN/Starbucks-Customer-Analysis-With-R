#------------------------------- import and examine the data
getwd()
setwd("C:/Users/Julian Sotelo/OneDrive/Documents/R/Foundation of Business Analytics/Class 1")
sbux <- read.table('starbucks final data.txt', header=T, sep = "\t")
sbux
head(sbux, 10)

colnames(sbux)
str(sbux)
summary(sbux)

#---------- report the number of missing values
sbux[!complete.cases(sbux),]
missing <- colSums(is.na(sbux)) #Sum of NA's for each Column
sum(is.na(sbux)) #total NA's
as.matrix(missing)
sum(missing)
#---------------------------- 2. Strip out the rows of data
sbux2 <- sbux 
clean_sbux <- sbux2[complete.cases(sbux2),]

incomplete_sbux <- sbux2[!complete.cases(sbux2),]

nrow(incomplete_sbux) #we have 3879 incomplete rows

nrow(clean_sbux) #we have 6121 complete rows


# The data says we will be using 6121/10000 rows, therefore about 1/3 of the data is not used.
# Thus continuing without the incomplete data is not recommended, as it may create bias.


#--------------------------- Report the Impossible Values (values > 5 | values < 1) using clean data
filter_sbux <- clean_sbux[,1:22] #select the x1-x22 columns
sbux3 <- colSums(filter_sbux > 5) + colSums(filter_sbux < 1) #filter
sbux3 #total for each column
sum(sbux3) #sum the total 


#---------------------------------- Replacing the Data
clean_sbux
clean_sbux[,1:22][clean_sbux[,1:22] > 5] <- 5
clean_sbux[,1:22][clean_sbux[,1:22] < 1] <- 1

#check frequency for replaced data for x1:x22
check_frequency <- apply(clean_sbux[,1:22], 2, table) 
check_frequency
# Total frequency across 1:5
sum_frequency<-clean_sbux[,1:22]

sum(sum_frequency == 1) 
sum(sum_frequency == 2)
sum(sum_frequency == 3)
sum(sum_frequency == 4)
sum(sum_frequency == 5)

#----------------------------- Replacing the last 4 columns
clean_sbux
#fix the impossible values for "satis100" column
clean_sbux[,23][clean_sbux[,23] > 100] <- 100
clean_sbux[,23][clean_sbux[,23] < 0] <- 0
clean_sbux # check

#fix the impossible values for "recommend" column
clean_sbux[,24][clean_sbux[,24] > 10] <- 10
clean_sbux[,24][clean_sbux[,24] < 0] <- 0
clean_sbux # check

#check the number of unique values for recommend column
table(clean_sbux$recommend)

#report the average values for all variables in final clean data set
apply(clean_sbux , 2, mean)

#final clean dataset
clean_sbux

#-------------------------- PART 2

library(ggplot2)
sbux2 <- clean_sbux
sbux<- sbux2
head(sbux)

attach(sbux)
#------------------------------- Bin Data to preferred levels
newprofits <- sbux$profits

newsatisfaction <- sbux$satis100

newincome <- sbux$Income

newrecommend <- sbux$recommend


sbux$newrecommend <- cut(newrecommend, 3, labels = c("No", "Maybe", "Yes"))
#round(cut(newrecommend, 3))

sbux$newincome <- cut(newincome, 5, labels=c("Lowest", "Low", "Average", "Above Average", "Highest"))

sbux$profitlevel <- cut(newprofits, 5, labels = c("Lowest", "Low", "Average", "Above Average", "Highest"))

sbux$satisfactionlevels <- cut(newsatisfaction, 5, labels=c("0-20", "21-40","41-60", "61-80", "81-100")) 



#------------------------------ Begin Visualization 1
#Total Profit for each Starbucks and its satisfaction per customer


Viz1 <- ggplot(data = sbux, mapping = aes(x=satisfactionlevels, 
                                          y=profits, col=satisfactionlevels)) 
Viz1 <- Viz1 + geom_boxplot() 
Viz1
Viz1 <- Viz1 + xlab("Satisfaction Level") + ylab("Profit Per Customer") + 
  guides(color = guide_legend(title = "Satisfaction Rating"))

Viz1

Viz1 <- Viz1 + ggtitle('Profit by Satisfaction Boxplot') +
  theme(axis.title.x=element_text(colour = "Red", size=18),
        axis.title.y = element_text(colour ="Red", size=18),
        axis.text.x = element_text(size=12.5),
        axis.text.y = element_text(size=12.5),
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        plot.title = element_text(colour="DarkBlue", size=18))
Viz1
#------------------------------- Visualization 2
#Income Level for every Starbucks and the likelihood of a recommendation 

Viz2 <- ggplot(data = sbux, mapping = aes(x=newincome, fill=newrecommend)) + geom_bar()

Viz2 <- Viz2 + xlab("Total Income Level") + ylab("Count") + ggtitle("Income Level Based Recommendation Chart")
Viz2 <- Viz2 + guides(fill=guide_legend(title="Recommend")) + theme(axis.title.x=element_text(face = "bold", colour = "Black", size=18),
                                                                    axis.title.y = element_text(face = "bold", colour ="Black", size=18),
                                                                    axis.text.x = element_text(size=12.5),
                                                                    axis.text.y = element_text(size=12.5),
                                                                    legend.title = element_text(size=13),
                                                                    legend.text = element_text(size=12),
                                                                    plot.title = element_text(colour="DarkBlue", size=18))
Viz2

detach(sbux)
#------------------------------ Linear Regression

sbux3 <- sbux2
sbux3
attach(sbux3)
options(scipen=999)


reg1 <- lm(profits ~ satis100 + recommend + Income)
reg1

summary(reg1)
#----Question A
#The Multiple R Squared is at .907 and adjusted R squared is equal to 90.69
# which means approximately 90% of variation in profits can be explained by 
# our independent variables. 

# The P values confirm the accuracy of our independent variables, since p < .000001 (***)

#----Question B
coefs <- coef(reg1)
coefs
result <- coefs[2]*10 
result 
#$2.38
#coefs[1] + coefs[2]*10 + coefs[3] + coefs[4]
#For Every 10 point increase in Satisfaction, profits per customer rise by $2.38

#----Question C
result2 <- coefs[1] + coefs[2]*77 + coefs[3]*8 + coefs[4]*121500
result2 #124.8108
#The predicted average monthly profits for a customer with satis100 = 77, recommend=8,
#income = $121,500 is $124.81

#----------------------------Dummy Variables

fail <- as.integer(sbux3$satis100 < 20)
exceed <- as.integer(sbux3$satis100 > 80)

reg2 <- lm(profits ~ fail + exceed + recommend + Income)
reg2
summary(reg2)
#----Question A
sum(fail == 1)#165
#----Question B
sum(exceed == 1)#495

#----Question C
coefs2 <- coef(reg2)
coefs2


# alter fail and exceed dummy variables
#coefs2[1] + coefs2[2] + coefs2[3] + coefs2[4] + coefs2[5] # Base Formula
# (Intercept) 6.20
#coefs2[1] + coefs2[2]*2 + coefs2[3] + coefs2[4] + coefs2[5] # Increase fail by two
# (Intercept) -6.87
#coefs2[1] + coefs2[2] + coefs2[3]*2 + coefs2[4] + coefs2[5]# Increase exceed by two
# (Intercept) 8.88

# Based off the summary in the new regression model, we included two dummy variables,
# 'fail' and 'exceed'. Our analysis suggests that the 'fail' dummy variable has a much 
# larger impact than the 'exceed' variable. Our reported profit for the 'fail' variable is 
# -$13.067.21, therefore resulting in a large loss for dissatisfied customers. When we compare this
# to the 'exceed' dummy variable, there is minimal impact on profit since it 
# results to $2.68 for every customer. We suggests that we try to minimize very dissatisfied 
# customers as they have the largest impact on profit. In other words, figure out
# the dissatisfied customer pain points and fix them. 


#---------------------- Part 3


sbux <- sbux3
sbux
head(sbux)
summary(sbux)
attach(sbux)
detach(sbux)

#---------------------------- Training and Test Samples Regression

options(scipen=999)
#Make Training and Test Sample
k <- 5000
N <- nrow(sbux)
Train1 <- as.data.frame(sbux[1:k,])
nrow(Train1)

Test1 <- as.data.frame(sbux[(k+1):N,])
nrow(Test1)

#Run multiple regression with Training Sample
attach(Train1)
trainreg <- lm(recommend ~.,data=Train1[,1:22])
summary(trainreg)

#Run a multiple regression with the Test Sample 
detach(Train1)
attach(Test1)
library(miscTools)
Test.1 <- Test1[,1:22]
Test.2 <- Test1[,"recommend"]
test.predict <- as.vector(predict(object = trainreg, newdata = Test1))
testrtwo <- rSquared(y=Test.2, resid = (Test.2 - test.predict))
testrtwo
detach(Test1)



#---------------------------- Variable Selection

attach(Train1)
#fit the first model with no variables
null1 <- lm(recommend ~ 1)
summary(null1)
#use trainreg as a model with all variables
trainreg <- lm(recommend ~.,data=Train1[,1:22])
summary(trainreg)

# now use forward selection to filter the noise
fwd.result <- step(object=null1, direction="forward", scope=formula(trainreg))
summary(fwd.result)



#----------------------- Cluster Analysis and Interpretation
library(NbClust)
library(factoextra)
library(cluster)
# create data matrix
X <- as.matrix(sbux[,1:22])
typeof(X)
#get summary statistics by column
summary.X <- cbind(
  apply(X,2,mean),
  apply(X,2,min),
  apply(X,2,max),
  apply(X,2,sd)
)
colnames(summary.X) <- c("Avg","Min", "Max", "Std Dev")
round(summary.X,2)
# use Nbclust to determine the optimal number of clusters
nb1 <- NbClust(X, distance="euclidean", min.nc=2, max.nc=10, method="kmeans")
fviz_nbclust(nb1) # barplot

#k means cluster analysis
'cluster.results' = kmeans(x = X, centers = 2, iter.max=1000, nstart=100)
examine.cluster <- t(round(cluster.results$centers,2))
#find which cluster each individual is assigned to
cluster.numbers = cluster.results$cluster
cluster.numbers
# find out how many people belong in each customer
segment_sizes = table(cluster.numbers)
segment_sizes
# find out which segment has the highest ratings and flag them, 
# also report the values for this segment for X1-X5
examine.cluster[1:5,1]

# Split data to two groups, "Most Satisfied" and "All Other"
detach(Train1)
attach(sbux) 
sbux2 <- cbind(sbux[,1:22],cluster.numbers, recommend)

#most satisfied
examine.cluster[,1] 

most.satisfied <- sbux2[which(sbux2$cluster.numbers==1),]

#all other 
examine.cluster[,2]
all.other <- sbux2[which(sbux2$cluster.numbers==2),]



# run two separate regressions based on the two cluster groups
segment1.reg1 <- lm(recommend~., most.satisfied)
summary(segment1.reg1)

segment2.reg2 <- lm(recommend~., all.other)
summary(segment2.reg2)
# use fitted to predict 
avg.predicted.satisfied <- round(mean(fitted.values(segment1.reg1)),2) #7.33
avg.predicted.allother <- round(mean(fitted.values(segment2.reg2)),2) #5.19
x <- round(mean(predict(segment1.reg1)), 2)
x

#-------------------------------- What-if analysis
all.other2 <- all.other
#add the extra point to the requested columns 
all.other3 <- all.other2[, c("X1", "X2", "X7", "X8", "X10")] + 1
all.other2[c('X1','X2', 'X7', 'X8', 'X10')] <- all.other3[c("X1","X2", "X7", "X8", "X10")]
all.other2
#fix the impossible values due to the extra point added
all.other2[,1:22][all.other2[,1:22] > 5] <- 5
all.other <- all.other2
all.other

#find predicted values
test.preds.new <- as.vector(predict(object=segment2.reg2, newdata=all.other))
#round predicted values
number.4.answer <- round(mean(test.preds.new),2) # 7.05
number.4.answer

