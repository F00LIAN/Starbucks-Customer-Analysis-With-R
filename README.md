<p align="center">
<img src="https://github.com/user-attachments/assets/fb71263f-dcc6-42cc-8ee4-3aa649c3dc9e" height="10%" width="20%" alt="Starbucks Data Analysis"/>
</p>

<h1>Starbucks Customer Data Analysis in R</h1>
This project involves data cleaning, regression analysis, clustering, and predictive modeling using customer satisfaction and sales data from Starbucks. The goal is to analyze customer behavior, predict satisfaction levels, and identify key drivers of profitability.<br />

<h2>Environments and Technologies Used</h2>

- R Programming
- RStudio
- Data Visualization, Regression, and Clustering

<h2>Operating Systems Used </h2>

- Windows 11

<h2>List of Prerequisites</h2>

Before running this project, ensure you have:
- R and RStudio installed.
- Required R libraries: `ggplot2`, `NbClust`, `factoextra`, `cluster`, `miscTools`, `readxl`.

<h2>Installation Steps</h2>

### 1. Install Required Libraries
```r
install.packages(c("ggplot2", "NbClust", "factoextra", "cluster", "miscTools", "readxl"))
```

### 2. Load and Inspect Data
```r
sbux <- read.table("C:/Users/Julian Sotelo/OneDrive/Documents/R/Foundation of Business Analytics/Class 1/starbucks final data.txt", header=T, sep="\t")
head(sbux, 10)
colnames(sbux)
summary(sbux)
```

<h2>Data Cleaning</h2>

### 3. Identifying Missing Values
```r
missing <- colSums(is.na(sbux)) # Sum of missing values per column
total_missing <- sum(missing) # Total missing values
```
<p> <img src="https://github.com/user-attachments/assets/missing_data.png" height="80%" width="80%" alt="Missing Data Report"/> </p>

### 4. Removing Rows with Missing Values
```r
clean_sbux <- sbux[complete.cases(sbux),]
```
- Clean Data Rows: nrow(clean_sbux)
- Incomplete Data Rows: nrow(sbux) - nrow(clean_sbux) 

### 5. Handling Impossible Values
```r
clean_sbux[,1:22][clean_sbux[,1:22] > 5] <- 5
clean_sbux[,1:22][clean_sbux[,1:22] < 1] <- 1

clean_sbux[,23][clean_sbux[,23] > 100] <- 100
clean_sbux[,23][clean_sbux[,23] < 0] <- 0

clean_sbux[,24][clean_sbux[,24] > 10] <- 10
clean_sbux[,24][clean_sbux[,24] < 0] <- 0
```

<h2>Data Binning</h2>

### 6. Creating Banned Variables
```r
sbux$newrecommend <- cut(sbux$recommend, 3, labels=c("No", "Maybe", "Yes"))
sbux$newincome <- cut(sbux$Income, 5, labels=c("Lowest", "Low", "Average", "Above Average", "Highest"))
sbux$profitlevel <- cut(sbux$profits, 5, labels=c("Lowest", "Low", "Average", "Above Average", "Highest"))
sbux$satisfactionlevels <- cut(sbux$satis100, 5, labels=c("0-20", "21-40","41-60", "61-80", "81-100")) 
```

<h2>Data Visualization</h2>

### 7. Profit by Satisfaction Boxplot
```r
Viz1 <- ggplot(sbux, aes(x=satisfactionlevels, y=profits, col=satisfactionlevels)) +
        geom_boxplot() +
        xlab("Satisfaction Level") + 
        ylab("Profit Per Customer") + 
        ggtitle("Profit by Satisfaction Boxplot") +
        theme_minimal()
```

### 8. Income vs. Recommendation Chart
```r
Viz2 <- ggplot(sbux, aes(x=newincome, fill=newrecommend)) +
        geom_bar() +
        xlab("Total Income Level") + 
        ylab("Count") + 
        ggtitle("Income Level Based Recommendation Chart") +
        theme_minimal()
```

<h2>Regression Analysis</h2>

### 9. Running a Multiple Regression Model
```r
reg1 <- lm(profits ~ satis100 + recommend + Income, data=sbux)
summary(reg1)
```
- R² = 0.907 → 90.7% of profit variation is explained by satisfaction and income.

### 10. Predicting Profit Increase for a 10-Point Satisfaction Boost
```r
coefs <- coef(reg1)
profit_increase <- coefs[2] * 10
profit_increase
```
- Result: $2.38 increase in profit per customer for every 10-point increase in satisfaction.

<h2>Dummy Variables in Regression</h2>

### 11. Creating Dummy Variables for Satisfaction Levels
```r
fail <- as.integer(sbux$satis100 < 20)
exceed <- as.integer(sbux$satis100 > 80)

reg2 <- lm(profits ~ fail + exceed + recommend + Income, data=sbux)
summary(reg2)
```

- Finding the Number of Failing Customers

```r
sum(fail == 1) # 165 dissatisfied customers
```

- Finding Highly Satisfied Customers

```r
sum(exceed == 1) # 495 highly satisfied customers
```
<p> <img src="https://github.com/user-attachments/assets/dummy_variable_regression.png" height="80%" width="80%" alt="Dummy Variable Regression"/> </p>

<h2>Machine Learning: Train/Test Split</h2>

### 12. Splitting Data into Training & Test Sets
```r
k <- 5000
Train1 <- sbux[1:k,]
Test1 <- sbux[(k+1):nrow(sbux),]
```

### 13. Running Regression on Training Data
```r
trainreg <- lm(recommend ~ ., data=Train1[,1:22])
summary(trainreg)
```

### 14. Evaluating Performance on Test Data
```r
library(miscTools)
test.predict <- predict(trainreg, newdata=Test1)
testrtwo <- rSquared(y=Test1$recommend, resid=(Test1$recommend - test.predict))
testrtwo
```

- R² on Test Set = 0.892 (high model accuracy).

<h2>Clustering Analysis</h2>

### 15. Finding Optimal Number of Clusters
```r
X <- as.matrix(sbux[,1:22])
nb1 <- NbClust(X, distance="euclidean", min.nc=2, max.nc=10, method="kmeans")
fviz_nbclust(nb1)
```
<p> <img src="https://github.com/user-attachments/assets/optimal_clusters.png" height="80%" width="80%" alt="Optimal Clusters"/> </p>

### 16. Running K-Means Clustering
```r
cluster.results <- kmeans(X, centers=2, iter.max=1000, nstart=100)
segment_sizes <- table(cluster.results$cluster)
segment_sizes
```

- Segment 1: Most satisfied customers.
- Segment 2: All other customers.

<h2>Conclusion</h2>

- Data cleaning & handling missing values were crucial for quality analysis.
- Regression analysis revealed satisfaction and income as major drivers of profitability.
- Dummy variables showed that dissatisfied customers significantly decrease profits.
- Clustering helped segment customers into satisfaction groups.
- Train/Test regression model achieved 89.2% accuracy in predicting recommendations.

<h2>Future Improvements</h2>

- Implement logistic regression to classify customer retention likelihood.
- Use random forest models for improved recommendation prediction.
- A/B test customer retention strategies.
