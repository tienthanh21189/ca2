# ============================================================
# 1. DATA PREPARATION
# ============================================================

# 1.1. Read dataset
# ============================================================
# Read dataset from csv file
world_happiness <- read.csv('World-happiness.csv', na='')

# Examine the structure of the dataset
str(world_happiness)

# The total rows
nrow(world_happiness)
# 1949


# 1.2. Examine the missing data 
# ============================================================
# List rows with missing values
incomplete_data <- world_happiness[!complete.cases(world_happiness),]

# Show the number rows of missing data
nrow(incomplete_data)
# 241

# Visualize the missing data
# install packages("VIM")
library(VIM)
incomplete_data <- aggr(world_happiness, prop = FALSE, numbers = TRUE)
summary(incomplete_data)

# Remove NA values
world_happiness <- na.omit(world_happiness)


# ============================================================
# 2. MULTIPLE LINEAR REGRESSION
# ============================================================

# 2.1. Examine the relationships among the variables
# ============================================================
# Explore the relationship between Life Ladder
# and other factors, including:
# - Log GDP per capita
# - Social support
# - Healthy life expectancy at birth
# - Freedom to make life choices
# - Generosity
# - Perceptions of corruption
# - Positive affect
# - Negative affect

# Life Ladder will be the predictor (independent variable).
# And the other variables will be the response (dependent variable).


# 2.1.1. Check for linearity
# ============================================================
# Visualize the distribution and correlation
# install.packages("psych")
library(psych)

pairs.panels(world_happiness, 
             smooth = FALSE, # If TRUE, draws less smooths
             scale = FALSE, # If TRUE, scales the correlation text font
             density = TRUE, # If TRUE, adds density plots and histograms
             ellipses = FALSE, # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21, # pch symbol
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE, # If TRUE, reports correlations
             jiggle = FALSE, # If TRUE, data points are jitered
             factor = 2, # Jitering factor
             hist.col = 4, # Histograms color
             stars = TRUE, # If TRUE, adds significance level with stars
             ci = TRUE) # If TRUE, adds confidence intervals

# Use scatter plot to check more detail
attach(world_happiness)

# Log.GDP.per.capita
scatter.smooth(x = Life.Ladder, y = Log.GDP.per.capita, 
               main = "Life.Ladder ~ Log.GDP.per.capita", 
               xlab = "Life Ladder",
               ylab = "Log GDP per capita")
cor(Life.Ladder, Log.GDP.per.capita)
# cor(Life.Ladder, Log.GDP.per.capita) = 0.7928484
# => High correlation


# Social.support
scatter.smooth(x = Life.Ladder, y = Social.support, 
               main = "Life.Ladder ~ Social.support", 
               xlab = "Life Ladder",
               ylab = "Social support")
cor(Life.Ladder, Social.support)
# cor(Life.Ladder, Social.support) = 0.713211 
# => High correlation


# Healthy.life.expectancy.at.birth
scatter.smooth(x = Life.Ladder, y = Healthy.life.expectancy.at.birth, 
               main = "Life.Ladder ~ Healthy.life.expectancy.at.birth", 
               xlab = "Life Ladder",
               ylab = "Healthy life expectancy at birth")

cor(Life.Ladder, Healthy.life.expectancy.at.birth)
# cor(Life.Ladder, Healthy.life.expectancy.at.birth) = 0.7546972 
# => High correlation


# Freedom.to.make.life.choices
scatter.smooth(x = Life.Ladder, y = Freedom.to.make.life.choices, 
               main = "Life.Ladder ~ Freedom.to.make.life.choices", 
               xlab = "Life Ladder",
               ylab = "Freedom to make life choices")

cor(Life.Ladder, Freedom.to.make.life.choices)
# cor(Life.Ladder, Freedom.to.make.life.choices) = 0.5250891 
# => Medium correlation


# Generosity
scatter.smooth(x = Life.Ladder, y = Generosity, 
               main = "Life.Ladder ~ Generosity", 
               xlab = "Life Ladder",
               ylab = "Generosity")

cor(Life.Ladder, Generosity)
# cor(Life.Ladder, Generosity) = 0.1827577
# (-0.2 < x < 0.2)
# => Low correlation


# Perceptions.of.corruption
scatter.smooth(x = Life.Ladder, y = Perceptions.of.corruption, 
               main = "Life.Ladder ~ Perceptions.of.corruption", 
               xlab = "Life Ladder",
               ylab = "Perceptions of corruption")

cor(Life.Ladder, Perceptions.of.corruption)
# cor(Life.Ladder, Perceptions.of.corruption) = -0.4481566
# => Medium correlation


# Positive.affect
scatter.smooth(x = Life.Ladder, y = Positive.affect, 
               main = "Life.Ladder ~ Positive.affect", 
               xlab = "Life Ladder",
               ylab = "Positive affect")

cor(Life.Ladder, Positive.affect)
# cor(Life.Ladder, Positive.affect) = 0.5330917
# => Medium correlation


# Negative.affect
scatter.smooth(x = Life.Ladder, y = Negative.affect, 
               main = "Life.Ladder ~ Negative.affect", 
               xlab = "Life Ladder",
               ylab = "Negative affect")

cor(Life.Ladder, Negative.affect)
# cor(Life.Ladder, Negative.affect) = -0.3004663
# => Medium correlation

# Examine all other correlations using the cor() function
paste("Correlation for Life.Ladder and Log.GDP.per.capita:", 
      cor(Life.Ladder, Log.GDP.per.capita))
paste("Correlation for Life.Ladder and Social.support:", 
      cor(Life.Ladder, Social.support))
paste("Correlation for Life.Ladder and Healthy.life.expectancy.at.birth:", 
      cor(Life.Ladder, Healthy.life.expectancy.at.birth))
paste("Correlation for Life.Ladder and Freedom.to.make.life.choices:", 
      cor(Life.Ladder, Freedom.to.make.life.choices))
paste("Correlation for Life.Ladder and Generosity:", 
      cor(Life.Ladder, Generosity))
paste("Correlation for Life.Ladder and Perceptions.of.corruption:", 
      cor(Life.Ladder, Perceptions.of.corruption))
paste("Correlation for Life.Ladder and Positive.affect:", 
      cor(Life.Ladder, Positive.affect))
paste("Correlation for Life.Ladder and Negative.affect:", 
      cor(Life.Ladder, Negative.affect))

# Correlation for Life.Ladder and Log.GDP.per.capita: 0.79284839569346
# Correlation for Life.Ladder and Social.support: 0.713211047468834
# Correlation for Life.Ladder and Healthy.life.expectancy.at.birth: 0.754697218907465
# Correlation for Life.Ladder and Freedom.to.make.life.choices: 0.525089129352153
# Correlation for Life.Ladder and Generosity: 0.182757695042654
# Correlation for Life.Ladder and Perceptions.of.corruption: -0.448156570367567
# Correlation for Life.Ladder and Positive.affect: 0.53309173309273
# Correlation for Life.Ladder and Negative.affect: -0.300466271837106

# It appears that the variable Generosity and Negative.affect have a 
# low correlation with Life.Ladder.
# Therefore I'll remove it from the dataset.
world_happiness <- subset(world_happiness, select = -c(Generosity, Negative.affect))


# 2.2.2. Check for outliers
# ============================================================
attach(world_happiness)

opar <- par(no.readonly = TRUE)
par(mfrow =c(3,3)) # 3rows * 3cols
# Life.Ladder
boxplot(Life.Ladder, 
        main = "Life.Ladder", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Life.Ladder)$out))
# The graph shows that there is no outlier


# Log.GDP.per.capita
boxplot(Log.GDP.per.capita, 
        main = "Log.GDP.per.capita", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Log.GDP.per.capita)$out))
# The graph shows that there is no outlier


# Social.support
boxplot(Social.support, 
        main = "Social.support", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Social.support)$out))
# The graph shows that the variable contains contain outliers.

# Healthy.life.expectancy.at.birth
boxplot(Healthy.life.expectancy.at.birth, 
        main = "Healthy.life.expectancy.at.birth", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Healthy.life.expectancy.at.birth)$out))
# The graph shows that the variable contains contain outliers.

# Freedom.to.make.life.choices
boxplot(Freedom.to.make.life.choices, 
        main = "Freedom.to.make.life.choices", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Freedom.to.make.life.choices)$out))
# The graph shows that the variable contains contain outliers.

# Perceptions.of.corruption
boxplot(Perceptions.of.corruption, 
        main = "Perceptions.of.corruption", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Perceptions.of.corruption)$out))
# The graph shows that the variable contains contain outliers.


# Positive.affect
boxplot(Positive.affect, 
        main = "Positive.affect", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Positive.affect)$out))
# The graph shows that the variable contains contain outliers.


detach(world_happiness)
# As we will remove outliers in next code blocks,
# the world_happiness dataset will be changed many times.
# Therefore, no use attach() function
# to prevent loading the old data in attach() function

# Check & remove outliers - 1st time
# ============================================================
# Output the outliers more clearly
Social_outliers <- boxplot.stats(world_happiness$Social.support)$out
Healthy_outliers <- boxplot.stats(world_happiness$Healthy.life.expectancy.at.birth)$out
Freedom_outliers <- boxplot.stats(world_happiness$Freedom.to.make.life.choices)$out
Corruption_outliers <- boxplot.stats(world_happiness$Perceptions.of.corruption)$out
Positive_outliers <- boxplot.stats(world_happiness$Positive.affect)$out

paste("Social.support Outlier: ", 
      paste(Social_outliers, collapse=", "))
paste("Healthy.life.expectancy.at.birth Outlier: ", 
      paste(Healthy_outliers, collapse=", "))
paste("Freedom.to.make.life.choices Outlier: ", 
      paste(Freedom_outliers, collapse=", "))
paste("Perceptions.of.corruption Outlier: ", 
      paste(Corruption_outliers, collapse=", "))
paste("Positive.affect Outlier: ", 
      paste(Positive_outliers, collapse=", "))


# Remove Social.support Outlier
world_happiness <- subset(world_happiness, 
                        !(world_happiness$Social.support 
                          %in% Social_outliers))
# Remove Healthy.life.expectancy.at.birth Outlier
world_happiness <- subset(world_happiness, 
                          !(world_happiness$Healthy.life.expectancy.at.birth 
                            %in% Healthy_outliers))
# Remove Freedom.to.make.life.choices Outlier
world_happiness <- subset(world_happiness, 
                          !(world_happiness$Freedom.to.make.life.choices 
                            %in% Freedom_outliers))
# Remove Perceptions.of.corruption Outlier
world_happiness <- subset(world_happiness, 
                          !(world_happiness$Perceptions.of.corruption 
                            %in% Corruption_outliers))
# Remove Positive.affect Outlier
world_happiness <- subset(world_happiness, 
                          !(world_happiness$Positive.affect 
                            %in% Positive_outliers))

# Check & remove outliers - 2nd time
# ============================================================
# Re-check the outliers
Social_outliers <- boxplot.stats(world_happiness$Social.support)$out
Healthy_outliers <- boxplot.stats(world_happiness$Healthy.life.expectancy.at.birth)$out
Freedom_outliers <- boxplot.stats(world_happiness$Freedom.to.make.life.choices)$out
Corruption_outliers <- boxplot.stats(world_happiness$Perceptions.of.corruption)$out
Positive_outliers <- boxplot.stats(world_happiness$Positive.affect)$out

paste("Social.support Outlier: ", 
      paste(Social_outliers, collapse=", "))
paste("Healthy.life.expectancy.at.birth Outlier: ", 
      paste(Healthy_outliers, collapse=", "))
paste("Freedom.to.make.life.choices Outlier: ", 
      paste(Freedom_outliers, collapse=", "))
paste("Perceptions.of.corruption Outlier: ", 
      paste(Corruption_outliers, collapse=", "))
paste("Positive.affect Outlier: ", 
      paste(Positive_outliers, collapse=", "))

# Positive.affect has no outlier now

# Continue remove the outliers of other variables
# Remove Social.support Outlier
world_happiness <- subset(world_happiness, 
                          !(world_happiness$Social.support 
                            %in% Social_outliers))
# Remove Healthy.life.expectancy.at.birth Outlier
world_happiness <- subset(world_happiness, 
                          !(world_happiness$Healthy.life.expectancy.at.birth 
                            %in% Healthy_outliers))
# Remove Freedom.to.make.life.choices Outlier 
world_happiness <- subset(world_happiness, 
                          !(world_happiness$Freedom.to.make.life.choices 
                            %in% Freedom_outliers))
# Remove Perceptions.of.corruption Outlier 
world_happiness <- subset(world_happiness, 
                          !(world_happiness$Perceptions.of.corruption 
                            %in% Corruption_outliers))

# Check & remove outliers - 3rd time
# ============================================================
# Re-check the outliers
Social_outliers <- boxplot.stats(world_happiness$Social.support)$out
Healthy_outliers <- boxplot.stats(world_happiness$Healthy.life.expectancy.at.birth)$out
Freedom_outliers <- boxplot.stats(world_happiness$Freedom.to.make.life.choices)$out
Corruption_outliers <- boxplot.stats(world_happiness$Perceptions.of.corruption)$out

paste("Social.support Outlier: ", 
      paste(Social_outliers, collapse=", "))
paste("Healthy.life.expectancy.at.birth Outlier: ", 
      paste(Healthy_outliers, collapse=", "))
paste("Freedom.to.make.life.choices Outlier: ", 
      paste(Freedom_outliers, collapse=", "))
paste("Perceptions.of.corruption Outlier: ", 
      paste(Corruption_outliers, collapse=", "))

# Healthy.life.expectancy.at.birth and
# Freedom.to.make.life.choices have no outlier now

# Continue remove the outliers of other variables
# Remove Social.support Outlier
world_happiness <- subset(world_happiness, 
                          !(world_happiness$Social.support 
                            %in% Social_outliers))
# Remove Perceptions.of.corruption Outlier 
world_happiness <- subset(world_happiness, 
                          !(world_happiness$Perceptions.of.corruption 
                            %in% Corruption_outliers))

# Check & remove outliers - 4th time
# ============================================================
# Re-check the outliers
Social_outliers <- boxplot.stats(world_happiness$Social.support)$out
Corruption_outliers <- boxplot.stats(world_happiness$Perceptions.of.corruption)$out

paste("Social.support Outlier: ", 
      paste(Social_outliers, collapse=", "))
paste("Perceptions.of.corruption Outlier: ", 
      paste(Corruption_outliers, collapse=", "))

# There are some outliers

# Continue remove the outliers of the remaining variables
# Remove Social.support Outlier
world_happiness <- subset(world_happiness, 
                          !(world_happiness$Social.support 
                            %in% Social_outliers))
# Remove Perceptions.of.corruption Outlier 
world_happiness <- subset(world_happiness, 
                          !(world_happiness$Perceptions.of.corruption 
                            %in% Corruption_outliers))

# Check & remove outliers - 5th time
# ============================================================
# Re-check the outliers
Social_outliers <- boxplot.stats(world_happiness$Social.support)$out
Corruption_outliers <- boxplot.stats(world_happiness$Perceptions.of.corruption)$out

paste("Social.support Outlier: ", 
      paste(Social_outliers, collapse=", "))
paste("Perceptions.of.corruption Outlier: ", 
      paste(Corruption_outliers, collapse=", "))

# Social.support has no outlier now
# Continue remove the outliers of Perceptions.of.corruption variable
# Remove Perceptions.of.corruption Outlier
world_happiness <- subset(world_happiness, 
                          !(world_happiness$Perceptions.of.corruption 
                            %in% Corruption_outliers))

# Check & remove outliers - 6th time
# ============================================================
# Re-check the outliers
Corruption_outliers <- boxplot.stats(world_happiness$Perceptions.of.corruption)$out

paste("Perceptions.of.corruption Outlier: ", 
      paste(Corruption_outliers, collapse=", "))

# Now, all variables have no outlier.


# 2.2.3. Check for normality
# ============================================================
# Skewness function to examine normality
# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approximately symetrical

#install.packages("e1071")
library(e1071)

opar <- par(no.readonly = TRUE)
par(mfrow =c(3,3)) # 3rows * 3cols

plot(density(world_happiness$Life.Ladder), 
     main = "Density plot for Life.Ladder", 
     ylab = "Frequency", xlab = "Life.Ladder",
     sub = paste("Skewness:", 
                 round(e1071::skewness(world_happiness$Life.Ladder), 2)))
# Fill in the area under the plot with red
polygon(density(world_happiness$Life.Ladder), col = "red")


plot(density(world_happiness$Log.GDP.per.capita), 
     main = "Density plot for Log.GDP.per.capita", 
     ylab = "Frequency", xlab = "Log.GDP.per.capita",
     sub = paste("Skewness:", 
                 round(e1071::skewness(world_happiness$Log.GDP.per.capita), 2)))
# Fill in the area under the plot with red
polygon(density(world_happiness$Log.GDP.per.capita), col = "red")


plot(density(world_happiness$Social.support), 
     main = "Density plot for Social.support", 
     ylab = "Frequency", xlab = "Social.support",
     sub = paste("Skewness:", 
                 round(e1071::skewness(world_happiness$Social.support), 2)))
# Fill in the area under the plot with red
polygon(density(world_happiness$Social.support), col = "red")


plot(density(world_happiness$Healthy.life.expectancy.at.birth), 
     main = "Density plot for Healthy.life.expectancy.at.birth", 
     ylab = "Frequency", xlab = "Healthy.life.expectancy.at.birth",
     sub = paste("Skewness:", 
                 round(e1071::skewness(world_happiness$Healthy.life.expectancy.at.birth), 2)))
# Fill in the area under the plot with red
polygon(density(world_happiness$Healthy.life.expectancy.at.birth), col = "red")


plot(density(world_happiness$Freedom.to.make.life.choices), 
     main = "Density plot for Freedom.to.make.life.choices", 
     ylab = "Frequency", xlab = "Freedom.to.make.life.choices",
     sub = paste("Skewness:", 
                 round(e1071::skewness(world_happiness$Freedom.to.make.life.choices), 2)))
# Fill in the area under the plot with red
polygon(density(world_happiness$Freedom.to.make.life.choices), col = "red")


plot(density(world_happiness$Perceptions.of.corruption), 
     main = "Density plot for Perceptions.of.corruption", 
     ylab = "Frequency", xlab = "Perceptions.of.corruption",
     sub = paste("Skewness:", 
                 round(e1071::skewness(world_happiness$Perceptions.of.corruption), 2)))
# Fill in the area under the plot with red
polygon(density(world_happiness$Perceptions.of.corruption), col = "red")


plot(density(world_happiness$Positive.affect), 
     main = "Density plot for Positive.affect", 
     ylab = "Frequency", xlab = "Positive.affect",
     sub = paste("Skewness:", 
                 round(e1071::skewness(world_happiness$Positive.affect), 2)))
# Fill in the area under the plot with red
polygon(density(world_happiness$Positive.affect), col = "red")


par <- opar

# Show all skewness values:
paste("Skewness for Life.Ladder:", round(e1071::skewness(
        world_happiness$Life.Ladder), 2))
paste("Skewness for Log.GDP.per.capita:", round(e1071::skewness(
        world_happiness$Log.GDP.per.capita), 2))
paste("Skewness for Social.support:", round(e1071::skewness(
        world_happiness$Social.support), 2))
paste("Skewness for Healthy.life.expectancy.at.birth:", round(e1071::skewness(
        world_happiness$Healthy.life.expectancy.at.birth), 2))
paste("Skewness for Freedom.to.make.life.choices:", round(e1071::skewness(
        world_happiness$Freedom.to.make.life.choices), 2))
paste("Skewness for Perceptions.of.corruption:", round(e1071::skewness(
        world_happiness$Perceptions.of.corruption), 2))
paste("Skewness for Positive.affect:", round(e1071::skewness(
        world_happiness$Positive.affect), 2))

# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approximately symmetric

# Skewness for Life.Ladder: 0.09 ==> approximately symmetric
# Skewness for Log.GDP.per.capita: -0.36 ==> approximately symmetric
# Skewness for Social.support: -0.70 ==> moderately skewed
# Skewness for Healthy.life.expectancy.at.birth: -0.62 ==> moderately skewed
# Skewness for Freedom.to.make.life.choices: -0.48 ==> approximately symmetric
# Skewness for Perceptions.of.corruption: -0.52 => moderately skewed
# Skewness for Positive.affect: -0.17 ==> approximately symmetric


# Examine normality using the qqnorm() function 
# and a histogram of the data for distribution
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2)) # 1row * 2cols
hist(world_happiness$Life.Ladder, 
     main = "Normality proportion of Life.Ladder", 
     xlab = "Life.Ladder")
qqnorm(world_happiness$Life.Ladder)
qqline(world_happiness$Life.Ladder)
par <- opar


# 2.3. Build the multiple linear regression model
# ============================================================

# 2.3.1. Training data and Testing data
# ============================================================
# The dataset will be split into 2 groups Training data and Testing data
# In previous steps, some rows of the dataset world_happiness
# have been removed. I'll reset the row names of
# this dataset first so that I can use the seed(1) to create data sample
rownames(world_happiness) <- NULL

set.seed(1)
rownames(df) <- NULL
no_rows <- nrow(world_happiness)
data_sample <- sample(1:no_rows, 
                 size = round(0.7 * no_rows), 
                 replace = FALSE)

training_data <- world_happiness[data_sample, ]
testing_data <- world_happiness[-data_sample, ]


# 2.3.2. Specify and examine the model
# ============================================================
# Specify a model and train it with training_data
attach(world_happiness)
fit_model <- lm(Life.Ladder ~ Log.GDP.per.capita +
                  Social.support + Healthy.life.expectancy.at.birth +
                  Freedom.to.make.life.choices + Perceptions.of.corruption +
                  Positive.affect, 
                data = training_data)

# Examine the results of the model in detail
summary(fit_model)
# Residuals:
#  Min         1Q     Median    3Q       Max 
# -1.85556 -0.32566  0.01933  0.35047  1.76026 

# Coefficients:
#                                 Estimate Std. Error    t value Pr(>|t|)    
# (Intercept)                      -2.418987   0.259688  -9.315  < 2e-16 ***
# Log.GDP.per.capita                0.317278   0.034223   9.271  < 2e-16 ***
# Social.support                    1.912536   0.232832   8.214 6.64e-16 ***
# Healthy.life.expectancy.at.birth  0.034870   0.004534   7.690 3.53e-14 ***
# Freedom.to.make.life.choices      0.470347   0.168604   2.790 0.005378 ** 
# Perceptions.of.corruption        -0.754612   0.194234  -3.885 0.000109 ***
# Positive.affect                   1.905996   0.201863   9.442  < 2e-16 ***
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.5445 on 989 degrees of freedom
# Multiple R-squared:  0.6811,	Adjusted R-squared:  0.6792 
# F-statistic:   352 on 6 and 989 DF,  p-value: < 2.2e-16


# The coefficient is significantly different from zero at the p < .0001 level

# The coefficient for Freedom.to.make.life.choices and 
# Perceptions.of.corruption aren’t significantly different from zero 
# (p = 0.005378) and (p = 0.000109) suggesting that 
# Freedom.to.make.life.choices and Perceptions.of.corruption 
# aren’t linearly related when controlling for the other predictor variables.

# Log.GDP.per.capita, Social.support, Healthy.life.expectancy.at.birth, 
# and Positive.affect are showing a linear relationship.

# All of the predictor variables account for 68% of the variance in Life.Ladder.
# This is indicated by the Multiple R-squared value (0.6811)

# Look at the output from the confint() function
confint(fit_model)
#                                     2.5 %        97.5 %
#  (Intercept)                      -2.92858835  -1.9093847
# Log.GDP.per.capita                0.25012019   0.3844363
# Social.support                    1.45563406   2.3694388
# Healthy.life.expectancy.at.birth  0.02597232   0.0437678
# Freedom.to.make.life.choices      0.13948358   0.8012102
# Perceptions.of.corruption        -1.13576917  -0.3734543
# Positive.affect                   1.50986659   2.3021249


# 2.3.3. Use Studentized residuals to analyze the model
# ============================================================
library(car)
par(mfrow = c(1,1))
# Plots empirical quantiles of studentized residuals from a linear model, 
# against theoretical quantiles of a comparison distribution
qqPlot(fit_model, 
       labels=row(world_happiness), 
       id.method="identify", 
       simulate=TRUE, 
       main = "Q-Q Plot for fit_model")
# The grahp shows that there are two values in the plot which have outliers

# Training the outlier data and analyzing whether it affect or not
training_data["149",]
training_data["1422",]

# Fitting the outlier data to the new model
fitted(fit_model)["149"]
fitted(fit_model)["1422"]


# 2.3.3. Use the standardize residuals for better statistical analysis
# ============================================================

# Generate a histogram of the studentized residuals and 
# superimposes a normal curve, kernel-density curve, and rug plot
student_fit_model <- rstudent(fit_model)
hist(student_fit_model,
     breaks=10,
     freq=FALSE,
     xlab="Studentized Residual",
     main="Distribution of Errors")

rug(jitter(student_fit_model), col="brown")

curve(dnorm(x, mean=mean(student_fit_model), sd=sd(student_fit_model)), 
      add=TRUE, col="blue", lwd=2)

lines(density(student_fit_model)$x, density(student_fit_model)$y, col="red", lwd=2, lty=2)

legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), 
       lty=1:2, col=c("blue","red"), cex=.7)

# As you can see in this chart, the errors follow 
# a normal distribution quite well, with the exception 
# of 3 outliers on the left and 1 outlier on the right of the chart
# (outside the boundary)

# Next, use the car package to check whether a model contains any outlier
outlierTest(fit_model)
# No Studentized residuals with Bonferroni p < 0.05
# Largest |rstudent|:
#  rstudent unadjusted     p-value    Bonferroni p
#   1422     -3.432208    0.0006235      0.62101

# Here, we see that the row "1422" is identified as an outlier
# Notes: This row is the data of Zimbabwe in 2019 with Life.Ladder = 2.694

# Now I’m going to delete the row "1422" and then re-run the test.
world_happiness <- world_happiness[-c(1422), ]

# Split the data into training and testing again

# Reset the row names of this dataset again 
# before using the seed(1) to create data sample
rownames(world_happiness) <- NULL

set.seed(1)
no_rows <- nrow(world_happiness)
data_sample <- sample(1:no_rows, 
                      size = round(0.7 * no_rows), 
                      replace = FALSE)

training_data <- world_happiness[data_sample, ]
testing_data <- world_happiness[-data_sample, ]

# Rebuild the MLR model
attach(world_happiness)
fit_model <- lm(Life.Ladder ~ Log.GDP.per.capita +
                  Social.support + Healthy.life.expectancy.at.birth +
                  Freedom.to.make.life.choices + Perceptions.of.corruption +
                  Positive.affect, 
                data = training_data)

# Re-running the outlier test
outlierTest(fit_model)
# No Studentized residuals with Bonferroni p < 0.05
# Largest |rstudent|:
# rstudent unadjusted     p-value    Bonferroni p
# 149      -3.489442    0.00050541      0.50289
# Here, we see that the row "149" is identified as an outlier
# Notes: This row is the data of Botswana in 2011 with Life.Ladder = 3.520

# Continue delete the row "149" and then re-run the test.
world_happiness <- world_happiness[-c(149), ]

# Split the data into training and testing again

# Reset the row names of this dataset again 
# before using the seed(1) to create data sample
rownames(world_happiness) <- NULL

set.seed(1)
no_rows <- nrow(world_happiness)
data_sample <- sample(1:no_rows, 
                      size = round(0.7 * no_rows), 
                      replace = FALSE)

training_data <- world_happiness[data_sample, ]
testing_data <- world_happiness[-data_sample, ]

# Rebuild the MLR model
attach(world_happiness)
fit_model <- lm(Life.Ladder ~ Log.GDP.per.capita +
                  Social.support + Healthy.life.expectancy.at.birth +
                  Freedom.to.make.life.choices + Perceptions.of.corruption +
                  Positive.affect, 
                data = training_data)

# Re-running the outlier test
outlierTest(fit_model)
# No Studentized residuals with Bonferroni p < 0.05
# Largest |rstudent|:
# rstudent unadjusted     p-value    Bonferroni p
# 153       -3.50988    0.00046856      0.46621
# Here, we see that the row "153" is identified as an outlier
# Notes: This row is the data of Botswana in 2016 with Life.Ladder = 3.499

# Continue delete the row "153" and then re-run the test.
world_happiness <- world_happiness[-c(153), ]

# Split the data into training and testing again

# Reset the row names of this dataset again 
# before using the seed(1) to create data sample
rownames(world_happiness) <- NULL

set.seed(1)
no_rows <- nrow(world_happiness)
data_sample <- sample(1:no_rows, 
                      size = round(0.7 * no_rows), 
                      replace = FALSE)

training_data <- world_happiness[data_sample, ]
testing_data <- world_happiness[-data_sample, ]

# Rebuild the MLR model
attach(world_happiness)
fit_model <- lm(Life.Ladder ~ Log.GDP.per.capita +
                  Social.support + Healthy.life.expectancy.at.birth +
                  Freedom.to.make.life.choices + Perceptions.of.corruption +
                  Positive.affect, 
                data = training_data)

# Re-running the outlier test
outlierTest(fit_model)
# No Studentized residuals with Bonferroni p < 0.05
# Largest |rstudent|:
# rstudent unadjusted     p-value    Bonferroni p
# 153       -3.310283    0.0009658         0.96
# Here, we see that the row "153" is identified as an outlier
# Notes: This row is the data of Botswana in 2017 with Life.Ladder = 3.505

# Continue delete the row "153" and then re-run the test.
world_happiness <- world_happiness[-c(153), ]

# Split the data into training and testing again

# Reset the row names of this dataset again 
# before using the seed(1) to create data sample
rownames(world_happiness) <- NULL

set.seed(1)
no_rows <- nrow(world_happiness)
data_sample <- sample(1:no_rows, 
                      size = round(0.7 * no_rows), 
                      replace = FALSE)

training_data <- world_happiness[data_sample, ]
testing_data <- world_happiness[-data_sample, ]

# Rebuild the MLR model
attach(world_happiness)
fit_model <- lm(Life.Ladder ~ Log.GDP.per.capita +
                  Social.support + Healthy.life.expectancy.at.birth +
                  Freedom.to.make.life.choices + Perceptions.of.corruption +
                  Positive.affect, 
                data = training_data)

# Re-running the outlier test
outlierTest(fit_model)
# No Studentized residuals with Bonferroni p < 0.05
# Largest |rstudent|:
# rstudent unadjusted     p-value    Bonferroni p
# 153       -3.72016    0.00021035      0.20888
# Here, we see that the row "153" is identified as an outlier
# Notes: This row is the data of Botswana in 2018 with Life.Ladder = 3.461

# Continue delete the row "153" and then re-run the test.
world_happiness <- world_happiness[-c(153), ]

# Split the data into training and testing again

# Reset the row names of this dataset again 
# before using the seed(1) to create data sample
rownames(world_happiness) <- NULL

set.seed(1)
no_rows <- nrow(world_happiness)
data_sample <- sample(1:no_rows, 
                      size = round(0.7 * no_rows), 
                      replace = FALSE)

training_data <- world_happiness[data_sample, ]
testing_data <- world_happiness[-data_sample, ]

# Rebuild the MLR model
attach(world_happiness)
fit_model <- lm(Life.Ladder ~ Log.GDP.per.capita +
                  Social.support + Healthy.life.expectancy.at.birth +
                  Freedom.to.make.life.choices + Perceptions.of.corruption +
                  Positive.affect, 
                data = training_data)

# Re-running the outlier test
outlierTest(fit_model)
# No Studentized residuals with Bonferroni p < 0.05
# Largest |rstudent|:
# rstudent unadjusted     p-value    Bonferroni p
# 153      -3.680634    0.00024528      0.24356
# Here, we see that the row "153" is identified as an outlier
# Notes: This row is the data of Botswana in 2019 with Life.Ladder = 3.471

# Continue delete the row "153" and then re-run the test.
world_happiness <- world_happiness[-c(153), ]

# Split the data into training and testing again

# Reset the row names of this dataset again 
# before using the seed(1) to create data sample
rownames(world_happiness) <- NULL

set.seed(1)
no_rows <- nrow(world_happiness)
data_sample <- sample(1:no_rows, 
                      size = round(0.7 * no_rows), 
                      replace = FALSE)

training_data <- world_happiness[data_sample, ]
testing_data <- world_happiness[-data_sample, ]

# Rebuild the MLR model
attach(world_happiness)
fit_model <- lm(Life.Ladder ~ Log.GDP.per.capita +
                  Social.support + Healthy.life.expectancy.at.birth +
                  Freedom.to.make.life.choices + Perceptions.of.corruption +
                  Positive.affect, 
                data = training_data)

# Re-running the outlier test
outlierTest(fit_model)
# No Studentized residuals with Bonferroni p < 0.05
# Largest |rstudent|:
# rstudent unadjusted     p-value    Bonferroni p
# 981       3.305117     0.00098365    0.97578

# Here, we see that the row "981" is identified as an outlier
# Notes: This row is the data of Pakistan in 2010 with Life.Ladder = 5.786

# Continue delete the row "981" and then re-run the test.
world_happiness <- world_happiness[-c(981), ]

# Split the data into training and testing again

# Reset the row names of this dataset again 
# before using the seed(1) to create data sample
rownames(world_happiness) <- NULL

set.seed(1)
no_rows <- nrow(world_happiness)
data_sample <- sample(1:no_rows, 
                      size = round(0.7 * no_rows), 
                      replace = FALSE)

training_data <- world_happiness[data_sample, ]
testing_data <- world_happiness[-data_sample, ]

# Rebuild the MLR model
attach(world_happiness)
fit_model <- lm(Life.Ladder ~ Log.GDP.per.capita +
                  Social.support + Healthy.life.expectancy.at.birth +
                  Freedom.to.make.life.choices + Perceptions.of.corruption +
                  Positive.affect, 
                data = training_data)

# Re-running the outlier test
outlierTest(fit_model)
# No Studentized residuals with Bonferroni p < 0.05
# Largest |rstudent|:
# rstudent unadjusted     p-value    Bonferroni p
# 148      -3.158012     0.0016369       NA

# Here, we see that the "Bonferroni p" value of the row "148" is NA.
# Therefore, I’m going to proceed and not remove the row "148".

# The outlier is now removed. Re-run the histogram of the studentized residuals
# and superimposes a normal curve, kernel-density curve, and rug plot

student_fit_model <- rstudent(fit_model)
hist(student_fit_model,
     breaks=10,
     freq=FALSE,
     xlab="Studentized Residual",
     main="Distribution of Errors")

rug(jitter(student_fit_model), col="brown")

curve(dnorm(x, mean=mean(student_fit_model), sd=sd(student_fit_model)), 
      add=TRUE, col="blue", lwd=2)

lines(density(student_fit_model)$x, density(student_fit_model)$y, col="red", lwd=2, lty=2)

legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), 
       lty=1:2, col=c("blue","red"), cex=.7)

# There are still two outliers on the left of the chart.
# However, it gets better so far.

# 2.4. Check Linearity
# ============================================================
# Visualize the linear relationship between the 
# dependent and independent through a linear line
crPlots(fit_model)

# Influential observations
# Use the Cook's distance formula on trained data
cutoff <- 4/(nrow(training_data) - length(fit_model$coefficients) - 2)
plot(fit_model, which = 4, cook.levels = cutoff)
# Any points above the red cut-off line are influencial observations
abline(h = cutoff, lty = 2, col = "red")
# The chart suggests that there are approximately 45 rows 
# could be an influential observation in the data.

# Notes: Cook’s distance plots can help identify influential observations, 
# but they don’t provide information about how these observations affect the model

# Show the regression coefficient of the predictor variables
avPlots(fit_model, ask = FALSE)

# Combine the information from outlier, leverage, and influence plots 
# into one highly informative plot by using the influencePlot() function 
# from the car package
influencePlot(fit_model, main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")

# The influence plot shows that "148" are very close to the boundary (-3 to 3) 
# and so could be outliers.
# "1129", "552" and "1209" are influential observations.


# 2.5. Check Homoscedasticity
# ============================================================
# Check the Homoscedasticty Test using (ncvTest) which generates the 
# result for the hypothesis of constant error variance with a fitted model data
# If p-value < 0.05, then the error variance value may change (Homoscedasticity)
# If p-value > 0.05, then the error variance value may not change (Heteroscedasticity)
ncvTest(fit_model)
# A result shows that p-value = 0.018058 < the cut-off value (0.05),
# then the error variance value may may change (Homoscedasticity)

# Now, use the spreadLevelPlot() function creates a scatter plot 
# of the absolute standardized residuals versus the fitted values 
# and superimposes a line of best fit.

spreadLevelPlot(fit_model)
# Suggested power transformation:  1.614425


# 2.6. Global validation of linear model assumption
# ============================================================
# Evaluate separately the different test performed while building a model
# install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(fit_model)
summary(gvmodel)

# ASSESSMENT OF THE LINEAR MODEL ASSUMPTIONS
# USING THE GLOBAL TEST ON 4 DEGREES-OF-FREEDOM:
#  Level of Significance =  0.05 

#Call:
#  gvlma(x = fit_model) 

#                     Value   p-value         Decision
# Global Stat        83.053   0.00000   Assumptions NOT satisfied!
# Skewness            3.662   0.05565   Assumptions acceptable.
# Kurtosis            1.561   0.21147   Assumptions acceptable.
# Link Function      72.664   0.00000   Assumptions NOT satisfied!
# Heteroscedasticity  5.166   0.02304   Assumptions NOT satisfied!

# The decision line indicated that the assumptions were violated (p < 0.05)

# As MLR model doesn’t meet the linearity assumptions, 
# we now transform one or more variables to improve the model

# Generate a maximum-likelihood estimation of the power ff 
# most likely to normalize the variable X
summary(powerTransform(training_data$Life.Ladder))

# The results suggest that we can normalize the variable 
# Life.Ladder by replacing it with Life.Ladder 0.7241
# So, we could apply a square-root transformation to 
# improve the model’s fit to normality


# 2.7. Transforming variables
# ============================================================
sqrt_transform_Life.Ladder <- sqrt(training_data$Life.Ladder)
training_data$Life.Ladder_sqrt <- sqrt_transform_Life.Ladder

fit_model1 <- lm(Life.Ladder ~ Log.GDP.per.capita +
                  Social.support + Healthy.life.expectancy.at.birth +
                  Freedom.to.make.life.choices + Perceptions.of.corruption +
                  Positive.affect, 
                data = training_data)

fit_model2 <- lm(Life.Ladder_sqrt ~ Log.GDP.per.capita +
                   Social.support + Healthy.life.expectancy.at.birth +
                   Freedom.to.make.life.choices + Perceptions.of.corruption +
                   Positive.affect, 
                 data = training_data)


# 2.8. Comparing models using AIC
# ============================================================
AIC(fit_model1,fit_model2)
#             df    AIC
# fit_model1  8  1608.776
# fit_model2  8 -1398.921

# We can see that fit_model2 might better than fit_model1


# 2.9. Comparing models using Step-wise Regression
# ============================================================
library(MASS)
fit_test <- lm(Life.Ladder ~ Log.GDP.per.capita +
                   Social.support + Healthy.life.expectancy.at.birth +
                   Freedom.to.make.life.choices + Perceptions.of.corruption +
                   Positive.affect, 
                 data = training_data)
stepAIC(fit_test, direction="backward")

# No variables are removed, so it can be assumed that 
# using all predictor variables so far is good for the model. 
# However, there is no guarantee that it will find the “best” model.

# An approach that attempts to overcome this limitation 
# is all subsets regression.


# Now, use the leap plot to show the best correlation of the variables 
# with the score of R-squared and Adjusted R-squared values on the y-axis.

# install.packages("leaps")
library(leaps)

# Check with Life.Ladder
leaps <- regsubsets(Life.Ladder ~ Log.GDP.per.capita +
                      Social.support + Healthy.life.expectancy.at.birth +
                      Freedom.to.make.life.choices + Perceptions.of.corruption +
                      Positive.affect, 
                    data = training_data, 
                    nbest = 4)
plot(leaps,scale = "adjr2")
# The top row with the value 0.68 shows that 
# the variables used are the best fit model build.
# There are all six variables:
# - Log.GDP.per.capita
# - Social.support
# - Healthy.life.expectancy.at.birth
# - Freedom.to.make.life.choices
# - Perceptions.of.corruption
# - Positive.affect

# Lets see how that works with the transformed Life.Ladder response variable
# Check with Life.Ladder_sqrt
leaps <- regsubsets(Life.Ladder_sqrt ~ Log.GDP.per.capita +
                      Social.support + Healthy.life.expectancy.at.birth +
                      Freedom.to.make.life.choices + Perceptions.of.corruption +
                      Positive.affect, 
                    data = training_data, 
                    nbest = 4)
plot(leaps,scale = "adjr2")
# The top row with the value 0.67 shows that 
# the variables used are the best fit model build.

# So, the model with the six above variables is still the best.


# 2.10. Examine the accuracy of the model predicted
# ============================================================
fit_model <- lm(Life.Ladder ~ Log.GDP.per.capita +
                   Social.support + Healthy.life.expectancy.at.birth +
                   Freedom.to.make.life.choices + Perceptions.of.corruption +
                   Positive.affect, 
                 data = training_data)

fit_model_sqrt <- lm(Life.Ladder_sqrt ~ Log.GDP.per.capita +
                  Social.support + Healthy.life.expectancy.at.birth +
                  Freedom.to.make.life.choices + Perceptions.of.corruption +
                  Positive.affect, 
                data = training_data)

predicted_Life.Ladder <- predict(fit_model, testing_data)
predicted_Life.Ladder_sqrt <- predict(fit_model_sqrt, testing_data)
converted_Life.Ladder_sqrt <- predicted_Life.Ladder_sqrt ^2

# Actuals_predicted dataframe
actuals_predictions <- data.frame(cbind(actuals = testing_data$Life.Ladder, 
                                        predicted = predicted_Life.Ladder))
head(actuals_predictions)
#     actuals predicted
# 1    4.402  3.659251
# 2    4.758  3.781094
# 4    3.783  3.876339
# 9    5.485  5.213702
# 10   5.269  5.180138
# 12   5.510  5.177760

# Actuals_predicted dataframe for sqrt(Life.Ladder)
actuals_predictions_sqrt <- data.frame(cbind(actuals = testing_data$Life.Ladder, 
                                        predicted = converted_Life.Ladder_sqrt))
head(actuals_predictions_sqrt)
#     actuals predicted
# 1    4.402  3.744709
# 2    4.758  3.832640
# 4    3.783  3.914780
# 9    5.485  5.180980
# 10   5.269  5.129976
# 12   5.510  5.145010

# Check the accuracy
# Model with Life.Ladder
correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy
#             actuals predicted
# actuals   1.0000000 0.8412972
# predicted 0.8412972 1.0000000
# The model shows us that it has 84.13% correlation accuracy

# Model with Life.Ladder_sqrt
correlation_accuracy <- cor(actuals_predictions_sqrt)
correlation_accuracy
#             actuals predicted
# actuals   1.0000000 0.8466165
# predicted 0.8466165 1.0000000
# The model shows us that it has 84.66% correlation accuracy

# Not much difference between both models

# Calculate min max accuracy and Mean Absolute Percentage Error (MAPE) 
# which is a measure of prediction accuracy

# Model with Life.Ladder
min_max_accuracy <- mean(apply(actuals_predictions, 1, min) / 
                           apply(actuals_predictions, 1, max))
min_max_accuracy
# 0.9245523

# Model with Life.Ladder_sqrt
min_max_accuracy <- mean(apply(actuals_predictions_sqrt, 1, min) / 
                           apply(actuals_predictions_sqrt, 1, max))
min_max_accuracy
# 0.9257978

# The MAPE for Life.Ladder model is better than Life.Ladder_sqrt model


# 2.11. Run some output with the final model
# ============================================================

# See ranges in input data
summary(world_happiness)
# Life.Ladder    Log.GDP.per.capita  Social.support   Healthy.life.expectancy.at.birth
# Min.   :2.688   Min.   : 6.678     Min.   :0.5140   Min.   :43.90                   
# 1st Qu.:4.600   1st Qu.: 8.386     1st Qu.:0.7410   1st Qu.:57.90                   
# Median :5.247   Median : 9.376     Median :0.8260   Median :64.69                   
# Mean   :5.305   Mean   : 9.203     Mean   :0.8082   Mean   :62.66                   
# 3rd Qu.:5.984   3rd Qu.:10.061     3rd Qu.:0.8930   3rd Qu.:67.50                   
# Max.   :7.615   Max.   :11.065     Max.   :0.9850   Max.   :75.20    

# Freedom.to.make.life.choices  Perceptions.of.corruption  Positive.affect 
# Min.   :0.3580                Min.   :0.5560             Min.   :0.3840  
# 1st Qu.:0.6380                1st Qu.:0.7518             1st Qu.:0.6190  
# Median :0.7385                Median :0.8240             Median :0.7060  
# Mean   :0.7237                Mean   :0.8116             Mean   :0.7033  
# 3rd Qu.:0.8240                3rd Qu.:0.8850             3rd Qu.:0.7900  
# Max.   :0.9640                Max.   :0.9830             Max.   :0.9440 

df <- data.frame(Log.GDP.per.capita = c(8.3), 
                 Social.support = c(0.7), 
                 Healthy.life.expectancy.at.birth = c(57), 
                 Freedom.to.make.life.choices = c(0.6),
                 Perceptions.of.corruption = c(0.7), 
                 Positive.affect = c(0.6))
predicted_Life.Ladder <- predict(fit_model, df)
predicted_Life.Ladder
# 4.430539 
# Results suggest that for a small Log.GDP.per.capita of 8.3 
# and small Social.support of 0.7
# and small Healthy.life.expectancy.at.birth of 57 
# and small Freedom.to.make.life.choices of 0.6
# and small Perceptions.of.corruption of 0.7 
# and small Positive.affect of 0.6, the Life.Ladder point is 4.430539


df <- data.frame(Log.GDP.per.capita = c(11), 
                 Social.support = c(0.98), 
                 Healthy.life.expectancy.at.birth = c(57), 
                 Freedom.to.make.life.choices = c(0.6),
                 Perceptions.of.corruption = c(0.7), 
                 Positive.affect = c(0.6))
predicted_Life.Ladder <- predict(fit_model, df)
predicted_Life.Ladder
# 5.800077
# With higher Log.GDP.per.capita and higher Social.support, 
# same Healthy.life.expectancy.at.birth and same Freedom.to.make.life.choices
# and same Perceptions.of.corruption and same Positive.affect, 
# the Life.Ladder point increases to 5.800077.


df <- data.frame(Log.GDP.per.capita = c(11), 
                 Social.support = c(0.98), 
                 Healthy.life.expectancy.at.birth = c(67), 
                 Freedom.to.make.life.choices = c(0.8),
                 Perceptions.of.corruption = c(0.7), 
                 Positive.affect = c(0.6))
predicted_Life.Ladder <- predict(fit_model, df)
predicted_Life.Ladder
# 6.266201
# With higher Healthy.life.expectancy.at.birth, 
# the Life.Ladder point increases to 6.266201


# Test the model by inputting some real data from the dataset
# The data for Ireland in 2013.
df <- data.frame(Log.GDP.per.capita = c(10.884), 
                 Social.support = c(0.955), 
                 Healthy.life.expectancy.at.birth = c(71.80), 
                 Freedom.to.make.life.choices = c(0.884),
                 Perceptions.of.corruption = c(0.558), 
                 Positive.affect = c(0.814))
predicted_Life.Ladder <- predict(fit_model, df)
predicted_Life.Ladder
# 6.890734

predicted_Life.Ladder_sqrt <- predict(fit_model_sqrt, df)
converted_Life.Ladder_sqrt <- predicted_Life.Ladder_sqrt ^2
converted_Life.Ladder_sqrt
# 6.936635 
