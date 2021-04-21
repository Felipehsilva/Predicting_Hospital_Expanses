# Predicting Hospital Expenses

# For this analysis, we will use a data set simulating hypothetical medical expenses
# for a group of patients spread across 4 regions of Brazil.
# This dataset has 1,338 observations and 7 variables.

# Step 1 - Collecting the data
despesas <- read.csv("despesas.csv")


# Step 2: Exploring and Preparing the Data
# Viewing the variables (Age,Sex,bmi,children,smoking, region, spending)
str(despesas)

# Central Trend Means of the spending variable
summary(despesas$gastos)

# Building a histogram
hist(despesas$gastos, main = 'Histogram', xlab = 'spending')

# Contingency table for regions
table(despesas$regiao)

# Exploring the relationship between variables: Correlation Matrix
cor(despesas[c("idade", "bmi", "filhos", "gastos")])

# None of the correlations in the matrix are considered strong, but there are some interesting associations.
# For example, age and BMI (BMI) appear to have a weak positive correlation, which means that
# with increasing age, body mass tends to increase. There is also a positive correlation
# moderate between age and expenses, in addition to the number of children and expenses. These associations imply
# that as age, body mass and number of children increase, the expected cost of health insurance goes up.

# Viewing the relationship between variables: Scatterplot
# Realize that there is no clear relationship between the variables
pairs(despesas[c("idade", "bmi", "filhos", "gastos")])
colunas_numericas <- sapply(despesas, is.numeric)
colunas_numericas
data_cor <- cor(despesas[,colunas_numericas])
data_cor
# install.packages('corrgram')
# install.packages('corrplot')
library(corrplot)
library(corrgram)
corrplot(data_cor, method = 'color')
# Scatterplot Matrix
# install.packages("psych")
library(psych)
# This chart provides more information about the relationship between variables
pairs.panels(despesas[c("idade", "bmi", "filhos", "gastos")])

# Step 3: Training the Model
modelo <- lm(gastos ~ idade + filhos + bmi + sexo + fumante + regiao,
             data = despesas)
modelo
# Similar to the previous item
modelo <- lm(gastos ~ ., data = despesas)

# Viewing the coefficients
modelo

# Predicting medical expenses
previsao <- predict(modelo)
class(previsao)
head(previsao)


# Step 4: Assessing the Model's Performance
# More details about the model
summary(modelo)


# Step 5: Optimizing the Model's Performance

# Adding a variable with twice the age value

# One of the main differences from regression modeling to other Machine Learning techniques is that regression
# typically leaves the selection of the model specification characteristics to the analyst. Consequently,
# if we have enough information on how the selection of variables is related to the result, we can use this
# information to specify the model and thus improve performance.

# In linear regression, the relationship between the independent variable and the dependent variable is considered linear,
# although this may not always be true. For example, the effect of age on medical expenses may not be constant
# through all ages. Medical treatment may be disproportionately higher among the older population.

# Linear regression responds by the formula: y = A + Bx

# However, in some situations, we may want to include a non-linear relationship, adding a higher order term to the regression model,
# treating the model as polynomial. Therefore, the formula will be: y = A + B1x + B2xˆ2

# The difference between these two equations is that the additional item B2 (Beta coefficient) will be estimated from the effect of xˆ2 (x squared),
#thus capturing the impact of age as a function of age squared.

# By adding age and age2 to the model, this will allow us to separate the linear and non-linear impact of age on medical expenses.
# ** The creation of the age2 variable could lead to questions about multicollinearity. See an explanation below for this.

despesas$idade2 <- despesas$idade ^ 2

# Adding an indicator for BMI> = 30
despesas$bmi30 <- ifelse(despesas$bmi >= 30, 1, 0)

# Creating the final model
modelo_v2 <- lm(gastos ~ idade + idade2 + filhos + bmi + sexo +
                  bmi30 * fumante + regiao, data = despesas)

summary(modelo_v2)




# ** Multicollinearity

# The creation of age2 could lead to questions about multicollinearity. But what is multicollinearity?

# Multicollinearity is a common problem when estimating linear regression models, including logistic regression. This problem
# occurs when there is a high correlation between the predictive variables, generating unreliable estimates of the regression coefficients.
# This phenomenon is certainly something that requires special attention from the Data Scientist, but in some cases it can be safely ignored.

# In our project, multicollinearity is not a problem. Multicollinearity needs to be verified and resolved when we want to estimate the
# independent effect of two variables that are correlated. In our case, we are not interested in assessing the effect of the change
# regardless of age and age2. Whenever a study involving age is carried out, it is a good practice to include age at
# square to reduce the effect of age on the modeling process, because as we saw the relationship of the independent variable with the variable
# age (dependent) may not necessarily be linear. Multicollinearity will be present, but it will not affect our ultimate goal.


