
This is my MSc's Data Analysis subject's Course work

**Task 1**

Consider the “SriLanka_Weather_Dataset.csv” file. It consists of a comprehensive collection
of weather data for 30 prominent cities in Sri Lanka, covering the period from January 1, 2010,
to January 1, 2023. Generate two important plots that can be used to visualize important
aspects of the dataset. Discuss how you can improve the plots. [LO1, LO2, LO3]

**Task 2**

Consider the “lepto_data.csv” dataset. The descriptions of the variables are given in the
“lepto_description.xls” file. This dataset contains demographic and clinical data of 1735
patients related to leptospirosis. The variable “Final” (last column) reports the leptospirosis
status of the patient (1-confirmed, 2-not detected). [LO1, LO2, LO2, LO4]

**Task 2.1**

Do a thorough descriptive analysis and identify the patterns and potential significant
variables. Use appropriate plots and tables.

**Task 2.2**

Fit a suitable predictive model to predict the leptospirosis status of the patient using a proper
train dataset. [You may use transformations, etc. techniques to improve the model]

**Task 2.3**

Get the predictions from the model for the corresponding test dataset.

**Task 2.4**

Now fit a suitable predictive model taking only the non-clinical variables and get the predictions for the same training and testing datasets. Compare your prediction metrics
and discuss the answers.

**Task 3**

Write an R function to do the following tasks. When a dataset is fed to the function, your
function should: [LO3, LO4]

**Task 3.1**

Identify qualitative and quantitative variables in the dataset.

**Task 3.2**

Count the missing values in each variable. Impute the missing values using: the mean value
of the variable if it is numeric; the mode of the variable if it is categorical.

**Task 3.3**

Identify univariate outliers for each numeric variable.

**Task 3.4**

Summarize each variable using a proper visualization tool for the respective variable (eg:
histogram, boxplot etc.).

**Task 3.5**

When the response variable is specified as an argument, it should run the best predictive
model for that response category (consider only continuous and binary response variables)
and select features considering all other meaningful variables. Your function should print
relevant diagnostic metrics and plots for the selected model.

**Task 3.6**

Implement the above functions in an R Shiny app/dashboard.
