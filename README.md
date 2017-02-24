# Challenge 1
Your task is to simulate a cohort data set (Step 1) including both continuous and categorical characteristics. Then characterise the cohort (Step 2) using descriptive statistics (usual Table 1).

## Step 1

The simulation will have two parts, one assigned to Jakob and the other to Omar. 

Both of you have to create an ID variable called "id" that goes from 1 to 1000 (tip: 1:1000 in R). Create also a sex variable called ("sex") including 0 (male) values for id numbers up to 600 and 1 from 601 to 1000. Having this common structure, you can then merge two data sets you generate.

**Jakob**'s task

1. Work in the script called ch1_simContinuous.R
2. Simulate an age variable ("age") from a uniform distribution between 40 and 70 years (tip: use the runif function).
3. Simulate a BMI variable ("bmi") that depends on both age and sex. Use the following equations: *21 + 0.1 Age* and *20 + 0.15 Age* for men and women, respectively. Add random noise to both using the rnorm function with mean=0 (both) and sd=2 (men), sd=2.5 (women).
4. Your script should return a data frame called dataContinuous and include id, sex, age, bmi 

**Omar**'s task

1. Work in the script called ch1_simCategorical.R
2. Simulate an ethnicity variable ("ethnic") that has 0 (white) and 1 (non-white) values. Make the population prevalence of white ethnicity 95%. Use the rbinom function (tip: prob=0.95).
3. Simulate a smoking variable ("smoke") that takes the following values: 0 (never), 1 (ex), 2 (current). This variable should depend on sex by using different probabilities for men (0.5, 0.3, 0.2) and women (0.6, 0.3, 0.1). Use the sample function.
4. Your script should return a data frame called dataCategorical and include id, sex, ethnic, smoke

**IMPORTANT NOTE FOR BOTH:** set the seed for the random number generation at the beginning of the script, so that the data set is reproducible.

## Step 2

Work together in the ch1_analysis.R script. You can divide the steps between each other, but each of you should be able to explain what is happening in the script.

1. Source the two scripts that you wrote using the source command (use the project directory as working directory, so that we don't have to change the path each time we run the script on a different machine). After this, you should see two data frames in the R environment.
2. Merge the two data frames by id using the merge function.
3. Change the type of the categorical variables to factor using the factor commands. Use the labels from the description of Step 1.
4. Calculate descriptive statistics by sex using the summary and table/prop.table functions for continuous and categorical variables, respectively.
    * Median (Q1-Q3) for continuous variables
    * Frequency (%) for categorical variables

Remember to document your code and write a brief description when you commit a version. Ask each other or me for help if you are stuck, and remember to use R help, Google, StackExchange.
