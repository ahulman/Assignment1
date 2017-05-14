# Week 18
### Session overview
  * We discussed briefly the role of random effects in our models from the last few weeks (check out Chapter 4 in ALDA or the presentation at the following link: http://gseacademic.harvard.edu/alda/)
  * We checked the solutions of Challenge 6

### For next week (May 11)
As three of us will be at EDEG, there is no assignment in the usual way, BUT we agreed on that
 * Omar and Lasse will give a brief practical overview  of the LEAD symposium
 * Jakob will talk about the use of the aggregate() function
 * Adam will upload a script how to create a very detailed, nice figure using base graphics in R
 * EXTRA: If you have the time and interest, check out this paper on piecewise models (when we are not only analysing what happened before the event, but also after) http://isites.harvard.edu/fs/docs/icb.topic1250132.files/ije.pdf

# Challenge 6 (trajectory before event occurence)
  * Check out whether the deceased or the transplant group is larger and keep that for the rest of the exercise
  * Set time=0 at event occurence and fit log-serum bilirubin trajectories "backwards in time" (see Tabak et al., Lancet 2009)
  * Compare the treatment and the placebo group
  * Let me know if something is not clear

We aim to discuss your solutions next Thursday (May 4). If any urgent matter comes up that limits your ability to work on the project, let me know in time. 


# Challenge 5 (most of you have done it already)
The dataset is in the JM package and is called pbc2 (there are other datasets with similar name, but use this one).
Outcome: log-transformed serum bilirubin.
Trajectory: do at least a linear analysis, but you can test non-linear relationships too (maybe a scatterplot could help to decide)
Explore the three different time-scales (if you think a priori that some of them don’t make sense then argue instead of fitting the model).
The main aim is to assess the effect of treatment compared to placebo. 

# Challenge 4 - Trajectory analysis (continued)
Materials:
   * R codes for all chapters of ALDA: http://stats.idre.ucla.edu/r/examples/alda/
   * All materials, including presentations: http://gseacademic.harvard.edu/alda/
   * I uploaded the code that I explained today to our github (codeCI.R).
   
Don't hesitate to e-mail me while I am in Malaga. I will follow your progress on github.

# Challenge 3 - Trajectory analysis
In this challenge you will simulate a follow-up study with a similar structure to Whitehall II and then analyse the data.

0. Set the random seed to 123 so that we all get the same results. *set.seed(123)*
1. Generate a sample of 6021 men and 4107 women with age uniformly distributed between 40 and 60 years at baseline (*age_0*). 
2. Follow-up visits are planned every 5 years, but as in reality they do not happen exactly 5 years apart. Therefore let's add some random noise at each phase the following way. Create a follow-up time variable for each phase: *fup_0* is 0 for everyone, becaause that's baseline. *fup_n*=*fup_0* + 5*n* + *u*, where *n*=1,2,3 and u is a random number from a uniform distribution between -0.5 and 0.5. This way each study phase lasts for a year.
3. Calculate the corresponding ages at each phase i.e. *age_1*, *age_2*, *age_3*. (*age_0* had already been calculated)
4. Assuming that the study started in 1990 (create an examination "date" for everyone from a uniform distribution between 1990 and 1991) and calculate year of birth for everyone (called it *yob*).
5. Calculate BMI at each phase using the following equations:
    * Men: 2.04 + 0.944 age - 0.008 age^2 - 0.08 (yob - 1950) + E, where E is random noise from N(mean=0,sd=3.5)
    * Women: -14.4 + 1.549 age - 0.013 age^2 + 0.08 (yob - 1950) + E, where E is random noise from N(mean=0,sd=3.5)
    * Make sure that the random noise is different at each phase, otherwise you will get a perfect quadratic curve for each individual
    * Age changes from phase to phase, baseline age doesn't!
6. Simulate loss to follow-up (MCAR or MAR assumption? look up what this means) the following way:
    * Assume that if someone does not attend a phase then (s)he will never return (000x is ok, 0x00 is not ok)
    * Proportion of participants at each phase, men: 100%, 80%, 70%, 60% (or as close as possible if N is not a whole number)
    * Proportion of participants at each phase, women: 100%, 90%, 80%, 70%
    * Hint: generate the data for each phase for everyone, but then change the ones not attending to NA (both age and BMI)
7. Now you have a so-called wide dataset (each row represents one individual), but for the trajectory analysis, you need to transform it to long format (read more about it in ALDA, p.17, ALDA=Applied Longitudinal Data Analysis). Use the *reshape* function in R.
8. You are ready to fit the first BMI trajectories, more info on that is coming later...








# Challenge 2
In this challenge you will analyse the dataset generated in the previous step (with some modifications), and also interpret the results. Each one of you works in separate scripts (find them in the project folder).

Based on what you have done last week (a cleaned-up version), write a function called *simCohort* that takes two parameters: *N* (number of individuals in the cohort) and *seed* (seed for the random number generation). Stick to the proportion of men/women, smoker/non-smoker etc. from the previous exercise.

**TIP**: Check out the corresponding coursera videos on creating functions.

Then simulate two cohorts and use them when solving the 4 tasks below.

cohort1 <- simCohort(100, 123)

cohort2 <- simCohort(10000, 987)

1. Examine the association between age and sex (exposures) and bmi (continuous outcome). Use the *lm* function. 

2. Examine whether sex modifies the effect of age on bmi. Include an interaction between age and sex in the model. 

3. Do you see any differences between the results from the two cohorts? Why is that? Write a few lines on the interpretation of the results.

4. Bonus exercise: make a scatterplot and display the regression line. You can use both ggplot2 or the base graphics system. Be creative!









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

**IMPORTANT NOTE FOR BOTH:** set the seed (set.seed function) for the random number generation at the beginning of the script, so that the data set is reproducible.

## Step 2

Work together in the ch1_analysis.R script. You can divide the steps between each other, but each of you should be able to explain what is happening in the script.

1. Source the two scripts that you wrote using the source command (use the project directory as working directory, so that we don't have to change the path each time we run the script on a different machine). After this, you should see two data frames in the R environment.
2. Merge the two data frames by id using the merge function.
3. Change the type of the categorical variables to factor using the factor commands. Use the labels from the description of Step 1.
4. Calculate descriptive statistics overall (e.g. summary function) and by sex (summary within a tapply) using the summary/tapply and table/prop.table functions for continuous and categorical variables, respectively.
    * Median (Q1-Q3) for continuous variables
    * Frequency (%) for categorical variables

Remember to document your code and write a brief description when you commit a version. Ask each other or me for help if you are stuck, and remember to use R help, Google, StackExchange.
