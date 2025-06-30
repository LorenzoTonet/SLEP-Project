## NCCTG Lung Cancer Data

### The dataset

The dataset we chose to analyze is the `cancer` dataset from the r package `survival`. The dataset itself was first collected and analyzed by the North Central Cancer Treatment Group. In this prospective study the partecipants, before hospitalization due to lung cancer were provided with a questionare. The patients were then later evaluated by a phisician that assigned to them a performance score.

The variables collected are the followin:

|  |  |
|:---|:---|
| inst | The code of the institution in which the patient was hospitalized |
| time | The survival time in days |
| status | The censoring status |
| age | The age in years |
| sex | Patient sex |
| ph.ecog | ECOG performance score as rated by the physician. 0=asymptomatic, 1= symptomatic but completely ambulatory, 2= in bed \<50% of the day, 3= in bed \> 50% of the day but not bedbound, 4 = bedbound |
| ph.karno | Karnofsky performance score (bad=0-good=100) rated by physician |
| pat.karno | Karnofsky performance score as rated by the patient itself |
| meal.cal | Calories consumed at the meal before hospitalization |
| wt.loss | Weight loss in pounds in the last six months |

Most of the present variables are categorical. We decided to add another categorical varible, cutting the age range into three age ranges: 0-50, 50-70, 70+. We also converted some of the variables into categorical factors.

### First summary

### Na distribution in the file 

We first tried to see if there was any systematic distribution of missing values inside the dataset. The first thing we did is a comprehensive NA plot for all the variables.

We then tryed to stratify with respect to the different categorical variables, so to check if there was any substancial difference in the NA distribution.

Most of the missing values, around 20% of the total, are in the variable `meal.cal`. So for some initial data analysis we decided to ignore this variable

### (Is there a specific distribution)

## Exploratory data analysis

### Univariate plots

### Correlations

## Survival analysis 

### Kaplan-Meier curves

### Cox model

## Conclusion and results
