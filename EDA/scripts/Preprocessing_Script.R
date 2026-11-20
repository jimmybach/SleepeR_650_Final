library(conflicted)
library(tidyverse)
library(nhanesA)
library(haven)

'## Grabbing raw data ----
Outcome: Sleep
Workday sleep hours SLD012
Covariates:
  Demographics- 18+
  Monthly Income (IND235) - Monthly Family Income
Race (RIDRETH3) - Race/Hispanic origin w/ NH Asian
Gender (RIAGENDR) - Gender of the participant
Age (RIDAGEYR) - Age in Years at screening

Physical Activity- maybe pick some occupational 
The following are binary variables, and there are corresponding questions for “number of days” and “minutes of” corresponding to each of them if we want more info:
  PAQ605 - Vigorous work activity (binary)
PAQ620 - Moderate work activity
PAQ635 - Walk or bicycle (specifically for travelling)
PAQ650 - Vigorous recreational activities
PAQ665 - Moderate recreational activities
PAD680 - Minutes sedentary activity
Alcohol Use
ALQ121 - How often have alcohol drink past 12 months
ALQ130 - Avg # of Alcohol drinks per day past 12 months
Marijuana use
DUQ217 - How often would you use marijuana?
  DUQ219 - How many joints or pipes smoke in a day? (“during the time you smoked…”)
DUQ220U - Last time used marijuana or hashish/unit 
Categorize this into recent use vs non-recent use
Work Hours
OCQ210 - Usually work 35 or more hours per week
OCQ670 - Overall work schedule past 3 months (this gives categories like 9-5 job, evenings/nights, early mornings, variable, etc.)
Mental Health 
DPQ_J if we want to use depression screener (“Over the last 2 weeks, have you felt depressed?”, etc.) 
Don’t bother with this for now, but it gives us additional info if needed
DLQ140 for “How often do you feel depressed?”
DLQ100 - “How often do you feel worried or anxious?”
#'

# demographics
demo <- nhanes('DEMO_J') |>
  select(
    SEQN, #identifier
    RIAGENDR, #Gender
    RIDAGEYR, #Age
    RIDRETH3, #Race/Hispanic origin w/ NH Asian
    DMDEDUC2, #Education level in Adults 20+
    INDFMPIR, #Ratio of family income to poverty
    SDMVPSU, # these two are used to make sample weights
    SDMVSTRA,
  )

# sleep
sleep <- nhanes('SLQ_J') |>
  select(
    SEQN, 
    SLD012 #
    )

dataset=inner_join(demo, sleep, by="SEQN")
rm(demo)
rm(sleep)

activity = nhanes("PAQ_J")|>
  select(
    SEQN,
    PAD680 #Minutes sedentary activity
  )

dataset=inner_join(dataset, activity, by="SEQN")
rm(activity)

alcohol=nhanes("ALQ_J") |>
  select(
    SEQN,
    ALQ130 #Avg. number of drinks in past yr
  )
dataset=inner_join(dataset, alcohol, by="SEQN")
rm(alcohol)


marijuana=nhanes("DUQ_J") |>
  select(
    SEQN,
    DUQ219, #How many joints or pipes smoke in a day?
    DUQ220U #Last time used marijuana
  )
dataset=inner_join(dataset, marijuana, by="SEQN")

dataset <- dataset %>%
  mutate(
    DUQ220U = case_when(
      DUQ220U == "Days"  ~ "Past Month",
      DUQ220U == "Weeks"  ~ "Past Month",
      DUQ220U == "Months"  ~ "Not Past Month",
      DUQ220U == "Years" ~ "Not Past Month",
      TRUE ~ "Not Past Month"
    )
  )
rm(marijuana)


tobacco=nhanes("SMQRTU_J") |>
  select(
    SEQN,
    SMDANY #Used any tobacco product in last 5 days
  )
dataset=inner_join(dataset, tobacco, by="SEQN")
rm(tobacco)

occupation=nhanes("OCQ_J") |>
  select(
    SEQN,
    OCQ210, #Usually work 35 or more hrs per week
    OCQ670 #Overall work schedule past 3 months
  )
dataset=inner_join(dataset, occupation, by="SEQN")
rm(occupation)

mental=nhanes("DLQ_J") |>
  select(
    SEQN,
    DLQ140, #How often do you feel depressed?
    DLQ100 #How often do you feel worried or anxious?
  )
dataset=inner_join(dataset, mental, by="SEQN")
rm(mental)

#Summarize Missing Data
install.packages("naniar")
library(naniar)

#Built in function to summarize frequencies and percentages of missing data
missing_var_totals = dataset %>%
  miss_var_summary()

#Prevent problems saving data as csv
missing_var_totals = missing_var_totals %>%
  mutate(pct_miss = as.numeric(pct_miss))

write.csv(dataset, "data/unweighted_dataset_11_17.csv", row.names = FALSE)
write.csv(as.data.frame(missing_var_totals), "data/missing_data_totals.csv", row.names = FALSE)


