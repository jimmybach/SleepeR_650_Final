library(conflicted)
library(tidyverse)
library(nhanesA)
library(survey)
library(srvyr)

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
    WTINT2YR,
    WTMEC2YR # alcohol and drug use need these weights and they are more restrictive, so we will use them
  )

# Our study is looking at adults 20+
# now if we inner_join on
demo <- demo |>
  dplyr::filter(RIDAGEYR >= 20 & RIDAGEYR <= 59)

# sleep
sleep <- nhanes('SLQ_J') |>
  select(
    SEQN,
    SLD012 #
  )

dataset = inner_join(demo, sleep, by = "SEQN")
rm(sleep)

activity = nhanes("PAQ_J") |>
  select(
    SEQN,
    PAD680 #Minutes sedentary activity
  )

dataset = inner_join(dataset, activity, by = "SEQN")
rm(activity)

#if alq111 is no or alq121 is never in the past year
alcohol = nhanes("ALQ_J") |>
  select(
    SEQN,
    ALQ111,
    ALQ121,
    ALQ130 #Avg. number of drinks in past yr
  )
dataset = inner_join(dataset, alcohol, by = "SEQN")
dataset = dataset %>%
  mutate(
    ALQ130 = if_else(
      ALQ111 == "No" | ALQ121 == "Never in the last year",
      0,
      ALQ130
    )
  )
dataset = dataset %>%
  select(-ALQ111, -ALQ121)
rm(alcohol)

marijuana = nhanes("DUQ_J") |>
  select(
    SEQN,
    DUQ200,
    DUQ211,
    DUQ219, #How many joints or pipes smoke in a day?
    DUQ220U #Last time used marijuana
  )
dataset = inner_join(dataset, marijuana, by = "SEQN")

dataset <- dataset %>%
  mutate(
    DUQ220U = case_when(
      DUQ220U == "Days" ~ "Past Month",
      DUQ220U == "Weeks" ~ "Past Month",
      DUQ220U == "Months" ~ "Not Past Month",
      DUQ220U == "Years" ~ "Not Past Month",
      !is.na(DUQ220U) | DUQ200 == "No" ~ "Not Past Month"
    )
  )

dataset <- dataset |>
  mutate(
    DUQ219 = if_else(
      DUQ200 == "No" | DUQ211 == "No",
      "0 per day", # represent anyone who never smoked regularly as having smoked 0 per day
      DUQ219
    )
  )
rm(marijuana)


tobacco = nhanes("SMQRTU_J") |>
  select(
    SEQN,
    SMDANY #Used any tobacco product in last 5 days
  )
dataset = inner_join(dataset, tobacco, by = "SEQN")
rm(tobacco)

occupation = nhanes("OCQ_J") |>
  select(
    SEQN,
    OCQ210, #Usually work 35 or more hrs per week
    OCQ670 #Overall work schedule past 3 months
  )
dataset = inner_join(dataset, occupation, by = "SEQN")
rm(occupation)

mental = nhanes("DLQ_J") |>
  select(
    SEQN,
    DLQ140, #How often do you feel depressed?
    DLQ100 #How often do you feel worried or anxious?
  )
dataset = inner_join(dataset, mental, by = "SEQN")
rm(mental)

#Summarize Missing Data
if (!require("naniar", quietly = TRUE)) {
  install.packages("naniar")
}
library(naniar)

#Built in function to summarize frequencies and percentages of missing data
missing_var_totals = dataset %>%
  miss_var_summary()

#Prevent problems saving data as csv
missing_var_totals = missing_var_totals %>%
  mutate(pct_miss = as.numeric(pct_miss))

colnames(dataset)

dataset = dataset |>
  mutate(across(
    where(~ is.character(.x) || is.factor(.x)),
    ~ replace(.x, as.character(.x) %in% c("Don't know", "Refused"), NA)))
dataset = dataset |>
  mutate(PAD680 = replace(PAD680, PAD680 %in% c(9999), NA),
         ALQ130 = replace(ALQ130, ALQ130 %in% c(999), NA),)


write.csv(dataset, "data/unweighted_dataset.csv", row.names = FALSE)
write.csv(
  as.data.frame(missing_var_totals),
  "data/missing_data_totals.csv",
  row.names = FALSE
)

# Prep for Survey Weights Object and Dataset ----

covariates <- list(
  nhanes("SLQ_J") |>  select(SEQN, SLD012),
  nhanes("PAQ_J") |>  select(SEQN, PAD680),
  nhanes("ALQ_J") |>  select(SEQN, ALQ111, ALQ121, ALQ130),
  nhanes("DUQ_J") |> select(SEQN, DUQ200, DUQ211, DUQ219, DUQ220U),
  nhanes("SMQRTU_J") |>  select(SEQN, SMDANY),
  nhanes("OCQ_J") |> select(SEQN, OCQ210, OCQ670),
  nhanes("DLQ_J") |> select(SEQN, DLQ140, DLQ100)
)

# I need to keep the NA's for the survey design object, so using left join
# additionally all of the filtering needs to be a little different to not drop
# people from the full DEMO dataset pre-emptively. Also reduce of a list into left_join is kind of an awesome way of
full <- reduce(covariates, left_join, by = "SEQN", .init = demo)
sum(!complete.cases(full))


# we can uniformly say we are going to refactor every character vector as a factor
# and then make every Don't Know or Refused into an NA
full <- full |>
  mutate(across(
    where(~ is.character(.x) || is.factor(.x)),
    ~ replace(.x, as.character(.x) %in% c("Don't know", "Refused"), NA)
  ))

# actually no change in the number of complete cases
sum(!complete.cases(full))

# From above we chose to similarly recode the 7777 and 9999 as NAs, and then the other changes to the variables
# some of this will hurt the numer of complete cases some will help
full <- full |>
  mutate(
    PAD680 = replace(PAD680, PAD680 %in% c(9999), NA),
    ALQ130 = replace(ALQ130, ALQ130 %in% c(999), NA),
    ALQ130 = if_else(
      ALQ111 == "No" | ALQ121 == "Never in the last year",
      0,
      ALQ130
    ),
    DUQ220U = case_when(
      DUQ220U %in% c("Days", "Weeks")   ~ "Past Month",
      DUQ220U %in% c("Months", "Years") ~ "Not Past Month",
      DUQ200 == "No"                    ~ "Not Past Month" # specifically here we can't turn the NA's into "Not Past Month"
    ),
    DUQ219 = if_else(
      DUQ200 == "No" | DUQ211 == "No",
      "0 per day",
      as.character(DUQ219)
    )
  ) |>
  select(-ALQ111, -ALQ121)

# we actually gain a couple hundred complete cases with the recoding
sum(!complete.cases(full))

# Kept all of the covariates we had been looking at above, so then we can only
# filter on a subset of those used in the analysis. DO WE WANT any physical activity variable
analysis_vars <- c(
  "SLD012", "RIAGENDR", "RIDAGEYR", "RIDRETH3", "INDFMPIR", "PAD680",
  "ALQ130", "DUQ219", "DUQ220U", "DLQ100", "DLQ140"
)

# The easiest integration for the survey design object is to subset, so this sets
# up logical vectors to subset by in the subset step
full <- full |>
  mutate(
    in_age_range = RIDAGEYR >= 20 & RIDAGEYR <= 59,
    # we want only the analysis variables and to make sure they have no NA's
    complete     = if_all(all_of(analysis_vars), ~ !is.na(.x)),
    in_analysis  = in_age_range & complete & WTMEC2YR > 0 # need valid weights
  )

# okay but when we reduce the number of covariates to only the ones we analyze we
# have N=2250
sum(full$in_analysis)

NA_count <- full |>
  summarize(across(everything(), ~ sum(is.na(.x))) )


# full design
full_design <- full |>
  as_survey_design(
    ids     = SDMVPSU,
    strata  = SDMVSTRA,
    weights = WTMEC2YR,
    nest    = TRUE
  )

# We can just filter the design like a dplyr argument thanks to srvyr
analytic_design <- full_design |>
  srvyr::filter(in_analysis)

saveRDS(analytic_design, "data/analytic_design.rds")
write.csv(full, "data/full_data.csv", row.names = FALSE)

