library(conflicted)
library(tidyverse)
library(nhanesA)
library(haven)
source("functions.r")

## Grabbing raw data ----
# demographics
demo <- nhanes('DEMO_J') |>
  select(
    SEQN, #identifier
    RIAGENDR,
    RIDAGEYR,
    RIDRETH3,
    DMDEDUC2,
    INDFMPIR, #one of the income variables
    SDMVPSU, # these two are used to make sample weights
    SDMVSTRA,
  )


# sleep
sleep <- nhanes('SLQ_J') |>
  select(SEQN, SLD012, SLD013, SLQ120)

# lab measures
labs <- nhanes('GHB_J') |>
  select(SEQN, LBXGH)

# exam measures
exams <- nhanes('BPX_J') |>
  select(SEQN, BPAARM, BPXSY1, BPXDI1, BPXSY2, BPXDI2)
