library(ggplot2)
library(forcats)

dataset = read.csv('./data/unweighted_dataset.csv')

colnames(dataset)

gender_plot = ggplot(dataset, aes(RIAGENDR, fill = RIAGENDR)) +
  geom_bar() +
  scale_fill_manual(
    values = c(
      "indianred",
      "cornflowerblue"
    )
  ) +
  labs(
    title = "Gender Frequency in 2017-18 NHANES Dataset",
    fill = "Gender",
    x = "Gender",
    y = "Total # of Individuals"
  ) +
  theme_classic()


age_plot = ggplot(dataset, aes(RIDAGEYR)) +
  geom_histogram(binwidth = 5, fill = "indianred", color = "black") +
  labs(
    title = "Age Frequency in 2017-18 NHANES Dataset",
    x = "Age (years)",
    y = "Total # of Individuals"
  ) +
  theme_classic()

re_plot = ggplot(dataset, aes(fct_infreq(RIDRETH3), fill = RIDRETH3)) +
  geom_bar(fill = "indianred", color = "black") +
  labs(
    title = "Race/Ethnicity Frequency in 2017-18 NHANES Dataset",
    x = "Race/Ethnicity",
    y = "Total # of Individuals"
  ) +
  scale_x_discrete(
    labels = c(
      "Mexican American" = "Mex. Am.",
      "Non-Hispanic Asian" = "NH Asian",
      "Non-Hispanic Black" = "NH Black",
      "Non-Hispanic White" = "NH White",
      "Other Hispanic" = "Other Hisp.",
      "Other Race - Including Multi-Racial" = "Other"
    )
  ) +
  theme_classic()
re_plot

library(forcats)
educ_plot = ggplot(dataset, aes(fct_infreq(DMDEDUC2))) +
  geom_bar(fill = "indianred", color = "black") +
  labs(
    title = "Education Frequency in 2017-18 NHANES Dataset",
    x = "Education Status",
    y = "Total # of Individuals"
  ) +
  scale_x_discrete(
    labels = c(
      "9-11th grade (Includes 12th grade with no diploma)" = "Some HS",
      "College graduate or above" = "College Grad.",
      "High school graduate/GED or equivalent" = "HS Grad.",
      "Less than 9th grade" = "No HS",
      "Some college or AA degree" = "Some College"
    )
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
educ_plot

table(dataset$INDFMPIR)
income_plot = ggplot(dataset, aes(INDFMPIR)) +
  geom_histogram(binwidth = 0.25, fill = "indianred", color = "black") +
  labs(
    title = "Ratio of Family Income to Poverty Line",
    x = "Income/Poverty Ratio",
    y = "Total # of Individuals"
  ) +
  scale_x_continuous(
    breaks = seq(0, 5, by = 0.5),
    labels = c(as.character(seq(0, 4.5, by = 0.5)), "5+")
  ) +
  geom_vline(
    xintercept = 1.0,
    linetype = "dashed",
    color = "black"
  ) +
  annotate(
    "text",
    x = 1.0,
    y = Inf,
    label = "Poverty Line",
    vjust = 1.5,
    hjust = -0.1,
    size = 4
  ) +
  theme_classic()
income_plot

sleep_plot = ggplot(dataset, aes(SLD012)) +
  geom_histogram(binwidth = 1, fill = "indianred", color = "black") +
  labs(
    title = "Workday Sleep Hours of Individuals in 2017-18 NHANES Dataset",
    x = "Number of Hours Usually Slept on Workdays",
    y = "Total # of Individuals"
  ) +
  scale_x_continuous(breaks = seq(0, 14, by = 2)) +
  theme_classic()
sleep_plot

dataset = dataset %>%
  mutate(
    PAD680 = na_if(PAD680, 9999)
  )

sedentary_plot = ggplot(dataset, aes(PAD680)) +
  geom_histogram(binwidth = 120, fill = "indianred", color = "black") +
  labs(
    title = "Sedentary Minutes per Day of Individuals in 2017-18 NHANES Dataset",
    x = "Sedentary Minutes per Day",
    y = "Total # of Individuals"
  ) +
  scale_x_continuous(breaks = seq(0, 1440, by = 120)) +
  theme_classic()
sedentary_plot


dataset = dataset %>%
  mutate(
    ALQ130 = replace(ALQ130, ALQ130 %in% c(777, 999), NA)
  )

alc_plot = ggplot(dataset, aes(x = ALQ130)) +
  geom_histogram(
    binwidth = 1,
    boundary = 0.5,
    fill = "indianred",
    color = "black"
  ) +
  labs(
    title = "Avg. Number of Alcoholic Drinks/day in Last Year in 2017-18 NHANES Dataset",
    x = "Avg. # of Alcoholic Drinks/day in Last Year",
    y = "Total # of Individuals"
  ) +
  scale_x_continuous(
    breaks = 1:15,
    labels = c(as.character(1:14), "15+")
  ) +
  theme_classic()
alc_plot

joint_plot = ggplot(
  dataset %>%
    dplyr::filter(!is.na(DUQ219)) %>%
    mutate(
      DUQ219 = factor(
        DUQ219,
        levels = c(
          "1 per day",
          "2 per day",
          "3-5 per day",
          "Six or more per day"
        )
      )
    ),
  aes(DUQ219)
) +
  geom_bar(fill = "indianred", color = "black") +
  labs(
    title = "Number of Joints Smoked per Day by Individuals in 2017-18 NHANES Dataset",
    x = "Joints/Day",
    y = "Total # of Individuals"
  ) +
  scale_x_discrete(
    labels = c(
      "1 per day" = "1",
      "2 per day" = "2",
      "3-5 per day" = "3-5",
      "Six or more per day" = "6+"
    )
  ) +
  theme_classic()
joint_plot


table(dataset$DUQ220U)
recent_weed_plot = ggplot(dataset, aes(fct_infreq(DUQ220U), fill = DUQ220U)) +
  geom_bar() +
  scale_fill_manual(
    values = c(
      "gray",
      "darkgreen"
    )
  ) +
  labs(
    title = "Last Time Individuals Used Marijuana in 2017-18 NHANES Dataset",
    fill = "Last Time Using Marijuana",
    x = "Last Time Using Marijuana",
    y = "Total # of Individuals"
  ) +
  theme_classic()
recent_weed_plot

tobacco_plot = ggplot(dataset, aes(fct_infreq(SMDANY))) +
  geom_bar(fill = "indianred", color = "black") +
  labs(
    title = "Last Time Individuals Used Tobacco in 2017-18 NHANES Dataset",
    fill = "Last Time Using Tobacco",
    x = "Last Time Using Tobacco",
    y = "Total # of Individuals"
  ) +
  scale_x_discrete(
    labels = c(
      "Yes" = "Past 5 Days",
      "No" = "Not In Past 5 Days"
    )
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
tobacco_plot
colnames(dataset)

long_work_plot = ggplot(
  dataset %>% dplyr::filter(!is.na(OCQ210)),
  aes(fct_infreq(OCQ210), fill = OCQ210)
) +
  geom_bar() +
  scale_fill_manual(
    values = c(
      "gray",
      "indianred"
    )
  ) +
  labs(
    title = "Individuals Often Work 35+ Hours per Week in 2017-18 NHANES Dataset (NA=3741)",
    fill = "Works 35+ Hrs/Week",
    x = "Works 35+ Hrs/Week",
    y = "Total # of Individuals"
  ) +
  theme_classic()
long_work_plot
