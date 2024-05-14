# Grading formula for PSCI 3200
library(tidyverse)

## Read-in data
dat = readxl::read_xlsx("C:/Users/jerem/Dropbox/Course/grades/ClassList.xlsx")

## Create assignment groups by column

# quizzes and workshops (.025 each)
qw = c("01_quarto", "w1_quarto", "02_gitrepo", "03_fprqd", "fp_essentials", "Quiz_1", "fp_essentials2", "quiz")

# data assignments (.12 each)
da = c("aau_survey", "04_fpdesign", "05_fptest")

# final project (.44)
fp = c("final_project")


dat <- dat %>%
  mutate(across(qw, ~ .x * 2.5),    # Multiply columns in 'qw' by 0.025
         across(da, ~ .x * 0.12),     # Multiply columns in 'da' by 0.12
         final_project = final_project * 0.44,  # Multiply 'final_project' by 0.44
         grade = rowSums(across(c(qw, da, fp))))  # Calculate row sums across selected columns

write.csv("C:/Users/jerem/Dropbox/Course/grades/final_grades.csv")
