
library(readstata13)

dat = readstata13::read.dta13("/home/jeremy/Downloads/bias_replication/justice_analysis.dta")


dat.new <- dat %>%
  group_by(state_code, year) %>%
  summarise(
    #generating acquittal counts and rates for females
    total_cases_women_def = sum(def_name_female == 1),
    female_def_acquitted = sum(def_name_female == 1 & disp_name == 4),
    female_def_aquittal_rate = (female_def_acquitted/total_cases_women_def),
    total_cases_women_pet = sum(pet_name_female ==1),
    female_pet_acquitted = sum(pet_name_female == 1 & disp_name == 4),
    female_pet_aquittal_rate = (female_pet_acquitted/total_cases_women_pet),
    #generating acquittal counts and rates for males
    total_cases_male_def = sum(def_name_female == 0),
    male_def_acquitted = sum(def_name_female == 0 & disp_name == 4),
    male_def_aquittal_rate = (male_def_acquitted/total_cases_male_def),
    total_cases_male_pet = sum(pet_name_female == 0),
    male_pet_acquitted = sum(pet_name_female == 0 & disp_name == 4),
    male_pet_aquittal_rate = (male_pet_acquitted/total_cases_male_pet),
    #generating dismissal counts and rates for females
    female_def_dismissed = sum(def_name_female == 1 & disp_name == 22),
    female_def_dismissal_rate = (female_def_dismissed/total_cases_women_def),
    female_pet_dismissed = sum(pet_name_female == 1 & disp_name == 22),
    female_pet_dismissal_rate = (female_pet_dismissed/total_cases_women_pet),
    #generating dismissal counts and rates for males
    male_def_dismissed = sum(def_name_female == 0 & disp_name == 22),
    male_def_dismissal_rate = (male_def_dismissed/total_cases_male_def),
    male_pet_dismissed = sum(pet_name_female == 0 & disp_name == 22),
    male_pet_dismissal_rate = (male_pet_dismissed/total_cases_male_pet),
    #generating conviction counts and rates for females
    female_def_convicted= sum(def_name_female == 1 & disp_name == 19),
    female_def_convicted_rate = (female_def_convicted/total_cases_women_def),
    female_pet_convicted = sum(pet_name_female == 1 & disp_name == 19),
    female_pet_convicted_rate = (female_pet_convicted/total_cases_women_pet),
    #generating conviction counts and rates for males
    male_def_convicted = sum(def_name_female == 0 & disp_name == 19),
    male_def_convicted_rate = (male_def_convicted/total_cases_male_def),
    male_pet_convicted = sum(pet_name_female == 0 & disp_name == 19),
    male_pet_convicted_rate = (male_pet_convicted/total_cases_male_pet))


