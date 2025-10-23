#-------------------------------------------------------------------
# Project: Education Production Function
# Script: Data Preparation
# Author: Garen Avanesian
# Date: 16 September 2024
#-------------------------------------------------------------------

# SECTION 1: LOAD DATASETS
cat("📂 SECTION 1: DATA LOADING\n")

# pisa_2018_school  <- read_sav(file.path(inputData, "pisa_2018_schools.sav"))
# print("PISA 2018 school data loaded.")
# pisa_2018_student <- read_sav(file.path(inputData, "pisa_2018_students.sav"))
# print("PISA 2018 student data loaded.")

pisa_2018_schools <- readRDS(file.path(inputData, "pisa_2018_schools.rds"))
pisa_2018_students <- readRDS(file.path(inputData, "pisa_2018_students_selected.rds"))
mow_2018_schools <- readRDS(file.path(inputData, "pisa_2018_moscow_schools.rds"))
mow_2018_students <- readRDS(file.path(inputData, "pisa_2018_moscow_students.rds"))

# pisa_2018_students_sel <- 
#   pisa_2018_students %>%
#   filter(CNT %in% country_names)

# # savew rds file
# saveRDS(pisa_2018_students_sel, file.path(inputData, "pisa_2018_students_selected.rds"))

print("PISA 2018 data loaded from RDS files.")
cat("📂 SECTION 1: COMPLETE\n")

# SECTION 2: DATA PREPARATION

# Territorial units to be selected from the data
country_names <- c("Brazil",
                   "B-S-J-Z (China)",
                   "Hong Kong",
                   "Macao",
                   "Moscow City (RUS)",
                   "Moscow Region (RUS)",    
                   "Tatarstan (RUS)", 
                   "Russian Federation"
)

# Vector of variables to join datasets
join_vars = c("CNT", "CNTRYID", "CNTSCHID", "CYC", "NatCen", "STRATUM", "SUBNATIO")

# Vector of student variables to be selected
student_vars = c("CNTSTUID", 	"ST001D01T", "ST003D02T", 
                 "ST003D03T", "ST004D01T", "ST022Q01TA", "ST126Q01TA",
                 "AGE", "DURECEC",  "WORKMAST", "COGFLEX", "ST184Q01HA",
                 "REPEAT", "BSMJ", "TMINS", "ESCS", "W_FSTUWT")

# Vector of school variables to be selected
school_vars = c("SC001Q01TA", "SC013Q01TA",
                "PRIVATESCH", "SCHLTYPE", "STRATIO", "SCHSIZE",
                "RATCMP1", "RATCMP2", #ict
                "PROATCE", "PROAT5AB", "PROAT5AM", "PROAT6", #teacher qualifications 
                "CLSIZE", "CREACTIV", "EDUSHORT", "STAFFSHORT", 
                "SC017Q08NA")

# SECTION 3: DATA MERGING AND VARIABLE CREATION

### Read separate datasets for Moscow City
student_moscow <- 
  mow_2018_students %>%
  select(all_of(join_vars), all_of(student_vars), matches("^PV.*(READ|MATH|SCIE)")) %>%
  mutate(CNT = as_factor(CNT),
         CNTRYID = as_factor(CNTRYID)) 

# View(student_moscow)

school_moscow <-
  mow_2018_schools %>%
  select(all_of(join_vars), all_of(school_vars)) %>%
  mutate(CNT = as_factor(CNT),
         CNTRYID = as_factor(CNTRYID)) 

# Merge student files
student_brc <- 
  pisa_2018_students %>%
  select(all_of(join_vars), all_of(student_vars), matches("^PV.*(READ|MAT|SCIE)")) %>%
  #uses as factor from haven to use value labels
  mutate(CNT = as_factor(CNT),
         CNTRYID = as_factor(CNTRYID)) %>%
  full_join(student_moscow) %>%
  filter(CNT %in% country_names) %>%
  mutate(lang_home = case_when(ST022Q01TA == 1 ~ "1. Language of the test", 
                               ST022Q01TA == 2 ~ "2. Other language",
                               TRUE ~ as.character(NA))) %>%
  mutate(school_age_start = ifelse(ST126Q01TA > 90, as.numeric(NA), ST126Q01TA )) %>%
  mutate(growth_mindset = scale(ST184Q01HA, scale = TRUE, center = TRUE)[, 1],
         task_mastery = scale(WORKMAST, scale = TRUE, center = TRUE)[, 1]) %>%
  # rescale ESCS
  mutate(ESCS = scale(ESCS, scale = TRUE, center = TRUE)[, 1]) %>%
  # perform rowwise operation and take average of all variables that start with PV1
  rowwise() %>%
  # take an average of rows thaty contain the word READ
  mutate(reading = mean(c_across(contains("READ")), na.rm = TRUE) ) %>%
  mutate(math = mean(c_across(contains("MATH")), na.rm = TRUE) ) %>%
  mutate(science = mean(c_across(contains("SCIE")), na.rm = TRUE) ) %>%
  mutate(learning = mean(reading, math, science, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(CNT) %>%
  # we need to divide the ESCS variable on 3 groups, bottom 40, middle 50 and top 10
  mutate(ESCS_group = cut(ESCS, breaks = quantile(ESCS, probs = c(0, 0.4, 0.9, 1), na.rm = TRUE),
                          labels = c("Bottom 40%", "Middle 50%", "Top 10%"))) %>%
  ungroup()

# student_brc %>%
#   count(CNT)

# Merge school files
schools_brc <-
  pisa_2018_schools %>%
  mutate(CNT = as_factor(CNT),
         CNTRYID = as_factor(CNTRYID)) %>%
  filter(CNT %in% country_names) %>%
  select(all_of(join_vars), all_of(school_vars)) %>%
  full_join(school_moscow) %>%
  mutate(sch_private = case_when(PRIVATESCH == "private" ~ 1, TRUE ~ 0))
  

# schools_brc %>%
#   count(CNT)

# summary(factor(schools_brc$PRIVATESCH))

# to set as the levels order
grades = c("Grade 7", "Grade 8", "Grade 9", "Grade 10", "Grade 11", "Grade 12")
residence = c("1. Village/Small Town", "2. Town", "3. City/Large City")

# now we need to merge data 
brc <-
  student_brc %>%
  left_join(schools_brc) %>%
  mutate(country = case_when(
    CNT == "Brazil" ~ "Brazil",
    CNT == "B-S-J-Z (China)" ~ "BSJZ",
    CNT == "Hong Kong" ~ "Hong Kong",
    CNT == "Macao" ~ "Macao (China)",
    CNT == "Moscow City (RUS)" ~ "Moscow",
    CNT == "Moscow Region (RUS)" ~ "Moscow Region",
    CNT == "Tatarstan (RUS)" ~ "Tatarstan",
    CNT == "Russian Federation" ~ "Russia"
  )) %>%
  mutate(SC001Q01TA = case_when(SC001Q01TA %in% c(4,5) ~ "3. City/Large City",
                                SC001Q01TA %in% c(1,2) ~ "1. Village/Small Town",
                                SC001Q01TA == 3 ~ "2. Town",
                                TRUE ~ as.character(NA))) %>%
  mutate(ST004D01T = case_when(ST004D01T == 1 ~ "Female",
                               ST004D01T == 2 ~ "Male",
                               TRUE ~ as.character(NA))) %>%
  mutate_at(vars("AGE", "ESCS"), as.numeric) %>%
  mutate(Area = haven::as_factor(SC001Q01TA),
         Grade = haven::as_factor(ST001D01T),
         Gender = haven::as_factor(ST004D01T)) %>%
  mutate(Grade = factor(Grade, levels = grades),
         Area = factor(Area, levels = residence))

brc %>%
  count(CNT)



# Now we need to create a dataset for the model (without NAs)
model_data <-
  brc %>%
  select(
    # student variables
    CNTSTUID, AGE, ESCS,  Grade, Gender,  reading, task_mastery, ESCS_group, #REPEAT,
    lang_home, learning, #school_age_start, # learning,
    # school variables
    CNTSCHID, Area, sch_private, CLSIZE, STRATIO, PROAT5AM, RATCMP2, CREACTIV, starts_with("SC017"), #STAFFSHORT, #EDUSHORT, 
    CNT, CNTRYID, country
  ) %>%
  # from grade variable, export a number from the string and convert into numeric
  mutate(Grade1 = as.numeric(str_extract(Grade, "\\d+"))) %>%
  mutate(reading_std = scale(reading, center = TRUE, scale = TRUE)[, 1]) %>%
  mutate(learning_std = scale(learning, center = TRUE, scale = TRUE)[, 1]) %>%
  # create a physical infrastructure binary 
  mutate(sch_infrastr = ifelse(SC017Q08NA %in% c(4), 1, 0)) %>%
  # mutate(pc_internet = as_factor(RATCMP2)) %>%
  # calculate peer effects 
  group_by(CNT, CNTSCHID) %>%
  mutate(sch_learning = mean(learning_std),
         sch_reading = mean(reading_std),
         sch_escs = mean(ESCS)) %>%
  ungroup() # %>%
  # drop_na() 


model_data %>% group_by(CNT) %>% summarise(mean = mean(sch_infrastr, na.rm = TRUE), min = min(sch_infrastr, na.rm = TRUE), max = max(sch_infrastr, na.rm = TRUE))

model_data %>% group_by(CNT) %>% summarise(mean = mean(PROAT5AM, na.rm = TRUE), 
                                           min = min(sch_infrastr, na.rm = TRUE), 
                                           max = max(sch_infrastr, na.rm = TRUE),
                                           sd = sd(PROAT5AM, na.rm = TRUE))

model_data %>%
  count(CNT)

# View(model_data)

# write the dataset for the study in the repository
saveRDS(brc, file.path(output, "merged_data.rds"))
saveRDS(model_data, file.path(output, "model_data.rds"))

cat("📂 SECTION 2: DATA PREPARATION COMPLETE\n")