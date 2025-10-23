#-------------------------------------------------------------------
# Project: Education Production Function
# Script: Data Preparation
# Author: Garen Avanesian
# Date: 16 September 2024
#-------------------------------------------------------------------

cat("\n", rep("=", 70), "\n")
cat("🎯 EDUCATION PRODUCTION FUNCTION - DATA PREPARATION\n")
cat("📅 Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 70), "\n\n")

# SECTION 1: LOAD DATASETS
cat("📂 SECTION 1: DATA LOADING\n")
cat("Loading PISA 2018 datasets from RDS files...\n\n")

# Load main datasets
pisa_2018_schools <- readRDS(file.path(inputData, "pisa_2018_schools.rds"))
pisa_2018_students <- readRDS(file.path(inputData, "pisa_2018_students_selected.rds"))
mow_2018_schools <- readRDS(file.path(inputData, "pisa_2018_moscow_schools.rds"))
mow_2018_students <- readRDS(file.path(inputData, "pisa_2018_moscow_students.rds"))

cat("\n   📋 Data loading summary:\n")
cat("      • Total global schools:", nrow(pisa_2018_schools), "\n")
cat("      • Total global students:", nrow(pisa_2018_students), "\n")
cat("      • Moscow schools:", nrow(mow_2018_schools), "\n")
cat("      • Moscow students:", nrow(mow_2018_students), "\n")

cat("\n✅ SECTION 1: DATA LOADING COMPLETE\n\n")

# SECTION 2: CONFIGURATION & VARIABLE SELECTION
cat("⚙️  SECTION 2: CONFIGURATION & VARIABLE SELECTION\n")

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

cat("\n✅ SECTION 2: CONFIGURATION COMPLETE\n\n")

# SECTION 3: DATA MERGING AND VARIABLE CREATION
cat("🔄 SECTION 3: DATA PROCESSING & TRANSFORMATION\n")

### Process Moscow datasets first

student_moscow <- 
  mow_2018_students %>%
  select(all_of(join_vars), all_of(student_vars), matches("^PV.*(READ|MATH|SCIE)")) %>%
  mutate(CNT = as_factor(CNT),
         CNTRYID = as_factor(CNTRYID)) 

school_moscow <-
  mow_2018_schools %>%
  select(all_of(join_vars), all_of(school_vars)) %>%
  mutate(CNT = as_factor(CNT),
         CNTRYID = as_factor(CNTRYID)) 


# Process and merge student files
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

cat("      ✅ Student data merged and processed -", nrow(student_brc), "students\n")
cat("      📊 Countries in student dataset:\n")

student_counts <- student_brc %>% count(CNT, sort = TRUE)
for(i in 1:nrow(student_counts)) {
  cat("         •", student_counts$CNT[i], "-", student_counts$n[i], "students\n")
}

# Process and merge school files
cat("\n   🏫 Processing and merging school datasets...\n")
schools_brc <-
  pisa_2018_schools %>%
  mutate(CNT = as_factor(CNT),
         CNTRYID = as_factor(CNTRYID)) %>%
  filter(CNT %in% country_names) %>%
  select(all_of(join_vars), all_of(school_vars)) %>%
  full_join(school_moscow) %>%
  mutate(sch_private = case_when(PRIVATESCH == "private" ~ 1, TRUE ~ 0))

cat("      ✅ School data merged and processed -", nrow(schools_brc), "schools\n")
cat("      🏫 Countries in school dataset:\n")
school_counts <- schools_brc %>% count(CNT, sort = TRUE)
for(i in 1:nrow(school_counts)) {
  cat("         •", school_counts$CNT[i], "-", school_counts$n[i], "schools\n")
}

# Final data merging and variable creation
cat("\n   🔗 Creating final merged dataset...\n")

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

cat("      ✅ Final dataset created -", nrow(brc), "student-school observations\n")
cat("      📈 Variables in final dataset:", ncol(brc), "\n")

final_counts <- brc %>% count(CNT, sort = TRUE)
cat("      🌍 Final country distribution:\n")
for(i in 1:nrow(final_counts)) {
  cat("         •", final_counts$CNT[i], "-", final_counts$n[i], "observations\n")
}

cat("\n✅ SECTION 3: DATA PROCESSING COMPLETE\n\n")

# SECTION 4: MODEL DATASET CREATION
cat("🎯 SECTION 4: MODEL DATASET PREPARATION\n")
cat("   📋 Creating analysis-ready dataset with key variables...\n")

# Now we need to create a dataset for the model (without NAs)
model_data <-
  brc %>%
  select(
    # student variables
    CNTSTUID, AGE, ESCS,  Grade, Gender,  
    reading, math, science,
    task_mastery, ESCS_group, #REPEAT,
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

cat("      ✅ Model dataset created -", nrow(model_data), "observations\n")
cat("      🎯 Analysis variables:", ncol(model_data), "\n")

# Data quality checks
cat("\n   🔍 Data quality assessment:\n")
infrastructure_summary <- 
  model_data %>% 
  group_by(CNT) %>% 
  summarise(
    mean_infrastr = mean(sch_infrastr, na.rm = TRUE),
    min_infrastr = min(sch_infrastr, na.rm = TRUE),
    max_infrastr = max(sch_infrastr, na.rm = TRUE),
    .groups = 'drop'
  )

cat("      📊 School infrastructure by country:\n")
for(i in 1:nrow(infrastructure_summary)) {
  cat("         •", infrastructure_summary$CNT[i], 
      "- Mean:", round(infrastructure_summary$mean_infrastr[i], 3), "\n")
}

teacher_summary <- 
  model_data %>% 
  group_by(CNT) %>% 
  summarise(
    mean_teacher = mean(PROAT5AM, na.rm = TRUE),
    sd_teacher = sd(PROAT5AM, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\n      👨‍🏫 Teacher qualifications by country:\n")
for(i in 1:nrow(teacher_summary)) {
  cat("         •", teacher_summary$CNT[i], 
      "- Mean:", round(teacher_summary$mean_teacher[i], 3),
      "(SD:", round(teacher_summary$sd_teacher[i], 3), ")\n")
}

model_counts <- model_data %>% count(CNT, sort = TRUE)
cat("\n      📈 Model dataset country distribution:\n")
for(i in 1:nrow(model_counts)) {
  cat("         •", model_counts$CNT[i], "-", model_counts$n[i], "observations\n")
}

cat("\n✅ SECTION 4: MODEL DATASET COMPLETE\n\n")

# SECTION 5: DATA EXPORT
cat("💾 SECTION 5: DATA EXPORT\n")
cat("   📁 Saving processed datasets to output directory...\n")


saveRDS(model_data, file.path(inputData, "model_data.rds"))
cat("      ✅ Model dataset saved:", file.path(output, "model_data.rds"), "\n")
cat("         • Observations:", nrow(model_data), "\n")
cat("         • Variables:", ncol(model_data), "\n")

cat("\n✅ SECTION 5: DATA EXPORT COMPLETE\n\n")

cat(rep("=", 70), "\n")
cat("🎉 DATA PREPARATION SUCCESSFULLY COMPLETED!\n")
cat("📅 Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("📊 Final outputs:\n")
cat("   • Full dataset:", nrow(brc), "observations across", length(unique(brc$CNT)), "countries/regions\n")
cat("   • Model dataset:", nrow(model_data), "analysis-ready observations\n")
cat("   • Datasets saved to:", inputData, "\n")
cat(rep("=", 70), "\n\n")