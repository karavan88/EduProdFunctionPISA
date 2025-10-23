#-------------------------------------------------------------------
# Project: Education Production Function
# Script: Descriptive Analysis and Correlations
# Author: Garen Avanesian
# Date: 16 September 2024
#-------------------------------------------------------------------

# Upload the data into the memory

model_data = readRDS(file.path(output, "model_data.rds"))
full_data = readRDS(file.path(output, "merged_data.rds"))

# Produce table of sample summary

grades = c("Grade 7", "Grade 8", "Grade 9", "Grade 10", "Grade 11", "Grade 12")

model_data$CNT <- droplevels(model_data$CNT)

summary_table_vars <-
  model_data %>%
  select(AGE, Gender, ESCS, Area, country, Grade, lang_home) %>%
  mutate_at(vars("AGE", "ESCS"), as.numeric) %>%
  mutate(Grade = case_when(Grade %in% grades ~ Grade,
                           TRUE ~ as.character(NA))) %>%
  rename(Age = AGE) %>%
  rename(`Language at Home` = lang_home) %>%
  as.data.frame() 

# table1 <-
#   gtsummary::tbl_summary(summary_table_vars, by = country) %>%
#   bold_labels()

table1 <-
  summary_table_vars %>%
  tbl_summary(
    by = country, # you can change or add more grouping variables
    type = list(
      Age ~ "continuous",
      ESCS ~ "continuous"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  #add_n() %>%
  #add_overall() %>%
  modify_header(label = "Variable") %>%
  bold_labels() 

#datasummary_skim(summary_table_vars, by = "country")

### Calculate the averages by cnt and wealth 
descr_escs <-
  model_data %>%
  group_by(CNT, ESCS_group) %>%
  summarise(reading = mean(reading_std)) %>%
  drop_na() %>%
  pivot_wider(names_from = "ESCS_group", values_from = "reading")

descr_cnt <-
  model_data %>%
  group_by(CNT) %>%
  summarise(Total = mean(reading_std)) %>%
  full_join(descr_escs) %>%
  rename(`Country/Territory` = CNT) 


### Correlation matrix

cor_data <-
  model_data %>%
  select(ESCS, reading_std, task_mastery, CLSIZE,
         STRATIO, PROAT5AM, sch_escs) %>%
  rename(`Reading Score` = reading_std,
         `Task Mastery`  = task_mastery,
         `Class Size`    = CLSIZE,
         `Student-Teacher Ratio` = STRATIO,
         `Qualified Teachers` = PROAT5AM  ,
         `School ESCS (avg)` = sch_escs ) %>%
  drop_na()

corr <- round(cor(cor_data), 1)

p.mat <- cor_pmat(cor_data)

cor_plot <- 
  ggcorrplot(corr, type = "lower", p.mat = p.mat,
           lab = TRUE)

### Descriptive table appendix

desc <-
  model_data %>%
  select(reading_std, task_mastery, CLSIZE, 
         STRATIO, sch_infrastr, PROAT5AM, sch_escs, CNT)



skimdata <-
  desc %>%
  #select(-CNT) %>%
  group_by(CNT) %>%
  rename(`Reading Score` = reading_std,
         `Task Mastery` = task_mastery) %>%
  datasummary_skim()

summary(desc$CNT)

# Assuming 'desc' is your data frame and 'CNT' is the grouping variable
desc_summary <- 
  describeBy(reading_std + task_mastery + CLSIZE + 
         STRATIO + sch_infrastr + PROAT5AM + sch_escs ~ CNT, 
    data = desc, mat = TRUE) %>%
  rownames_to_column() %>%
  select(group1, rowname, mean, sd, median, min, max) %>%
  mutate_if(is.numeric, round, 2) %>%
  arrange(group1) %>%
  rename(`Country/Territory` = group1,
         Variable = rowname) %>%
  mutate(Variable = case_when(str_detect(Variable, "reading_std")  ~ "Reading Score",
                              str_detect(Variable, "task_mastery") ~ "Task Mastery",
                              str_detect(Variable, "CLSIZE")       ~ "Class Size",
                              str_detect(Variable, "STRATIO")      ~ "Student-Teacher Ratio",
                              str_detect(Variable, "sch_infrastr") ~ "Poor School Infrastructire",
                              str_detect(Variable, "PROAT5AM")     ~ "Qualified Teachers (%)",
                              str_detect(Variable, "sch_escs")     ~ "School ESCS (avg)"))

# Automatically wrap long strings in the 'Variable' column
#desc_summary$Variable <- str_wrap(desc_summary$Variable, width = 15)

# View(desc_summary)

### correlation between learning outcomes

learn <-
  full_data %>%
  select(reading, math, science)

cor_panel <-
  pairs.panels(learn, 
             method = "spearman", 
             hist.col = "blue")

cor_panel




### probability density by territory

density <-
  ggplot(model_data, aes(reading_std)) +
  geom_density(fill = "lightblue") +
  theme_minimal() +
  facet_wrap(CNT~.) +
  xlab("Reading Score (Standardized)") +
  ylab("")


