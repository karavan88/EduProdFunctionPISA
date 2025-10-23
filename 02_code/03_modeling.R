#-------------------------------------------------------------------#
# Project: Education Production Function
# Script: Regression Modeling
# Author: Garen Avanesian
# Date: 16 September 2024
#-------------------------------------------------------------------#

cat("\n🎯 MULTILEVEL REGRESSION MODELING\n")
cat("=================================\n")

# Upload the data into the memory
cat("📊 Loading and preparing modeling data...\n")

model_data = 
  readRDS(file.path(inputData, "model_data.rds")) %>%
  mutate(CNT = droplevels(factor(CNT))) %>%
  mutate(age_group = as.factor(AGE)) %>%
  # we need to group by CNT and divide ESCS for school by three groups - bottom 40%, middle 50%, top 10%
  group_by(CNT) %>%
  mutate(ESCS_sch_group = case_when(
    sch_escs <= quantile(sch_escs, 0.4, na.rm = TRUE) ~ "Bottom 40%",
    sch_escs > quantile(sch_escs, 0.4, na.rm = TRUE) & sch_escs <= quantile(sch_escs, 0.9, na.rm = TRUE) ~ "Middle 50%",
    sch_escs > quantile(sch_escs, 0.9, na.rm = TRUE) ~ "Top 10%",
    TRUE ~ as.character(NA)
  )) %>%
  ungroup() %>%
  mutate(ESCS_sch_group = factor(ESCS_sch_group, levels = c("Bottom 40%", "Middle 50%", "Top 10%")))  

cat("   ✅ Data prepared:", nrow(model_data), "observations\n")
cat("   ✅ Countries:", length(unique(model_data$CNT)), "\n")
cat("   ✅ Schools:", length(unique(model_data$CNTSCHID)), "\n\n")  

# Baseline model but for each country (CNT) separately - use map or loop or apply

cat("🏁 BASELINE MODEL: NULL MODEL WITH RANDOM INTERCEPT\n")
#------ BASELINE MODEL: NULL MODEL WITH RANDOM INTERCEPT FOR SCHOOL ------ ####

# Split data by country
cnt_baseline_model <- 
  model_data %>%
  mutate(CNT = droplevels(factor(CNT)))  %>% # Drop empty factor levels
  split(.$CNT) %>%
  # group_by(CNT) %>%
  # group_split() %>%
  #set_names(~ unique(.x$CNT)) %>%  # Name list elements by country
  map(~ lmer(reading_std ~ 
               (1 | CNTSCHID), data = .x))

# Check the ICC for each country
icc(cnt_baseline_model$Brazil, by_group = TRUE)
icc(cnt_baseline_model$`Russian Federation`, by_group = TRUE)
icc(cnt_baseline_model$`B-S-J-Z (China)`, by_group = TRUE)
icc(cnt_baseline_model$HongKong, by_group = TRUE)
icc(cnt_baseline_model$Macao, by_group = TRUE)
icc(cnt_baseline_model$`Moscow City (RUS)`, by_group = TRUE)
icc(cnt_baseline_model$`Moscow Region (RUS)`, by_group = TRUE)
icc(cnt_baseline_model$Tatarstan, by_group = TRUE)

### create one database with all ICCs
icc_results <- 
  imap_dfr(cnt_baseline_model, ~ {
  icc_value <- icc(.x, by_group = TRUE)$ICC[1]  # Extract ICC value
  data.frame(country = .y, ICC = icc_value)
})

# View(icc_results)

cat("   ✅ Baseline models fitted for", length(unique(icc_results$country)), "countries\n")
cat("   ✅ ICC analysis completed\n\n")

# create a barchart
icc_plot <- 
  ggplot(icc_results, aes(x = fct_reorder(country, ICC), y = ICC)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "",
       y = "Between-School Variance in Reading Score (ICC)") +
  theme_minimal()


cat("📊 MODEL 1: FIXED EFFECTS WITH RANDOM INTERCEPT\n")
#------ MODEL 1: FIXED EFFECTS WITH RANDOM INTERCEPT FOR SCHOOL ------ #####

#create regression formulas for each country (RUS should exclude private school variable)
rus_formula <- formula(reading_std ~ 
                         Grade1 +
                         # student level characteristics
                         Gender + ESCS_group +  lang_home + 
                         # school level characteristics
                         Area + CLSIZE + I(CLSIZE^2) +
                         STRATIO + 
                         PROAT5AM + sch_infrastr + 
                         # personality traits
                         task_mastery + 
                         # peer effects
                         sch_escs + 
                         # isolate the effects of age, grade, and school by taking random intercept
                         (1 | age_group) + (1 | CNTSCHID))

mow_formula <- formula(reading_std ~ 
                         Grade1 +
                         # student level characteristics
                         Gender + ESCS_group +  lang_home + 
                         # school level characteristics
                         CLSIZE + I(CLSIZE^2) +
                         STRATIO + 
                         PROAT5AM + sch_infrastr + 
                         # personality traits
                         task_mastery + 
                         # peer effects
                         sch_escs + 
                         # isolate the effects of age, grade, and school by taking random intercept
                         (1 | age_group) + (1 | CNTSCHID))

bsjz_hkg_formula <- formula(reading_std ~ 
                              Grade1 +
                              # student level characteristics
                              Gender + ESCS_group +  lang_home + 
                              # school level characteristics
                              CLSIZE + I(CLSIZE^2) +
                              STRATIO + 
                              PROAT5AM + sch_private + sch_infrastr + 
                              # personality traits
                              task_mastery + 
                              # peer effects
                              sch_escs + 
                              # isolate the effects of age, grade, and school by taking random intercept
                              (1 | age_group) + (1 | CNTSCHID))

macao_formula <- formula(reading_std ~ 
                           Grade1 +
                           # student level characteristics
                           Gender + ESCS_group +  lang_home + 
                           # school level characteristics
                           # Area + 
                           CLSIZE + I(CLSIZE^2) + 
                           sch_private + sch_infrastr + 
                           # personality traits
                           task_mastery + 
                           # peer effects
                           sch_escs + 
                           # isolate the effects of age, grade, and school by taking random intercept
                           (1 | age_group) + (1 | CNTSCHID)) 

bra_formula  <- formula(reading_std ~ 
                          Grade1 +
                          # student level characteristics
                          Gender + ESCS_group +  lang_home + 
                          # school level characteristics
                          Area + CLSIZE + I(CLSIZE^2) + 
                          STRATIO + 
                          PROAT5AM + sch_private + sch_infrastr + 
                          # personality traits
                          task_mastery + 
                          # peer effects
                          sch_escs + 
                          # isolate the effects of age, grade, and school by taking random intercept
                          (1 | age_group) + (1 | CNTSCHID))


m1_rus <- lmer(formula = rus_formula,
           data = model_data[model_data$CNT == "Russian Federation", ])

m1_mow <- lmer(formula = mow_formula,
           data = model_data[model_data$CNT == "Moscow City (RUS)", ])

m1_tat <- lmer(formula = rus_formula,
           data = model_data[model_data$CNT == "Tatarstan (RUS)", ])

m1_mow_reg <- lmer(formula = rus_formula,
           data = model_data[model_data$CNT == "Moscow Region (RUS)", ])

m1_bsjz <- lmer(formula = bsjz_hkg_formula,
            data = model_data[model_data$CNT == "B-S-J-Z (China)", ])

m1_hkg <- lmer(formula = bsjz_hkg_formula,
           data = model_data[model_data$CNT == "Hong Kong", ])

m1_macao <- lmer(formula = macao_formula,
              data = model_data[model_data$CNT == "Macao", ])

m1_bra <- lmer(formula = bra_formula,
           data = model_data[model_data$CNT == "Brazil", ])


# table of sch_private by CNT
table(model_data$Area, model_data$CNT)

# put all models into a list
cnt_m1 <- list("Russia" = m1_rus,
               "Moscow City" = m1_mow,
               "Moscow Region" = m1_mow_reg,
               "Tatarstan" = m1_tat,
               "BSJZ" = m1_bsjz,
               "Hong Kong" = m1_hkg,
               "Macao" = m1_macao,
               "Brazil" = m1_bra)

rename_vector <- c(
  "Grade1"                    = "School year",
  "GenderMale"                = "Sex: Male",
  "ESCS_groupMiddle 50%"      = "SES: Middle 50%",
  "ESCS_groupTop 10%"         = "SES: Top 10%",
  "lang_home2. Other language"= "Language minority",
  "Area2. Town"               = "Area: Town",
  "Area3. City/Large City"    = "Area: City/Large City",
  "CLSIZE"                    = "Class size",
  "I(CLSIZE^2)"               = "Class size (squared)",
  "STRATIO"                   = "Student–teacher ratio",
  "sch_infrastr"              = "Poor School Infrastructure",
  "PROAT5AM"                  = "Teachers with Master's (%)",
  "task_mastery"              = "Task performance",
  "sch_escs"                  = "Average school SES",
  "sch_private"               = "Private school"
)

# check the summary of one of the models
reg_tables_m1 <-
  modelsummary(cnt_m1,
               statistic = "({std.error}) {stars}",
               gof_omit = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector,
               notes   = "  ",
               output = "tinytable") 





# Extract EYOS and significance from all models
eyos_results <- imap_dfr(cnt_m1, ~ {
  
  # Tidy fixed effects only
  coefs <- 
    broom.mixed::tidy(.x, effects = "fixed") %>%
    select(term, estimate, p.value) %>%
    filter(term != "(Intercept)")
  
  # Get Grade1 coefficient
  grade_coef <- coefs %>% filter(term == "Grade1") %>% pull(estimate)
  
  # If missing or 0, skip
  if (is.na(grade_coef) || grade_coef == 0) return(NULL)
  
  # Compute EYOS
  coefs %>%
    mutate(
      country = .y,
      EYOS = estimate / grade_coef,
      grade_coef = grade_coef,
      sig = if_else(p.value < 0.1, "p < 0.1", "n.s.")  # significance flag
    )
})

# View(eyos_results)

# Reorder terms for nicer plot
eyos_results <- 
  eyos_results %>%
  mutate(
    term = fct_reorder(term, EYOS, .fun = median, .desc = TRUE),
    # use the rename vector to rename the terms
    term = recode(term, !!!rename_vector),
    country = fct_relevel(country, 
                          "Brazil",
                          "BSJZ", "Hong Kong", "Macao", 
                          "Russia", "Moscow City", 
                          "Moscow Region", "Tatarstan")
  )

grade_coef_values <- 
  eyos_results %>%
  filter(term == "School year") %>%
  select(country, grade_coef)


# Okabe-Ito colorblind-friendly palette (up to 8 colors)
okabe_ito <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00", # vermillion
  "#CC79A7", # reddish purple
  "#999999"  # gray
)

# Plot
eyos_plot <-
  ggplot(eyos_results %>% filter(term != "School year"), 
       aes(x = EYOS, y = term, color = country, shape = sig)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_shape_manual(
    values = c("p < 0.1" = 16, "n.s." = 18),  # circle vs diamond
    guide = guide_legend(order = 2, override.aes = list(size = 3))
  ) +
  scale_color_manual(
    values = okabe_ito,
    guide = guide_legend(order = 1, override.aes = list(shape = 16, size = 3))
  ) +
  labs(
    #title = "Effect of Covariates in Equivalent Years of Schooling (EYOS)",
    #subtitle = "Relative to the learning gain from one school year",
    x = "Equivalent Years of Schooling (EYOS)",
    y = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",  # stack one legend below the other
    legend.title = element_blank(),  # remove legend titles
    legend.spacing.y = unit(0.2, "cm")
  )


  )

cat("   ✅ Model 1 completed for all countries\n")
cat("   ✅ EYOS coefficients extracted and visualized\n\n")

cat("📈 MODEL 2: RANDOM INTERCEPT + RANDOM SLOPE FOR SES\n")
#------ MODEL 2: FIXED EFFECTS WITH RANDOM INTERCEPT FOR SCHOOL AND RANDOM SLOPE FOR SES ------ #####

# Update the formulas to include random slope for ESCS
# Update all the models to include random slope for ESCS_group (1 + Grade1 | ESCS_group)

# Remove ESCS_sch_group from fixed effects in the formulas above before this section!
# Then fit new models with random slope for Grade1 by ESCS_sch_group

m2_rus <- lmer(
  update.formula(rus_formula, . ~ . - ESCS_group + (1 + Grade1 | ESCS_group)),
  data = model_data[model_data$CNT == "Russian Federation", ]
)
m2_mow <- lmer(
  update.formula(mow_formula, . ~ . - ESCS_group + (1 + Grade1 | ESCS_group)),
  data = model_data[model_data$CNT == "Moscow City (RUS)", ]
)
m2_tat <- lmer(
  update.formula(rus_formula, . ~ . - ESCS_group + (1 + Grade1 | ESCS_group)),
  data = model_data[model_data$CNT == "Tatarstan (RUS)", ]
)
m2_mow_reg <- lmer(
  update.formula(rus_formula, . ~ . - ESCS_group + (1 + Grade1 | ESCS_group)),
  data = model_data[model_data$CNT == "Moscow Region (RUS)", ]
)
m2_bsjz <- lmer(
  update.formula(bsjz_hkg_formula, . ~ . - ESCS_group + (1 + Grade1 | ESCS_group)),
  data = model_data[model_data$CNT == "B-S-J-Z (China)", ]
)
m2_hkg <- lmer(
  update.formula(bsjz_hkg_formula, . ~ . - ESCS_group + (1 + Grade1 | ESCS_group)),
  data = model_data[model_data$CNT == "Hong Kong", ]
)
m2_macao <- lmer(
  update.formula(macao_formula, . ~ . - ESCS_group + (1 + Grade1 | ESCS_group)),
  data = model_data[model_data$CNT == "Macao", ]
)
m2_bra <- lmer(
  update.formula(bra_formula, . ~ . - ESCS_group + (1 + Grade1 | ESCS_group)),
  data = model_data[model_data$CNT == "Brazil", ]
)

# put all Model 2 models into a list
cnt_m2 <- list("Russia" = m2_rus,
               "Moscow City" = m2_mow,
               "Moscow Region" = m2_mow_reg,
               "Tatarstan" = m2_tat,
               "BSJZ" = m2_bsjz,
               "Hong Kong" = m2_hkg,
               "Macao" = m2_macao,
               "Brazil" = m2_bra)

#------ EXTRACT RANDOM SLOPES FOR Grade1 BY ESCS GROUP ------ #####

unique(model_data$ESCS_group)

unique(model_data$ESCS_group)
# [1] Bottom 40% Middle 50% Top 10%    <NA>      
# Levels: Bottom 40% Middle 50% Top 10%

# Extract coefficients for all m2 models
m2_learning_ses <- 
  imap_dfr(cnt_m2, ~ {
    coef(.x)$ESCS_group %>%
      as.data.frame() %>%
      rownames_to_column(var = "SES_Group") %>%
      select(SES_Group, Grade1) %>%
      mutate(Grade1 = round(Grade1, 2),
             Territory = .y)
  }) %>%
  select(Territory, SES_Group, Grade1)


#------ VISUALIZATION OF RANDOM SLOPES ------ #####

# Create bar chart with specified order and colors
library(scales)

# Define territory order and colors
territory_order <- c("Brazil", "BSJZ", "Hong Kong", "Macao", 
                    "Russia", "Moscow City", "Moscow Region", "Tatarstan")

# Define colors: red for bottom 40%, green for middle 50%, blue for top 10%
ses_colors <- c("Bottom 40%" = "#E31A1C",    # Red
                "Middle 50%" = "#33A02C",     # Green  
                "Top 10%" = "#1F78B4")       # Blue

# Bar chart with specified order and colors
bar_plot_m2 <- 
  m2_learning_ses %>%
  mutate(Territory = factor(Territory, levels = territory_order)) %>%
  ggplot(aes(x = Territory, y = Grade1, fill = SES_Group)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(
    aes(label = Grade1), 
    position = position_dodge(width = 0.7),
    vjust = ifelse(m2_learning_ses$Grade1 >= 0, -0.3, 1.3),
    size = 3,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = ses_colors,
    name = "SES Group"
  ) +
  labs(
    title = "Learning Gains by Territory and SES Group",
    subtitle = "Grade1 random slope coefficients showing differential learning effects",
    x = "Territory",
    y = "Learning Gain (in SDs)",
    caption = "Higher values indicate greater learning gains from additional schooling"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Display the bar chart
print(bar_plot_m2)

# check the summary of one of the models
reg_tables_m2 <-
  modelsummary(cnt_m2,
               statistic = "({std.error}) {stars}",
               gof_omit = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector,
               notes   = "  ",
               output = "tinytable") 

cat("   ✅ Model 2 completed for all countries\n")
cat("   ✅ Random slope effects analyzed\n\n")

cat("🎯 MODEL 3: RANDOM INTERCEPT + RANDOM SLOPE FOR SCHOOL SES\n")
#------ MODEL 3: FIXED EFFECTS WITH RANDOM INTERCEPT FOR SCHOOL AND RANDOM SLOPE FOR SCHOOL SES ------ #####

# Update the formulas to include random slope for ESCS_sch_group (1 + Grade1 | ESCS_sch_group)
# Update all the models to include random slope for Grade1 by ESCS_sch_group
m3_rus <- lmer(
  update.formula(rus_formula, . ~ . - sch_escs + (1 + Grade1 | ESCS_sch_group)),
  data = model_data[model_data$CNT == "Russian Federation", ]
)
m3_mow <- lmer(
  update.formula(mow_formula, . ~ . - sch_escs + (1 + Grade1 | ESCS_sch_group)),
  data = model_data[model_data$CNT == "Moscow City (RUS)", ]
)
m3_tat <- lmer(
  update.formula(rus_formula, . ~ . - sch_escs + (1 + Grade1 | ESCS_sch_group)),
  data = model_data[model_data$CNT == "Tatarstan (RUS)", ]
)
m3_mow_reg <- lmer(
  update.formula(rus_formula, . ~ . - sch_escs + (1 + Grade1 | ESCS_sch_group)),
  data = model_data[model_data$CNT == "Moscow Region (RUS)", ]
)
m3_bsjz <- lmer(
  update.formula(bsjz_hkg_formula, . ~ . - sch_escs + (1 + Grade1 | ESCS_sch_group)),
  data = model_data[model_data$CNT == "B-S-J-Z (China)", ]
)
m3_hkg <- lmer(
  update.formula(bsjz_hkg_formula, . ~ . - sch_escs + (1 + Grade1 | ESCS_sch_group)),
  data = model_data[model_data$CNT == "Hong Kong", ]               
)

m3_macao <- lmer(
  update.formula(macao_formula, . ~ . - sch_escs + (1 + Grade1 | ESCS_sch_group)),
  data = model_data[model_data$CNT == "Macao", ]
)
m3_bra <- lmer(
  update.formula(bra_formula, . ~ . - sch_escs + (1 + Grade1 | ESCS_sch_group)),
  data = model_data[model_data$CNT == "Brazil", ]
)     

# put all Model 3 models into a list
cnt_m3 <- list("Russia" = m3_rus,
                "Moscow City" = m3_mow,
                "Moscow Region" = m3_mow_reg,
                "Tatarstan" = m3_tat,
                "BSJZ" = m3_bsjz,
                "Hong Kong" = m3_hkg,
                "Macao" = m3_macao,
                "Brazil" = m3_bra)

   

# Export the coefficients of Grade1 by SES group from Model 3
m3_learning_ses <- 
  imap_dfr(cnt_m3, ~ {
    coef(.x)$ESCS_sch_group %>%
      as.data.frame() %>%
      rownames_to_column(var = "SES_School_Group") %>%
      select(SES_School_Group, Grade1) %>%
      mutate(Grade1 = round(Grade1, 2),
             Territory = .y)
  }) %>%
  select(Territory, SES_School_Group, Grade1)

  # Produce the same bar chart as above but for Model 3
bar_plot_m3 <-
  m3_learning_ses %>%
  mutate(Territory = factor(Territory, levels = territory_order)) %>%
  ggplot(aes(x = Territory, y = Grade1, fill = SES_School_Group)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(
    aes(label = Grade1), 
    position = position_dodge(width = 0.7),
    vjust = ifelse(m3_learning_ses$Grade1 >= 0, -0.3, 1.3),
    size = 3,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = ses_colors,
    name = "SES School Group"
  ) +
  labs(
    title = "Learning Gains by Territory and School SES Group (Model 3)",
    subtitle = "Grade1 random slope coefficients showing differential learning effects",
    x = "Territory",
    y = "Learning Gain (in SDs)",
    caption = "Higher values indicate greater learning gains from additional schooling"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

  # print(bar_plot_m3)

  # check the summary of one of the models
reg_tables_m3 <-
  modelsummary(cnt_m3,
               statistic = "({std.error}) {stars}",
               gof_omit = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector,
               notes   = "  ",
               output = "tinytable") 

cat("   ✅ Model 3 completed for all countries\n")
cat("   ✅ School SES random slope effects analyzed\n\n")

# =============================================================================
cat("🎉 MULTILEVEL MODELING COMPLETED!\n")
cat("📊 Summary of Regression Analysis:\n")
cat("   • Baseline Models (ICC): ", length(unique(icc_results$country)), "countries\n")
cat("   • Model 1 (Fixed Effects): ", length(cnt_m1), "regression models\n")
cat("   • Model 2 (Random SES Slope): ", length(cnt_m2), "regression models\n") 
cat("   • Model 3 (Random School SES): ", length(cnt_m3), "regression models\n")
cat("   • Total models fitted: ", length(cnt_baseline_model) + length(cnt_m1) + length(cnt_m2) + length(cnt_m3), "\n")
cat("   • Visualizations created: ICC plot, EYOS plots, coefficient plots\n")
cat("   • Model summaries generated for all specifications\n")
cat("=============================================================================\n\n") 

