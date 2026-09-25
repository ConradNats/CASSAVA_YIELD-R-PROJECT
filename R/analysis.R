# ============================================================
# CASSAVA YIELD ANALYSIS
# Beginner-friendly R programming project
# ============================================================

# -----------------------------
# 0. PACKAGES
# -----------------------------
packages <- c("readxl", "dplyr", "ggplot2", "tidyr")

installed <- rownames(installed.packages())

for (p in packages) {
  if (!(p %in% installed)) {
    install.packages(p) 
  }
}

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# -----------------------------
# 1. IMPORT THE EXCEL DATA
# -----------------------------
# Here, read_excel() loads the "Cassava Data" worksheet.
df <- read_excel("data/raw/Cassava_Yield_Data.xlsx",
                 sheet = "Cassava Data")

# Remove accidental spaces from column names.
names(df) <- trimws(names(df))

# Look at the data
head(df)
str(df)
dim(df)
names(df)

# -----------------------------
# 2. BASIC EXPLORATION
# -----------------------------

# Summary of every variable
summary(df)

# Number of rows and columns
nrow(df)
ncol(df)

# Data types
sapply(df, class)

# Frequency tables for categorical variables
table(df$tillage)
table(df$ferT)
table(df$Sesn)
table(df$locn)

# -----------------------------
# 3. MISSING VALUES
# -----------------------------

missing_by_variable <- colSums(is.na(df))
print(missing_by_variable)

total_missing <- sum(is.na(df))
print(total_missing)

# Visual missing-value map
missing_long <- df %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(
    cols = -row_id,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(is_missing = is.na(value))

p_missing <- ggplot(missing_long,
                    aes(x = variable, y = row_id, fill = is_missing)) +
  geom_tile() +
  labs(
    title = "Missing-Value Map",
    x = "Variable",
    y = "Observation",
    fill = "Missing?"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(p_missing)
ggsave("figures/01_missing_value_map.png",
       p_missing, width = 12, height = 7, dpi = 300)

# -----------------------------
# 4. IDENTIFY NUMERIC VARIABLES
# -----------------------------

# These are the continuous/quantitative variables we will
# examine for distributions and outliers.
numeric_vars <- names(df)[sapply(df, is.numeric)]

print(numeric_vars)

# -----------------------------
# 5. DISTRIBUTIONS
# -----------------------------

# Histograms for all numeric variables
for (v in numeric_vars) {

  p <- ggplot(df, aes(x = .data[[v]])) +
    geom_histogram(bins = 20, na.rm = TRUE) +
    labs(
      title = paste("Distribution of", v),
      x = v,
      y = "Frequency"
    ) +
    theme_minimal()

  print(p)

  ggsave(
    filename = paste0("figures/hist_", v, ".png"),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
}

# -----------------------------
# 6. OUTLIERS USING THE IQR RULE
# -----------------------------
# IQR = Q3 - Q1
# Lower fence = Q1 - 1.5*IQR
# Upper fence = Q3 + 1.5*IQR

outlier_report <- data.frame(
  variable = character(),
  Q1 = numeric(),
  Q3 = numeric(),
  IQR = numeric(),
  lower_fence = numeric(),
  upper_fence = numeric(),
  outlier_count = integer()
)

for (v in numeric_vars) {

  x <- df[[v]]

  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1

  lower <- q1 - 1.5 * iqr_value
  upper <- q3 + 1.5 * iqr_value

  outliers <- sum(x < lower | x > upper, na.rm = TRUE)

  outlier_report <- rbind(
    outlier_report,
    data.frame(
      variable = v,
      Q1 = q1,
      Q3 = q3,
      IQR = iqr_value,
      lower_fence = lower,
      upper_fence = upper,
      outlier_count = outliers
    )
  )
}

print(outlier_report)
write.csv(outlier_report,
          "results/outlier_report.csv",
          row.names = FALSE)

# Boxplots for all numeric variables
for (v in numeric_vars) {

  p <- ggplot(df, aes(y = .data[[v]])) +
    geom_boxplot(na.rm = TRUE) +
    labs(
      title = paste("Boxplot of", v),
      y = v
    ) +
    theme_minimal()

  print(p)

  ggsave(
    filename = paste0("figures/box_", v, ".png"),
    plot = p,
    width = 6,
    height = 5,
    dpi = 300
  )
}

# -----------------------------
# 7. HANDLE MISSING VALUES
# -----------------------------
# The supplied dataset has no missing values.
# We still show the correct workflow:
#
# For numeric variables, median imputation is a simple
# beginner-friendly option.
#
# For this dataset, no rows need to be changed because
# there are zero missing values.

df_clean <- df

for (v in numeric_vars) {
  med <- median(df_clean[[v]], na.rm = TRUE)

  df_clean[[v]][is.na(df_clean[[v]])] <- med
}

# Confirm missing values after imputation
print(colSums(is.na(df_clean)))

# -----------------------------
# 8. HANDLE OUTLIERS
# -----------------------------
# We use winsorization/capping:
# values below the lower IQR fence are changed to the lower
# fence, and values above the upper fence are changed to the
# upper fence.
#
# This keeps the observation instead of deleting it.

# Do not cap identifier/design variables.
vars_to_cap <- c(
  "Plants_harvested",
  "No_bigtubers",
  "Weigh_bigtubers",
  "No_mediumtubers",
  "Weight_mediumtubers",
  "No_smalltubers",
  "Weight_smalltubers",
  "Totaltuberno",
  "AV_tubers_Plant",
  "Total_tubweight",
  "plotsize",
  "TotalWeightperhectare",
  "TotalTuberperHectare"
)

for (v in vars_to_cap) {

  q1 <- quantile(df_clean[[v]], 0.25, na.rm = TRUE)
  q3 <- quantile(df_clean[[v]], 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1

  lower <- q1 - 1.5 * iqr_value
  upper <- q3 + 1.5 * iqr_value

  df_clean[[v]] <- pmax(df_clean[[v]], lower)
  df_clean[[v]] <- pmin(df_clean[[v]], upper)
}

# Save cleaned data
write.csv(df_clean,
          "data/processed/cassava_yield_clean.csv",
          row.names = FALSE)

# -----------------------------
# 9. QUESTION 2A:
# TWO CONTINUOUS VARIABLES
# -----------------------------
# We use:
# TotalWeightperhectare
# TotalTuberperHectare

p_scatter <- ggplot(
  df_clean,
  aes(x = TotalTuberperHectare,
      y = TotalWeightperhectare)
) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Projected Total Weight vs Total Tuber Number per Hectare",
    x = "Total Tuber per Hectare",
    y = "Total Weight per Hectare"
  ) +
  theme_minimal()

print(p_scatter)

ggsave("figures/02_continuous_scatterplot.png",
       p_scatter, width = 8, height = 6, dpi = 300)

# Pearson correlation
cor_test <- cor.test(
  df_clean$TotalTuberperHectare,
  df_clean$TotalWeightperhectare,
  method = "pearson"
)

print(cor_test)

capture.output(
  cor_test,
  file = "results/pearson_correlation.txt"
)

# -----------------------------
# 10. QUESTION 2B:
# ONE CONTINUOUS + ONE CATEGORICAL
# -----------------------------
# We use TotalWeightperhectare and tillage.
# Tillage has two groups: conventional and minimum.

p_tillage_weight <- ggplot(
  df_clean,
  aes(x = tillage,
      y = TotalWeightperhectare)
) +
  geom_boxplot() +
  labs(
    title = "Total Weight per Hectare by Tillage Method",
    x = "Tillage Method",
    y = "Total Weight per Hectare"
  ) +
  theme_minimal()

print(p_tillage_weight)

ggsave("figures/03_tillage_weight_boxplot.png",
       p_tillage_weight, width = 7, height = 6, dpi = 300)

# Welch two-sample t-test
tillage_weight_test <- t.test(
  TotalWeightperhectare ~ tillage,
  data = df_clean
)

print(tillage_weight_test)

capture.output(
  tillage_weight_test,
  file = "results/tillage_weight_ttest.txt"
)

# Also test total tuber number per hectare by tillage
p_tillage_tuber <- ggplot(
  df_clean,
  aes(x = tillage,
      y = TotalTuberperHectare)
) +
  geom_boxplot() +
  labs(
    title = "Total Tuber per Hectare by Tillage Method",
    x = "Tillage Method",
    y = "Total Tuber per Hectare"
  ) +
  theme_minimal()

print(p_tillage_tuber)

ggsave("figures/04_tillage_tuber_boxplot.png",
       p_tillage_tuber, width = 7, height = 6, dpi = 300)

tillage_tuber_test <- t.test(
  TotalTuberperHectare ~ tillage,
  data = df_clean
)

print(tillage_tuber_test)

capture.output(
  tillage_tuber_test,
  file = "results/tillage_tuber_ttest.txt"
)

# -----------------------------
# 11. QUESTION 2C:
# TWO CATEGORICAL VARIABLES
# -----------------------------
# We use tillage and fertilizer.

tillage_fertilizer_table <- table(
  df_clean$tillage,
  df_clean$ferT
)

print(tillage_fertilizer_table)

p_bar <- ggplot(
  df_clean,
  aes(x = ferT, fill = tillage)
) +
  geom_bar(position = "dodge") +
  labs(
    title = "Tillage Method by Fertilizer Application",
    x = "Fertilizer Code",
    y = "Count",
    fill = "Tillage"
  ) +
  theme_minimal()

print(p_bar)

ggsave("figures/05_tillage_fertilizer_barplot.png",
       p_bar, width = 8, height = 6, dpi = 300)

chi_test <- chisq.test(tillage_fertilizer_table)

print(chi_test)

capture.output(
  chi_test,
  file = "results/tillage_fertilizer_chisq.txt"
)

# -----------------------------
# 12. QUESTION 3A:
# DOES FERTILIZER AFFECT YIELD?
# -----------------------------
# Fertilizer has 5 groups, so we use one-way ANOVA.

p_fertilizer_weight <- ggplot(
  df_clean,
  aes(x = ferT,
      y = TotalWeightperhectare)
) +
  geom_boxplot() +
  labs(
    title = "Total Weight per Hectare by Fertilizer",
    x = "Fertilizer",
    y = "Total Weight per Hectare"
  ) +
  theme_minimal()

print(p_fertilizer_weight)

ggsave("figures/06_fertilizer_weight_boxplot.png",
       p_fertilizer_weight, width = 8, height = 6, dpi = 300)

anova_weight <- aov(
  TotalWeightperhectare ~ ferT,
  data = df_clean
)

print(summary(anova_weight))

capture.output(
  summary(anova_weight),
  file = "results/fertilizer_weight_anova.txt"
)

# Tukey post-hoc comparisons
tukey_weight <- TukeyHSD(anova_weight)
print(tukey_weight)

capture.output(
  tukey_weight,
  file = "results/fertilizer_weight_tukey.txt"
)

# Total tuber number
p_fertilizer_tuber <- ggplot(
  df_clean,
  aes(x = ferT,
      y = TotalTuberperHectare)
) +
  geom_boxplot() +
  labs(
    title = "Total Tuber per Hectare by Fertilizer",
    x = "Fertilizer",
    y = "Total Tuber per Hectare"
  ) +
  theme_minimal()

print(p_fertilizer_tuber)

ggsave("figures/07_fertilizer_tuber_boxplot.png",
       p_fertilizer_tuber, width = 8, height = 6, dpi = 300)

anova_tuber <- aov(
  TotalTuberperHectare ~ ferT,
  data = df_clean
)

print(summary(anova_tuber))

capture.output(
  summary(anova_tuber),
  file = "results/fertilizer_tuber_anova.txt"
)

tukey_tuber <- TukeyHSD(anova_tuber)
print(tukey_tuber)

capture.output(
  tukey_tuber,
  file = "results/fertilizer_tuber_tukey.txt"
)

# -----------------------------
# 13. QUESTION 3B:
# DOES TILLAGE AFFECT YIELD?
# -----------------------------

# Weight per hectare
tillage_weight <- t.test(
  TotalWeightperhectare ~ tillage,
  data = df_clean
)

# Tuber number per hectare
tillage_tuber <- t.test(
  TotalTuberperHectare ~ tillage,
  data = df_clean
)

print(tillage_weight)
print(tillage_tuber)

# -----------------------------
# 14. GROUP SUMMARIES
# -----------------------------

fertilizer_summary <- df_clean %>%
  group_by(ferT) %>%
  summarise(
    n = n(),
    mean_weight_per_ha = mean(TotalWeightperhectare),
    sd_weight_per_ha = sd(TotalWeightperhectare),
    mean_tuber_per_ha = mean(TotalTuberperHectare),
    sd_tuber_per_ha = sd(TotalTuberperHectare),
    .groups = "drop"
  )

print(fertilizer_summary)

write.csv(
  fertilizer_summary,
  "results/fertilizer_group_summary.csv",
  row.names = FALSE
)

tillage_summary <- df_clean %>%
  group_by(tillage) %>%
  summarise(
    n = n(),
    mean_weight_per_ha = mean(TotalWeightperhectare),
    sd_weight_per_ha = sd(TotalWeightperhectare),
    mean_tuber_per_ha = mean(TotalTuberperHectare),
    sd_tuber_per_ha = sd(TotalTuberperHectare),
    .groups = "drop"
  )

print(tillage_summary)

write.csv(
  tillage_summary,
  "results/tillage_group_summary.csv",
  row.names = FALSE
)

# -----------------------------
# 15. FINISHED
# -----------------------------
cat("\nAnalysis complete!\n")
cat("Check the figures/ and results/ folders.\n")
