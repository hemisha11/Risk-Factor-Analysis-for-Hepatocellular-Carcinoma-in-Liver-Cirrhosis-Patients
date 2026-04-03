install.packages("tidyverse")
library(tidyverse)

# import 12 files from folder into R
dataa <- "/Users/hemishadesai/Downloads/Costello Medical Epidemiologist Assessment/Data Files/"
files_12 <- paste0(dataa, "DN0", c(1:12), ".csv")
df_hcc <- map_dfr(files_12, ~ read_csv(.x, show_col_types = FALSE), .id = "team")
glimpse(df_hcc)

# Data Cleaning 
df_final_hcc <- df_hcc %>%
select(-`...1`) %>%
rename(
HCC = `HCC at 5 years`,
prior_tx = `Number of prior treatments for cirrhosis`,
alcohol = Alcohol,
age = Age,
weight = Weight,
height = Height,
comorbidity = Comorbidities
  ) %>%
mutate(
comorbidity = str_to_lower(str_trim(comorbidity)),
comorbidity = case_when(
comorbidity == "hypertension" ~ "Hypertension only",
comorbidity == "diabetes" ~ "Diabetes only",
comorbidity == "diabetes and hypertension" ~ "Both",
TRUE ~ NA_character_  
),
comorbidity = factor(comorbidity, levels = c("Hypertension only", "Diabetes only", "Both")),
HCC = factor(HCC, levels = c("N", "Y")),
team = factor(team)
)
glimpse(df_final_hcc)

# Checking Missing Values across all 8 variables
df_final_hcc %>%
summarise(across(everything(), ~ sum(is.na(.)))) %>%
pivot_longer(everything(),names_to = "variable", values_to = "n_missing") %>%
mutate(pct_missing = round(n_missing / nrow(df_final_hcc) * 100, 1)) %>%
arrange(desc(n_missing))
# 37 missing for alcohol, 12 missing for comorbidity, 8 missing for height

# Check where missing values are concentrated in terms of 12 clinical teams
df_final_hcc %>%
group_by(team) %>%
summarise(
missing_for_alcohol = sum(is.na(alcohol)),
missing_for_comorbidity = sum(is.na(comorbidity)),
missing_for_height = sum(is.na(height)),
total_patients = n()
  ) %>%
arrange(team)

# Descriptive statistics for continuous variables
df_final_hcc %>%
group_by(HCC) %>%
summarise(
n = n(),
age_mean = round(mean(age, na.rm = TRUE), 1),
age_sd = round(sd(age, na.rm = TRUE), 1),
age_median = round(median(age, na.rm = TRUE), 1),
age_q1 = round(quantile(age, 0.25, na.rm = TRUE), 1),
age_q3 = round(quantile(age, 0.75, na.rm = TRUE), 1),
age_min = round(min(age, na.rm = TRUE), 1),
age_max = round(max(age, na.rm = TRUE), 1),
weight_mean = round(mean(weight, na.rm = TRUE), 1),
weight_sd = round(sd(weight, na.rm = TRUE), 1),
weight_median = round(median(weight, na.rm = TRUE), 1),
weight_q1 = round(quantile(weight, 0.25, na.rm = TRUE), 1),
weight_q3 = round(quantile(weight, 0.75, na.rm = TRUE), 1),
weight_min = round(min(weight, na.rm = TRUE), 1),
weight_max = round(max(weight, na.rm = TRUE), 1),
height_mean = round(mean(height, na.rm = TRUE), 1),
height_sd = round(sd(height, na.rm = TRUE), 1),
height_median = round(median(height, na.rm = TRUE), 1),
height_q1 = round(quantile(height, 0.25, na.rm = TRUE), 1),
height_q3 = round(quantile(height, 0.75, na.rm = TRUE), 1),
height_min = round(min(height, na.rm = TRUE), 1),
height_max = round(max(height, na.rm = TRUE), 1),
alcohol_mean = round(mean(alcohol, na.rm = TRUE), 1),
alcohol_sd = round(sd(alcohol, na.rm = TRUE), 1),
alcohol_median = round(median(alcohol, na.rm = TRUE), 1),
alcohol_q1 = round(quantile(alcohol, 0.25, na.rm = TRUE), 1),
alcohol_q3 = round(quantile(alcohol, 0.75, na.rm = TRUE), 1),
alcohol_min = round(min(alcohol, na.rm = TRUE), 1),
alcohol_max = round(max(alcohol, na.rm = TRUE), 1),
prior_tx_mean = round(mean(prior_tx, na.rm = TRUE), 1),
prior_tx_sd = round(sd(prior_tx, na.rm = TRUE), 1),
prior_tx_median = round(median(prior_tx, na.rm = TRUE), 1),
prior_tx_q1 = round(quantile(prior_tx, 0.25, na.rm = TRUE), 1),
prior_tx_q3 = round(quantile(prior_tx, 0.75, na.rm = TRUE), 1),
prior_tx_min = round(min(prior_tx, na.rm = TRUE), 1),
prior_tx_max = round(max(prior_tx, na.rm = TRUE), 1)
  ) %>%
print(width = Inf)

# frequency table for comorbidity by HCC
df_final_hcc %>%
filter(!is.na(comorbidity)) %>%
group_by(comorbidity, HCC) %>%
summarise(n = n(), .groups = "drop") %>%
group_by(comorbidity) %>%
mutate(pct = round(n / sum(n) * 100, 1)) %>%
arrange(comorbidity, HCC) %>%
print()

# Checking for skewness of continuous variables
install.packages("moments")
library(moments)
df_final_hcc %>%
group_by(HCC) %>%
summarise(
age_skew = round(skewness(age, na.rm = TRUE), 2),
weight_skew = round(skewness(weight, na.rm = TRUE), 2),
height_skew = round(skewness(height, na.rm = TRUE), 2),
alcohol_skew = round(skewness(alcohol, na.rm = TRUE), 2),
prior_tx_skew = round(skewness(prior_tx, na.rm = TRUE), 2)
)

install.packages("patchwork")
library(patchwork)
hcc_colors <- c("N" = "#4C9BE8", "Y" = "#E8694C")

# Density plot for age
p_age <- ggplot(df_final_hcc, aes(x = age, fill = HCC, colour = HCC)) +
geom_density(alpha = 0.4) +
scale_fill_manual(values = hcc_colors, labels = c("No HCC", "HCC")) +
scale_colour_manual(values = hcc_colors, labels = c("No HCC", "HCC")) +
labs(title = "Age", x = "Age (years)", y = "Density", fill = "HCC status", colour = "HCC status") +
theme_bw()

# Density plot for weight
p_weight <- ggplot(df_final_hcc, aes(x = weight, fill = HCC, colour = HCC)) +
geom_density(alpha = 0.4) +
scale_fill_manual(values = hcc_colors, labels = c("No HCC", "HCC")) +
scale_colour_manual(values = hcc_colors, labels = c("No HCC", "HCC")) +
labs(title = "Weight", x = "Weight (kg)", y = "Density", fill = "HCC status", colour = "HCC status") +
theme_bw()

# Density plot for height
p_height <- ggplot(df_final_hcc, aes(x = height, fill = HCC, colour = HCC)) +
geom_density(alpha = 0.4) +
scale_fill_manual(values = hcc_colors, labels = c("No HCC", "HCC")) +
scale_colour_manual(values = hcc_colors, labels = c("No HCC", "HCC")) +
labs(title = "Height", x = "Height (cm)", y = "Density",fill = "HCC status", colour = "HCC status",caption = "Note: 8 values missing") +
theme_bw()

# Density plot for alcohol
p_alcohol <- ggplot(df_final_hcc, aes(x = alcohol, fill = HCC, colour = HCC)) +
geom_density(alpha = 0.4) +
scale_fill_manual(values = hcc_colors, labels = c("No HCC", "HCC")) +
scale_colour_manual(values = hcc_colors, labels = c("No HCC", "HCC")) +
labs(title = "Alcohol intake", x = "Alcohol (units)", y = "Density",fill = "HCC status", colour = "HCC status",caption = "Note: 37 values missing") +
theme_bw()

# Bar chart for prior treatments
p_prior_tx <- df_final_hcc %>%
group_by(prior_tx, HCC) %>%
summarise(n = n(), .groups = "drop") %>%
group_by(prior_tx) %>%
mutate(pct = round(n / sum(n) * 100, 1)) %>%
ggplot(aes(x = factor(prior_tx), y = pct, fill = HCC)) +
geom_col(position = "dodge", alpha = 0.85) +
scale_fill_manual(values = hcc_colors, labels = c("No HCC", "HCC")) +
labs(title = "Prior treatments", x = "Number of prior treatments",y = "Percentage (%)", fill = "HCC status") +
theme_bw()

p_comorb <- df_final_hcc %>%
filter(!is.na(comorbidity)) %>%
group_by(comorbidity, HCC) %>%
summarise(n = n(), .groups = "drop") %>%
group_by(comorbidity) %>%
mutate(pct = round(n / sum(n) * 100, 1)) %>%
ggplot(aes(x = comorbidity, y = pct, fill = HCC)) +
geom_col(position = "dodge", alpha = 0.85) +
scale_fill_manual(values = hcc_colors, labels = c("No HCC", "HCC")) +
labs(title = "Comorbidities", x = "Comorbidity group",y = "Percentage (%)", fill = "HCC status",caption = "Note: 12 values missing") +
theme_bw()

# Combine all plots into one 
combined_plot <- (p_age | p_weight | p_height) /
(p_alcohol | p_prior_tx | p_comorb) +
plot_annotation(title = "Figure 1. Distribution of patient characteristics by HCC status",
theme = theme(plot.title = element_text(size = 13, face = "bold"))
) + plot_layout(guides = "collect")

combined_plot


