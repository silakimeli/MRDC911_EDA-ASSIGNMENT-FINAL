# =============================
# MRDC 911 Assignment 1 - EDA & Preprocessing on Kenyan  Student  Dataset
# Student Name: [SILA KIMELI RONOH]
# Date: [11/06/2025]
# =============================
library(ggplot2)
library(tidyverse)
library(corrplot)
# ===========================
#1.	Load the dataset and display its structure (e.g., column names, data types, first few rows). How many numerical and categorical variables are there?
data <- read_csv("kenya_student_data.csv")
str(data)
head(data)
glimpse(data)
summary(data)
# ===========================
#2.	Compute summary statistics (mean, median, min, max, etc.) for all numerical variables (e.g., family_income, study_hours_weekly). What insights do these provide about the data?
numeric_data <- select(data, where(is.numeric))
summary(numeric_data)
# ===========================
#3.	Create a bar plot to visualize the distribution of academic_performance. Is the target variable balanced across its classes (Poor, Average, Good, Excellent)?
ggplot(data, aes(x = academic_performance)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Academic Performance",
       x = "Academic Performance",
       y = "Number of Students") +
  theme_minimal()
library(dplyr)
data %>%
  count(academic_performance) %>%
  mutate(percent = round(n / sum(n) * 100, 1))
# ===========================
#4.	Visualize the distribution of study_hours_weekly using a histogram. How does it vary between urban and rural students (use a faceted histogram)?
# -----------------------------
# Histogram of study hours (overall)
ggplot(data, aes(x = study_hours_weekly)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Weekly Study Hours (All Students)",
       x = "Study Hours per Week",
       y = "Count") +
  theme_minimal()
# ===========================
ggplot(data, aes(x = study_hours_weekly)) +
  geom_histogram(binwidth = 2, fill = "#443785") +
  facet_wrap(~residency) +
  labs(title = "Study Hours by Residency")
# ===========================
#5.Create boxplots of math_score by academic_performance and gender. What patterns do you observe?
ggplot(data, aes(x = academic_performance, y = math_score, fill = gender)) +
  geom_boxplot() +
  labs(title = "Math Score by Academic Performance and Gender")
# ===========================
#6.	Compute the proportion of each category in extracurricular_activities and faculty. Which categories are most common?
data %>% count(extracurricular_activities) %>% mutate(prop = n / sum(n))
data %>% count(faculty) %>% mutate(prop = n / sum(n))
# ===========================
#7.	Create a correlation matrix for numerical variables (excluding student_id) and visualize it using a heatmap. Which pairs have the strongest correlations?

num_data <- data %>% select(where(is.numeric), -student_id)
corr_matrix <- cor(num_data, use = "complete.obs")
corrplot(corr_matrix, method = "color", type = "lower")
# ===========================
# Select only numeric columns (excluding student_id)
numeric_data <- data %>%
  select(where(is.numeric)) %>%
  select(-student_id)

# Remove rows with missing values for correlation computation
numeric_data_clean <- na.omit(numeric_data)

# Compute correlation matrix
cor_matrix <- cor(numeric_data_clean, use = "complete.obs")

# Visualize correlation matrix using corrplot
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black", # optional: add correlation values
         tl.col = "black",
         tl.srt = 45,
         number.cex = 0.7,
         title = "Correlation Matrix of Numeric Variables",
         mar = c(0, 0, 1, 0))
#=======================
#8.Use a statistical test (e.g., chi-squared) to check if internet_access is associated with academic_performance. Interpret the results.
chisq.test(table(data$internet_access, data$academic_performance))
#=======================
#9.	Identify columns with missing values and report their percentages. Why might these variables have missing data in a Kenyan context?
# Calculate number and percentage of missing values for each column
missing_summary <- data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_count") %>%
  mutate(missing_percent = round((missing_count / nrow(data)) * 100, 2)) %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_percent))

# View summary
print(missing_summary)
#=======================
#10.	Impute missing values in family_income and math_score using the median. Justify why the median is appropriate for these variables.
data$family_income[is.na(data$family_income)] <- median(data$family_income, na.rm = TRUE)
data$math_score[is.na(data$math_score)] <- median(data$math_score, na.rm = TRUE)
data$family_income[is.na(data$family_income)] <- median(data$family_income, na.rm = TRUE)
data$math_score[is.na(data$math_score)] <- median(data$math_score, na.rm = TRUE)
data$attendance_rate[is.na(data$attendance_rate)] <- mean(data$attendance_rate, na.rm = TRUE)
#11.Impute missing values in attendance_rate using the mean. Compare the distributions before and after imputation using histograms.
#=======================
hist_before <- ggplot(data, aes(x = attendance_rate)) + geom_histogram()
data$attendance_rate[is.na(data$attendance_rate)] <- mean(data$attendance_rate, na.rm = TRUE)
hist_after <- ggplot(data, aes(x = attendance_rate)) + geom_histogram()
# =========================
# Backup original data before imputation
original_data <- data

# Create a version with missing attendance_rate
before_impute <- original_data %>% 
  filter(!is.na(attendance_rate)) %>%
  mutate(stage = "Before Imputation")

# After imputation was done in Q10, so we tag the full dataset as "After"
after_impute <- data %>%
  mutate(stage = "After Imputation")

# Combine the two for plotting
combined <- bind_rows(before_impute, after_impute)

# Plot histograms side-by-side
ggplot(combined, aes(x = attendance_rate, fill = stage)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~stage) +
  labs(title = "Distribution of Attendance Rate Before and After Imputation",
       x = "Attendance Rate",
       y = "Number of Students") +
  theme_minimal()
# =========================
#12.	Detect outliers in family_income using the IQR method. How many outliers are there, and what might they represent in a Kenyan context?
# =========================
iqr <- IQR(data$family_income, na.rm = TRUE)
lower <- quantile(data$family_income, 0.25, na.rm = TRUE) - 1.5 * iqr
upper <- quantile(data$family_income, 0.75, na.rm = TRUE) + 1.5 * iqr
sum(data$family_income < lower | data$family_income > upper)
# =========================
#13.	Cap outliers in family_income at the 1.5*IQR bounds. Visualize the distribution before and after capping using boxplots.
# =========================
data$family_income_original <- data$family_income

# =========================
cap_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[x < lower_bound] <- lower_bound
  x[x > upper_bound] <- upper_bound
  return(x)
}

data$family_income_capped <- cap_outliers(data$family_income_original)

# =========================
ggplot(data, aes(y = family_income_original)) +
  geom_boxplot(fill = "coral", outlier.color = "red") +
  labs(title = "Boxplot of Family Income (Before Capping)",
       y = "Family Income (KES)") +
  theme_minimal()

# =========================
ggplot(data, aes(y = family_income_capped)) +
  geom_boxplot(fill = "seagreen", outlier.color = "darkgreen") +
  labs(title = "Boxplot of Family Income (After Capping)",
       y = "Family Income (KES)") +
  theme_minimal()
# =========================
# Function to cap outliers using the IQR method
cap_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[x < lower_bound] <- lower_bound
  x[x > upper_bound] <- upper_bound
  return(x)
}

# Apply capping to the selected variables
data$family_income <- cap_outliers(data$family_income)
data$commute_time <- cap_outliers(data$commute_time)
data$math_score <- cap_outliers(data$math_score)
# =========================
#Q14.	Discretize study_hours_weekly into four bins (e.g., Low, Moderate, High, Very High). Create a bar plot of the binned variable.
# Create binned version of study hours
data$study_hours_binned <- cut(
  data$study_hours_weekly,
  breaks = c(-Inf, 10, 15, 20, Inf),
  labels = c("Low", "Moderate", "High", "Very High"),
  right = FALSE
)

# View the count in each bin
table(data$study_hours_binned)

# Plot the distribution of binned study hours
library(ggplot2)

ggplot(data, aes(x = study_hours_binned)) +
  geom_bar(fill = "mediumseagreen") +
  labs(title = "Binned Study Hours per Week",
       x = "Study Hours Category",
       y = "Number of Students") +
  theme_minimal()
# =========================
#Q15.	Discretize family_income into quartiles (Low, Medium-Low, Medium-High, High). How does the binned variable correlate with academic_performance?
# =========================
# Bin family_income into 4 quartile-based categories
# Discretize family_income into 4 income brackets (quartiles)
data$family_income_binned <- cut(
  data$family_income,
  breaks = quantile(data$family_income, probs = seq(0, 1, 0.25), na.rm = TRUE),
  labels = c("Low", "Lower-Mid", "Upper-Mid", "High"),
  include.lowest = TRUE
)

# View the relationship using a contingency table
table(data$family_income_binned, data$academic_performance)

# Optional: Visualize using a proportional bar plot
library(ggplot2)
ggplot(data, aes(x = family_income_binned, fill = academic_performance)) +
  geom_bar(position = "fill") +
  labs(title = "Academic Performance by Family Income Bracket",
       x = "Family Income Category",
       y = "Proportion of Students") +
  theme_minimal()
# =========================
#16.	Create a new feature total_score by averaging math_score, science_score, and english_score. Visualize its distribution.
# =========================
# Create total_score by averaging math, science, and english scores
data$total_score <- rowMeans(
  data[, c("math_score", "science_score", "english_score")],
  na.rm = TRUE
)

# Quick summary of total_score
summary(data$total_score)

# Visualize the distribution with a histogram
library(ggplot2)
ggplot(data, aes(x = total_score)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Total Academic Score",
       x = "Total Score",
       y = "Number of Students") +
  theme_minimal()
#17Create a contingency table for extracurricular_activities vs. academic_performance. What patterns suggest about student involvement?

# Contingency table of extracurricular involvement vs academic performance
table(data$extracurricular_activities, data$academic_performance)

# Visualize the relationship
library(ggplot2)
ggplot(data, aes(x = extracurricular_activities, fill = academic_performance)) +
  geom_bar(position = "fill") +
  labs(title = "Academic Performance by Extracurricular Activities",
       x = "Extracurricular Activities",
       y = "Proportion of Students") +
  theme_minimal()

#18Visualize the relationship between study_hours_weekly and total_score (from Q16) using a scatter plot, colored by residency. What trends do you observe?
# =========================
# Scatter plot of study hours vs total score, colored by residency
ggplot(data, aes(x = study_hours_weekly, y = total_score, color = residency)) +
  geom_point(alpha = 0.6) +
  labs(title = "Study Hours vs. Total Score by Residency",
       x = "Weekly Study Hours",
       y = "Total Academic Score") +
  theme_minimal()
# =========================