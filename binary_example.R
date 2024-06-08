# Comparing predictor distributions in binary classification
# Deriving mean and spread for all numeric attributes grouped by binary outcome
# Custom sorting based on distributional differences
# Results information feature selection and EDA


# Write a Function to Share in R and Convert to Python
# Use the HP-universal data

hp <- read.csv("HP-Universal_DF.csv")
hp$X <- NULL

scale_to_01 <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# model to predict BN
# Create a nonlinear combination of existing attributes to determine the probability

# Set seed for reproducibility
set.seed(7)

# Create a complex binary variable
hp$BN <- ifelse(
  (hp$Age * runif(nrow(hp)) +
     hp$CC_Count^2 +
     as.numeric(hp$Gender == "female") * runif(nrow(hp)) * 5 +
     hp$ER_Count * rnorm(nrow(hp), mean = 2, sd = 1) +
     hp$Risk_Count^3) > 100,
  1, 0
)


# Creating dummy variables
hp <- hp %>%
 mutate(
   Gender = as.factor(Gender),
   ER_Copay = as.factor(ER_Copay),
   PCP_Copay = as.factor(PCP_Copay)
 ) %>%
  bind_cols(
    as.data.frame(model.matrix(~ Gender - 1, data = .)),
    as.data.frame(model.matrix(~ ER_Copay - 1, data = .)),
    as.data.frame(model.matrix(~ PCP_Copay - 1, data = .))
  ) %>%
  # Remove original columns
  dplyr::select(-Gender, -ER_Copay, -PCP_Copay)

# Fixing Column Names
colnames(hp) <- gsub("\\$", "_", names(hp))


### Defining the Function for EDA

calculate_group_stats <- function(df, group_col) {
  # Ensure that group_col is a factor
  df[[group_col]] <- as.factor(df[[group_col]])

  # Initialize an empty dataframe to store the results
  result <- data.frame(Column_Name = character(),
                       Mean_0 = numeric(),
                       Mean_1 = numeric(),
                       SD_0 = numeric(),
                       SD_1 = numeric(),
                       Relative_Difference = numeric(),
                       stringsAsFactors = FALSE)

  # Get the column names excluding the grouping column
  columns <- setdiff(names(df), group_col)

  # Loop through each column to calculate the required statistics
  for (col in columns) {
    if (is.numeric(df[[col]]) || is.integer(df[[col]])) {
      mean_0 <- mean(df[[col]][df[[group_col]] == 0], na.rm = TRUE)
      mean_1 <- mean(df[[col]][df[[group_col]] == 1], na.rm = TRUE)
      sd_0 <- sd(df[[col]][df[[group_col]] == 0], na.rm = TRUE)
      sd_1 <- sd(df[[col]][df[[group_col]] == 1], na.rm = TRUE)

      # Calculate normalized absolute difference
      normalized_difference <- (mean_1 - mean_0) / ((mean_0 + mean_1) / 2) * 100

      # Append the results to the result dataframe
      result <- rbind(result, data.frame(Column_Name = col,
                                         Mean_0 = mean_0,
                                         Mean_1 = mean_1,
                                         SD_0 = sd_0,
                                         SD_1 = sd_1,
                                         Normalized_Difference = normalized_difference))
    }
  }

  return(result)
}


calculate_group_stats(hp, 'BN') %>%
  arrange(desc(abs(Normalized_Difference)))


hp %>%
  rename(COVID_Complication = BN) %>%
  mutate(COVID_Complication = as.factor(COVID_Complication)) %>%
  ggplot(aes(x = COVID_Complication, y = CC_Count)) +
  xlab("COVID Complication") +
  ylab("Chronic Condition Count") +
  geom_boxplot()




library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggsignif)

# Customized and enhanced boxplot
hp %>%
  rename(COVID_Complication = BN) %>%
  mutate(COVID_Complication = as.factor(COVID_Complication)) %>%
  ggplot(aes(x = COVID_Complication, y = CC_Count, fill = COVID_Complication)) +
  xlab("COVID Complication") +
  ylab("Chronic Condition Count") +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.7, width = 0.2) +
  scale_fill_brewer(palette = "Set2") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  labs(
    title = "Distribution of Chronic Condition Counts\n by COVID Complication Status",
    caption = "Data Source: HP-Universal Fictional Data"
  ) +
  geom_signif(comparisons = list(c('0', "1"), c("0", "1")),
              map_signif_level = TRUE, test = "t.test")


