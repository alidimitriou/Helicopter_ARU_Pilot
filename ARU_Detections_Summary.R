# Load necessary libraries
library(dplyr)
library(ggplot2)
library(shiny) 
library(bslib)
library(shinyWidgets) 
library(shinyFiles)
library(tidyverse)
library(DT)
library(praise)
library(tuneR)

# Read the CSV files
data1 <- read.csv("~/Desktop/ARU/Data/ARU01_spbyloc_sens1.25.csv")
data2 <- read.csv("~/Desktop/ARU/Data/ARU02_spbyloc_sens1.25.csv")

# Combine the two datasets
combined_data <- bind_rows(data1, data2)

# Filter combined data for confidence >= 0.7
filtered_data <- combined_data %>%
  filter(Confidence >= 0.7)

# Summarize the number of detections by species
species_summary <- filtered_data %>%
  group_by(Common.name) %>%
  summarize(Total.Detections = n()) %>%
  arrange(desc(Total.Detections))

# Create the bar graph with horizontal bars and no y-axis line
ggplot(species_summary, aes(x = reorder(Common.name, Total.Detections), y = Total.Detections)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() + # Rotate the bar graph
  labs(
    x = "Species",
    y = "Total Detections"
  ) +
  scale_y_continuous(expand = c(0, 0)) + # Ensure y-axis starts exactly at 0
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 12),  # Keep axis titles
    panel.grid = element_blank(),          # Remove gridlines
    plot.title = element_blank(),          # Remove title
    panel.border = element_blank(),        # Remove the default border
    axis.line.x = element_line(color = "black", size = 0.8), # Keep x-axis line
    axis.line.y = element_blank()          # Remove y-axis line
  )


validation <- read.csv("~/Desktop/ARU/Heli_validation3.csv")
head(validation)

# Summarize data: Count detections for each confidence bin by validation status
validation_summary <- validation %>% 
  group_by(class, Validate) %>% 
  summarise(count = n(), .groups = "drop")

# Define the desired order of confidence bins
ordered_levels <- c("[0.0,0.1]", "(0.1,0.15]", "(0.15,0.2]", "(0.2,0.25]", "(0.25,0.3]", 
                    "(0.3,0.35]", "(0.35,0.4]", "(0.4,0.45]", "(0.45,0.5]", "(0.5,0.55]", 
                    "(0.55,0.6]", "(0.6,0.65]", "(0.65,0.7]", "(0.7,0.75]", "(0.75,0.8]", 
                    "(0.8,0.85]", "(0.85,0.9]", "(0.9,0.95]", "(0.95,1]")

library(ggplot2)

# Ensure Validate is a factor
validation_summary$Validate <- as.factor(validation_summary$Validate)

# Create the plot
ggplot(validation_summary, aes(x = class, y = count, fill = Validate)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("red", "blue")) +  # Customize colors
  labs(title = "Detection Validation by Confidence Bin",
       x = "BirdNet Confidence",
       y = "Number of Detections",
       fill = "Validation Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

# Ensure Validate is a factor
validation_summary$Validate <- as.factor(validation_summary$Validate)

# Create the plot
ggplot(validation_summary, aes(x = class, y = count, fill = Validate)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values = c("red", "blue"),  # Customize colors
    labels = c("0" = "False Positive", "1" = "True Positive")  # Rename legend labels
  ) +
  labs(
    x = "BirdNet Confidence",
    y = "Number of Detections",
    fill = "Validation Status"
  ) +
  theme_minimal(base_size = 16) +  # Increase base font size
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 18),  # Larger axis text
    axis.text.y = element_text(size = 18),
    panel.grid = element_blank(),  # Remove gridlines
    plot.title = element_blank()   # Remove title
  )


heli_glm <- glm(Validate ~ confidence, 
    data = validation, 
    family = binomial)

summary(heli_glm)

# Load required libraries
library(ggplot2)

# Create a dataframe for predicted values
new_data <- data.frame(confidence = seq(0, 1, length.out = 100))  # Confidence range
new_data$predicted_prob <- predict(heli_glm, newdata = new_data, type = "response")

# Find the maximum probability of validation
max_prob_value <- max(new_data$predicted_prob)

# Plot with a horizontal dashed red line at max validation probability
ggplot(validation, aes(x = confidence, y = Validate)) +
  geom_jitter(width = 0.02, height = 0.02, alpha = 0.3) +  # Adds some scatter to data points
  geom_line(data = new_data, aes(x = confidence, y = predicted_prob), color = "blue", size = 1) +  # Logistic curve
  geom_hline(yintercept = max_prob_value, linetype = "dashed", color = "red", size = 1) +  # Add horizontal red dashed line
  labs(
    x = "BirdNET Confidence Score",
    y = "True Positive Rate"
  ) +
  theme_minimal(base_size = 18) +  # Increase overall text size
  theme(
    panel.grid = element_blank(),  # Removes all gridlines
    axis.line = element_line(color = "black"),  # Adds x and y axis lines
    axis.ticks = element_line(color = "black"),  # Adds ticks to the axes
    axis.text.x = element_text(size = 20),  # Larger x-axis text
    axis.text.y = element_text(size = 20),  # Larger y-axis text
    axis.title.x = element_text(size = 22),  # Larger x-axis title
    axis.title.y = element_text(size = 22)   # Larger y-axis title
  )



# Load required library
library(readr)
library(dplyr)

# Read the two BirdNET output files
birdnet_1 <- read_csv("~/Desktop/ARU/ARU01_BirdNET_RTable.csv")
birdnet_2 <- read_csv("~/Desktop/ARU/ARU02_BirdNET_RTable.csv")

# Combine the two files into one dataframe
birdnet_output <- bind_rows(birdnet_1, birdnet_2)

# Check the structure of the combined dataframe
str(birdnet_output)
head(birdnet_output)


probability_data <- birdnet_output %>%
  filter(common_name == "helicopter") %>%
  mutate(probability = predict(heli_glm, newdata = ., type = "response"))  # Logistic regression predictions


# Function to calculate precision at a given threshold
threshold2precision <- function(validation, threshold) {
  precision <- validation %>%
    filter(confidence > threshold) %>%
    pull(probability) %>%
    mean(na.rm = TRUE)  # Handle NA values safely
  
  return(precision)
}

# Function to calculate proportion of data retained at a given threshold
threshold2remain <- function(validation, threshold) {
  remain <- validation %>%
    filter(confidence > threshold) %>%
    nrow()
  
  return(remain / nrow(validation))
}

# Predict probabilities for Helicopter data
heli_probability <- birdnet_output %>%
  filter(common_name == "helicopter") %>%
  mutate(probability = predict(heli_glm, newdata = ., type = "response"))

# Create a table of thresholds with precision and data retention
threshold_table <- tibble(threshold = seq(0, 1, 0.001)) %>%
  mutate(
    data_remained = map_dbl(.x = threshold, .f = ~ threshold2remain(heli_probability, .x)),
    precision = map_dbl(.x = threshold, .f = ~ threshold2precision(heli_probability, .x))
  )

threshold_table <- threshold_table %>%
  filter(!is.nan(precision))  # Remove NaN values

tail(threshold_table)

# Find intersection threshold (where precision ≈ data retention)
cross_threshold <- threshold_table %>%
  filter(abs(precision - data_remained) == min(abs(precision - data_remained))) %>%
  pull(threshold)



# Updated plot with intersection line
ggplot(threshold_table, aes(x = threshold)) +
  geom_line(aes(y = precision, color = "Precision"), size = 1) +
  geom_line(aes(y = data_remained, color = "Data Retention"), size = 1) +
  geom_vline(xintercept = cross_threshold, linetype = "dashed", color = "black", size = 1) +  # Vertical dashed line
  scale_color_manual(values = c("Precision" = "blue", "Data Retention" = "red")) +
  labs(title = "Precision and Data Retention by Confidence Threshold",
       x = "BirdNET Confidence Threshold",
       y = "Proportion",
       color = "Metric") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

selected_threshold <- 0.95  # Choose 80% confidence as the threshold
filtered_detections <- heli_probability %>%
  filter(confidence > selected_threshold)

# View the first few retained detections
head(filtered_detections)

# Proportion of detections retained
n_retained <- nrow(filtered_detections)
n_total <- nrow(heli_probability)
retention_rate <- n_retained / n_total

# Print summary
cat("Threshold:", selected_threshold, "\n",
    "Total Detections:", n_total, "\n",
    "Retained Detections:", n_retained, "\n",
    "Retention Rate:", round(retention_rate * 100, 2), "%\n")



# function to determine the threshold given specified precision level
precision2threshold <- function(threshold_table, precision){
  model <- glm(precision ~ threshold, 
               data = threshold_table,
               family = binomial)
  
  (log(precision/(1 - precision)) - model$coefficients[1])/model$coefficients[2]
  
}


# thresholds for precision = 0.9 (red), 0.95 (blue), and 0.99 (green)
t_0.9 <- precision2threshold(threshold_table, 0.60)
t_0.95 <- precision2threshold(threshold_table, 0.675)
t_0.99 <- precision2threshold(threshold_table, 0.70)


# visualization of precision vs. threshold
ggplot(threshold_table, aes(x = threshold, 
                            y = precision)) +
  geom_line(stat = "smooth",
            method = "glm", 
            se = FALSE, 
            method.args = list(family = binomial),
            linewidth = 1.5) +
 # geom_vline(xintercept = t_0.9, colour = "red", linewidth = 1.5) +
  geom_vline(xintercept = t_0.95, colour = "blue", linewidth = 1.5) +
  geom_vline(xintercept = t_0.99, colour = "darkgreen", linewidth = 1.5) +
  ylim(0, 1) +
  theme_bw() +
  labs(x = "Threshold", 
       y = "Precision")




threshold_table %>%
  filter(is.na(precision))

