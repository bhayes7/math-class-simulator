# Presentation Plots

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# Plotting gathered study habits data

studyData <- read_xlsx("data/student_study_data.xlsx", col_names = TRUE) %>%
  mutate(dailyStudyProb = `Days of Study Per Week` / 7)

studyPlot <- ggplot(data = studyData) +
  geom_histogram(mapping = aes(x = dailyStudyProb, y = ..density..), binwidth = 0.1) +
  geom_density(mapping = aes(x = dailyStudyProb), color = "blue", size = 1.5) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(studyData$dailyStudyProb), sd = sd(studyData$dailyStudyProb)), 
                color = "red", size = 1.5) +
  labs(title = "Density Distribution of Daily Study Probabilities", 
       x = "Probability of Studying per Day", y = "Density") +
  theme_bw()

# Plotting quiz performance distributions
