# Main Simulation function
library(purrr) # for rbernoulli
library(magrittr) # for pipe
library(readxl)
library(dplyr)

# USER DEFINED INPUTS
numStudents <- 1
freePointsFrac <- 0.5 # fraction of points awarded automatically for turning in an assignment

# OTHER CONSTANTS
classesPerWeek <- 3 # unnecessary variable, included for future development
numWeeks <- 16
examFrequency <- 2 # exams occur on the last day of this many weeks (must divide numWeeks)
                   # TODO include an option for "total exams"

# student behavior distributions
studyData <- read_xlsx("data/student_study_data.xlsx", col_names = TRUE) %>%
  mutate(dailyStudyProb = `Days of Study Per Week` / 7)

studyMean <- mean(studyData$dailyStudyProb)
studySD <- sd(studyData$dailyStudyProb)

studyProb <- function(){return(rnorm(1, mean = studyMean, sd = studySD))}
attendProb <- function(dayOfWeek){
  if (dayOfWeek %in% c(0, 2, 4, 6)){return(0)}
  else{return(0.95)}
}

# CALENDAR VARIABLES
days <- 1:(numWeeks * 7)
daysOfWeek <- days %% 7 # assume 0 is Sunday, 1 is Monday, 2 is Tuesday, etc...

# specify when exams occur
examDays <- lapply(1:numWeeks, function(week){
  if (week %% examFrequency == 0){
    return(c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  }
  else{
    return(rep(FALSE, times = 7))
  }
}) %>%
  unlist() %>%
  tail(n = length(examDays) - 1) %>% # fix indexing to begin on Monday
  c(FALSE)

# specify when quizzes occur
quizDays <- lapply(days, function(day){
  return((day %% 7) %in% c(1, 3, 5))
}) %>%
  unlist() %>%
  xor(examDays) # quizzes don't occur on exam days

# PERFORMANCE MEASUREMENTS

# function for ensuring performance metrics are within acceptable bounds
gradeCap <- function(performance){
  return(max(freePointsFrac, min(performance, 1)))
}

# FOR EXAMS

# TODO create this distribution file
examPerformanceDistribution <- read_xlsx("data/exam_performance_distribution.xlsx")

examPerformanceMap <- function(numAttendDays, numStudyDays){
  row <- filter(examPerformanceDistribution, (attendDays == numAttendDays & studyDays == numStudyDays))
  mean <- row$mean
  sd <- row$sd
  return(c(mean, sd))
}

examPerformance(day, studyDays, attendDays){
  # student gets no points if they aren't there to take the quiz!
  if (!attendDays[day]){return(0)}
  else{
    # check whether this is the first exam
    if (day == first(which(examDays))){lastExamDay <- 0}
    # determine when the last exam day was
    else{lastExamDay <- last(which(examDays[1:(day - 1)]))}
    
    # count study and attendance days
    numStudyDays <- sum(studyDays[(lastExamDay + 1):day])
    numAttendDays <- sum(attendDays[(lastExamDay + 1):day])
    
    # generate performance
    performanceDist <- examPerformanceMap(numAttendDays, numStudyDays)
    performanceMean <- performanceDist[1]
    performanceSD <- performanceDist[2]
    performance <- round(gradeCap(rnorm(1, mean = performanceMean, sd = performanceSD)), digits = 1)
    
    return(performance)
  }
}
  
# FOR QUIZZES

quizPerformanceDistribution <- read_xlsx("data/quiz_performance_distribution.xlsx")

quizPerformanceMap <- function(attend, numStudyDays){
  row <- filter(quizPerformanceDistribution, (attendPrev == attend & studyDays == numStudyDays))
  mean <- row$mean
  sd <- row$sd
  return(c(mean, sd))
}

quizPerformance <- function(day, studyDays, attendDays){
  # student gets no points if they aren't there to take the quiz!
  if (!attendDays[day]){return(0)}
  # everyone who attends gets 100% on first quiz
  if (day == 1){return(1)}
  # otherwise, calculate quiz performance
  else{
    # determine whether student attended previous class
    lastQuizDay <- last(which(quizDays[1:(day - 1)]))
    attend <- attendDays[lastQuizDay]
    
    # count # of days student has studied since last quiz
    numStudyDays <- sum(studyDays[(lastQuizDay + 1):day])
    
    # calculate performance
    performanceDist <- quizPerformanceMap(attend, numStudyDays)
    performanceMean <- performanceDist[1]
    performanceSD <- performanceDist[2]
    performance <- round(gradeCap(rnorm(1, mean = performanceMean, sd = performanceSD)), digits = 1)
    
    return(performance)
  }
}

# in A, A-, B+, B, B-,... order; must have 11 elements
gradeCutoffs <- c(92, 90, 88, 82, 80, 78, 72, 70, 68, 62, 0)

# define grade mapping
# TODO finish this, it's not complete obviously
gradeMap <- function(performance, gradeCutoffs){
  lettergrades <- c("A", "A-")
  cutoffsDec <- gradeCutoffs / 100
  earnedCutoff <- first(which(performance >= cutoffsDec))
  
}

# generate assignment performances for a single student
studentPerformance <- function(gradeMap){
  # generate student's study and attendance habits; these will determine grades
  studyDays <- replicate(numWeeks * 7, rbernoulli(1, studyProb()))
  attendDays <- sapply(daysOfWeek, function(day){return(rbernoulli(1, attendProb(day)))})
  
  # determine quiz and exam performances
  quizPerformances <- sapply(quizDays, quizPerformance, studyDays, attendDays)
  examPerformances <- sapply(examDays, examPerformance, studyDays, attendDays)
  
  # combine performances into dataframe
  labels <- c(rep("quizPerformance", times = length(quizPerformances)),
              rep("examPerformance", times = length(examPerformances)))
  numbers <- c(1:length(quizPerformances), 1:length(examPerformances))
  performances <- c(quizPerformances, examPerformances)
  data <- data.frame(labels, numbers, performances)
  colnames(data) <- c("evalType", "evalNumber", "value")
  return(data)
}
  
simulateClass(numStudents){
  
}  

