
# usage -------------------------------------------------------------------
# load dependencies
# read whats going on here
# load data [import()]:
#   run the line and choose files from a file tree
#   or: provide vector of paths to files as an arg to the function and run
# decide on how to transform the resultss
# run a summary
# save results to a file (function not written, do you need it?)

# run ---------------------------------------------------------------------
# first step - import and parse data
#   calls WIN windows

# reads a single file
#   returns a list of data frames
#dData <- wrap()

# reads chosen files
#   returns a list of data frames
dBig <- import()

# dependency
library(dplyr)

# filters by msn and transforms to data frame
#   %>% is a pipe, a stream, to avoid temporary variables
dITI <- filtermsn(dBig, "RAT_PRL_80_20_vPele_5s_ITI") %>%
  dplyr::bind_rows(.id = "sessionfile")

#View(dITI)

# analysis ----------------------------------------------------------------
# mail:
# rewards, 
# %correct, 
# reversals, 
# omissions, 
# Win.Stay.Correct, 
# Lose.Shift.Correct, 
# Latency.Correct, 
# Win.Stay.InCorrect, 
# Lose.Shift.InCorrect,
# Latency.InCorrect, 
# ulamek.Win.Stay, 
# ulamek.Lose.Shift

# dependency
library(dplyr)

# summary
summary <- 
dITI %>%
  mutate(
    correct = ifelse(!is.na(choice) & choice == correctlever, 1, 0),
    time = as.POSIXct(paste0(startDate, startTime, sep = ' '), 
                      format="%m/%d/%y %H:%M:%S")) %>%
  group_by(subject, time) %>%
  arrange(time, trial, .by_group = TRUE) %>%
    summarise(
    trials = length(unique(trial)),
    `%correct` = sum(correct, na.rm = T)/trials, #omission as 'incorrect'
    rewards = sum(reward, na.rm = T),
    omissions = length(choice[choice %in% -1]),
    reverals = length(which(lag(correctlever) != correctlever))) %>%
  arrange(time, .by_group = T) %>%
  mutate(session = as.numeric(as.factor(time))) %>%
  ungroup()
  
View(summary)


# battle field ------------------------------------------------------------

sample <- dITI %>% filter(subject == 3 & startDate == " 12/04/19")
sample <- head(sample, 12)
sample$choice[c(6,10)] <- -1

(function(a){
prevchoice = NA
for(i in 1:nrow(a)){
  choice = a$choice[i]
  stay = NA
  if(choice != -1){
    stay = ifelse(choice == prevchoice, 1, 0)
    prevchoice = choice}
  a$stay[i] = stay}
a})(sample)

sample$won <- lag(sample$reward) # poprzedni

(function(input, value){
  lapply(input, function(l){
    if(unique(l$subject) %in% value){l}})})(dBig, 40)
