
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
#summary <- 
dITI %>%
  mutate(
    correct = ifelse(!is.na(choice) & choice == correctlever, 1, 0),
    time = as.POSIXct(paste0(startDate, startTime, sep = ' '), 
                      format="%m/%d/%y %H:%M:%S")) %>%
  assignStay() %>%
  group_by(subject, time) %>%
  arrange(time, trial, .by_group = TRUE) %>%
  mutate(won = lag(reward)) %>%
  summarise(
    trials = length(unique(trial)),
    `%correct` = sum(correct, na.rm = T)/trials, #omission as 'incorrect'
    rewards = sum(reward, na.rm = T),
    omissions = length(choice[choice %in% -1]),
    reverals = length(which(lag(correctlever) != correctlever)),
    winstaycorrect = length(
      which(
        won == 1 & 
          stay == 1 & #omission has `stay = NA`
          lag(correct) == 1)),
    loseshiftcorrect = length(
      which(
        won == 0 &
          stay == 0 &
          lag(correct) == 1)),
    winstayincorrect = length(
      which(
        won == 1 & 
          stay == 1 &
          lag(correct) == 0)),
    loseshiftincorrect = length(
      which(
        won == 0 &
          stay == 0 &
          lag(correct) == 0))) %>%
  arrange(time, .by_group = T) %>%
  mutate(session = as.numeric(as.factor(time))) %>%
  ungroup()

# battle field ------------------------------------------------------------

sample <- dITI %>% filter(subject == 41 & startDate == " 11/18/19")
sample <- head(sample, 12)
sample$choice[c(6,10)] <- -1
sample = sample %>%
  mutate(
    correct = ifelse(!is.na(choice) & choice == correctlever, 1, 0),
    time = as.POSIXct(paste0(startDate, startTime, sep = ' '), 
                      format="%m/%d/%y %H:%M:%S")) 

(function(input, value){
  lapply(input, function(l){
    if(unique(l$subject) %in% value){l}})})(dBig, 41)
