# 2020-01-16 
# judyta

# util --------------------------------------------------------------------
# short functions to simplify the code

nonempty <- function(x){x[!grepl(x, pattern = "^$")]}
deblank <- function(x){gsub(x, pattern = "[[:blank:]]", replacement = "")}
# strtype <- function(x, fn = as.numeric){
#   do.call(
#     what = fn,
#     args = list(gsub(x, pattern = "[[:blank:]]", replacement = "")))}
filtermsn <- function(input, msn = "RAT_PRL_80_20_vPele_5s_ITI"){
  lapply(input, function(l){if(unique(l$msn) %in% msn){l}})}

filtersubject <- function(input, value){
  lapply(input, function(l){if(unique(l$subject) %in% value){l}})}


# import dataset ----------------------------------------------------------

intolist <-
  function(
    input,
    key = "^([[:lower:]]|[[:upper:]]|[[:blank:]])+\\:",
    sep = "\\:",
    replacement = ""){
    output <- list()
    point <- c(grep(input, pattern = key), length(input)+1)
    label <- regmatches(input, regexpr(input, pattern = key))
    label = gsub(x = label, pattern = sep, replacement = replacement)
    for(i in 1:(length(point)-1)){
      part_input <- input[(point[i]):(point[i+1]-1)]
      part_input <- gsub(x = part_input, pattern = paste0(key), 
                         replacement = replacement)
      output[[i]] <- part_input}
    names(output) <- label
    output}

converse_Z_ <-
  function(input){
    input = paste0(input, collapse = "")
    input = gsub(input, pattern = "[[:digit:]]+\\:", replacement = "")
    input = strsplit(input, "[[:blank:]]")
    as.numeric(nonempty(unlist(input)))}

converse_N_ <-
  function(input, decode = c(
    `10` = 'reward',
    `20` = 'nonreward',
    `5` = 'correct',
    `6` = 'incorrect',
    `7` = 'omission',
    `8` = 'reversal',
    `11` = 'start',
    `12` = 'end',
    `15` = 'lever')){
    input = paste0(input, collapse = "")
    input = gsub(input, pattern = "[[:digit:]]+\\:", replacement = "")
    input = strsplit(input, "[[:space:]]")
    input = as.numeric(nonempty(unlist(input)))
    length(input) <- prod(dim(matrix(input, ncol = 4))) #avoid filling up with recycling
    input = matrix(input, byrow = T, ncol = 4)
    colnames(input) = c("time","trial","correctLever","response")
    input = as.data.frame(input)
    input$label = decode[match(as.character(input$response), names(decode))]
    input}

shorten_N_ <- function(input){
  # TODO checkout for trials without proper form
  lp <- unique(input$trial)
  lever = c(1,2)
  output <- lapply(lp, function(x){
    piece <- subset(input, trial == x)
    piece = subset(piece, label != 'reversal')
    action = piece$label
    if(nrow(piece) != 0 | (length(unique(piece$trial)) > 1)){
      with(piece, {
        data.frame(
          trial = unique(trial),
          correctlever = ifelse(length(unique(correctLever)) == 1,
                                unique(correctLever), NA),
          choice = (function(x = action, y = piece){
            out <- NA
            if('omission' %in% x){out = -1}
            if('correct' %in% x){out = lever[unique(y$correctLever)]}
            if('incorrect' %in% x){out = lever[-(unique(y$correctLever))]}
            out})(),
          reward = ifelse(any(action == "reward"), 1, 0),
          latency = time[label %in% c('reward', 'nonreward', 'omission')] - 
            time[label == 'lever'], 
          stringsAsFactors = FALSE)})
    }else{
      data.frame(
        trial = NA,
        correctlever = NA,
        choice = NA,
        reward = NA,
        latency = NA)}})
  do.call(rbind, output)}

rewrite_ <- function(input){
  if(!is.null(input)){
    MSN = deblank(input$MSN)
    if(MSN == "RAT_PRL_80_20_vPele_5s_ITI"){ 
      return(with(input, {
        data.frame(
          shorten_N_(converse_N_(N)),
          msn = deblank(MSN),
          subject = as.numeric(Subject),
          group = deblank(Group),
          box = deblank(Box),
          experiment = as.numeric(Experiment),
          startDate = `Start Date`,
          startTime = `Start Time`,
          endDate = `End Date`,
          endTime = `End Time`, 
          stringsAsFactors = FALSE)}))}
    if(MSN == "progressive_ratio_RR1_cukierki"){
      return(with(input, {
        data.frame(
          forage = converse_Z_(Z),
          msn = deblank(MSN),
          subject = as.numeric(Subject),
          group = deblank(Group),
          box = deblank(Box),
          experiment = as.numeric(Experiment),
          startDate = `Start Date`,
          startTime = `Start Time`,
          endDate = `End Date`,
          endTime = `End Time`, 
          stringsAsFactors = FALSE)}))}}}

wrap <- 
  function(
    input = NULL, 
    start.character = "^(Start\\sDate)", 
    fn = function(x){rewrite_(intolist(x))},
    progress = T, ...){
    tryCatch({
      if(is.null(input)){
        input <- file.choose()}
      input = readLines(input)
      output <- list()
      start_point <- c(grep(input, pattern = start.character), length(input))
      n_session <- length(start_point)
      stopifnot(n_session > 0)
      if(progress){
        pb = txtProgressBar(min = 0, max = length(start_point)-1, initial = 0, style = 3)}
      for(i in 1:(n_session - 1)){
        if(progress){setTxtProgressBar(pb,i)}
        part_input <- input[start_point[i]:start_point[(i + 1)]]
        output[[i]] <- do.call(fn, args = list(part_input, ...))}
      if(progress){close(pb)}
      output}, 
      finally = message("done"))}

import <- function(my_dirs = NULL, print = T, ...){
  output <- list()
  if(is.null(my_dirs)){
    message("Please select proper files...")
    my_dirs = choose.files()}
  for(i in my_dirs){
    if(print){print(i)}
    output <- append(output, wrap(input = i, ...))}
  output}

# analysis ----------------------------------------------------------------

assignStay_ <- function(a){
  prevchoice = NA
  for(i in 1:nrow(a)){
    choice = a$choice[i]
    stay = NA
    if(choice != -1){
      stay = ifelse(choice == prevchoice, 1, 0)
      prevchoice = choice}
    a$stay[i] = stay}
  a}

assignStay <- function(a){
  output = list()
  i = 0
  for(s in unique(a$subject)){
    a_subject = subset(a, subject == s)
    for(t in unique(a_subject$time)){
      a_ses = subset(a_subject, time == t)
      a_ses = a_ses[order(a_ses$trial),]
      output[[(i = i + 1)]] = assignStay_(a_ses)}}
  do.call(rbind, output)}

# summary -----------------------------------------------------------------

createsummary <- function(input = dITI){
  
  temp <- input %>%
    mutate(
      correct = 
        ifelse(!is.na(choice) & choice == correctlever, 1, 0),
      time = 
        as.POSIXct(paste0(startDate, startTime, sep = ' '), 
                   format="%m/%d/%y %H:%M:%S")) %>%
    assignStay() %>%
    group_by(subject) %>%
    arrange(time, trial, .by_group = TRUE) %>%
    mutate(session = as.numeric(as.factor(time))) %>%
    ungroup()
  
  summary <- list(
    
    # all data
    brief = temp %>%
      group_by(subject, time) %>%
      arrange(time, trial, .by_group = TRUE) %>%
      summarise(
        session = unique(session),
        n = 
          length(unique(trial)),
        trials = 
          length(unique(trial[choice != -1])),
        `%correct` = 
          sum(correct, na.rm = T)/trials,
        rewards = 
          sum(reward, na.rm = T),
        omissions = 
          length(choice[choice %in% -1]),
        reverals = 
          length(which(lag(correctlever) != correctlever))) %>%
      ungroup(),
    
    # without omissions
    specific = temp %>%
      filter(choice != -1) %>% #remove all omissions
      group_by(subject, time) %>%
      arrange(time, trial, .by_group = TRUE) %>%
      mutate(
        won = 
          lag(reward)) %>%
      summarise(
        winstaycorrect = 
          length(which(won == 1 & stay == 1 & lag(correct) == 1)),
        loseshiftcorrect = 
          length(which(won == 0 & stay == 0 & lag(correct) == 1)),
        winstayincorrect = 
          length(which(won == 1 & stay == 1 & lag(correct) == 0)),
        loseshiftincorrect = 
          length(which(won == 0 & stay == 0 & lag(correct) == 0)),
        ratiowinstay = 
          length(which(won == 1 & stay == 1)) /
          length(which(won == 1 & !is.na(stay))),
        ratioloseshift= 
          length(which(won == 0 & stay == 0)) / 
          length(which(won == 0 & !is.na(stay))),
        latencycorrect = 
          mean(latency[correct == 1], na.rm = T),
        latencyincorrect = 
          mean(latency[correct == 0], na.rm = T)) %>%
      ungroup())
  
  full_join(summary$brief, summary$specific, by = c("subject", "time"))}

# save result -------------------------------------------------------------

savegently <- function(whatto = createsummary()){
  temp <- menu(c("Yes", "No"), title="Do you want to save created summary to a file?")
  if(temp == 1){
    filename = readline(prompt="Name of the file:")
    thefile = paste0(filename, ".csv")
    write.csv2(whatto, thefile)
    if(file.exists(thefile)){
      where = file.path(normalizePath(dirname(thefile)), thefile)
      message("Summary saved in the file: \n", where)}}}

# usage -------------------------------------------------------------------
# load dependencies
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')

# load custom functions
#source("R/function.R")

# import the datasets
#   returns a list of data frames
my_data <- import()

# filters by msn and transforms to data frame
data_prl <- filtermsn(my_data, "RAT_PRL_80_20_vPele_5s_ITI") %>%
  dplyr::bind_rows(.id = "sessionfile")

# run a summary
summary <- createsummary(dITI)

# message
cat("\n")
message("Summary created:")
# print the summary
print(summary)

# save results to a file
cat("\n")
savegently(summary)

# inform
cat("\n")
message("To see processed data type 'data_prl'")