# 2020-01-16 
# judyta

# usage -------------------------------------------------------------------
# run all [ctrl+alt+r]

# notebook ----------------------------------------------------------------
# spotted errors
# err1: bush in the file '!2019-12-04' - 
#   not able to read more than 18th session
# err2: subject 78 start 17:30:sth repeated 0's - file of '!2019-11-18'
#   means no action was ever done
#   it's problematic for the transformation, but dplyr::bind_rows handles it
# err3: looking for it

# dependencies ------------------------------------------------------------
# for basic import and parse
#   none #yeah, I'm good.
#
# for the further analysis
library(dplyr)
#   it's fast and easy to use
#   as all the tidyverse tools

# util --------------------------------------------------------------------
# short functions to simplify the code

nonempty <- function(x){x[!grepl(x, pattern = "^$")]}
deblank <- function(x){gsub(x, pattern = "[[:blank:]]", replacement = "")}
# strtype <- function(x, fn = as.numeric){
#   do.call(
#     what = fn,
#     args = list(gsub(x, pattern = "[[:blank:]]", replacement = "")))}
filtermsn <- function(input, msn = "RAT_PRL_80_20_vPele_5s_ITI"){
  lapply(input, function(l){
    if(unique(l$msn) %in% msn){l}})}

# function ----------------------------------------------------------------
# custom functions
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
  if(!is.null(input))
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
        stringsAsFactors = FALSE)}))}}

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

import <- function(my_dirs = NULL, print = T){
  output <- list()
  if(is.null(my_dirs)){my_dirs = choose.files()}
  for(i in my_dirs){
    if(print){print(i)}
    output <- append(output, wrap(i))}
  output}