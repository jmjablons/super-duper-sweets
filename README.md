
![](https://github.com/jmjablons/super-duper-sweets/blob/master/readme/super_duper_sweets_usage_rstudio.gif)

## How it works

First the raw files are provided to the `import()` function and being transformed and preprocessed to the R objects - lists and data frames (tibbles).
Then the summary analysis is conducted and the final result saved.

The analysis part requires the `dplyr` package, which is automatically installed if needed.

The easiest way to make the tool work is to call `source`, which loads all the included functions and performs the analysis with default settings.

```{r}
source("R/useme.R")
```

## Usage

### Import

It's possible to choose files from a directory

```{r}
mydata_list <- import()
```
or provide paths.

```{r}
mydata_list <- import(c("~/data/file1","~/data/file2"))
```
The `import` function returns a list of data frames, where each element of the list is a single session's result.

### Transform

Loaded dataset must be filtered by a `msn` variable, as each scheme has different output.

```{r}
mydata_list <- filtermsn(mydata_list, "RAT_PRL_ITI")
```

A list of data frames is returned, so it's convenient to combine it into single data frame e.x. with `dplyr::bind_rows(mydata)`.

```{r}
my_data_df <- dplyr::bind_rows(mydata_list)
```

### Summarise

Summary is done on the final object `my_data_df` since there are great, fast and easy to use methods to handle the df type. 
It also enables saving the whole dataset in one file for further analyses.

Summary is done separately as desribed in the `analysis` section of the `useme.R` file.

### Save

Writing the summary isn't done fully automatically, as user must provide the name of an output file. It protects from accidentally overwriting existing result files.

