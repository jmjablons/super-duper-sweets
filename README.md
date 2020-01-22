# usage

## import data

choose from a directory
```{r}
mydata <- import()
```
or provide paths
```{r}
mydata <- import(c("~/data/file1","~/data/file2"))
```

## transform

filter by a msn and return list

```{r}
mydata_prl <- filtermsn(dBig, "RAT_PRL_80_20_vPele_5s_ITI")
```
and then it's better to make it a data frame

```{r}
#library(dplyr)
mydata_prl = dplyr::bind_rows(mydata_prl)
```

## summarise

as you please, mine attempts are in the `user.R` file