# Project2

### Author: Jason M. Pattison, Student, NCSU ST 588-601, SUM I 2024

### Date: 2024-07-09

## The application and it's purpose

> The purpose of this app is to allow the user to summarize federal department spending information of ten department agencies as reported by [USASpending.gov](https://www.usaspending.gov/).

## R Packages required

> There are six R packages required to run this app are `tidyverse`, `readr`, `ggplot2`, `dplyr`, `scales`, and `treemapify`.

## Package Installation

> It is strongly recommended that you copy and paste the below code block and run it in your R Console to ensure the app works properly.

```{r}
  
install.packages(tidyverse)
install.packages(readr)
install.packages(ggplot2)
install.packages(dplyr)
install.packages(scales)
install.packages(treemapify)
install.packages(shinydashboard)

```

## Operating the App

> Once the above packages are installed, you can access the app by copying the below line of code and running it in R.

```{r}
  
  shiny::runGitHub("Project2", "jpatt1980", subdir = "/Project2")
  
```
