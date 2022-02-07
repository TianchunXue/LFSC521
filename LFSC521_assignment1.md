Assignment 1
================
Tianchun Xue
2/6/2022

# Load library

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(titanic)
library(ggplot2)
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

# Data input and organization

I will use “titanic\_trian” dataset from the ‘titanic’ package to do the
following analysis.

In this section, I will calculate the survival rate based on different
genders.

``` r
glimpse(titanic_train)
```

    ## Rows: 891
    ## Columns: 12
    ## $ PassengerId <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…
    ## $ Survived    <int> 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1…
    ## $ Pclass      <int> 3, 1, 3, 1, 3, 3, 1, 3, 3, 2, 3, 1, 3, 3, 3, 2, 3, 2, 3, 3…
    ## $ Name        <chr> "Braund, Mr. Owen Harris", "Cumings, Mrs. John Bradley (Fl…
    ## $ Sex         <chr> "male", "female", "female", "female", "male", "male", "mal…
    ## $ Age         <dbl> 22, 38, 26, 35, 35, NA, 54, 2, 27, 14, 4, 58, 20, 39, 14, …
    ## $ SibSp       <int> 1, 1, 0, 1, 0, 0, 0, 3, 0, 1, 1, 0, 0, 1, 0, 0, 4, 0, 1, 0…
    ## $ Parch       <int> 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 1, 0, 0, 5, 0, 0, 1, 0, 0, 0…
    ## $ Ticket      <chr> "A/5 21171", "PC 17599", "STON/O2. 3101282", "113803", "37…
    ## $ Fare        <dbl> 7.2500, 71.2833, 7.9250, 53.1000, 8.0500, 8.4583, 51.8625,…
    ## $ Cabin       <chr> "", "C85", "", "C123", "", "", "E46", "", "", "", "G6", "C…
    ## $ Embarked    <chr> "S", "C", "S", "S", "S", "Q", "S", "S", "S", "C", "S", "S"…

``` r
gender_survive <- titanic_train %>%
  group_by(Sex) %>%
  summarise(Survived.count = sum(Survived))  %>%
  mutate(Total.count = table(titanic_train$Sex)) %>%
  mutate(Survival.rate = Survived.count/Total.count)
```

``` r
gender_survive <- as.data.frame(gender_survive)
gender_survive
```

    ##      Sex Survived.count Total.count Survival.rate
    ## 1 female            233         314     0.7420382
    ## 2   male            109         577     0.1889081

From the table, we can know that 233 female passengers survived out of
all 314 female passengers, and only 109 male passengers survived out of
all 577 male passengers.

# Data visualization

I will visualize the survival rate by gender by “ggplot2”

First, I will convert “integer” into “factor”, for later filling the
gender groups with different survival states.

``` r
titanic_train$Survived <- as.factor(titanic_train$Survived)
```

Then, I will generate a bar plot to show the survival rates of different
gender.

``` r
ggplot(data= titanic_train,aes(x=Sex, fill=Survived))+
  geom_bar()+
  scale_fill_brewer(palette = 'Paired')+
  annotate(geom="text", x= 1, y=250, label=percent(gender_survive[1,4]),
              color="white")+
  annotate(geom="text", x= 2, y=130, label=percent(gender_survive[2,4]),
              color="white")+
  labs(y = "Number of Passengers", title = "Survival Rate by Gender")+
  theme_classic()
```

![](LFSC521_assignment1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Here, “0” of survival states refers to death, and “1” means survival. We
can see the x-axis contains two groups, female and male. And the y-axis
is the number of passengers. There are more male passengers than female
passengers, but the survival rate of male passengers (19%) is much lower
than that of female passengers (74%).
