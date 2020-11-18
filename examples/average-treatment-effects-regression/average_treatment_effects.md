-   [Data](#data)
-   [Weights](#weights)
-   [Regression](#regression)
-   [Full Interaction](#full-interaction)

Data
====

Example shown in Business Data Science pg 134

``` r
library(foreign)

descr <- read.dta("data/OHIE_Public_Use_Files/OHIE_Data/oregonhie_descriptive_vars.dta")
prgm <- read.dta("data/OHIE_Public_Use_Files/OHIE_Data/oregonhie_stateprograms_vars.dta")
s12 <- read.dta("data/OHIE_Public_Use_Files/OHIE_Data/oregonhie_survey12m_vars.dta")

head(descr)
```

    ##   person_id household_id    treatment                             draw_treat
    ## 1         1       100001     Selected Draw 7: selected in lottery 08/01/2008
    ## 2         2       100002     Selected Draw 6: selected in lottery 07/01/2008
    ## 3         3       100003 Not selected                                   <NA>
    ## 4         4       100004 Not selected                                   <NA>
    ## 5         5       100005     Selected Draw 7: selected in lottery 08/01/2008
    ## 6         6       100006     Selected Draw 3: selected in lottery 04/08/2008
    ##     draw_lottery                          applied_app approved_app
    ## 1 Lottery Draw 7      Submitted an Application to OHP           No
    ## 2 Lottery Draw 6 Did NOT submit an application to OHP           No
    ## 3 Lottery Draw 2                                 <NA>         <NA>
    ## 4 Lottery Draw 8                                 <NA>         <NA>
    ## 5 Lottery Draw 7 Did NOT submit an application to OHP           No
    ## 6 Lottery Draw 3      Submitted an Application to OHP          Yes
    ##   dt_notify_lottery dt_retro_coverage dt_app_decision postn_death
    ## 1        2008-08-12        2008-09-08      2008-12-31       Alive
    ## 2        2008-07-14        2008-08-08            <NA>       Alive
    ## 3        2008-04-07        2008-04-08            <NA>       Alive
    ## 4        2008-09-11        2008-10-08            <NA>       Alive
    ## 5        2008-08-12        2008-09-08            <NA>       Alive
    ## 6        2008-04-16        2008-05-08      2008-06-16       Alive
    ##       numhh_list birthyear_list   have_phone_list
    ## 1 signed self up           1978 Gave Phone Number
    ## 2 signed self up           1984 Gave Phone Number
    ## 3 signed self up           1971 Gave Phone Number
    ## 4 signed self up           1955 Gave Phone Number
    ## 5 signed self up           1969 Gave Phone Number
    ## 6 signed self up           1946 Gave Phone Number
    ##                                         english_list female_list
    ## 1                        Requested English materials     0: Male
    ## 2                        Requested English materials   1: Female
    ## 3                        Requested English materials   1: Female
    ## 4                        Requested English materials   1: Female
    ## 5 Requested materials in language other than english   1: Female
    ## 6                        Requested English materials     0: Male
    ##                                  first_day_list
    ## 1 Did NOT sign up for lottery list on first day
    ## 2 Did NOT sign up for lottery list on first day
    ## 3 Did NOT sign up for lottery list on first day
    ## 4 Did NOT sign up for lottery list on first day
    ## 5 Did NOT sign up for lottery list on first day
    ## 6 Did NOT sign up for lottery list on first day
    ##                                  last_day_list   pobox_list      self_list
    ## 1 Did NOT sign up for lottery list on last day     1: POBOX Signed self up
    ## 2 Did NOT sign up for lottery list on last day 0: Not POBOX Signed self up
    ## 3 Did NOT sign up for lottery list on last day 0: Not POBOX Signed self up
    ## 4 Did NOT sign up for lottery list on last day 0: Not POBOX Signed self up
    ## 5 Did NOT sign up for lottery list on last day 0: Not POBOX Signed self up
    ## 6 Did NOT sign up for lottery list on last day 0: Not POBOX Signed self up
    ##   week_list                   zip_msa_list
    ## 1    Week 2 Zip code of residence in a MSA
    ## 2    Week 3 Zip code of residence in a MSA
    ## 3    Week 3 Zip code of residence in a MSA
    ## 4    Week 1 Zip code of residence in a MSA
    ## 5    Week 2 Zip code of residence in a MSA
    ## 6    Week 4 Zip code of residence in a MSA

``` r
#head(prgm)
#head(s12)


# nicely organized, one row per person
stopifnot(all(s12$person_id == descr$person_id))
stopifnot(all(s12$person_id == prgm$person_id))

P <- descr[,c("person_id","household_id", "numhh_list")]
P <- P %>% rename(numhh = numhh_list)
P$medicaid <- as.numeric(prgm[,"ohp_all_ever_firstn_30sep2009"]=="Enrolled")
P$selected <- as.numeric(descr[,"treatment"]=="Selected")
P$weight <- s12$weight_12m
P$doc_any_12m <- as.numeric(s12$doc_any_12m == "Yes")
levels(P$numhh) <- c("1","2","3+")

P <- P[complete.cases(P),]
head(P)
```

    ##   person_id household_id numhh medicaid selected weight doc_any_12m
    ## 1         1       100001     1        0        1      1           0
    ## 2         2       100002     1        1        1      1           0
    ## 5         5       100005     1        0        1      1           0
    ## 6         6       100006     1        0        1      1           1
    ## 8         8       102094     2        0        0      1           0
    ## 9         9       100009     1        0        0      1           1

Average Treatment Effect

``` r
(ybar <- tapply(P$doc_any_12m, P$selected, mean))
```

    ##         0         1 
    ## 0.5759034 0.6329168

``` r
(ATE <- ybar['1'] - ybar['0'])
```

    ##         1 
    ## 0.0570134

``` r
(num_sample <- table(P$selected))
```

    ## 
    ##     0     1 
    ## 11844 11684

``` r
(yvar <- tapply(P$doc_any_12m, P$selected, var))
```

    ##         0         1 
    ## 0.2442593 0.2323530

``` r
(starndard_errors <- sqrt(sum(yvar / num_sample)))
```

    ## [1] 0.006364705

``` r
ATE + (c(-2, 2) * starndard_errors)
```

    ## [1] 0.04428399 0.06974281

Weights
=======

``` r
(num_selected_weighted <- tapply(P$weight, P$selected, sum))
```

    ##        0        1 
    ## 14414.61 14456.53

``` r
(num_doc_any_12_weighted <- tapply(P$weight * P$doc_any_12m, P$selected, sum))
```

    ##        0        1 
    ## 8280.966 9107.804

``` r
(ybar_weighted <- num_doc_any_12_weighted / num_selected_weighted)
```

    ##         0         1 
    ## 0.5744842 0.6300129

``` r
(ATE_weighted <- ybar_weighted['1'] - ybar_weighted['0'])
```

    ##          1 
    ## 0.05552873

Regression
==========

We have an imballance in the data, from the control to the treatment:
the family members of the people `selected` are also selected. So that
families of 2, and 3+ will be overresented in the `selected` group. We
don’t simply want to group by household because we want it at the
individual level, and each individual in the household may or may not
visit doc in the 12 month period. (Actually the author does do this
later on by averaging doc\_any\_12m per household id)

``` r
table(P$selected, P$numhh)
```

    ##    
    ##        1    2   3+
    ##   0 8844 2994    6
    ##   1 7679 3953   52

``` r
table(P$selected, P$numhh) / as.numeric(table(P$selected))
```

    ##    
    ##                1            2           3+
    ##   0 0.7467071935 0.2527862209 0.0005065856
    ##   1 0.6572235536 0.3383259158 0.0044505306

``` r
reg_results <- lm(doc_any_12m ~ selected + numhh, data=P)
summary(reg_results)
```

    ## 
    ## Call:
    ## lm(formula = doc_any_12m ~ selected + numhh, data = P)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.6559 -0.5926  0.3441  0.4074  0.5913 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)  0.592545   0.004815 123.071 < 0.0000000000000002 ***
    ## selected     0.063338   0.006387   9.916 < 0.0000000000000002 ***
    ## numhh2      -0.065465   0.006999  -9.353 < 0.0000000000000002 ***
    ## numhh3+     -0.183814   0.064151  -2.865              0.00417 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4873 on 23524 degrees of freedom
    ## Multiple R-squared:  0.00736,    Adjusted R-squared:  0.007233 
    ## F-statistic: 58.14 on 3 and 23524 DF,  p-value: < 0.00000000000000022

``` r
get_regression_equation(reg_results, .round_by = 4)
```

    ## [1] "doc_any_12m = 0.5925(Intercept) + 0.0633selected + -0.0655numhh2 + -0.1838numhh3+ + error"

``` r
# the intercept is equivalent to single person household not being selected
# i.e. doc_any_12m = 0.5925(Intercept) + 0.0633*0 + -0.0655*0 + -0.1838*0
predict(reg_results, newdata = data.frame(selected=0, numhh='1'))
```

    ##         1 
    ## 0.5925453

``` r
coef(reg_results)['(Intercept)']
```

    ## (Intercept) 
    ##   0.5925453

``` r
predict(reg_results, newdata = data.frame(selected=1, numhh='1'))
```

    ##         1 
    ## 0.6558835

``` r
coef(reg_results)['(Intercept)'] + coef(reg_results)['selected']
```

    ## (Intercept) 
    ##   0.6558835

Lets generate predictions for every combination, take the differences in
predictions for the treatment minus the control, and then take the
weighted average of those predictions. That value should equal the value
of the `selected` coefficient.

``` r
new_data <- data.frame(selected=c(1, 1, 1, 0, 0, 0),
                       numhh=c('1', '2', '3+', '1', '2', '3+'))
new_data$predictions <- predict(reg_results, newdata = new_data)
new_data
```

    ##   selected numhh predictions
    ## 1        1     1   0.6558835
    ## 2        1     2   0.5904181
    ## 3        1    3+   0.4720695
    ## 4        0     1   0.5925453
    ## 5        0     2   0.5270799
    ## 6        0    3+   0.4087312

This weighted average of the predictions make sense since the `selected`
coefficient is the expected difference across each numhh value (since
there is no interaction terms i.e. same slope)

``` r
# diff the selected == 1 from the selected == 0
# the predictions will have the same diff value
# because (since we aren't interacting the terms) the selected coefficient
# is the expected difference across ALL values of numhh (i.e. same slope, same difference between lines at all values)
# but, we will do the same thing below for the model with interactions
(diffs <- new_data$predictions[1:3] - new_data$predictions[4:6])
```

    ## [1] 0.06333824 0.06333824 0.06333824

``` r
# do this across control and treatment
numhh_frequency <- table(P$numhh) / nrow(P)

# weighted average
(ATE <- sum(diffs * numhh_frequency))
```

    ## [1] 0.06333824

``` r
coef(reg_results)['selected']
```

    ##   selected 
    ## 0.06333824

``` r
as.numeric(ATE - coef(reg_results)['selected'])
```

    ## [1] -0.00000000000000001387779

Full Interaction
================

The author mentions we have to center `numhh`

`scale=FALSE` means we will only center the data rather than
center/scale

``` r
X <- scale(model.matrix(~numhh, data=P)[, -1], scale=FALSE)
# data centered around 0
colMeans(X)
```

    ##                        numhh2                       numhh3+ 
    ##  0.00000000000000000104519891 -0.00000000000000000001736346

``` r
reg_results_full <- lm(doc_any_12m ~ selected * X, data=P)
summary(reg_results_full)
```

    ## 
    ## Call:
    ## lm(formula = doc_any_12m ~ selected * X, data = P)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.6597 -0.5892  0.3403  0.4108  0.8333 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)        0.572875   0.004516 126.865 < 0.0000000000000002 ***
    ## selected           0.063680   0.006395   9.957 < 0.0000000000000002 ***
    ## Xnumhh2           -0.051805   0.010302  -5.029          0.000000498 ***
    ## Xnumhh3+          -0.422546   0.198977  -2.124               0.0337 *  
    ## selected:Xnumhh2  -0.025321   0.014039  -1.804               0.0713 .  
    ## selected:Xnumhh3+  0.262825   0.210209   1.250               0.2112    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4872 on 23522 degrees of freedom
    ## Multiple R-squared:  0.007567,   Adjusted R-squared:  0.007356 
    ## F-statistic: 35.87 on 5 and 23522 DF,  p-value: < 0.00000000000000022

``` r
get_regression_equation(reg_results_full, .round_by = 4)
```

    ## [1] "doc_any_12m = 0.5729(Intercept) + 0.0637selected + -0.0518Xnumhh2 + -0.4225Xnumhh3+ + -0.0253selected:Xnumhh2 + 0.2628selected:Xnumhh3+ + error"

``` r
coefficients(reg_results_full)['selected']
```

    ##  selected 
    ## 0.0636798

But we didn’t scale when we did `selected + numhh`

``` r
reg_results_full2 <- lm(doc_any_12m ~ selected + X, data=P)
summary(reg_results_full2)
```

    ## 
    ## Call:
    ## lm(formula = doc_any_12m ~ selected + X, data = P)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.6559 -0.5926  0.3441  0.4074  0.5913 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)  0.572762   0.004489 127.588 < 0.0000000000000002 ***
    ## selected     0.063338   0.006387   9.916 < 0.0000000000000002 ***
    ## Xnumhh2     -0.065465   0.006999  -9.353 < 0.0000000000000002 ***
    ## Xnumhh3+    -0.183814   0.064151  -2.865              0.00417 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4873 on 23524 degrees of freedom
    ## Multiple R-squared:  0.00736,    Adjusted R-squared:  0.007233 
    ## F-statistic: 58.14 on 3 and 23524 DF,  p-value: < 0.00000000000000022

But turns out if we had we would have gotten the same coefficient.

It’s because the original regression coefficient says that at the
**every value of numhh (i.e. same number of people in the household)**,
`selected` increases the `doc_any_12m` by `0.06333824`

``` r
coef(reg_results)['selected']
```

    ##   selected 
    ## 0.06333824

``` r
coef(reg_results_full2)['selected']
```

    ##   selected 
    ## 0.06333824

What if we hadn’t scaled `numhh`?

``` r
reg_results_full_non_scaled <- lm(doc_any_12m ~ selected * numhh, data=P)
summary(reg_results_full_non_scaled)
```

    ## 
    ## Call:
    ## lm(formula = doc_any_12m ~ selected * numhh, data = P)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.6597 -0.5892  0.3403  0.4108  0.8333 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)       0.589213   0.005181 113.727 < 0.0000000000000002 ***
    ## selected          0.070508   0.007600   9.278 < 0.0000000000000002 ***
    ## numhh2           -0.051805   0.010302  -5.029          0.000000498 ***
    ## numhh3+          -0.422546   0.198977  -2.124               0.0337 *  
    ## selected:numhh2  -0.025321   0.014039  -1.804               0.0713 .  
    ## selected:numhh3+  0.262825   0.210209   1.250               0.2112    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4872 on 23522 degrees of freedom
    ## Multiple R-squared:  0.007567,   Adjusted R-squared:  0.007356 
    ## F-statistic: 35.87 on 5 and 23522 DF,  p-value: < 0.00000000000000022

``` r
coef(reg_results_full)['selected']
```

    ##  selected 
    ## 0.0636798

``` r
coef(reg_results_full_non_scaled)['selected']
```

    ##   selected 
    ## 0.07050829

Before, the `selected` coefficient was the predicted difference in in
selected vs not selected across all `numhh` values. Now, however, we’ve
added an interaction term and, as a result, the difference between a
selected vs not selected with the same `numhh` depends on what the
`numhh` is. **So the coefficient for `selected` is the predicted
difference between a selected vs not selected person who both have
`numhh1` (1 person in the household).**

> The implication is that, once you add interaction effects, the main
> effects may or may not be particularly interesting, at least as they
> stand, and you should be careful in how you interpret them… Once
> interaction terms are added, you are primarily interested in their
> significance, rather than the significance of the terms used to
> compute them.
> <a href="https://www3.nd.edu/~rwilliam/stats2/l53.pdf" class="uri">https://www3.nd.edu/~rwilliam/stats2/l53.pdf</a>

But, if we take this non-centered model, and calculate difference
between selected and non-selected using weighted average of frequencies
of numhh, then we get the coeffient value of the centered data/model

``` r
new_data <- data.frame(selected=c(1, 1, 1, 0, 0, 0),
                       numhh=c('1', '2', '3+', '1', '2', '3+'))
```

``` r
new_data$predictions <- predict(reg_results_full_non_scaled, newdata = new_data)
```

``` r
diffs <- new_data$predictions[1:3] - new_data$predictions[4:6]
# do this across control and treatment
numhh_frequency <- table(P$numhh) / nrow(P)
# weighted average
(ATE <- sum(diffs * numhh_frequency))
```

    ## [1] 0.0636798

``` r
coef(reg_results_full)['selected'] 
```

    ##  selected 
    ## 0.0636798

``` r
coef(reg_results_full_non_scaled)['selected']
```

    ##   selected 
    ## 0.07050829

``` r
(coef(reg_results_full_non_scaled)['selected'] - ATE) / ATE
```

    ##  selected 
    ## 0.1072317

``` r
(coef(reg_results_full)['selected'] - ATE) / ATE
```

    ##                selected 
    ## -0.00000000000005600821

Now there is a difference, and the `ATE` of the weighted average
non-centered is pretty much identical to the selected coefficient of the
full/centered.

Perhaps
<a href="https://www3.nd.edu/~rwilliam/stats2/l53.pdf" class="uri">https://www3.nd.edu/~rwilliam/stats2/l53.pdf</a>
can explain

It does,

> -   The coefficient for male (3.55) is now the average difference
>     between a male with an average gpa and a female with an average
>     gpa. This is probably more meaningful than looking at the
>     difference between the nonexistent man and woman who are flunking
>     everything.

So translating the `coef(reg_results_full)['selected']` 0.0636798 is the
average difference between selected with average numhh vs not selected
with average numhh
