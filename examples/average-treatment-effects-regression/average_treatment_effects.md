-   [Data](#data)
-   [Average Treatment Effect](#average-treatment-effect)
-   [Weights](#weights)
-   [Imbalance](#imbalance)
    -   [Model with Interaction](#model-with-interaction)

Data
====

Example shown in Business Data Science pg 134

``` r
library(foreign)

descr <- read.dta("data/OHIE_Public_Use_Files/OHIE_Data/oregonhie_descriptive_vars.dta")
prgm <- read.dta("data/OHIE_Public_Use_Files/OHIE_Data/oregonhie_stateprograms_vars.dta")
s12 <- read.dta("data/OHIE_Public_Use_Files/OHIE_Data/oregonhie_survey12m_vars.dta")

#head(descr)
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
========================

ybar is the percent of people with `doc_any_12m == 1` in the
`selected == 0` (control) and `selected == 1` (treatment) groups.

ATE is the difference of the percent doc\_any\_12m in the treatment vs
control

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
(standard_errors <- sqrt(sum(yvar / num_sample)))
```

    ## [1] 0.006364705

``` r
ATE
```

    ##         1 
    ## 0.0570134

``` r
# 95% confidence interval
ATE + (c(-2, 2) * standard_errors)
```

    ## [1] 0.04428399 0.06974281

``` r
lm(doc_any_12m ~ selected, data=P) %>% tidy()
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)   0.576    0.00449    128.   0.      
    ## 2 selected      0.0570   0.00637      8.96 3.60e-19

Note: `Intercept` is the average value of `doc_any_12m` when
`selected = 0`, which is the same as `ybar['0']` above. `selected`
coefficient matches `ATE`.

Weights
=======

In the example, the data conteight weights because of the imbalance i
the data vs the population (e.g. perhaps young people are harder to
reach for a survey)

instead of `num_sample` we have `num_selected_weighted`

instead of `ybar` we have `ybar_weighted`

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

``` r
lm(doc_any_12m ~ selected, weights=weight, data=P) %>% tidy()
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)   0.574    0.00451    127.   0.      
    ## 2 selected      0.0555   0.00638      8.71 3.28e-18

Now, `Intercept` equals `ybar_weighted['0']`, and `selected` coefficient
equals `ATE_weighted`

Imbalance
=========

A similar scenario, ignoring the population imbalance, is that we have
an imballance in the data due to how people are selected, from the
control to the treatment: the family members of the people `selected`
are also selected. So that families of 2, and 3+ will be overrepresented
in the `selected` group. We don’t simply want to group by household
because we want it at the individual level, and each individual in the
household may or may not visit doc in the 12 month period. (Actually the
author does do this later on by averaging doc\_any\_12m per household
id)

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

Now, we can control for `numhh` by including it in the model.

``` r
model_simple <- lm(doc_any_12m ~ selected + numhh, data=P)
summary(model_simple)
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
get_regression_equation(model_simple, .round_by = 4)
```

    ## [1] "doc_any_12m = 0.5925(Intercept) + 0.0633selected + -0.0655numhh2 + -0.1838numhh3+ + error"

``` r
# the intercept is equivalent to single person household not being selected
# i.e. doc_any_12m = 0.5925(Intercept) + 0.0633*0 + -0.0655*0 + -0.1838*0
predict(model_simple, newdata = data.frame(selected=0, numhh='1'))
```

    ##         1 
    ## 0.5925453

``` r
coef(model_simple)['(Intercept)']
```

    ## (Intercept) 
    ##   0.5925453

``` r
predict(model_simple, newdata = data.frame(selected=1, numhh='1'))
```

    ##         1 
    ## 0.6558835

``` r
coef(model_simple)['(Intercept)'] + coef(model_simple)['selected']
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
new_data$predictions <- predict(model_simple, newdata = new_data)
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
coef(model_simple)['selected']
```

    ##   selected 
    ## 0.06333824

``` r
as.numeric(ATE - coef(model_simple)['selected'])
```

    ## [1] -0.00000000000000001387779

`ATE` which is weighted based on the population `numhh` equals
`coef(model_simple)['selected']`, i.e. we are controlling for the
imbalance in `numhh`.

Model with Interaction
----------------------

The author mentions we have to center `numhh`. This is so that the
`selected` coefficient is relative to the average value `numhh` and
interactions. It makes the `selected` coefficient more interpretable.

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
model_interaction <- lm(doc_any_12m ~ selected * X, data=P)
summary(model_interaction)
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
get_regression_equation(model_interaction, .round_by = 4)
```

    ## [1] "doc_any_12m = 0.5729(Intercept) + 0.0637selected + -0.0518Xnumhh2 + -0.4225Xnumhh3+ + -0.0253selected:Xnumhh2 + 0.2628selected:Xnumhh3+ + error"

The coefficient of `selected` is very close, but different than the
model with no interactions.

``` r
coefficients(model_simple)['selected']
```

    ##   selected 
    ## 0.06333824

``` r
coefficients(model_interaction)['selected']
```

    ##  selected 
    ## 0.0636798

But we didn’t center when we did `selected + numhh`.

It turns out the models would have been been identical, with the
exception of the Intercept. This is because centering doesn’t change the
effect of selected or numhh, but now, the coefficient for the Intercept
corresponds to the average doc\_any\_12 value for the average numhh
household. (before the Intercept was the average doc\_any\_12 value for
numhh == 1 households). So, in the model without interactions, centering
doesn’t change selected because selected is the same (i.e. same slope)
at every value of numhh.

``` r
summary(model_simple)
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
summary(lm(doc_any_12m ~ selected + X, data=P))
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

What if we hadn’t centered `numhh` in the interaction?

``` r
model_interaction_non_centered <- lm(doc_any_12m ~ selected * numhh, data=P)
summary(model_interaction_non_centered)
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
coef(model_interaction)['selected']
```

    ##  selected 
    ## 0.0636798

``` r
coef(model_interaction_non_centered)['selected']
```

    ##   selected 
    ## 0.07050829

Before, in the original model with no interaction, the `selected`
coefficient was the predicted difference in `selected` vs `not selected`
across all `numhh` values (i.e. same slope for each group).

For the model with the interaction, without centering, difference
between a `selected` vs `not selected` with the same `numhh` depends on
what the `numhh` is. **So the coefficient for `selected` is the average
difference between a `selected` vs `not selected` person who both have
`numhh1` (1 person in the household), since that is the
reference/holdout group.**

For the model with the interaction, with centering, **the coefficient
for `selected` is the average difference between a `selected` vs
`not selected` person at the average value of `numhh`**

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
new_data$predictions <- predict(model_interaction_non_centered, newdata = new_data)
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
coef(model_interaction)['selected'] 
```

    ##  selected 
    ## 0.0636798

In summary, if we take the predictions from the non-centered model (with
interactions), and weight them according to the `numhh` frequencies, we
get the same coefficient that is in the `centered` model (with
interactions).

<a href="https://www3.nd.edu/~rwilliam/stats2/l53.pdf" class="uri">https://www3.nd.edu/~rwilliam/stats2/l53.pdf</a>
can explain more on centering & interactions.

It does,

> -   The coefficient for male (3.55) is now the average difference
>     between a male with an average gpa and a female with an average
>     gpa. This is probably more meaningful than looking at the
>     difference between the nonexistent man and woman who are flunking
>     everything.

So translating the `coef(model_interaction)['selected']` `0.0636798` is
the average difference between selected with average numhh vs not
selected with average numhh
