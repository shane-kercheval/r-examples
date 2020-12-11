-   [Overview](#overview)
-   [Resources](#resources)
-   [Packages](#packages)
    -   [statistical packages](#statistical-packages)
    -   [base packages](#base-packages)
    -   [Settings](#settings)
-   [Chapter 1 - Overview](#chapter-1---overview)
    -   [Simple Example with `stan_glm`](#simple-example-with-stan_glm)
-   [Chapter 3 - Basic Methods](#chapter-3---basic-methods)
    -   [Log-Log Interpretation](#log-log-interpretation)
        -   [Example Model](#example-model)
        -   [Interpretation](#interpretation)
            -   [Interpretation from
                `Regression and Other Stores`](#interpretation-from-regression-and-other-stores)
            -   [Interpretation from
                `Introductory Econometrics 7e`](#interpretation-from-introductory-econometrics-7e)
        -   [Predictions](#predictions)
        -   [Simulation of Log-Log
            Predictions](#simulation-of-log-log-predictions)
-   [Chapter 4 - Statistcal
    Inference](#chapter-4---statistcal-inference)
    -   [Confidence Interval of
        Proportion](#confidence-interval-of-proportion)
-   [Chapter 5 - Simulation](#chapter-5---simulation)
    -   [How many girls in 400 births?](#how-many-girls-in-400-births)
    -   [Simulation of continuous and mixed discrete/continuous
        models](#simulation-of-continuous-and-mixed-discretecontinuous-models)
    -   [Median Absolute Deviation
        (`MAD SD`)](#median-absolute-deviation-mad-sd)
-   [Chapter 9 - Prediction and Bayesian
    Inference](#chapter-9---prediction-and-bayesian-inference)
    -   [Point Prediction](#point-prediction)
    -   [Linear Predictor with
        Uncertainty](#linear-predictor-with-uncertainty)
    -   [Predictive Distribution](#predictive-distribution)

Overview
========

This document includes examples and exercises from
`Regression and Other Stories` by Andrew Gelman, Jennifer Hill and Aki
Vehtari, 2020, first edition.

Resources
=========

[Errata](https://avehtari.github.io/ROS-Examples/errata.html)

[Authors’
Code](https://avehtari.github.io/ROS-Examples/examples.html#Examples_by_chapters)

Packages
========

statistical packages
--------------------

``` r
library(rstan)
library(rstanarm)
library(bayesplot)
library(loo)
```

base packages
-------------

``` r
library(broom)
library(broom.mixed)

library(tidyverse)
library(ggplot2)
library(scales)
library(ggridges)
```

Settings
--------

``` r
theme_set(theme_light())
options(scipen=999) # non-scientific notation
options(dplyr.summarise.inform=F)
```

Chapter 1 - Overview
====================

Simple Example with `stan_glm`
------------------------------

Load in the data:

``` r
head(hibbs)
```

    ##   year growth  vote inc_party_candidate other_candidate
    ## 1 1952   2.40 44.60           Stevenson      Eisenhower
    ## 2 1956   2.89 57.76          Eisenhower       Stevenson
    ## 3 1960   0.85 49.91               Nixon         Kennedy
    ## 4 1964   4.21 61.34             Johnson       Goldwater
    ## 5 1968   3.02 49.60            Humphrey           Nixon
    ## 6 1972   3.62 61.79               Nixon        McGovern

------------------------------------------------------------------------

Graph data:

``` r
hibbs %>%
    ggplot(aes(x=growth, y=vote)) +
    geom_hline(yintercept = 50, color="red", linetype="dashed") +
    geom_text(aes(label=year)) +
    geom_smooth(method='lm') +
    scale_x_continuous(labels=function(.x)paste0(.x, '%')) +
    scale_y_continuous(labels=function(.x)paste0(.x, '%')) +
    labs(title="Forecasting the election from the economy",
         y="Incuming party's vote share",
         x="Average recent growth in personal",
         caption='Figure 1.1')
```

![](Regression-and-Other-Stories_files/figure-markdown_github/figure.1.1.a-1.png)

------------------------------------------------------------------------

Build Model:

``` r
model <- stan_glm(vote ~ growth, data=hibbs)
```

    ## 
    ## SAMPLING FOR MODEL 'continuous' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 6.7e-05 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.67 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0.032231 seconds (Warm-up)
    ## Chain 1:                0.024812 seconds (Sampling)
    ## Chain 1:                0.057043 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL 'continuous' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 9e-06 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.09 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 0.02849 seconds (Warm-up)
    ## Chain 2:                0.026077 seconds (Sampling)
    ## Chain 2:                0.054567 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL 'continuous' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 9e-06 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.09 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 0.030518 seconds (Warm-up)
    ## Chain 3:                0.025418 seconds (Sampling)
    ## Chain 3:                0.055936 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL 'continuous' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 8e-06 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.08 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 0.028083 seconds (Warm-up)
    ## Chain 4:                0.033358 seconds (Sampling)
    ## Chain 4:                0.061441 seconds (Total)
    ## Chain 4:

------------------------------------------------------------------------

Model Summary:

``` r
summary(model)
```

    ## 
    ## Model Info:
    ##  function:     stan_glm
    ##  family:       gaussian [identity]
    ##  formula:      vote ~ growth
    ##  algorithm:    sampling
    ##  sample:       4000 (posterior sample size)
    ##  priors:       see help('prior_summary')
    ##  observations: 16
    ##  predictors:   2
    ## 
    ## Estimates:
    ##               mean   sd   10%   50%   90%
    ## (Intercept) 46.3    1.7 44.2  46.3  48.3 
    ## growth       3.1    0.7  2.1   3.1   3.9 
    ## sigma        4.0    0.8  3.1   3.9   5.0 
    ## 
    ## Fit Diagnostics:
    ##            mean   sd   10%   50%   90%
    ## mean_PPD 52.0    1.5 50.2  52.0  53.9 
    ## 
    ## The mean_ppd is the sample average posterior predictive distribution of the outcome variable (for details see help('summary.stanreg')).
    ## 
    ## MCMC diagnostics
    ##               mcse Rhat n_eff
    ## (Intercept)   0.0  1.0  3179 
    ## growth        0.0  1.0  3278 
    ## sigma         0.0  1.0  2524 
    ## mean_PPD      0.0  1.0  3542 
    ## log-posterior 0.0  1.0  1733 
    ## 
    ## For each parameter, mcse is Monte Carlo standard error, n_eff is a crude measure of effective sample size, and Rhat is the potential scale reduction factor on split chains (at convergence Rhat=1).

------------------------------------------------------------------------

Print()

``` r
print(model)
```

    ## stan_glm
    ##  family:       gaussian [identity]
    ##  formula:      vote ~ growth
    ##  observations: 16
    ##  predictors:   2
    ## ------
    ##             Median MAD_SD
    ## (Intercept) 46.3    1.6  
    ## growth       3.1    0.7  
    ## 
    ## Auxiliary parameter(s):
    ##       Median MAD_SD
    ## sigma 3.9    0.7   
    ## 
    ## ------
    ## * For help interpreting the printed output see ?print.stanreg
    ## * For info on the priors used see ?prior_summary.stanreg

------------------------------------------------------------------------

Model Coefficients:

``` r
coef(model)
```

    ## (Intercept)      growth 
    ##   46.258159    3.051908

------------------------------------------------------------------------

Compare to `lm()`:

``` r
summary(lm(vote ~ growth, data=hibbs))
```

    ## 
    ## Call:
    ## lm(formula = vote ~ growth, data = hibbs)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.9929 -0.6674  0.2556  2.3225  5.3094 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value           Pr(>|t|)    
    ## (Intercept)  46.2476     1.6219  28.514 0.0000000000000841 ***
    ## growth        3.0605     0.6963   4.396            0.00061 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.763 on 14 degrees of freedom
    ## Multiple R-squared:  0.5798, Adjusted R-squared:  0.5498 
    ## F-statistic: 19.32 on 1 and 14 DF,  p-value: 0.00061

(pretty close)

------------------------------------------------------------------------

Graph using model coefficients rather than `geom_smooth(method='lm')`

``` r
hibbs %>%
    ggplot(aes(x=growth, y=vote)) +
    geom_hline(yintercept = 50, color="red", linetype="dashed") +
    geom_text(aes(label=year)) +
    #geom_smooth(method='lm') +
    geom_abline(intercept = coef(model)['(Intercept)'], slope = coef(model)['growth']) +
    scale_x_continuous(labels=function(.x)paste0(.x, '%')) +
    scale_y_continuous(labels=function(.x)paste0(.x, '%')) +
    labs(title="Forecasting the election from the economy",
         y="Incuming party's vote share",
         x="Average recent growth in personal",
         caption='Figure 1.1')
```

![](Regression-and-Other-Stories_files/figure-markdown_github/figure.1.1.a2-1.png)

------------------------------------------------------------------------

Chapter 3 - Basic Methods
=========================

Log-Log Interpretation
----------------------

Example from \`Regression & Other Stories pg 39. (Data found elsewhere.)

### Example Model

``` r
metabolic_rate <- read_csv("data/Primate Body Mass and Basal Metabolic Rate.csv")
head(metabolic_rate)
```

    ## # A tibble: 6 x 2
    ##   `Primate Mass` `Metabolic Rate`
    ##            <dbl>            <dbl>
    ## 1           4670            11.6 
    ## 2           1020             2.56
    ## 3            206             0.73
    ## 4            190             0.86
    ## 5            105             0.55
    ## 6            300             1.1

``` r
metabolic_rate %>%
    ggplot(aes(x=`Primate Mass`, y=`Metabolic Rate`)) +
    geom_point() +
    geom_text(data=metabolic_rate, 
              aes(x=`Primate Mass`, y=`Metabolic Rate`,
                  label=glue::glue("({ `Primate Mass` }, { `Metabolic Rate` })"),
                  color=NULL),
              vjust=-0.5, check_overlap = TRUE) +
    labs(title="Primate Body Mass And Basal Metabolic Rate",
         subtitle = "(Not Scaled)")
```

![](Regression-and-Other-Stories_files/figure-markdown_github/chapter_3_log_log_not_scaled-1.png)

``` r
metabolic_rate %>%
    ggplot(aes(x=`Primate Mass`, y=`Metabolic Rate`)) +
    geom_point() +
    geom_text(data=metabolic_rate, 
              aes(x=`Primate Mass`, y=`Metabolic Rate`,
                  label=glue::glue("({ `Primate Mass` }, { `Metabolic Rate` })"),
                  color=NULL),
              vjust=-0.5, check_overlap = TRUE) +
    scale_x_log10() +
    scale_y_log10() +
    labs(title="Primate Body Mass And Basal Metabolic Rate",
         subtitle = "Log Scaled")
```

![](Regression-and-Other-Stories_files/figure-markdown_github/chapter_3_log_log_scaled-1.png)

``` r
model <- lm(log(`Metabolic Rate`) ~ log(`Primate Mass`), data=metabolic_rate)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = log(`Metabolic Rate`) ~ log(`Primate Mass`), data = metabolic_rate)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.71136 -0.13959  0.01586  0.14175  0.57990 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value        Pr(>|t|)    
    ## (Intercept)         -4.05787    0.28770  -14.11 0.0000000004613 ***
    ## log(`Primate Mass`)  0.74160    0.04198   17.66 0.0000000000189 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2983 on 15 degrees of freedom
    ## Multiple R-squared:  0.9541, Adjusted R-squared:  0.9511 
    ## F-statistic:   312 on 1 and 15 DF,  p-value: 0.00000000001891

``` r
coef(model)
```

    ##         (Intercept) log(`Primate Mass`) 
    ##           -4.057868            0.741598

### Interpretation

#### Interpretation from `Regression and Other Stores`

From `pg. 40`

“For example, when increasing body mass on this curve by a factor of 2,
metabolic rate is multiplied by `2^0.74` = `1.7`. Multiplgying body mass
by `10` corresponds to multiplying metabolic rate by `10^0.74` = `5.5`,
and so forth.”

``` r
(primate_mass_coefficient <- unname(coef(model)['log(`Primate Mass`)']))
```

    ## [1] 0.741598

Lets test this. We’ll start with a body mass of `105`.

``` r
(log_prediction_105 <- predict(model, newdata = data.frame(`Primate Mass`=105, check.names = FALSE)))
```

    ##       1 
    ## -0.6065

``` r
(prediction_105 <- exp(log_prediction_105))
```

    ##         1 
    ## 0.5452559

Our prediction of the metabolic rate is `0.5452559`.

What is our prediction if we **double** the body mass?

We would expect a Metabolic Rate of `0.9116825`.

``` r
2^primate_mass_coefficient * prediction_105
```

    ##         1 
    ## 0.9116825

What is the actual prediction if we double the body mass?

``` r
log_prediction_210 <- predict(model, newdata = data.frame(`Primate Mass`=210, check.names = FALSE))
(prediction_210 <- exp(log_prediction_210))
```

    ##         1 
    ## 0.9116825

------------------------------------------------------------------------

What is our prediction if we multiply body mass by `10`?

We would expect a Metabolic Rate of `3.0074496`.

``` r
10^primate_mass_coefficient * prediction_105
```

    ##       1 
    ## 3.00745

What is the actual prediction if we double the body mass?

``` r
log_prediction_1050 <- predict(model, newdata = data.frame(`Primate Mass`=1050, check.names = FALSE))
(prediction_1050 <- exp(log_prediction_1050))
```

    ##       1 
    ## 3.00745

------------------------------------------------------------------------

#### Interpretation from `Introductory Econometrics 7e`

> "A one-unit difference in `log x` corresponds to an additive
> difference of `b` in `log y`. (R&OS pg. 39)

This is essentially “elasticity” and is interpreted as a `1%` change in
x has a `b`% change in `y` (Intro Econometrics pg. 39), although this
does not hold for large changes in `y` (Intro Econometrics pg. 186).

``` r
format_percent <- function(.x) { paste0(round(.x, 3), '%')}
format_percent_scale <- function(.x) { paste0(round(.x * 100, 3), '%')}

(expected_one_percent_change <- format_percent(primate_mass_coefficient))
```

    ## [1] "0.742%"

So, the interpretation of the `log(Primate Mass)` coefficient is that a
`1%` change in `Primate Mass` results in a `~0.742%` change in
`Metabolic Rate`.

Let’s see if this is true.

If the `Primate Mass` is `105` then our prediction of
`log(Metabolic Rate`) is `-0.6065`, which means our prediction of
`Metabolic Rate` is `0.5452559`, which matches the point on the graph.

Now let’s change `Primate Mass` (x) by `1%` and see how much
`Metabolic Rate` (y) changes by.

``` r
(one_percent_change <- (0.01 * 105) + 105)
```

    ## [1] 106.05

``` r
one_percent_change_prediction <- predict(model, newdata = data.frame(`Primate Mass`=one_percent_change, check.names = FALSE))
(one_percent_change_prediction <- exp(one_percent_change_prediction))
```

    ##         1 
    ## 0.5492943

So if `Primate Mass` is `106.05` then we predict that `Metabolic Rate`
is `0.5492943`.

So a `1%` change in `Primate Mass` resulted in the following percent
change for `Metabolic Rate`:

``` r
format_percent_scale((one_percent_change_prediction - prediction_105) / prediction_105)
```

    ## [1] "0.741%"

Which matches the expected percent change from the coefficient.

``` r
primate_mass_coefficient
```

    ## [1] 0.741598

------------------------------------------------------------------------

Now, lets try with a larger change.

Lets predict `Metabolic Change` for a `Primate Mass` of `9,500`

``` r
(percent_change <- (9500 - 105) / 105)
```

    ## [1] 89.47619

The percent-change in `Primate Mass` between these two values is
`8947.619%`.

``` r
(expected_percent_change_y <- as.numeric(coef(model)["log(`Primate Mass`)"]) * percent_change)
```

    ## [1] 66.35536

Therefore, if `Primate Mass` changes by `8947.619%`, the **expected**
percent change in `Metabolic Rate` should be `6635.536%`.

But the actual percent change is:

``` r
(prediction_9500 <- exp(predict(model, newdata = data.frame(`Primate Mass`=9500, check.names = FALSE))))
```

    ##        1 
    ## 15.40147

``` r
format_percent_scale((prediction_9500 - prediction_105) / prediction_105)
```

    ## [1] "2724.632%"

Which is quite lower than what we expected.

So this rule of thumb doesn’t work across large changes in `y`.

### Predictions

According to `Introductory Econometrics, Wooldridge (pg. 206)`,

> Because the exponential undoes the log, our first guess for predicting
> y (when dependent variable is log(y)) is to simply exponentiate the
> predicted value for `log(y): y_predicted = exp(log y_predicted))`.
> **This does not work; in fact, it will systematically underestimate
> the expected value of y**.

One adjustment that the author describes is the
`Duan smearing estimate`.

> Given an esimtate `a0`, we can predict `y` as
>
> `y_predicted = a0 * exp(log y_predicted)`

where,

> `a0 = sum(exp(residuals)) / N`

The code below implements the `Duan smearing estimate`.

Note, that this adjustment necessary, not just for `log y = log x`
models, but for `log y = x` models (any time the dependent variable is
log y, regardless of X).

``` r
#' This method returns the adjustment coefficient described in `Introductory Econometrics, Wooldridge (pg. 206)`
#' @param .model the regression model
duan_smearing_adjustment <- function(.model) {
    model_residuals <- residuals(.model)
    duan_smearing_coefficient <- sum(exp(model_residuals)) / length(model_residuals)
    
    return (duan_smearing_coefficient)
}

#' This method returns the prediction of `y`, when the dependent variable is `log y`, based on the adjustment coefficient described in `Introductory Econometrics, Wooldridge (pg. 206)`
#' @param .model the regression model
#' @param .newdata the data (e.g. test data) from which predictions will be generated
predict_from_log_y <- function(.model, .newdata) {
    
    duan_smearing_coefficient <- duan_smearing_adjustment(.model)
    .predictions <- predict(.model, newdata=.newdata)
    .predictions <- duan_smearing_coefficient*exp(.predictions)

    return (.predictions)
}
```

Now, let’s create predictions for all `Primate Masses` between `50` and
`80000`, which is around the interval of our data.

``` r
all_predictions <- data.frame(`Primate Mass`=seq(50, 80000), check.names = FALSE)
log_y_predictions <- predict(model, newdata=all_predictions)

# this simple adjustment is also described on pg 206 but assumes normal residuals
all_predictions$`Predictions - Unadjusted` <- exp(log_y_predictions)
all_predictions$`Predictions - Adjusted - Simple` <- exp(((summary(model)$sigma)^2)/2)*exp(log_y_predictions)
all_predictions$`Predictions - Adjusted - Duan` <- predict_from_log_y(.model=model, .newdata=all_predictions)
head(all_predictions)
```

    ##   Primate Mass Predictions - Unadjusted Predictions - Adjusted - Simple
    ## 1           50                0.3145163                       0.3288212
    ## 2           51                0.3191693                       0.3336858
    ## 3           52                0.3237987                       0.3385258
    ## 4           53                0.3284052                       0.3433418
    ## 5           54                0.3329892                       0.3481343
    ## 6           55                0.3375514                       0.3529040
    ##   Predictions - Adjusted - Duan
    ## 1                     0.3266032
    ## 2                     0.3314350
    ## 3                     0.3362423
    ## 4                     0.3410258
    ## 5                     0.3457860
    ## 6                     0.3505236

``` r
predictions_long <- all_predictions %>% 
    # get the original values, where applicable
    #left_join(metabolic_rate, by = 'Primate Mass') %>%
    pivot_longer(-`Primate Mass`, names_to = 'Prediction Type', values_to='Metabolic Rate')

metabolic_rate_plot <- predictions_long %>%
    ggplot(aes(x=`Primate Mass`, y=`Metabolic Rate`, color=`Prediction Type`)) +
    geom_point(data=metabolic_rate, aes(x=`Primate Mass`, y=`Metabolic Rate`, color=NULL)) +
    geom_text(data=metabolic_rate, 
              aes(x=`Primate Mass`, y=`Metabolic Rate`,
                  label=glue::glue("({ `Primate Mass` }, { `Metabolic Rate` })"),
                  color=NULL),
              vjust=-0.5, check_overlap = TRUE) +
    geom_line()

metabolic_rate_plot +
    scale_x_log10() +
    scale_y_log10() +
    labs(title="Primate Body Mass And Basal Metabolic Rate",
         subtitle = "Log Scaled")
```

![](Regression-and-Other-Stories_files/figure-markdown_github/chapter_3_log_log_scaled_regression-1.png)

``` r
metabolic_rate_plot +
    labs(title="Primate Body Mass And Basal Metabolic Rate",
         subtitle = "Unscaled")
```

![](Regression-and-Other-Stories_files/figure-markdown_github/chapter_3_log_log_unscaled_regression-1.png)

------------------------------------------------------------------------

### Simulation of Log-Log Predictions

See discussion in previous section regarding systematic underprediction
of `y` when dependent varaible is `log y`.

`log y = a + b*log x + random-noise`

``` r
set.seed(1)
intercept <- 4
b_coefficient <- 3
simulated_x <- rexp(100000, rate = 0.5) + 1
noise <- rnorm(100000, sd = 0.5)

simulated_y <- exp(intercept + b_coefficient*log(simulated_x) + noise)

simulated_data <- data.frame(simulated_x, simulated_y)
simulated_data %>%
    ggplot(aes(x=simulated_x, y=simulated_y)) +
    geom_point(alpha=0.1) +
    scale_x_log10(breaks=2^seq(0,6)) +
    scale_y_log10(breaks=10^seq(1,6)) +
    labs(title="Simulated Data (`log y = a + b*log x + random-noise`)",
         subtitle = "Graphed on a log-scale.")
```

![](Regression-and-Other-Stories_files/figure-markdown_github/unnamed-chunk-29-1.png)

------------------------------------------------------------------------

Create training/test sets, fit model on training data.

``` r
training_indices <- sample.int(n=nrow(simulated_data), size = 0.7*nrow(simulated_data), replace = FALSE)
training_data <- simulated_data[training_indices,]
test_data <- simulated_data[-training_indices,]
model <- lm(log(simulated_y) ~ log(simulated_x), data=training_data)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = log(simulated_y) ~ log(simulated_x), data = training_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.1152 -0.3373 -0.0019  0.3348  2.1345 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value            Pr(>|t|)    
    ## (Intercept)      4.001214   0.003572  1120.1 <0.0000000000000002 ***
    ## log(simulated_x) 2.999279   0.003273   916.3 <0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.499 on 69998 degrees of freedom
    ## Multiple R-squared:  0.923,  Adjusted R-squared:  0.923 
    ## F-statistic: 8.396e+05 on 1 and 69998 DF,  p-value: < 0.00000000000000022

------------------------------------------------------------------------

Predict on test data.

``` r
log_y_predictions <- predict(model, newdata=test_data)

# this simple adjustment is also described on pg 206 but assumes normal residuals
test_data$`Predictions - Unadjusted` <- exp(log_y_predictions)
test_data$`Predictions - Adjusted - Simple` <- exp(((summary(model)$sigma)^2)/2)*exp(log_y_predictions)
test_data$`Predictions - Adjusted - Duan` <- predict_from_log_y(.model=model, .newdata=test_data)
head(test_data)
```

    ##    simulated_x simulated_y Predictions - Unadjusted
    ## 2     3.363286   3187.9213                2077.8609
    ## 6     6.789937  25807.5974               17088.4407
    ## 8     2.079366    495.4461                 491.2115
    ## 10    1.294092    110.9351                 118.4459
    ## 12    2.524060    700.3071                 878.4439
    ## 17    4.752070   3923.7267                5859.5674
    ##    Predictions - Adjusted - Simple Predictions - Adjusted - Duan
    ## 2                        2353.3864                     2354.6418
    ## 6                       19354.3771                    19364.7015
    ## 8                         556.3464                      556.6431
    ## 10                        134.1518                      134.2234
    ## 12                        994.9260                      995.4568
    ## 17                       6636.5492                     6640.0894

------------------------------------------------------------------------

``` r
predictions_long <- test_data %>%
    select(-simulated_y) %>%
    # get the original values, where applicable
    #left_join(metabolic_rate, by = 'Primate Mass') %>%
    pivot_longer(-simulated_x, names_to = 'Prediction Type', values_to='Predicted Y')

simulated_plot <- predictions_long %>%
    ggplot(aes(x=simulated_x, y=`Predicted Y`, color=`Prediction Type`)) +
    geom_point(data=test_data, aes(x=simulated_x, y=simulated_y, color=NULL), alpha=0.2) +
    geom_line()

simulated_plot +
    scale_x_log10(breaks=2^seq(0,6)) +
    scale_y_log10(breaks=10^seq(1,6)) +
    labs(title="Simulated Data & Predictions",
         subtitle = "Log Scaled",
         caption="The 'Simple' and 'Duan' adjustments are slow close that they overlap,\nand the Duan is being hidden behind the Simple.")
```

![](Regression-and-Other-Stories_files/figure-markdown_github/chapter_3_log_log_scaled_simulation-1.png)

------------------------------------------------------------------------

``` r
simulated_plot +
    labs(title="Simulated Data & Predictions",
         subtitle = "Not Scaled",
         caption="The 'Simple' and 'Duan' adjustments are slow close that they overlap,\nand the Duan is being hidden behind the Simple.")
```

![](Regression-and-Other-Stories_files/figure-markdown_github/chapter_3_log_log_unscaled_simulation-1.png)

------------------------------------------------------------------------

The adjustments seem to make sense, visualy; but we can check the
`Root Mean Square Error` (`RMSE`) to see if the adjustments actually
results in a better **out-of-sample** fit, below (which it appears is
the case; there is a lower error for the adjusted predictions).

``` r
rmse = function(fitted, actual){
  sqrt(mean((fitted - actual)^2))
}

rmse(fitted=test_data$`Predictions - Unadjusted`, actual=test_data$simulated_y)
```

    ## [1] 8968.512

``` r
rmse(fitted=test_data$`Predictions - Adjusted - Simple`, actual=test_data$simulated_y)
```

    ## [1] 8546.481

``` r
rmse(fitted=test_data$`Predictions - Adjusted - Duan`, actual=test_data$simulated_y)
```

    ## [1] 8545.582

NOTE: the “Simple” adjustment (found in
`Introductory Econometrics, Wooldridge (pg. 206)`) is only valid for
residuals that are normally distributed. In this simulation, the
residuals are normally distributed; and the Simple and Duan adjustments
are quite similar.

------------------------------------------------------------------------

We can do another performance check using `R-Squared`, which is a
measure of the percent of variance in the dependent variable (y) that is
explained by the model. The higher the R-squared, the better.

``` r
#' these functions are from `Business Data Science, Taddy, pg 72`
#' @param y the actual y values
#' @param pred the predictions; must be probabilities (0<pred<1) for binomial
deviance <- function(y, pred, family=c("gaussian","binomial")){
    family <- match.arg(family)
    if(family=="gaussian"){
        return( sum( (y-pred)^2 ) )
    }else{
        if(is.factor(y)) y <- as.numeric(y)>1
        return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
    }
}
#' these functions are from `Business Data Science, Taddy, pg 72`
#' returns r-squared which is a measure of the percent of variance in the dependent variable (y) that is explained by the model.
#' @param y the actual y values
#' @param pred the predictions; must be probabilities (0<pred<1) for binomial
R2 <- function(y, pred, family=c("gaussian","binomial")){
    fam <- match.arg(family)
    if(fam=="binomial"){
        if(is.factor(y)){ y <- as.numeric(y)>1 }
    }
    dev <- deviance(y, pred, family=fam)
    dev0 <- deviance(y, mean(y), family=fam)
    return(1-dev/dev0)
}
```

We can see that the adjusted predictions do have a higher `R-Squared`
than the unadjsuted.

``` r
R2(pred=test_data$`Predictions - Unadjusted`, y=test_data$simulated_y)
```

    ## [1] 0.782397

``` r
R2(pred=test_data$`Predictions - Adjusted - Simple`, y=test_data$simulated_y)
```

    ## [1] 0.8023946

``` r
R2(pred=test_data$`Predictions - Adjusted - Duan`, y=test_data$simulated_y)
```

    ## [1] 0.8024362

Chapter 4 - Statistcal Inference
================================

Confidence Interval of Proportion
---------------------------------

``` r
sample_size <- 1000
respond_yes <- 700

(estimate <- respond_yes / sample_size)
```

    ## [1] 0.7

``` r
(standard_error <- sqrt(estimate * (1 - estimate) / sample_size))
```

    ## [1] 0.01449138

``` r
(z_scores <- qnorm(c(0.025, 0.975)))
```

    ## [1] -1.959964  1.959964

``` r
(confidence_interval_95 <- estimate + (z_scores * standard_error))
```

    ## [1] 0.6715974 0.7284026

------------------------------------------------------------------------

Compare to R’s `prop.test`.

``` r
prop.test(x=respond_yes, n=sample_size)
```

    ## 
    ##  1-sample proportions test with continuity correction
    ## 
    ## data:  respond_yes out of sample_size, null probability 0.5
    ## X-squared = 159.2, df = 1, p-value < 0.00000000000000022
    ## alternative hypothesis: true p is not equal to 0.5
    ## 95 percent confidence interval:
    ##  0.6703646 0.7280778
    ## sample estimates:
    ##   p 
    ## 0.7

Chapter 5 - Simulation
======================

How many girls in 400 births?
-----------------------------

``` r
probability_girl <- 0.488
sample_size <- 400
# one random sample (simulated)
number_of_simulated_samples <- 1
set.seed(1)
rbinom(n = number_of_simulated_samples, size=sample_size, prob = probability_girl)
```

    ## [1] 196

``` r
number_of_simulated_samples <- 1000
set.seed(1)
simulations <- rbinom(n = number_of_simulated_samples, size=sample_size, prob = probability_girl)
simulations[1:10]
```

    ##  [1] 196 195 184 213 198 183 192 168 178 196

95% Interval

``` r
quantile(simulations, c(0.025, 0.975))
```

    ##  2.5% 97.5% 
    ##   176   216

``` r
hist(simulations)
```

![](Regression-and-Other-Stories_files/figure-markdown_github/simulations_girls-1.png)

------------------------------------------------------------------------

Accounting for twins.

First, simulate birth types of 400 births.

``` r
set.seed(2)
probability_girl_if_twins <- 0.495
probability_of_twins_fraternal <- 1/125  # each has a 49.5% of being a girl
probability_of_twins_identical <- 1/300  # 49.5% chance of being a pair of girls.

simulate_birth_types <- function() {
    
    sample(x=c('fraternal twin', 'identical twin', 'single born'),
           size = sample_size,
           replace = TRUE,
           prob = c(probability_of_twins_fraternal,
                    probability_of_twins_identical,
                    1 - probability_of_twins_fraternal - probability_of_twins_identical))
}
birth_type <- simulate_birth_types()
table(birth_type)
```

    ## birth_type
    ## fraternal twin identical twin    single born 
    ##              2              1            397

Simulating Number of Girls if Fraternal Twin. In this scenario, there
are `2` different people, each of which has a
`49.5% percent chance of being a girl. So the possible outcomes are`0`,`1`,`2\`

-   `0`: 0 girls (both boys)
-   `1`: 1 girl
-   `2`: 2 both

``` r
hist(rbinom(n=10000,  # simulate this scenario 10K times
            size=2, # two different possible chances of being a girl
            prob=probability_girl_if_twins))
```

![](Regression-and-Other-Stories_files/figure-markdown_github/simulations_fraternal_twins-1.png)

Simulating Number of Girls if Identical Twin. In this scenario, they are
either both boys, or both girls. So the possible outcomes are either `0`
or `2`

-   `0`: 0 girls (both boys)
-   `2`: both girls

In other words, we’ll simulate the binary outcome of either both girls
or both boys, but the former means we have to count 2 people, so we’ll
multiple the number of people by 2

``` r
hist(2 * rbinom(n=10000,  # simulate this scenario 10K times
                size=1, # two different possible chances of being a girl
                prob=probability_girl_if_twins))
```

![](Regression-and-Other-Stories_files/figure-markdown_github/simulations_identical_twins-1.png)

Now, we will do 1000 simulations of births, considering
fraternal/identical twins; each simulation has a sample-size of 400.

``` r
set.seed(1)
simulation_results <- replicate(1000, {
    birth_type <- simulate_birth_types()
    
    sample_of_girls <- map_dbl(birth_type, ~ {

        if(. == 'fraternal twin') {
            # simulate number of girls for this particular simulated birth_type
            number_of_girls <- rbinom(n=1,
                   size=2, # two different possible chances of being a girl
                   prob=probability_girl_if_twins)
            
        } else if(. == 'identical twin') {
            # simulate number of girls for this particular simulated birth_type
            number_of_girls <- 2 * rbinom(n=1,
                size=1, # two different possible chances of being a girl
                prob=probability_girl_if_twins)
            
        } else if(. == 'single born') {
            # simulate number of girls for this particular simulated birth_type
            number_of_girls <- rbinom(1, 1, probability_girl)
            
        } else {
            stop()
        }
        
        return(number_of_girls)
    })
    stopifnot(length(sample_of_girls) == sample_size)
    return (sum(sample_of_girls))
})
```

``` r
quantile(simulation_results, c(0.025, 0.975))
```

    ##  2.5% 97.5% 
    ##   178   216

``` r
hist(simulation_results)
```

![](Regression-and-Other-Stories_files/figure-markdown_github/simulations_twins-1.png)

Simulation of continuous and mixed discrete/continuous models
-------------------------------------------------------------

``` r
n_sims <- 1000
y1 <- rnorm(n = n_sims, mean = 3, sd = 0.5)
y2 <- exp(x = y1)
y3 <- rbinom(n = n_sims, size = 20, prob = 0.6)
y4 <- rpois(n = n_sims, lambda = 5)
```

![](Regression-and-Other-Stories_files/figure-markdown_github/simulations_continuous-1.png)

Median Absolute Deviation (`MAD SD`)
------------------------------------

``` r
set.seed(1)
z <- rnorm(10000, 5, 2)
hist(z)
```

![](Regression-and-Other-Stories_files/figure-markdown_github/simulations_mad_sd-1.png)

``` r
rnd3 <- function(.x) {
    round(.x, 3)
}
glue::glue("mean = { rnd3(mean(z)) }, sd = { rnd3(sd(z)) }, median = { rnd3(median(z)) }, mad sd = { rnd3(mad(z)) }")
```

    ## mean = 4.987, sd = 2.025, median = 4.968, mad sd = 1.997

``` r
constant <- 1.4826  # 1.483 defined R&OS pg 73; but function uses more specific number
constant * median(abs(z - median(z)))
```

    ## [1] 1.996752

``` r
mad(z)
```

    ## [1] 1.996752

------------------------------------------------------------------------

The above is a single sample if `10,000` observations.

The `standard error` (i.e. expected **standard deviation** of the
**distribution** of **sample means** (if we were going to run this
simulation many times (or draw a random sample from the population many
times))) is:

``` r
sd(z) / sqrt(length(z))
```

    ## [1] 0.02024713

We can also simulate this.

First, lets run a `1,000` simulations of drawing a sample of `10,000`
observations. Each time we’ll take the mean of those 10,000
observations. So we’ll have a distribution of (1000) sample means.

The standard deviation of the distribution of sample means is:

``` r
set.seed(1)
simulations <- replicate(1000, mean(rnorm(10000, 5, 2)))
sd(simulations)
```

    ## [1] 0.01941111

Which is fairly close to our calculated standard error.

``` r
quantile(simulations, c(0.025, 0.975))
```

    ##     2.5%    97.5% 
    ## 4.960204 5.038125

``` r
hist(simulations, main = "Distribution of Sample Means")
```

![](Regression-and-Other-Stories_files/figure-markdown_github/simulations_sample_means-1.png)

Chapter 9 - Prediction and Bayesian Inference
=============================================

``` r
head(hibbs)
```

    ##   year growth  vote inc_party_candidate other_candidate
    ## 1 1952   2.40 44.60           Stevenson      Eisenhower
    ## 2 1956   2.89 57.76          Eisenhower       Stevenson
    ## 3 1960   0.85 49.91               Nixon         Kennedy
    ## 4 1964   4.21 61.34             Johnson       Goldwater
    ## 5 1968   3.02 49.60            Humphrey           Nixon
    ## 6 1972   3.62 61.79               Nixon        McGovern

``` r
# refresh = 0 supresses the default Stan sampling progress output
model <- stan_glm(vote ~ growth, data=hibbs, refresh=0)
print(model)
```

    ## stan_glm
    ##  family:       gaussian [identity]
    ##  formula:      vote ~ growth
    ##  observations: 16
    ##  predictors:   2
    ## ------
    ##             Median MAD_SD
    ## (Intercept) 46.3    1.7  
    ## growth       3.0    0.7  
    ## 
    ## Auxiliary parameter(s):
    ##       Median MAD_SD
    ## sigma 3.9    0.7   
    ## 
    ## ------
    ## * For help interpreting the printed output see ?print.stanreg
    ## * For info on the priors used see ?prior_summary.stanreg

> These numbers are summaries of a matrix of simulations representing
> different possible values of the parameters
> (intercept/slope/residual-standard-deviation). `pg. 113`

> We have a set of `posterior simulations` rather than a single point
> estimate because we have uncertainty about these parameters. `pg. 113`

``` r
simulations <- as.matrix(model)
head(simulations)
```

    ##           parameters
    ## iterations (Intercept)   growth    sigma
    ##       [1,]    47.62082 2.864881 6.521684
    ##       [2,]    45.43180 3.159804 2.789614
    ##       [3,]    45.00267 3.788277 2.664788
    ##       [4,]    47.63697 2.772102 2.962032
    ##       [5,]    46.50408 2.978387 3.019823
    ##       [6,]    46.60710 3.003798 3.251705

``` r
model %>% tidy()
```

    ## # A tibble: 2 x 3
    ##   term        estimate std.error
    ##   <chr>          <dbl>     <dbl>
    ## 1 (Intercept)    46.3      1.69 
    ## 2 growth          3.03     0.696

``` r
median(simulations[, '(Intercept)'])
```

    ## [1] 46.3117

``` r
mad(simulations[, '(Intercept)'])
```

    ## [1] 1.689381

``` r
median(simulations[, 'growth'])
```

    ## [1] 3.032582

``` r
mad(simulations[, 'growth'])
```

    ## [1] 0.6956641

------------------------------------------------------------------------

``` r
simulations %>%
    as.data.frame() %>%
    mutate(index=row_number()) %>%
    pivot_longer(-index) %>%
    #mutate(stat.sig = factor(stat.sig, levels=c("TRUE", "FALSE"))) %>%
    #mutate(name = fct_reorder(name, coef_median)) %>%
    ggplot(aes(x=value, y=name)) +
    geom_density_ridges(alpha=0.5) +
    geom_vline(xintercept = 0, color='red')
```

![](Regression-and-Other-Stories_files/figure-markdown_github/chapter_9_coefficient_joy_plot-1.png)

------------------------------------------------------------------------

``` r
simulations %>%
    as.data.frame() %>%
    mutate(index=row_number()) %>%
    pivot_longer(-index) %>%
    group_by(name) %>%
    summarise(coef_median = median(value),
              coef_mad = mad(value),
              conf.low = coef_median - 2*coef_mad,
              conf.high = coef_median + 2*coef_mad,
              # this is sudo p.value
              # it's the percent of times the simulated values are below zero (if coef_median is greater than zero), or vice-versa
              p.value=ifelse(coef_median > 0, sum(value <= 0) / n(), sum(value >= 0) / n()),
              stat.sig = p.value <= 0.05) %>%
    ungroup() %>%
    mutate(stat.sig = factor(stat.sig, levels=c("TRUE", "FALSE"))) %>%
    mutate(name = fct_reorder(name, coef_median)) %>%
    ggplot(aes(x=coef_median, y=name, color=stat.sig)) +
    geom_point() +
    geom_vline(xintercept = 0, color='red') +
    geom_errorbar(aes(xmin=conf.low, xmax=conf.high)) +
    scale_color_manual(values=c("#37B57F", "#DF585C"))
```

![](Regression-and-Other-Stories_files/figure-markdown_github/chapter_9_coefficient_tiefighter_plot-1.png)

Point Prediction
----------------

``` r
new_data <- data.frame(growth=2.0)
predict(object = model, newdata = new_data)
```

    ##        1 
    ## 52.36997

Linear Predictor with Uncertainty
---------------------------------

> We can use `posterior_linpred` to get uncertainty in the value of the
> fitted regression line. (pg. 116)

``` r
y_linpredict <- posterior_linpred(object=model, newdata = new_data)
head(y_linpredict)
```

    ##           
    ## iterations        1
    ##       [1,] 53.35058
    ##       [2,] 51.75141
    ##       [3,] 52.57922
    ##       [4,] 53.18117
    ##       [5,] 52.46085
    ##       [6,] 52.61470

> Equivalently we can use the function `posterior_epred`, which returns
> the expected prediction for a new data point. For linear regression,
> the expected value is the same as the linear predcitor, but as we
> dicuss in Chapter 13, these two quantities differ for nonlinear
> models. (pg 116)

``` r
head(posterior_epred(object=model, newdata = new_data))
```

    ##           
    ## iterations        1
    ##       [1,] 53.35058
    ##       [2,] 51.75141
    ##       [3,] 52.57922
    ##       [4,] 53.18117
    ##       [5,] 52.46085
    ##       [6,] 52.61470

To understand what these predictions are, we could have calculated them
by hand using the simulations output.

``` r
simulations %>%
    as.data.frame() %>%
    select(-sigma) %>%
    mutate(prediction = `(Intercept)` + growth * 2.0) %>%
    head()
```

    ##   (Intercept)   growth prediction
    ## 1    47.62082 2.864881   53.35058
    ## 2    45.43180 3.159804   51.75141
    ## 3    45.00267 3.788277   52.57922
    ## 4    47.63697 2.772102   53.18117
    ## 5    46.50408 2.978387   52.46085
    ## 6    46.60710 3.003798   52.61470

``` r
simulate_election_results_conf_int <- function(.growth) {

    predictions <- posterior_linpred(object=model, newdata = data.frame(growth=.growth))
    confidence_intervals <- quantile(predictions, c(0.025, 0.975))
    data.frame(prediction=median(predictions),
               conf.low=unname(confidence_intervals[1]),
               conf.high=unname(confidence_intervals[2]))
}
simulate_election_results_conf_int(2)
```

    ##   prediction conf.low conf.high
    ## 1   52.36322 50.30846  54.50298

``` r
data.frame(growth = -6:6) %>%
    mutate(results = map(growth, ~simulate_election_results_conf_int(.growth=.))) %>%
    unnest(results) %>%
    ggplot(aes(x=growth, y=prediction)) +
    geom_point() +
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high)) +
    scale_x_continuous(breaks = pretty_breaks(20)) +
    scale_y_continuous(breaks = pretty_breaks(10)) +
    labs(title="Uncertainty in the value of the fitted regression LINE (not the prediction)",
         subtitle="NOTE: I doubt this is how we ever want to express uncertainty in the actual predictions.")
```

![](Regression-and-Other-Stories_files/figure-markdown_github/chapter_9_posterior_linpred-1.png)

NOTE: I doubt this is how we ever want to express uncertainty in the
actual predictions.

Predictive Distribution
-----------------------

> We can construct a vector representing **predictive uncertainty** in a
> single election.

Note the confidence intervals are being simulated so the numbers/graph
will change each time unless a seed is set.

``` r
set.seed(1)
y_pred <- posterior_predict(object = model, newdata = new_data)
head(y_pred)
```

    ##             1
    ## [1,] 49.26505
    ## [2,] 52.26371
    ## [3,] 50.35245
    ## [4,] 57.90645
    ## [5,] 53.45591
    ## [6,] 49.94677

If we were to do this by hand, we do the same as before, but add the
error term to the computation (which assumes normal distribution).

``` r
set.seed(1)
simulations %>%
    as.data.frame() %>%
    mutate(error = rnorm(nrow(simulations), 0, sigma)) %>%
    mutate(prediction = `(Intercept)` + growth*2.0 + error) %>%
    head()
```

    ##   (Intercept)   growth    sigma      error prediction
    ## 1    47.62082 2.864881 6.521684 -4.0855339   49.26505
    ## 2    45.43180 3.159804 2.789614  0.5122939   52.26371
    ## 3    45.00267 3.788277 2.664788 -2.2267734   50.35245
    ## 4    47.63697 2.772102 2.962032  4.7252733   57.90645
    ## 5    46.50408 2.978387 3.019823  0.9950550   53.45591
    ## 6    46.60710 3.003798 3.251705 -2.6679209   49.94677

------------------------------------------------------------------------

``` r
simulate_election_results_conf_int <- function(.growth) {

    predictions <- posterior_predict(object=model, newdata = data.frame(growth=.growth))
    confidence_intervals_68 <- quantile(predictions, c(0.16, 1-0.16))
    confidence_intervals_95 <- quantile(predictions, c(0.025, 0.975))
    data.frame(prediction=median(predictions),
               conf.low.68=unname(confidence_intervals_68[1]),
               conf.high.68=unname(confidence_intervals_68[2]),
               conf.low.95=unname(confidence_intervals_95[1]),
               conf.high.95=unname(confidence_intervals_95[2]),
               probability_50_plus = mean(predictions > 50)) %>%
        mutate(stat.sig = !between(50, conf.low.95, conf.high.95))
}
set.seed(1)
simulate_election_results_conf_int(2)
```

    ##   prediction conf.low.68 conf.high.68 conf.low.95 conf.high.95
    ## 1   52.38854    48.31445     56.54039    43.57586     61.00448
    ##   probability_50_plus stat.sig
    ## 1             0.71825    FALSE

``` r
set.seed(1)
results <- data.frame(growth = -6:6) %>%
    mutate(results = map(growth, ~simulate_election_results_conf_int(.growth=.))) %>%
    unnest(results) 
results
```

    ## # A tibble: 13 x 8
    ##    growth prediction conf.low.68 conf.high.68 conf.low.95 conf.high.95
    ##     <int>      <dbl>       <dbl>        <dbl>       <dbl>        <dbl>
    ##  1     -6       28.1        21.2         35.1        13.8         42.4
    ##  2     -5       31.1        24.8         37.4        18.2         44.0
    ##  3     -4       34.2        28.2         39.9        21.8         45.9
    ##  4     -3       37.1        31.9         42.3        26.2         48.0
    ##  5     -2       40.3        35.5         45.0        30.4         50.6
    ##  6     -1       43.4        38.8         47.8        33.9         53.0
    ##  7      0       46.3        41.9         50.6        37.4         55.2
    ##  8      1       49.4        45.1         53.6        40.8         58.2
    ##  9      2       52.2        48.3         56.5        44.0         60.8
    ## 10      3       55.4        51.2         59.3        46.8         64.0
    ## 11      4       58.4        54.1         62.9        49.7         67.8
    ## 12      5       61.5        56.7         66.1        51.6         71.0
    ## 13      6       64.4        59.5         69.5        54.1         74.7
    ## # … with 2 more variables: probability_50_plus <dbl>, stat.sig <lgl>

Note the confidence intervals are being simulated so the numbers/graph
will change each time unless a seed is set.

------------------------------------------------------------------------

``` r
results %>%
    ggplot(aes(x=growth, y=prediction, color=stat.sig)) +
    geom_hline(yintercept = 50, color="#DF585C") +
    geom_point() +
    geom_errorbar(aes(ymin=conf.low.68, ymax=conf.high.68), width=0.5) +
    geom_errorbar(aes(ymin=conf.low.95, ymax=conf.high.95), width=0.5, alpha=0.5) +
    scale_x_continuous(breaks = pretty_breaks(10), labels = function(.x)paste0(.x, "%")) +
    scale_y_continuous(breaks = pretty_breaks(10), labels = function(.x)paste0(.x, "%")) +
    scale_color_manual(values=c("#7A7A7A", "#7AA9CF")) +
    labs(title="Prediction of Incumbent party's vote share, given various economic growth values.",
         x="Economic Growth (% from previous year)",
         y="Prediction of Incumbent party's vote share.")
```

![](Regression-and-Other-Stories_files/figure-markdown_github/chapter_9_posterior_predict-1.png)

------------------------------------------------------------------------

``` r
results %>%
    ggplot(aes(x=growth, y=prediction)) +
    geom_hline(yintercept = 50, color="#DF585C") +
    geom_ribbon(aes(ymin=conf.low.68, ymax=conf.high.68), alpha=0.3) +
    geom_ribbon(aes(ymin=conf.low.95, ymax=conf.high.95), alpha=0.3) +
    geom_point(aes(color=stat.sig)) +
    #geom_point() +
    #geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0.5) +
    scale_x_continuous(breaks = pretty_breaks(10), labels = function(.x)paste0(.x, "%")) +
    scale_y_continuous(breaks = pretty_breaks(10), labels = function(.x)paste0(.x, "%")) +
    scale_color_manual(values=c("black", "purple")) +
    labs(title="Prediction of Incumbent party's vote share, given various economic growth values.",
         x="Economic Growth (% from previous year)",
         y="Prediction of Incumbent party's vote share.")
```

![](Regression-and-Other-Stories_files/figure-markdown_github/chapter_9_posterior_predict_ribbon-1.png)

------------------------------------------------------------------------

``` r
results %>%
    mutate(growth = factor(growth)) %>%
    ggplot(aes(x=growth, y=probability_50_plus, fill=probability_50_plus)) +
    geom_col() +
    geom_text(aes(label=percent(probability_50_plus, accuracy = 0.1)), vjust=-0.3) +
    scale_y_continuous(breaks=pretty_breaks(10), labels = scales::percent_format()) +
    #scale_fill_gradient2(low = 'red', mid = 'white', high = 'green', midpoint = 0.5) +
    scale_fill_gradient2(low = '#DF585C', mid = 'white', high = '#37B57F', midpoint = 0.5) +
    labs(title="Probability of Incumbent party's vote share > 50%",
         x="Economic Growth (% from previous year)",
         y="Probability of Incumbent party's vote share > 50%")
```

![](Regression-and-Other-Stories_files/figure-markdown_github/chapter_9_posterior_predict_prob-1.png)

------------------------------------------------------------------------

If we were to predict on several instances, we would get a column for
each input value (i.e. growth of `-2` corresponds to column `1`)

``` r
head(posterior_predict(object=model, newdata = data.frame(growth=-2:2)))
```

    ##             1        2        3        4        5
    ## [1,] 46.58267 55.34217 55.85036 53.39316 53.90841
    ## [2,] 36.18921 43.53023 45.43683 50.94873 50.97626
    ## [3,] 36.13699 38.45776 48.39274 51.15055 53.88005
    ## [4,] 42.56424 44.40833 52.69218 53.49988 58.24306
    ## [5,] 34.25539 43.33577 45.38951 51.45033 52.82074
    ## [6,] 40.64915 42.16343 48.14879 51.82954 52.06827

``` r
all_predictions <- posterior_predict(object=model, newdata = data.frame(growth=-6:6))
head(all_predictions, 20)
```

    ##              1        2        3        4        5        6        7        8
    ##  [1,] 27.26693 32.33322 28.76607 33.15022 34.57823 48.08704 46.81508 51.21703
    ##  [2,] 30.14328 29.64747 30.78392 28.90250 35.17564 44.78823 47.81599 47.56478
    ##  [3,] 22.58081 23.33706 31.41935 38.43448 38.45445 47.11420 49.10445 47.38917
    ##  [4,] 32.06930 39.43968 34.91375 43.50813 38.07705 43.44959 46.23782 50.93487
    ##  [5,] 31.07284 29.31486 40.00841 34.57928 39.41087 43.09415 45.62382 49.71571
    ##  [6,] 28.63732 30.33937 37.25512 32.14609 39.96298 44.90074 43.60733 49.78096
    ##  [7,] 27.89311 34.84910 29.34077 41.36091 35.04268 46.57322 45.56597 55.36566
    ##  [8,] 22.78106 29.59833 29.56383 36.68117 36.77547 40.08542 49.32242 56.71129
    ##  [9,] 27.45513 30.07297 36.60706 35.81191 51.21570 35.35255 50.09946 45.07317
    ## [10,] 27.32359 28.69936 38.60485 36.05598 38.70026 40.99028 43.97654 56.10664
    ## [11,] 35.47400 36.83168 42.54231 44.85211 45.64484 49.01411 49.62550 50.39823
    ## [12,] 22.25673 18.14444 28.10293 29.47912 41.03656 41.64295 48.10747 51.23938
    ## [13,] 30.21465 32.03455 30.70669 39.55717 36.56647 41.98881 45.63678 50.59790
    ## [14,] 39.85085 40.34501 35.78144 47.34916 47.53722 45.05452 52.08145 52.07576
    ## [15,] 39.34603 37.86523 45.72294 43.88172 47.16000 51.80827 49.54676 55.55445
    ## [16,] 35.58506 44.59870 43.61821 51.84381 45.43059 48.71842 53.98606 48.76976
    ## [17,] 33.46959 33.77459 24.13620 30.17920 39.94170 37.24019 43.85713 52.72299
    ## [18,] 30.87361 30.96835 34.63624 45.13329 40.04962 48.09700 50.98859 53.09266
    ## [19,] 34.55347 29.68911 25.10473 36.54526 35.88297 32.96661 46.07625 48.53539
    ## [20,] 29.68140 31.03276 33.13615 33.34981 43.20603 43.82724 44.13889 55.74255
    ##              9       10       11       12       13
    ##  [1,] 59.48901 51.64006 61.25686 60.14944 59.80178
    ##  [2,] 56.14297 51.16957 60.43229 64.93340 61.40858
    ##  [3,] 51.73242 58.99176 57.83935 65.30924 68.48958
    ##  [4,] 52.28218 59.75567 57.15346 66.16872 62.20618
    ##  [5,] 55.08878 56.35769 57.27590 62.13067 66.00643
    ##  [6,] 53.49666 56.82682 55.82438 58.57286 71.14724
    ##  [7,] 54.06875 57.19627 59.87327 55.53374 63.70790
    ##  [8,] 55.42542 57.85049 58.81904 63.99051 65.58196
    ##  [9,] 45.76719 55.58322 57.56189 62.52415 62.39559
    ## [10,] 56.33341 54.32839 57.57029 61.61716 63.37175
    ## [11,] 46.37298 50.70586 58.51604 58.71830 61.96215
    ## [12,] 50.37424 54.60613 65.31832 68.73379 66.92885
    ## [13,] 52.30561 52.64160 57.77425 57.10227 66.31723
    ## [14,] 51.40101 57.56836 57.02827 55.71739 59.63451
    ## [15,] 46.64437 48.65122 64.32144 73.38135 60.94164
    ## [16,] 56.19864 56.13497 56.18857 51.84202 57.09881
    ## [17,] 56.32049 54.10519 50.63476 56.06837 62.42092
    ## [18,] 56.91258 55.87893 57.77936 65.87154 70.18914
    ## [19,] 41.41528 49.57154 57.89952 63.68779 64.66678
    ## [20,] 58.07777 54.06189 63.57910 62.23571 66.94661

``` r
all_predictions <- all_predictions %>%
    as.data.frame() %>%
    mutate(temp=1) %>%
    pivot_longer(-temp, names_to='growth', values_to='prediction') %>%
    mutate(growth = as.numeric(growth) - 6 - 1) %>%
    select(-temp)

results %>%
    ggplot(aes(x=growth, y=prediction, color=stat.sig)) +
    geom_hline(yintercept = 50, color="#DF585C") +
    #geom_point() +
    geom_jitter(data=all_predictions, aes(x=growth, y=prediction, color=NULL), width=0.1, alpha=0.1) +
    #geom_errorbar(aes(ymin=conf.low.68, ymax=conf.high.68), width=0.5) +
    geom_errorbar(aes(ymin=conf.low.95, ymax=conf.high.95), width=0.7) +
    scale_x_continuous(breaks = pretty_breaks(10), labels = function(.x)paste0(.x, "%")) +
    scale_y_continuous(breaks = pretty_breaks(20), labels = function(.x)paste0(.x, "%")) +
    scale_color_manual(values=c("#7A7A7A", "#7AA9CF")) +
    labs(title="Prediction of Incumbent party's vote share, given various economic growth values.",
         x="Economic Growth (% from previous year)",
         y="Prediction of Incumbent party's vote share.")
```

![](Regression-and-Other-Stories_files/figure-markdown_github/chapter_9_posterior_predict_points-1.png)
