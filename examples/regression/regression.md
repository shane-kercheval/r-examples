-   [Regression](#regression)
    -   [Results](#results)
    -   [Anova Table](#anova-table)
    -   [Amount of Variation Explained](#amount-of-variation-explained)
    -   [Anova to Compare Models](#anova-to-compare-models)
    -   [Plot Assumptions](#plot-assumptions)
    -   [Out of Sample R-Squared](#out-of-sample-r-squared)
    -   [Interpretation](#interpretation)
        -   [Elasticities (i.e. regression
            slopes)](#elasticities-i.e.regression-slopes)

This data contains weekly prices and sales for three OJ brands, as well
as an indicator `feature` showing whether each brand was advertised (in
store or flyer) that week. (Business Data Science, pg 43).

``` r
weekly_oj_sales <- read.csv('data/oj.csv') %>% rename(featured = feat) %>% mutate(featured = featured == 1)
head(weekly_oj_sales)
```

    ##   sales price     brand featured
    ## 1  8256  3.87 tropicana    FALSE
    ## 2  6144  3.87 tropicana    FALSE
    ## 3  3840  3.87 tropicana    FALSE
    ## 4  8000  3.87 tropicana    FALSE
    ## 5  8896  3.87 tropicana    FALSE
    ## 6  7168  3.87 tropicana    FALSE

``` r
weekly_oj_sales %>%
    ggplot(aes(x=price, y=sales, color = brand)) +
    geom_point(alpha=0.1) +
    geom_smooth() +
    facet_wrap(~ featured)
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](regression_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
weekly_oj_sales %>%
    ggplot(aes(x=log(price), y=log(sales), color = brand)) +
    geom_point(alpha=0.1) +
    geom_smooth() +
    facet_wrap(~ featured)
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](regression_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
weekly_oj_sales %>%
    ggplot(aes(x=log(price), y=log(sales), color = brand)) +
    geom_point(alpha=0.1) +
    geom_smooth(method = 'lm') +
    facet_wrap(~ featured)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](regression_files/figure-markdown_github/unnamed-chunk-3-1.png)

-   Sales decrease with price.

Regression
==========

Results
-------

``` r
reg_results <- lm(log(sales) ~ log(price)*brand*featured, data = weekly_oj_sales)
summary(reg_results)
```

    ## 
    ## Call:
    ## lm(formula = log(sales) ~ log(price) * brand * featured, data = weekly_oj_sales)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8893 -0.4290 -0.0091  0.4125  3.2368 
    ## 
    ## Coefficients:
    ##                                          Estimate Std. Error t value
    ## (Intercept)                              10.40658    0.02335 445.668
    ## log(price)                               -2.77415    0.03883 -71.445
    ## brandminute.maid                          0.04720    0.04663   1.012
    ## brandtropicana                            0.70794    0.05080  13.937
    ## featuredTRUE                              1.09441    0.03810  28.721
    ## log(price):brandminute.maid               0.78293    0.06140  12.750
    ## log(price):brandtropicana                 0.73579    0.05684  12.946
    ## log(price):featuredTRUE                  -0.47055    0.07409  -6.351
    ## brandminute.maid:featuredTRUE             1.17294    0.08196  14.312
    ## brandtropicana:featuredTRUE               0.78525    0.09875   7.952
    ## log(price):brandminute.maid:featuredTRUE -1.10922    0.12225  -9.074
    ## log(price):brandtropicana:featuredTRUE   -0.98614    0.12411  -7.946
    ##                                                      Pr(>|t|)    
    ## (Intercept)                              < 0.0000000000000002 ***
    ## log(price)                               < 0.0000000000000002 ***
    ## brandminute.maid                                        0.311    
    ## brandtropicana                           < 0.0000000000000002 ***
    ## featuredTRUE                             < 0.0000000000000002 ***
    ## log(price):brandminute.maid              < 0.0000000000000002 ***
    ## log(price):brandtropicana                < 0.0000000000000002 ***
    ## log(price):featuredTRUE                    0.0000000002171102 ***
    ## brandminute.maid:featuredTRUE            < 0.0000000000000002 ***
    ## brandtropicana:featuredTRUE                0.0000000000000019 ***
    ## log(price):brandminute.maid:featuredTRUE < 0.0000000000000002 ***
    ## log(price):brandtropicana:featuredTRUE     0.0000000000000020 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.695 on 28935 degrees of freedom
    ## Multiple R-squared:  0.5354, Adjusted R-squared:  0.5352 
    ## F-statistic:  3031 on 11 and 28935 DF,  p-value: < 0.00000000000000022

``` r
names(reg_results)
```

    ##  [1] "coefficients"  "residuals"     "effects"       "rank"         
    ##  [5] "fitted.values" "assign"        "qr"            "df.residual"  
    ##  [9] "contrasts"     "xlevels"       "call"          "terms"        
    ## [13] "model"

``` r
coefficients(reg_results)
```

    ##                              (Intercept) 
    ##                              10.40657579 
    ##                               log(price) 
    ##                              -2.77415436 
    ##                         brandminute.maid 
    ##                               0.04720317 
    ##                           brandtropicana 
    ##                               0.70794089 
    ##                             featuredTRUE 
    ##                               1.09440665 
    ##              log(price):brandminute.maid 
    ##                               0.78293210 
    ##                log(price):brandtropicana 
    ##                               0.73579299 
    ##                  log(price):featuredTRUE 
    ##                              -0.47055331 
    ##            brandminute.maid:featuredTRUE 
    ##                               1.17294361 
    ##              brandtropicana:featuredTRUE 
    ##                               0.78525237 
    ## log(price):brandminute.maid:featuredTRUE 
    ##                              -1.10922376 
    ##   log(price):brandtropicana:featuredTRUE 
    ##                              -0.98614093

``` r
coefficients(reg_results)['log(price)']
```

    ## log(price) 
    ##  -2.774154

``` r
reg_results %>%
    tidy() %>%
    mutate(conf.low = estimate - (2 * std.error),
           conf.high = estimate + (2 * std.error)) %>%
    filter(term != '(Intercept)') %>%
    mutate(color = case_when(
        estimate > 0 & conf.low > 0 ~ 'blue',
        estimate < 0 & conf.high < 0 ~ 'red',
        TRUE ~ 'grey'
    )) %>%
    mutate(term = fct_reorder(term, estimate)) %>%
    ggplot(aes(x = term, y=estimate, color=color)) +
    geom_point() +
    scale_color_manual(values=c('blue','dark grey','red')) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
    coord_flip() +
    theme(legend.position = 'none') +
    labs(title = 'Regression Coefficients')
```

![](regression_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
actual_values <- log(weekly_oj_sales$sales)
predicted_values <- predict(reg_results) %>% as.numeric()
residual_values <- residuals(reg_results) %>% as.numeric()

equal <- function(.expected, .actual, .precision=6) {
    round(.expected, .precision) == round(.actual, .precision)
}
all(equal(actual_values - predicted_values, residual_values))
```

    ## [1] TRUE

Anova Table
-----------

<a href="https://stats.stackexchange.com/questions/115304/interpreting-output-from-anova-when-using-lm-as-input" class="uri">https://stats.stackexchange.com/questions/115304/interpreting-output-from-anova-when-using-lm-as-input</a>
<a href="https://stats.stackexchange.com/questions/49924/how-does-anova-lm-in-r-calculates-sum-sq" class="uri">https://stats.stackexchange.com/questions/49924/how-does-anova-lm-in-r-calculates-sum-sq</a>
<a href="http://rinterested.github.io/statistics/anova_of_OLS_models.html" class="uri">http://rinterested.github.io/statistics/anova_of_OLS_models.html</a>

Why use anova?

The Anova Table can be used to calculate the percent of “explained
variance” of the model.

Also, from
<a href="https://stats.stackexchange.com/questions/115304/interpreting-output-from-anova-when-using-lm-as-input" class="uri">https://stats.stackexchange.com/questions/115304/interpreting-output-from-anova-when-using-lm-as-input</a>

> First of all, you may be perfectly satisfied with the summary output,
> and that’s fine. However, the ANOVA table may offer some advantages.
>
> First, if you have a categorical / factor variable with more than two
> levels, the summary output is hard to interpret. It will give you
> tests of individual levels against the reference level, but won’t give
> you a test of the factor as a whole.
>
> Another reason you might prefer to look at an ANOVA table is that it
> allows you to use information about the possible associations between
> your independent variables and

``` r
anova_table <- reg_results %>%
    anova() %>%
    tidy() %>%
    mutate(p.value = round(p.value, 5)) %>%
    arrange(term)
anova_table
```

    ## # A tibble: 8 x 6
    ##   term                         df   sumsq   meansq statistic p.value
    ##   <chr>                     <int>   <dbl>    <dbl>     <dbl>   <dbl>
    ## 1 brand                         2  5593.  2797.       5790.        0
    ## 2 brand:featured                2   216.   108.        223.        0
    ## 3 featured                      1  3650.  3650.       7557.        0
    ## 4 log(price)                    1  6261.  6261.      12963.        0
    ## 5 log(price):brand              2   150.    75.2       156.        0
    ## 6 log(price):brand:featured     2    51.8   25.9        53.7       0
    ## 7 log(price):featured           1   182.   182.        378.        0
    ## 8 Residuals                 28935 13975.     0.483      NA        NA

The sum of `sumsq` (i.e. `sum of squares`) is the same as
`Total Sum of Suares (SST)` explained in Introductory Econometrics 7e
pg. 34.

``` r
# Total Sum of Squares (SST)
(SST <- sum((log(weekly_oj_sales$sales) - mean(log(weekly_oj_sales$sales)))^2))
```

    ## [1] 30078.71

``` r
equal(SST, sum(anova_table$sumsq))
```

    ## [1] TRUE

The `sumsq` value of `Residuals` is the same as the
`Residual Sum of Squares (SSR)`

Sum of Residuals

``` r
# Residual Sum of Squares ()
(SSR <- sum(residual_values^2))
```

    ## [1] 13974.76

``` r
SSR == anova_table %>% filter(term == 'Residuals') %>% pull(sumsq)
```

    ## [1] TRUE

The `Explained Sum of Squares (SSE)` is the same as all of the `sumsq`
values added together, excluding the `Residuals` value.

`SST = SSE + SSR`

``` r
# Explained Sum of Squares (SSE)
(SSE <- sum((fitted(reg_results) - mean(log(weekly_oj_sales$sales)))^2))
```

    ## [1] 16103.96

``` r
equal(SSE, anova_table %>% filter(term != 'Residuals') %>% pull(sumsq) %>% sum())
```

    ## [1] TRUE

``` r
equal(SST, SSE + SSR)
```

    ## [1] TRUE

R-squared is simply the percent of variation explained by the model
(i.e. SSE / SST)

``` r
summary(reg_results)$r.squared
```

    ## [1] 0.5353939

``` r
SSE / SST
```

    ## [1] 0.5353939

However, R-squared will go up every time you add a new feature. So you
can artificially inflate this number.

`Adjusted R-Squared` “imposes a penatly for adding additional
independent variables to a model.” (Introductory Econometrics 7E)

``` r
summary(reg_results)$adj.r.squared
```

    ## [1] 0.5352172

Amount of Variation Explained
-----------------------------

``` r
anova_table %>%
    select(term, sumsq) %>%
    mutate(percent_variation = percent(sumsq / sum(sumsq))) %>%
    arrange(desc(sumsq))
```

    ## # A tibble: 8 x 3
    ##   term                        sumsq percent_variation
    ##   <chr>                       <dbl> <chr>            
    ## 1 Residuals                 13975.  46.46%           
    ## 2 log(price)                 6261.  20.81%           
    ## 3 brand                      5593.  18.60%           
    ## 4 featured                   3650.  12.13%           
    ## 5 brand:featured              216.  0.72%            
    ## 6 log(price):featured         182.  0.61%            
    ## 7 log(price):brand            150.  0.50%            
    ## 8 log(price):brand:featured    51.8 0.17%

``` r
plot_regression_variance_explained(reg_results)
```

![](regression_files/figure-markdown_github/unnamed-chunk-22-1.png)

Anova to Compare Models
-----------------------

<a href="https://bookdown.org/ndphillips/YaRrr/comparing-regression-models-with-anova.html" class="uri">https://bookdown.org/ndphillips/YaRrr/comparing-regression-models-with-anova.html</a>

> To compare the fits of two models, you can use the anova() function
> with the regression objects as two separate arguments. The anova()
> function will take the model objects as arguments, and return an ANOVA
> testing whether the more complex model is significantly better at
> capturing the data than the simpler model. If the resulting p-value is
> sufficiently low (usually less than 0.05), we conclude that the more
> complex model is significantly better than the simpler model, and thus
> favor the more complex model. If the p-value is not sufficiently low
> (usually greater than 0.05), we should favor the simpler model.

(This only make statistical sense if the models are nested.)

``` r
anova(lm(log(sales) ~ log(price)+brand+featured, data = weekly_oj_sales),
      lm(log(sales) ~ log(price)*brand*featured, data = weekly_oj_sales))
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: log(sales) ~ log(price) + brand + featured
    ## Model 2: log(sales) ~ log(price) * brand * featured
    ##   Res.Df   RSS Df Sum of Sq      F                Pr(>F)    
    ## 1  28942 14575                                              
    ## 2  28935 13975  7    600.48 177.61 < 0.00000000000000022 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#install.packages('effects')
plot(effects::effect(c('log(price)', 'brand'), reg_results))
```

    ## Warning in term == terms: longer object length is not a multiple of shorter
    ## object length

    ## Warning in term == names: longer object length is not a multiple of shorter
    ## object length

    ## NOTE: log(price)brand is not a high-order term in the model

![](regression_files/figure-markdown_github/unnamed-chunk-24-1.png)

Plot Assumptions
----------------

``` r
plot(reg_results) 
```

![](regression_files/figure-markdown_github/unnamed-chunk-25-1.png)![](regression_files/figure-markdown_github/unnamed-chunk-25-2.png)![](regression_files/figure-markdown_github/unnamed-chunk-25-3.png)![](regression_files/figure-markdown_github/unnamed-chunk-25-4.png)

``` r
plot_actual_vs_predicted(reg_results)
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](regression_files/figure-markdown_github/unnamed-chunk-26-1.png)

``` r
plot_residual_vs_predicted(model=reg_results)
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](regression_files/figure-markdown_github/unnamed-chunk-27-1.png)

``` r
plot_residual_vs_variable(model=reg_results,
                          predictor = 'price',
                          dataset = weekly_oj_sales %>% mutate(`log(price)` = log(price)))
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](regression_files/figure-markdown_github/unnamed-chunk-28-1.png)

Out of Sample R-Squared
-----------------------

r

Interpretation
--------------

### Elasticities (i.e. regression slopes)

`log(sales) = intercept + log(price) + e` gives a regression line
describing the expected sales given price. This is the price elasticity
of sales (i.e. elasticity of sales based on price).

``` r
reg_results <- lm(log(sales) ~ log(price), data = weekly_oj_sales)
get_regression_equation(reg_results)
```

    ## [1] "log(sales) = 10.42(Intercept) + -1.6log(price) + error"

``` r
coef(reg_results)['log(price)']
```

    ## log(price) 
    ##  -1.601307

This is an example of a log-log model, which have an interpretation of

> sales increase by B% for every 1% increase in price (Business Data
> Science pg. 45-47)

Where `B` is the coefficient on price.

So `sales` drop (i.e. increase by negative coefficient) by about `-1.6%`
for every `1%` increase in `price`.

``` r
#breaks <- c(-0.5, 0, 0.5, 1, 1.5)
weekly_oj_sales %>%
    ggplot(aes(x=log(price), y=log(sales))) +
    geom_point(alpha=0.2) +
    geom_smooth(method='lm')# +
```

    ## `geom_smooth()` using formula 'y ~ x'

![](regression_files/figure-markdown_github/unnamed-chunk-30-1.png)

``` r
   # scale_x_continuous(breaks = breaks, labels = round(exp(breaks), 1))
```

------------------------------------------------------------------------

`log(sales) = intercept + log(price) + brand + e` gives a regression
line that has a different intercept for each brand, but the same price
elasticities. This equation says “even though all brand sales have the
same elasticity to price, at the same price they will have different
expected sales.” i.e. they have the same regression slope.

``` r
reg_results <- lm(log(sales) ~ log(price) + brand, data = weekly_oj_sales)

price_elasticity <- coef(reg_results)['log(price)']

dominicks_intercept <- coefficients(reg_results)['(Intercept)']
minute_maid_intercept <- coefficients(reg_results)['(Intercept)'] + coefficients(reg_results)['brandminute.maid']
tropicana_intercept <- coefficients(reg_results)['(Intercept)'] + coefficients(reg_results)['brandtropicana']

get_regression_equation(reg_results)
```

    ## [1] "log(sales) = 10.83(Intercept) + -3.14log(price) + 0.87brandminute.maid + 1.53brandtropicana + error"

In this example, we see that when we control for brand, `sales` are
expected to drop by about `3.1%` for every `1%` increase in `price`.

Each brand is allowed to have it’s own intercept, meaning that “even
though all brand sales have the same elasticity to price, at the same
price they will have different expected sales.” (BDS pg 46)

The intercept gives the value for `Dominick`’s log sales at a log price
of 0 (i.e. when all variables have a value of zero i.e. `log(price)` =
$0, `brandminute.maid` = 0, `brandtropicana` = 0)

-   Intercept for Dominicks: `10.8288216`
-   Intercept for Minute Maid: `11.6989962`
-   Intercept for Tropicana: `12.3587643`

``` r
weekly_oj_sales %>%
    ggplot(aes(x=log(price), y=log(sales), color=brand)) +
    geom_point(alpha=0.2) +
    geom_abline(slope = price_elasticity, intercept = dominicks_intercept, color='red', size=1.5) +
    geom_abline(slope = price_elasticity, intercept = minute_maid_intercept, color='green', size=1.5) +
    geom_abline(slope = price_elasticity, intercept = tropicana_intercept, color='blue', size=1.5)
```

![](regression_files/figure-markdown_github/unnamed-chunk-32-1.png)

As mentioned, all brands in this model share that same elasticity
(regression slope). “This is unrealistic: money is less of an issue for
Tropicana customer than it is for the average Dominick’s customer.” (BDS
pg 47)

------------------------------------------------------------------------

`log(sales) = intercept + log(price) + brand + log(price)*brand + e`
allows us to add an interaction term. (regression shorthand is:
`log(sales) ~ log(price) * brand`). The result is a separate intercept
(which we had in the last model) as well as a separate slope
(elasticity) for each brand.

``` r
reg_results <- lm(log(sales) ~ log(price) * brand, data = weekly_oj_sales) # same as lm(log(sales) ~ log(price) + brand + log(price)*brand, data = weekly_oj_sales)

dominicks_price_elasticity <- coef(reg_results)['log(price)']
minute_maid_price_elasticity <- coef(reg_results)['log(price)'] + coef(reg_results)['log(price):brandminute.maid']
tropicana_price_elasticity <- coef(reg_results)['log(price)'] + coef(reg_results)['log(price):brandtropicana']

get_regression_equation(reg_results)
```

    ## [1] "log(sales) = 10.95(Intercept) + -3.38log(price) + 0.89brandminute.maid + 0.96brandtropicana + 0.06log(price):brandminute.maid + 0.67log(price):brandtropicana + error"

Now it no longer makes sense to talk about an overall elasticity number,
but rather elasticity for each brand.

-   Elasticity for Dominicks: `-3.3775296` (`sales` are expected to drop
    by about `3.4%` for every `1%` increase in `price`)
-   Intercept for Minute Maid: `-3.3207349` (`sales` are expected to
    drop by about `3.3%` for every `1%` increase in `price`)
-   Intercept for Tropicana: `-2.7117688` (`sales` are expected to drop
    by about `2.7%` for every `1%` increase in `price`)

``` r
weekly_oj_sales %>%
    ggplot(aes(x=log(price), y=log(sales), color=brand)) +
    geom_point(alpha=0.2) +
    geom_smooth(method='lm')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](regression_files/figure-markdown_github/unnamed-chunk-34-1.png)

------------------------------------------------------------------------

Now let’s look at the affect of advertising using the `featured` dummy
variable, “indicating that a given brand was promoted with either an
in-store display promo or a flier ad during the week that sales and
prices were recorded. The ads can increase sales at all prices, and they
can change the price sensitivity, and they can do both of these things
in a brand-specific manner.” (BDS pg 48)

We add a 3-way interaction between price, brand, and featured; meaning
that each brand will have different elasticities and intercepts, which
will be different for featured vs not-featured.

``` r
reg_results <- lm(log(sales) ~ log(price) * brand * featured, data = weekly_oj_sales)

format_elasticity <- function(.x) {
    paste0(round(.x, 1), '%')
}

dominicks_not_featured_price_elasticity <- format_elasticity(coef(reg_results)['log(price)'])
dominicks_featured_price_elasticity <- format_elasticity(coef(reg_results)['log(price)'] + coef(reg_results)['log(price):featuredTRUE'])

minute_maid_not_featured_price_elasticity <- coef(reg_results)['log(price)'] + coef(reg_results)['log(price):brandminute.maid']
minute_maid_featured_price_elasticity <- format_elasticity(minute_maid_not_featured_price_elasticity + 
                                                               coef(reg_results)['log(price):brandminute.maid:featuredTRUE'] +
                                                               coef(reg_results)['log(price):featuredTRUE'])
minute_maid_not_featured_price_elasticity <- format_elasticity(minute_maid_not_featured_price_elasticity)

topicana_not_featured_price_elasticity <- coef(reg_results)['log(price)'] + coef(reg_results)['log(price):brandtropicana']
topicana_featured_price_elasticity <- format_elasticity(topicana_not_featured_price_elasticity +
                                                            coef(reg_results)['log(price):brandtropicana:featuredTRUE'] +
                                                            coef(reg_results)['log(price):featuredTRUE'])
topicana_not_featured_price_elasticity <- format_elasticity(topicana_not_featured_price_elasticity)

get_regression_equation(reg_results)
```

    ## [1] "log(sales) = 10.41(Intercept) + -2.77log(price) + 0.05brandminute.maid + 0.71brandtropicana + 1.09featuredTRUE + 0.78log(price):brandminute.maid + 0.74log(price):brandtropicana + -0.47log(price):featuredTRUE + 1.17brandminute.maid:featuredTRUE + 0.79brandtropicana:featuredTRUE + -1.11log(price):brandminute.maid:featuredTRUE + -0.99log(price):brandtropicana:featuredTRUE + error"

Now it no longer makes sense to talk about an overall elasticity number
for each brand,, but rather elasticity per brand depending on
`featured`.

| Featured | Dominick’s | Minute Maid | Tropicana |
|----------|------------|-------------|-----------|
| No       | `-2.8%`    | `-2%`       | `-2%`     |
| Yes      | `-3.2%`    | `-3.6%`     | `-3.5%`   |
