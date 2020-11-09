library(tidyverse)
library(ggplot2)
library(stats)
library(broom)
library(forcats)
library(scales)

#' Plot the percent of variance explained for a given regression model
#'
#' @param .regression_results model
#'
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot aes geom_col geom_text scale_y_continuous scale_fill_manual coord_flip theme_light theme labs
#' @importFrom stats anova
#' @importFrom broom tidy
#' @importFrom dplyr mutate
#' @importFrom forcats fct_reorder
#' @importFrom scales percent pretty_breaks percent_format
#'
#' @export
plot_regression_variance_explained <- function(.regression_results) {
    
    # this shows the percent of variation explained by each variable in the model
    anova_results <- .regression_results %>%
        anova() %>%
        tidy() %>%
        mutate(percent_variation = sumsq / sum(sumsq))
    stopifnot(all.equal(sum(anova_results$percent_variation), 1))
    percent_variation_explained <- with(anova_results, sum(ifelse(term == 'Residuals', 0, sumsq)) / sum(sumsq))
    
    adjusted_r_squared <- summary(.regression_results)$adj.r.squared
    r_squared <- summary(.regression_results)$r.squared
    # should be TRUE. R-Squared (not adjusted R-Squared) is the percent of variation explained by the model
    # this is the same thing anova() gives us in sumsq
    # R-Squared == SSE/SST
    # So SSE is `sumsq`
    stopifnot(all.equal(percent_variation_explained, r_squared))
    return(
        anova_results %>%
            mutate(term = ifelse(term == "Residuals", "Unexplained Variation", term),
                   type = ifelse(term == 'Unexplained Variation', 'Unexplained', 'Model')) %>%
            mutate(term = fct_reorder(term, percent_variation)) %>%
            ggplot(aes(x=term, y=percent_variation, fill=type)) +
            geom_col(alpha=0.75) +
            geom_text(aes(label=percent(percent_variation))) +
            scale_y_continuous(breaks=pretty_breaks(10),
                               labels=percent_format()) +
            scale_fill_manual(values=c('#7AA9CF', '#B4B7B9')) +
            coord_flip() +
            theme_light() +
            theme(legend.position = 'none') +
            labs(title="Amount of Variation Explained by Each Term in Regression Model",
                 y="Term",
                 x="Percent Variation Explained",
                 caption = paste0("\nThe regression model explains ~", percent(percent_variation_explained),
                                  " of the variance in the data.",
                                  "(R-Squared: ",round(r_squared, 3),
                                  "; Adjusted R-Squared: ", round(adjusted_r_squared, 3),")"))
    )
}
