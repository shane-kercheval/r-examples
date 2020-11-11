library(tidyverse)
library(ggplot2)
library(stats)
library(broom)
library(forcats)
library(scales)

get_regression_equation <- function(.regression_results, .round_by=2) {
    
    actual_variable <- str_remove_all(string=as.character(.regression_results$terms)[2], pattern = '`')
    paste(actual_variable, "=", paste0(round(coefficients(.regression_results), .round_by),
                                       names(coefficients(.regression_results)),
                                       collapse = " + "),
          "+ error")
    
}

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

#' Actual vs. Predicted plot
#'
#' @param model model
#'
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 geom_line aes geom_smooth labs
#' @importFrom stringr str_remove_all
#' @export
plot_actual_vs_predicted <- function(model) {
    actual_variable <- str_remove_all(string=as.character(model$terms)[2], pattern = '`')
    
    data.frame(actual=model$model[[actual_variable]],
               pred=model$fitted.values) %>%
        ggplot(aes(x=pred, y=actual)) +
        geom_point(alpha=0.3) +
        #rt_explore_plot_scatter(variable='actual', comparison_variable='pred') +
        geom_line(aes(x=actual, y=actual), color='red') +
        geom_smooth(method = 'auto') +
        labs(title="Actual vs Predicted",
             y=paste0("Actual (`", actual_variable,"`)"),
             x="Model Prediction",
             caption="Red line indicates perfect prediction.\nBlue line represets pattern of Prediction vs `", actual_variable,"`")
}

#' Residual vs. Predicted plot
#'
#' @param model model
#'
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 geom_hline geom_smooth labs
#' @export
plot_residual_vs_predicted <- function(model) {
    actual_variable <- as.character(model$terms)[2]
    data.frame(resid=model$residuals,
               pred=model$fitted.values) %>%
        ggplot(aes(x=pred, y=resid)) +
        geom_point(alpha=0.3) +
        geom_hline(yintercept = 0, color='red') +
        geom_smooth(method = 'auto') +
        labs(title='Residual vs Predicted (a.k.a Fitted)',
             subtitle = 'Residual = Actual - Prediction; a positive Residual indicates the model is under-predicting.',
             y='Residual',
             x='Model Prediction',
             caption='Red line indicates perfect prediction (no residual).\nBlue line represets pattern of Residual vs Predicted (i.e. pattern that is not captured by the model).')
}

#' Residual vs. Predicted plot
#'
#' @param model model
#' @param predictor the predictor i.e. variable to use
#' @param dataset the original dataset
#'
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 geom_hline geom_smooth labs
#' @importFrom modelr add_residuals
#' @export
plot_residual_vs_variable <- function(model, predictor, dataset) {
    
    # need to do this because the regression (lm) automatically removes NAs
    dataset <- dataset[complete.cases(dataset), ]
    
    stopifnot(nrow(model$model) == nrow(dataset))
    
    transformed_columns <- base::setdiff(colnames(model$model), colnames(dataset))
    
    if(length(transformed_columns) > 0) {
        dataset <- cbind(dataset, model$model[, transformed_columns])
    }
    
    dataset <- dataset %>% modelr::add_residuals(model)
    
    if(is.numeric(dataset[[predictor]])) {
        
        dataset %>%
            ggplot(aes(x=!!sym(predictor), y=resid)) +
            geom_point() +
            # geom_point(alpha = 0.3) +
            geom_hline(yintercept = 0, color='red') +
            geom_smooth(method = 'auto') +
            labs(title=paste0("Residual vs Predictor (`", predictor,"`)"),
                 subtitle = "Residual = Actual - Prediction; a positive Residual indicates the model is under-predicting.",
                 y="Residual",
                 x=paste0("Predictor (`", predictor,"`)"),
                 caption="Red line indicates perfect prediction (no residual).\nBlue line represets pattern of Residual vs Predicted (i.e. pattern that is not captured by the model).")
    } else {
        dataset %>%
            rt_explore_plot_boxplot(variable='resid', comparison_variable=predictor) +
            labs(title=paste0("Residual vs Predictor (`", predictor,"`)"),
                 subtitle = 'Residual = Actual - Prediction; a positive Residual indicates the model is under-predicting.',
                 y='Residual',
                 x=paste0("Predictor (`", predictor,"`)"))
    }
}
