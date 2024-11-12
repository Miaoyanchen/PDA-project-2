------------------------------------------------------------------------

# Predicting smoking abstinence and moderation analysis for behavioral therapy and pharmacotherapy

### Background & aim

Previous randomized, placebo-controlled 2x2 factorial design in smokers with major depressive disorder (MDD) comparing behavioral activation for smoking cessation (BASC) versus standard behavioral treatment (ST) and varenicline versus placebo has discovered varenicline improved smoking abstinence. This is a follow-up analysis for the study to examine baseline variables as potential moderators of the effects of behaviorial treatment on abstinence and evaluate baseline variable as predictors of abstinence, controlling for behavioral treatment and pharmacotherapy.

### Methods
Multiple imputation is used to address missing data. We employed LASSO regression and best subset regression to perform moderation analysis to investigate the factors influencing smoke cessation and efficacy of the treatment.  Models are evaluated and compared for their performance metrics and coefficients. 

### Results
Predictors of smoking cessation and treatment moderators were identified; however, there is some uncertainty regarding the significance of the coefficients. Larger and more diverse samples are needed to validate these findings and enhance their reliability.

## Files

### Report files

-   `regression and moderation analysis.qmd` - R quarto file that contains the code for analysis, such as exploratory data analysis, model fitting, and model evaluation  
-   `references.bib` - Bibliography citation for references in the report
-   `Multiple imputation.R` - R script file that includes train-test split and multiple imputation for missing data
-   `regression and moderation analysis.pdf` - PDF version of the moderation and regression analysis report; code appendix is also available at the end of the report

## Dependencies

The following packages are required to run the code in the R quarto file:

-   Data manipulation: `dplyr`, `tidyverse`

-   Visualization: `ggplot2`

-   Cross-validation: `caret`

-   Model evaluation: `pROC`

-   Nice table - `kableExtra`

-   Exploratory data analysis - `gtsummary`, `psych`

-   Model fitting - `glmnet` (Lasso regression), `L0Learn` (Best Subset regression)



