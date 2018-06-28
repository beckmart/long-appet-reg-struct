Lasso-CrossValidated Results
================
Jared Lander and Rebecca Martin
6/24/2018

-   [Packages](#packages)
-   [Helper Functions](#helper-functions)
    -   [Datatable](#datatable-func)
    -   [Matrix Prepper](#matrix-prep-func)
    -   [Model Prep and Fit](#model-prep-fit)
    -   [Cross-Validation](#cross-validation)
    -   [Plotting Theme](#plot-theme)
    -   [Coefficient Path](#coef-path)
    -   [Coefficient Plot](#coef-plot)
    -   [Model Summary](#model-summary)
    -   [All Regions](#data-all)
-   [Model Prep](#model-prep)
    -   [Region Names](#region-names)
-   [glmnet Models](#models)
    -   [All Regions Models](#glmnet-all-regions)
        -   [Rating All: ageCent, ageSq, AgeCu, Gender, Type, Brain regions](#rating-all-agecent-agesq-agecu-gender-type-brain-regions)
        -   [Rating All: ageCent, Gender, Type, Brain regions](#rating-all-agecent-gender-type-brain-regions)
        -   [Far All: ageCent, ageSq, AgeCu, Gender, Brain regions](#far-all-agecent-agesq-agecu-gender-brain-regions)
        -   [Far All: ageCent, Gender, Brain regions](#far-all-agecent-gender-brain-regions)
        -   [Fit an `lmer` based on the variables chosen in `all4`](#fit-an-lmer-based-on-the-variables-chosen-in-all4)

Packages
========

These are the packages we are using frequently enough to be loaded. Other packages may be used as namespaces and are not shown here.

``` r
packages <- c('coefplot',
              'dplyr', 'DT',
              'ggplot2', 'glmnet', 
              'here', 
              'lme4',
              'magrittr', 
              'purrr',
              'rsample',
              'useful')
purrr::walk(packages, library, character.only=TRUE)
```

Helper Functions
================

These are functions to make things a little easier.

Datatable
---------

This sets a bunch of options when using `datatable` which should really be the default anyway.

``` r
makeDT <- function(data, elementId)
{
    if (missing(elementId)) {
        elementId <- sprintf("coefpath_%s", paste(sample(x = c(letters, 
            LETTERS, 0:9), size = 12, replace = TRUE), collapse = ""))
    }
    
    datatable(
        data,
        rownames=FALSE, 
        elementId=elementId,
        extensions=c('FixedColumns', 'Scroller'),
        filter='top',
        options=list(
            dom='trfi',
            fixedColumns=list(leftColumns=1),
            deferRender=TRUE,
            scrollY=400,
            scroller=TRUE
        )
    )
}
```

Matrix Prepper
--------------

For each model the design matrix and penalty factor will be different. This function computes all of that. Numeric columns are unchanged, categorical columns are converted into indicator variables. Baselines of the categorical variables are not dropped since `glmnet` does not need that. We also do not include an intercept since `glmnet` will compute that for us.

``` r
prepMatrices <- function(data, response, predictors, contrats=FALSE, sparse=TRUE, 
                         multiplier=1)
{
    # build a formula with the predictors and response
    # after the formula is build subtract the intercept
    modelFormula <- useful::build.formula(response, predictors) %>% 
        update(~ . -1)
    
    # build matrices
    x <- useful::build.x(modelFormula, data=data, 
                         contrasts=contrats, sparse=sparse)
    y <- useful::build.y(modelFormula, data=data)
    
    # build penalty factor
    # first start by setting all penalty multipliers to 1
    penaltyMultipliers <- rep(multiplier, length=ncol(x))
    # give them names so it is easier to work with
    names(penaltyMultipliers) <- colnames(x)
    # now set the ID names to 0
    penaltyMultipliers[grep(pattern='^ID', x=names(penaltyMultipliers))] <- 0
    
    return(
        list(x=x, y=y, formula=modelFormula, penalty.factor=penaltyMultipliers)
    )
}
```

Model Prep and Fit
------------------

``` r
fit_model <- function(data, response, predictors, nfolds=10, multiplier=1)
{
    prep_info <- prepMatrices(data=data, 
                              response=response,
                              predictors=predictors,
                              multiplier=multiplier
    )
    
    model <- cv.glmnet(x=prep_info$x, y=prep_info$y, 
                       alpha=1,
                       family='gaussian',
                       nfolds=nfolds,
                       penalty.factor=prep_info$penalty.factor
    )
}
```

Cross-Validation
----------------

This will be used for seeing how well our `lme4` models perform.

``` r
holdout_results <- function(splits, response, ...) {
    # Fit the model to the 90%
    mod <- lmer(..., data = analysis(splits))
    # Save the 10%
    holdout <- assessment(splits)
    # `augment` will save the predictions with the holdout data set
    res <- broom::augment(mod, newdata = holdout)
    predictions <- res$.fitted
    # Calculate whether the prediction was correct
    res$error <- predictions - holdout[[response]]
    # Return the assessment data set with the additional columns
    res
}
```

Plotting Theme
--------------

``` r
beckys.theme.conference = theme(
    panel.background = element_rect(fill='transparent'), #color="black"
    axis.line = element_line(color='black'),
    panel.grid.minor = element_line(color='transparent'),
    panel.grid.major=element_line(color="transparent"),
    axis.title.x = element_text(size=24,face = "bold"),
    axis.title.y = element_text(size=24, vjust=1, face = "bold"),
    axis.text.x = element_text(size=16, colour="black"),
    axis.text.y = element_text(size=16, colour="black"),
    legend.text=element_text(size=16),
    strip.text.y = element_text(size = 16, face="bold"),
    strip.text.x = element_text(size = 16, face="bold"),
    legend.title = element_text(face="bold"),
    axis.line.x=element_line(color = 'black'), 
    axis.line.y=element_line(color = 'black')
)
```

Coefficient Path
----------------

Orginarily when using `glmnet` we would just use `coefplot::coefpath()` but there are so many `ID` variables that we write a custom function for removing them.

``` r
pathSimple <- function(model, elementId)
{
    # get the coefficient path
    pathTable <- coefplot::extractPath(model) %>% 
        # remove the ID columns
        dplyr::select(-starts_with('ID'))
    
    # plot with dygraphs
    g <- dygraphs::dygraph(pathTable, elementId=elementId) %>% 
        dygraphs::dyAxis(name="x", label='Log Lambda') %>% 
        dygraphs::dyAxis(name="y", label='Coefficients') %>% 
        dygraphs::dyLegend(show="onmouseover") %>% 
        dygraphs::dyRangeSelector() %>% dygraphs::dyUnzoom() %>% 
        dygraphs::dyHighlight(highlightCircleSize=3, highlightSeriesBackgroundAlpha=0.5, 
                    highlightSeriesOpts=list(strokeWidth=3))
    g <- purrr::reduce(.x=names(pathTable)[-1], .f=coefplot:::annotateSeries, 
                       .init=g, x=min(pathTable$lambda))
    
    g <- g %>% dygraphs::dyEvent(x=log(model$lambda.min), label="lambda.min", 
                  color="black", labelLoc="bottom", strokePattern="dotted") %>% 
        dygraphs::dyEvent(x=log(model$lambda.1se), label="lambda.1se", 
                color="black", labelLoc="bottom", strokePattern="dotted")
    return(g)
}
```

Coefficient Plot
----------------

Likewise, we want to plot the coefplot without the `ID` variables.

``` r
coefSimple <- function(model, 
                       lambda=c('lambda.1se', 'lambda.min'), 
                       sort='magnitude', title='Coefficient Plot',
                       plot=TRUE)
{
    lambda <- match.arg(lambda)
    p <- coefplot::coefplot(model, sort=sort, lambda=lambda, plot=FALSE, 
                            intercept=FALSE) %>% 
        dplyr::filter(!grepl(pattern='^ID', Coefficient)) %>% 
        dplyr::mutate(Coefficient=factor(Coefficient, levels=Coefficient))
    
    if(plot)
    {
        p <- p %>% 
            dplyr::mutate(Coefficient=stringr::str_remove(Coefficient, '_thickness')) %>% 
            dplyr::mutate(Coefficient=stringr::str_replace_all(Coefficient, '_', ' ')) %>% 
            dplyr::mutate(Coefficient=stringr::str_replace(Coefficient, 'AgeCent', 'Age')) %>% 
            dplyr::mutate(Coefficient=factor(Coefficient, levels=Coefficient)) %>% 
            coefplot::coefplot(title=title) + beckys.theme.conference
    }
    
    p
}
```

Model Summary
-------------

``` r
modelSummary <- function(model, elementId=NULL)
{
    plot(model)
    coefSimple(model, lambda='lambda.1se', title='Coefplot 1se') %>% print
    coefSimple(model, lambda='lambda.min', title='Coefplot min') %>% print
    # pathSimple(model, elementId=elementId) %>% print
}
```

This analysis examines the raw data rather than the aggregated data.

The dataset being used (for the paper) is called 'data/AllRegionsRaw.csv' which is the dataset the paper uses. This includes all 62 aparc regions from the DKT atlas (31 per hemisphere).

Note that for the paper we use the "all4" model which includes only the regulation ratings, mean centered age, gender, and the dkt brain regions

All Regions
-----------

``` r
allRegions <- readr::read_csv(here('data', 'AllRegionsRaw.csv')) %>% 
    mutate(ID=factor(ID)) %>% 
    # correct the age centering
    mutate(AgeCent=as.numeric(scale(Age, scale = FALSE))) %>% 
    rename(AgeCentSq=AgedCentSq, AgeCentCu=AgedCentCu)
```

A sample of the data.

``` r
#makeDT(allRegions %>% sample_frac(0.10), elementId='DataAllRaw')
```

Model Prep
==========

Region Names
------------

For predictors (or features or covariates depending on your terminology) we are going to use various types of Age, Gender, BMI and the various regions. Rather than enumerating all the regions we programatically list them using regular expressions.

``` r
regionNames_all <- names(allRegions)[
    stringr::str_detect(names(allRegions), 
                        '(^(lh)|(rh)_)|(_(lh)|(rh)$)')
    ]
```

glmnet Models
=============

We originally intended to use [`cv.glmmLasso`](https://github.com/thepira/cv.glmmLasso) but it is not quite ready so instead we will use `glmnet` to approximately compute random slope models. For this to happen we turn `ID` into indicator variables. To ensure each one remains in the model we need to set their `penalty.factor`s to 0.

All Regions Models
------------------

### Rating All: ageCent, ageSq, AgeCu, Gender, Type, Brain regions

``` r
set.seed(1234)
all1 <- fit_model(data=allRegions, 
                  response='Rating',
                  predictors=c('ID', 'AgeCent', 'AgeCentSq', 'AgeCentCu', 
                               'Gender', 'Type', regionNames_all),
                  nfolds=10
)
```

We view the coefficient path, error curve and coefplot for both `lambda.1se` and `lambda.min`.

``` r
#pathSimple(all1, elementId='all1_mod') ## this makes interactive graphs. github doesn't like them b/c they are html widgets so we're using plot instead to make static graphs
plot(all1$glmnet.fit, xvar = "lambda")
```

![](lasso-crossValResults_files/figure-markdown_github/all1-error-curve-1.png)

``` r
modelSummary(all1)
```

![](lasso-crossValResults_files/figure-markdown_github/all1-error-curve-2.png)![](lasso-crossValResults_files/figure-markdown_github/all1-error-curve-3.png)![](lasso-crossValResults_files/figure-markdown_github/all1-error-curve-4.png)

### Rating All: ageCent, Gender, Type, Brain regions

``` r
set.seed(1234)
all2 <- fit_model(data=allRegions, 
                  response='Rating',
                  predictors=c('ID', 'AgeCent', 
                               'Gender', 'Type', regionNames_all),
                  nfolds=10
)
```

We view the coefficient path, error curve and coefplot for both `lambda.1se` and `lambda.min`.

``` r
#pathSimple(all2, elementId='all2_mod')
plot(all2$glmnet.fit, xvar = "lambda")
```

![](lasso-crossValResults_files/figure-markdown_github/all2-error-curve-1.png)

``` r
modelSummary(all2)
```

![](lasso-crossValResults_files/figure-markdown_github/all2-error-curve-2.png)![](lasso-crossValResults_files/figure-markdown_github/all2-error-curve-3.png)![](lasso-crossValResults_files/figure-markdown_github/all2-error-curve-4.png)

### Far All: ageCent, ageSq, AgeCu, Gender, Brain regions

``` r
set.seed(1234)
all3 <- fit_model(data=allRegions %>% filter(Type == 'Far'), 
                  response='Rating',
                  predictors=c('ID', 'AgeCent', 'AgeCentSq', 'AgeCentCu', 
                               'Gender', regionNames_all),
                  nfolds=10
)
```

We view the coefficient path, error curve and coefplot for both `lambda.1se` and `lambda.min`.

``` r
#pathSimple(all3, elementId='all3_mod')
plot(all3$glmnet.fit, xvar = "lambda")
```

![](lasso-crossValResults_files/figure-markdown_github/all3-error-curve-1.png)

``` r
modelSummary(all3)
```

![](lasso-crossValResults_files/figure-markdown_github/all3-error-curve-2.png)![](lasso-crossValResults_files/figure-markdown_github/all3-error-curve-3.png)![](lasso-crossValResults_files/figure-markdown_github/all3-error-curve-4.png)

### Far All: ageCent, Gender, Brain regions

``` r
set.seed(1234)
all4 <- fit_model(data=allRegions %>% filter(Type == 'Far'), 
                  response='Rating',
                  predictors=c('ID', 'AgeCent', 
                               'Gender', regionNames_all),
                  nfolds=10
)
```

We view the coefficient path, error curve and coefplot for both `lambda.1se` and `lambda.min`.

``` r
#pathSimple(all4, elementId='all4_mod')
plot(all4$glmnet.fit, xvar = "lambda")
```

![](lasso-crossValResults_files/figure-markdown_github/all4-error-curve-1.png)

``` r
modelSummary(all4)
```

![](lasso-crossValResults_files/figure-markdown_github/all4-error-curve-2.png)![](lasso-crossValResults_files/figure-markdown_github/all4-error-curve-3.png)![](lasso-crossValResults_files/figure-markdown_github/all4-error-curve-4.png)

### Fit an `lmer` based on the variables chosen in `all4`

Fit that model and compare with age only.

``` r
chosenVars_all <- coefplot::coefplot(all4, 
                                 sort='mag', lambda='lambda.1se', 
                                 plot=FALSE) %>% 
    dplyr::filter(!grepl(pattern='^ID', Coefficient)) %>% 
    dplyr::mutate(Coefficient=factor(Coefficient, 
                                     levels=Coefficient)) %>% 
    dplyr::filter(abs(Value) > 0.5) %>% 
    select(Coefficient) %>% 
    filter(!Coefficient %in% c('AgeCent', '(Intercept)')) %>% 
    mutate(Coefficient=as.character(Coefficient)) %>% 
    pull(Coefficient)

null_formula <- Rating ~ 1 + (1 | ID)
all4NoAge_formula <- build.formula('Rating', chosenVars_all) %>% 
    update(~ . + (1 | ID))
all4PlusAge_formula <- build.formula('Rating', chosenVars_all) %>% 
    update(~ . + AgeCent + (1 | ID))
ageOnly_formula <- Rating ~ AgeCent + (1 | ID)

null_mod <- lmer(null_formula, 
                 data=allRegions %>% filter(Type == 'Far') %>% 
                     mutate_at(.vars = chosenVars_all, scale))
all4NoAge_mod <- lmer(all4NoAge_formula, 
                        data=allRegions %>% filter(Type == 'Far') %>% 
                     mutate_at(.vars = chosenVars_all, scale))
all4PlusAge_mod <- lmer(all4PlusAge_formula, 
                          data=allRegions %>% filter(Type == 'Far') %>% 
                     mutate_at(.vars = chosenVars_all, scale))
ageOnly_mod <- lmer(ageOnly_formula, 
                    data=allRegions %>% filter(Type == 'Far') %>% 
                     mutate_at(.vars = chosenVars_all, scale))

AIC(null_mod, all4NoAge_mod, all4PlusAge_mod, ageOnly_mod)
```

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["AIC"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"3","2":"5733.397","_rn_":"null_mod"},{"1":"34","2":"5675.691","_rn_":"all4NoAge_mod"},{"1":"35","2":"5666.497","_rn_":"all4PlusAge_mod"},{"1":"4","2":"5675.648","_rn_":"ageOnly_mod"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

``` r
BIC(null_mod, all4NoAge_mod, all4PlusAge_mod, ageOnly_mod)
```

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["BIC"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"3","2":"5749.880","_rn_":"null_mod"},{"1":"34","2":"5862.502","_rn_":"all4NoAge_mod"},{"1":"35","2":"5858.802","_rn_":"all4PlusAge_mod"},{"1":"4","2":"5697.626","_rn_":"ageOnly_mod"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

``` r
# Print list of variables
chosenVars_all
```

    ##  [1] "lh_supramarginal_thickness"           
    ##  [2] "rh_pericalcarine_thickness"           
    ##  [3] "rh_middletemporal_thickness"          
    ##  [4] "rh_precuneus_thickness"               
    ##  [5] "rh_paracentral_thickness"             
    ##  [6] "rh_medialorbitofrontal_thickness"     
    ##  [7] "lh_parsopercularis_thickness"         
    ##  [8] "lh_fusiform_thickness"                
    ##  [9] "rh_entorhinal_thickness"              
    ## [10] "lh_pericalcarine_thickness"           
    ## [11] "lh_rostralanteriorcingulate_thickness"
    ## [12] "rh_parsopercularis_thickness"         
    ## [13] "lh_caudalanteriorcingulate_thickness" 
    ## [14] "lh_entorhinal_thickness"              
    ## [15] "lh_inferiortemporal_thickness"        
    ## [16] "lh_transversetemporal_thickness"      
    ## [17] "rh_lateralorbitofrontal_thickness"    
    ## [18] "rh_rostralanteriorcingulate_thickness"
    ## [19] "lh_middletemporal_thickness"          
    ## [20] "lh_caudalmiddlefrontal_thickness"     
    ## [21] "lh_isthmuscingulate_thickness"        
    ## [22] "lh_medialorbitofrontal_thickness"     
    ## [23] "lh_paracentral_thickness"             
    ## [24] "rh_postcentral_thickness"             
    ## [25] "rh_isthmuscingulate_thickness"        
    ## [26] "rh_parsorbitalis_thickness"           
    ## [27] "rh_posteriorcingulate_thickness"      
    ## [28] "rh_inferiortemporal_thickness"        
    ## [29] "lh_cuneus_thickness"                  
    ## [30] "rh_lingual_thickness"                 
    ## [31] "lh_lingual_thickness"

Now look at Cross-Validation.

``` r
set.seed(1234)
allFolds <- vfold_cv(allRegions %>% filter(Type == 'Far') %>% 
                     mutate_at(.vars = chosenVars_all, scale), V=10, repeats=10)

null_cv <- allFolds$splits %>% 
    purrr::map_df(~holdout_results(.x, 'Rating', null_formula))
all4NoAge_cv <- allFolds$splits %>% 
    purrr::map_df(~holdout_results(.x, 'Rating', all4NoAge_formula))
all4PlusAge_cv <- allFolds$splits %>% 
    purrr::map_df(~holdout_results(.x, 'Rating', all4PlusAge_formula))
ageOnly_cv <- allFolds$splits %>% 
    purrr::map_df(~holdout_results(.x, 'Rating', ageOnly_formula))
```

``` r
mean(null_cv$error^2)
```

    ## [1] 1.353117

``` r
mean(all4NoAge_cv$error^2)
```

    ## [1] 1.197715

``` r
mean(all4PlusAge_cv$error^2)
```

    ## [1] 1.189444

``` r
mean(ageOnly_cv$error^2)
```

    ## [1] 1.314538

Now we plot the AIC, BIC and MSE.

``` r
evalPlotter <- function(data, metric, modelNames)
{
    quo_var <- enquo(metric)
    metricName <- sprintf("%s", quo_name(quo_var))
    data <- data %>% 
        mutate(Model=modelNames) %>% 
        arrange(!! quo_var) %>% 
        mutate(Model = factor(Model, levels=Model))
    
    ggplot(data, aes_string(x=metricName, y='Model')) +
        geom_point() + 
        beckys.theme.conference
}
```

``` r
modelNames <- c('NULL', 'Brain Only', 'Brian and Age', 'Age Only')
evalPlotter(AIC(null_mod, all4NoAge_mod, all4PlusAge_mod, ageOnly_mod), AIC, modelNames)
```

![](lasso-crossValResults_files/figure-markdown_github/all4-vs-age-model-aic-1.png)

``` r
evalPlotter(BIC(null_mod, all4NoAge_mod, all4PlusAge_mod, ageOnly_mod), BIC, modelNames)
```

![](lasso-crossValResults_files/figure-markdown_github/all4-vs-age-model-bic-1.png)

``` r
mseDF <- tibble::tibble(
    Model=modelNames,
    MSE=c(
        mean(null_cv$error^2),
        mean(all4NoAge_cv$error^2),
        mean(all4PlusAge_cv$error^2),
        mean(ageOnly_cv$error^2)
    )
)

evalPlotter(mseDF, MSE, modelNames)
```

![](lasso-crossValResults_files/figure-markdown_github/all4-vs-age-model-mse-1.png)

Look at sjPlots of the models.

``` r
#sjPlot::plot_model(null_mod, sort.est=TRUE)
sjPlot::plot_model(all4NoAge_mod, sort.est=TRUE) + beckys.theme.conference
```

![](lasso-crossValResults_files/figure-markdown_github/all-sjplots-1.png)

``` r
sjPlot::plot_model(all4PlusAge_mod, sort.est=TRUE) + beckys.theme.conference
```

![](lasso-crossValResults_files/figure-markdown_github/all-sjplots-2.png)

``` r
sjPlot::plot_model(ageOnly_mod, sort.est=TRUE) + beckys.theme.conference
```

![](lasso-crossValResults_files/figure-markdown_github/all-sjplots-3.png)
