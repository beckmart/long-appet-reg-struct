Regulation ROI Analysis
================
Rebecca Martin
6/24/2018

-   [Settings](#settings)
-   [Packages](#packages)
-   [Read in data](#read-in-data)
-   [Merge ROI data with behavioral data and add age terms](#merge-roi-data-with-behavioral-data-and-add-age-terms)
-   [Prep Graphs](#prep-graphs)
-   [Cortical ROIs](#cortical-rois)
    -   [Plot GLM clusters](#plot-glm-clusters)
    -   [Cortical cluster statistics](#cortical-cluster-statistics)
        -   [What is the shape of change in these ROIs?](#what-is-the-shape-of-change-in-these-rois)
        -   [Rates of change](#rates-of-change)
    -   [Does change in thickness predict change in behavior?](#does-change-in-thickness-predict-change-in-behavior)
        -   [rACC](#racc-1)
        -   [dmPFC](#dmpfc-1)
        -   [dlPFC](#dlpfc-1)
        -   [vlPFC](#vlpfc-1)
        -   [What is the shape of the rate changes?](#what-is-the-shape-of-the-rate-changes)
        -   [Plot Rates of Change by ROI](#plot-rates-of-change-by-roi)
        -   [Do Rates of Change in behav correlate with rates of change in neural regions?](#do-rates-of-change-in-behav-correlate-with-rates-of-change-in-neural-regions)

Note: this is not a final version and should be cleaned and proofread.

Settings
--------

``` r
knitr::opts_chunk$set(cache=TRUE)
```

``` r
root <- rprojroot::find_rstudio_root_file()
dataDir <- file.path(root, 'data')
codeDir <- file.path(root, 'code')
figureDir <- file.path(root, 'figures')
```

Packages
--------

``` r
library(tidyverse)
library(lme4)
library(lmerTest)
library(bmlm)
library(arm)
```

Read in data
------------

``` r
## Behavioral
appAvgLong <- read.table(file.path(dataDir,"app_avg_long.txt"), header=TRUE, sep="\t") ## for graphing 
appAvgWide <- read.table(file.path(dataDir,"app_avg_wide.txt"), header=TRUE, sep="\t")
demographicsLong <- read.table(file.path(dataDir,"demographicsLong.txt"), header=TRUE, sep="\t")
appBehavLongRaw <- read.table(file.path(dataDir,"app_behav_long.txt"), header=TRUE, sep="\t")

## Far ROI
far2TP <- read.table(file.path(dataDir, "far_2TP_combined_reduced.txt"), header=TRUE, sep="\t")
farRates <- read.table(file.path(dataDir, "farRates_reduced.txt"), header=TRUE, sep="\t")
```

Merge ROI data with behavioral data and add age terms
-----------------------------------------------------

``` r
appAvgLongROIs <- merge(appAvgLong, far2TP, by=c("fsid", "Phase"))

appAvgLongROIs$AgeCent <- appAvgLongROIs$Age - mean(appAvgLongROIs$Age)
appAvgLongROIs$AgeCentSq <- appAvgLongROIs$AgeCent*appAvgLongROIs$AgeCent
appAvgLongROIs$AgeCentCu <- appAvgLongROIs$AgeCent*appAvgLongROIs$AgeCent*appAvgLongROIs$AgeCent

appAvgLongReduced <- appAvgLongROIs %>% dplyr::select(Reactivity=Close, Regulation=Far, Age, fsid, Phase, Gender, rACC=rostralanteriorcingulate_lh, dmPFC=superiorfrontal_rh, dlPFC=rostralmiddlefrontal_lh, vlPFC=parsopercularis_rh) %>% gather(key=ROI, value=Thickness, -fsid, -Age, -Phase, -Gender, -Reactivity, -Regulation)

appWideROIRates <- merge(appAvgWide,farRates, by="fsid")
```

Prep Graphs
-----------

Cortical ROIs
=============

Plot GLM clusters
-----------------

``` r
ggplot(appAvgLongReduced, aes(x=Age, y=Thickness)) +
    geom_line(aes(y=Thickness, group=fsid), alpha=.5, size=.75) + 
    geom_point(aes(y=Thickness), size=1, alpha=.5) +
    geom_smooth(method='lm', color="black", ) +
    facet_wrap(~ROI, ncol=2) + 
    beckys.theme.conference +
    #theme_bw() +
    ylab("Cortical Thickness (mm)") +
    xlim(5,26) +
    #ylim(0,4) +
    scale_color_brewer(palette="Set1") +     scale_fill_brewer(palette="Set1") +
    #scale_color_manual(values=c("#e41a1c", "#377eb8")) + 
    theme(legend.position="none", legend.title=element_blank(), strip.text.y = element_text(size = 14, face="bold"),
                        strip.text.x = element_text(size = 14, face="bold"),)
```

![](ROIResults_files/figure-markdown_github/plotGLM-1.png)

``` r
#ggsave("figures/AgeROI.pdf", width=7, height=5, dpi=300)
#ggsave("figures/AgeROIDiss.pdf", width=7, height=5, dpi=300)
```

Cortical cluster statistics
---------------------------

### What is the shape of change in these ROIs?

#### RACC

``` r
summary(nullModrAcc <- lmer(rostralanteriorcingulate_lh ~ 1 + (1|fsid), data=appAvgLongROIs))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: rostralanteriorcingulate_lh ~ 1 + (1 | fsid)
    ##    Data: appAvgLongROIs
    ## 
    ## REML criterion at convergence: 9.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.1348 -0.3301 -0.0047  0.2855  4.0356 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  fsid     (Intercept) 0.08445  0.2906  
    ##  Residual             0.02035  0.1427  
    ## Number of obs: 98, groups:  fsid, 49
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)  3.00799    0.04395 48.00000   68.45   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(linModrAcc <- lmer(rostralanteriorcingulate_lh ~ AgeCent + (1|fsid), data=appAvgLongROIs))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: rostralanteriorcingulate_lh ~ AgeCent + (1 | fsid)
    ##    Data: appAvgLongROIs
    ## 
    ## REML criterion at convergence: -9.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.1893 -0.3266 -0.0773  0.2483  3.7230 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  fsid     (Intercept) 0.05209  0.2282  
    ##  Residual             0.01848  0.1359  
    ## Number of obs: 98, groups:  fsid, 49
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)  3.007994   0.035378 47.328814  85.023  < 2e-16 ***
    ## AgeCent     -0.035649   0.006298 74.411006  -5.661 2.67e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##         (Intr)
    ## AgeCent 0.000

``` r
summary(quadModrAcc <- lmer(rostralanteriorcingulate_lh ~ AgeCent + AgeCentSq + (1|fsid), data=appAvgLongROIs))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: rostralanteriorcingulate_lh ~ AgeCent + AgeCentSq + (1 | fsid)
    ##    Data: appAvgLongROIs
    ## 
    ## REML criterion at convergence: 2.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.1060 -0.3374 -0.0672  0.2745  3.6442 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  fsid     (Intercept) 0.05214  0.2284  
    ##  Residual             0.01865  0.1366  
    ## Number of obs: 98, groups:  fsid, 49
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)  2.9891175  0.0453592 77.1789154  65.899  < 2e-16 ***
    ## AgeCent     -0.0363428  0.0063930 77.6393660  -5.685  2.2e-07 ***
    ## AgeCentSq    0.0007498  0.0011256 85.0569664   0.666    0.507    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##           (Intr) AgeCnt
    ## AgeCent    0.100       
    ## AgeCentSq -0.625 -0.161

``` r
summary(cubModrAcc <- lmer(rostralanteriorcingulate_lh ~ AgeCent + AgeCentSq + AgeCentCu + (1|fsid), data=appAvgLongROIs))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: rostralanteriorcingulate_lh ~ AgeCent + AgeCentSq + AgeCentCu +  
    ##     (1 | fsid)
    ##    Data: appAvgLongROIs
    ## 
    ## REML criterion at convergence: 14.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2513 -0.3520 -0.0757  0.2918  3.7318 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  fsid     (Intercept) 0.05198  0.2280  
    ##  Residual             0.01802  0.1342  
    ## Number of obs: 98, groups:  fsid, 49
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)  2.9900501  0.0450301 76.8182115  66.401  < 2e-16 ***
    ## AgeCent     -0.0525352  0.0112787 91.2480041  -4.658 1.08e-05 ***
    ## AgeCentSq    0.0002673  0.0011450 80.1700901   0.233   0.8160    
    ## AgeCentCu    0.0003448  0.0001980 80.3047272   1.741   0.0855 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##           (Intr) AgeCnt AgCntS
    ## AgeCent    0.046              
    ## AgeCentSq -0.606  0.113       
    ## AgeCentCu  0.011 -0.827 -0.241

``` r
AIC(nullModrAcc, linModrAcc, quadModrAcc, cubModrAcc)
```

|             |   df|        AIC|
|-------------|----:|----------:|
| nullModrAcc |    3|  15.126862|
| linModrAcc  |    4|  -1.138642|
| quadModrAcc |    5|  12.160883|
| cubModrAcc  |    6|  26.375141|

#### dmPFC

``` r
summary(nullModdmPFC <- lmer(superiorfrontal_rh ~ 1 + (1|fsid), data=appAvgLongROIs))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: superiorfrontal_rh ~ 1 + (1 | fsid)
    ##    Data: appAvgLongROIs
    ## 
    ## REML criterion at convergence: -76
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.55191 -0.34479  0.04357  0.42294  2.65309 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  fsid     (Intercept) 0.064280 0.25354 
    ##  Residual             0.005034 0.07095 
    ## Number of obs: 98, groups:  fsid, 49
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)  3.07977    0.03692 48.00000   83.41   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(linModdmPFC <- lmer(superiorfrontal_rh ~ AgeCent + (1|fsid), data=appAvgLongROIs))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: superiorfrontal_rh ~ AgeCent + (1 | fsid)
    ##    Data: appAvgLongROIs
    ## 
    ## REML criterion at convergence: -108.8
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.38586 -0.49084 -0.02841  0.52305  2.34302 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  fsid     (Intercept) 0.03545  0.18829 
    ##  Residual             0.00386  0.06213 
    ## Number of obs: 98, groups:  fsid, 49
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)  3.079769   0.027621 46.742745 111.501  < 2e-16 ***
    ## AgeCent     -0.029845   0.004074 95.679632  -7.326 7.47e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##         (Intr)
    ## AgeCent 0.000

``` r
summary(quadModdmPFC <- lmer(superiorfrontal_rh ~ AgeCent + AgeCentSq + (1|fsid), data=appAvgLongROIs))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: superiorfrontal_rh ~ AgeCent + AgeCentSq + (1 | fsid)
    ##    Data: appAvgLongROIs
    ## 
    ## REML criterion at convergence: -96.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.28061 -0.49562  0.03837  0.53871  2.24028 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  fsid     (Intercept) 0.034967 0.18700 
    ##  Residual             0.003938 0.06276 
    ## Number of obs: 98, groups:  fsid, 49
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)  3.0683096  0.0311301 67.3377979  98.564  < 2e-16 ***
    ## AgeCent     -0.0301432  0.0040889 94.8030586  -7.372 6.24e-11 ***
    ## AgeCentSq    0.0004552  0.0005828 63.0948442   0.781    0.438    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##           (Intr) AgeCnt
    ## AgeCent    0.031       
    ## AgeCentSq -0.471 -0.065

``` r
summary(cubModdmPFC <- lmer(superiorfrontal_rh ~ AgeCent + AgeCentSq + AgeCentCu + (1|fsid), data=appAvgLongROIs))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: superiorfrontal_rh ~ AgeCent + AgeCentSq + AgeCentCu + (1 | fsid)
    ##    Data: appAvgLongROIs
    ## 
    ## REML criterion at convergence: -93.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.62034 -0.40879  0.03099  0.50091  2.51844 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  fsid     (Intercept) 0.033164 0.18211 
    ##  Residual             0.003205 0.05661 
    ## Number of obs: 98, groups:  fsid, 49
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)  3.068e+00  2.981e-02  6.639e+01 102.926  < 2e-16 ***
    ## AgeCent     -4.961e-02  6.433e-03  9.260e+01  -7.712 1.38e-11 ***
    ## AgeCentSq   -1.616e-06  5.426e-04  6.072e+01  -0.003 0.997634    
    ## AgeCentCu    3.654e-04  9.484e-05  6.451e+01   3.853 0.000271 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##           (Intr) AgeCnt AgCntS
    ## AgeCent    0.022              
    ## AgeCentSq -0.437  0.133       
    ## AgeCentCu -0.010 -0.804 -0.204

``` r
AIC(nullModdmPFC, linModdmPFC, quadModdmPFC, cubModdmPFC)
```

|              |   df|         AIC|
|--------------|----:|-----------:|
| nullModdmPFC |    3|   -70.04486|
| linModdmPFC  |    4|  -100.75008|
| quadModdmPFC |    5|   -86.28984|
| cubModdmPFC  |    6|   -81.28673|

#### dlPFC

``` r
summary(nullModdlPFC <- lmer(rostralmiddlefrontal_lh ~ 1 + (1|fsid), data=appAvgLongROIs))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: rostralmiddlefrontal_lh ~ 1 + (1 | fsid)
    ##    Data: appAvgLongROIs
    ## 
    ## REML criterion at convergence: -30
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.25323 -0.48817 -0.01149  0.39428  1.96299 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  fsid     (Intercept) 0.06562  0.2562  
    ##  Residual             0.01203  0.1097  
    ## Number of obs: 98, groups:  fsid, 49
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)  2.42383    0.03823 48.00000   63.39   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(linModdlPFC <- lmer(rostralmiddlefrontal_lh ~ AgeCent + (1|fsid), data=appAvgLongROIs))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: rostralmiddlefrontal_lh ~ AgeCent + (1 | fsid)
    ##    Data: appAvgLongROIs
    ## 
    ## REML criterion at convergence: -46.4
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.20637 -0.46855 -0.07552  0.43090  1.73141 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  fsid     (Intercept) 0.04394  0.2096  
    ##  Residual             0.01063  0.1031  
    ## Number of obs: 98, groups:  fsid, 49
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)  2.423834   0.031705 47.435371  76.449  < 2e-16 ***
    ## AgeCent     -0.029166   0.005395 82.415316  -5.406 6.15e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##         (Intr)
    ## AgeCent 0.000

``` r
summary(quadModdlPFC <- lmer(rostralmiddlefrontal_lh ~ AgeCent + AgeCentSq + (1|fsid), data=appAvgLongROIs))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: rostralmiddlefrontal_lh ~ AgeCent + AgeCentSq + (1 | fsid)
    ##    Data: appAvgLongROIs
    ## 
    ## REML criterion at convergence: -36.5
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.24152 -0.38292 -0.07711  0.47034  1.64318 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  fsid     (Intercept) 0.04358  0.2088  
    ##  Residual             0.01045  0.1022  
    ## Number of obs: 98, groups:  fsid, 49
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)  2.3900586  0.0386442 75.3682023  61.848  < 2e-16 ***
    ## AgeCent     -0.0301994  0.0054079 84.7500798  -5.584 2.77e-07 ***
    ## AgeCentSq    0.0013416  0.0008858 77.1637223   1.515    0.134    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##           (Intr) AgeCnt
    ## AgeCent    0.073       
    ## AgeCentSq -0.577 -0.127

``` r
summary(cubModdlPFC <- lmer(rostralmiddlefrontal_lh ~ AgeCent + AgeCentSq + AgeCentCu + (1|fsid), data=appAvgLongROIs))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: rostralmiddlefrontal_lh ~ AgeCent + AgeCentSq + AgeCentCu + (1 |  
    ##     fsid)
    ##    Data: appAvgLongROIs
    ## 
    ## REML criterion at convergence: -21.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.19425 -0.42583 -0.05671  0.51234  1.62184 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  fsid     (Intercept) 0.04425  0.2104  
    ##  Residual             0.01042  0.1021  
    ## Number of obs: 98, groups:  fsid, 49
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)  2.3900745  0.0388247 74.7799232  61.561  < 2e-16 ***
    ## AgeCent     -0.0356811  0.0094395 93.0173761  -3.780 0.000277 ***
    ## AgeCentSq    0.0011958  0.0009095 73.2399659   1.315 0.192643    
    ## AgeCentCu    0.0001123  0.0001574 74.6752360   0.713 0.477839    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##           (Intr) AgeCnt AgCntS
    ## AgeCent    0.041              
    ## AgeCentSq -0.560  0.113       
    ## AgeCentCu  0.000 -0.818 -0.224

``` r
AIC(nullModdlPFC, linModdlPFC, quadModdlPFC, cubModdlPFC)
```

|              |   df|         AIC|
|--------------|----:|-----------:|
| nullModdlPFC |    3|  -24.024501|
| linModdlPFC  |    4|  -38.423693|
| quadModdlPFC |    5|  -26.487938|
| cubModdlPFC  |    6|   -9.317424|

#### vlPFC

``` r
summary(nullModvlPFC <- lmer(parsopercularis_rh ~ 1 + (1|fsid), data=appAvgLongROIs))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: parsopercularis_rh ~ 1 + (1 | fsid)
    ##    Data: appAvgLongROIs
    ## 
    ## REML criterion at convergence: -47.6
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.71700 -0.53536 -0.01913  0.53485  1.70044 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  fsid     (Intercept) 0.04666  0.2160  
    ##  Residual             0.01142  0.1069  
    ## Number of obs: 98, groups:  fsid, 49
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)  2.68167    0.03269 48.00000   82.03   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(linModvlPFC <- lmer(parsopercularis_rh ~ AgeCent + (1|fsid), data=appAvgLongROIs))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: parsopercularis_rh ~ AgeCent + (1 | fsid)
    ##    Data: appAvgLongROIs
    ## 
    ## REML criterion at convergence: -66.9
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84733 -0.50601 -0.05602  0.52227  1.53469 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  fsid     (Intercept) 0.039011 0.19751 
    ##  Residual             0.007968 0.08926 
    ## Number of obs: 98, groups:  fsid, 49
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)  2.681669   0.029622 45.809931  90.531  < 2e-16 ***
    ## AgeCent     -0.028134   0.004919 85.469555  -5.719 1.54e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##         (Intr)
    ## AgeCent 0.000

``` r
summary(quadModvlPFC <- lmer(parsopercularis_rh ~ AgeCent + AgeCentSq + (1|fsid), data=appAvgLongROIs))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: parsopercularis_rh ~ AgeCent + AgeCentSq + (1 | fsid)
    ##    Data: appAvgLongROIs
    ## 
    ## REML criterion at convergence: -56.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.70951 -0.49865  0.00993  0.51649  1.59586 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  fsid     (Intercept) 0.041450 0.2036  
    ##  Residual             0.007448 0.0863  
    ## Number of obs: 98, groups:  fsid, 49
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)  2.6535420  0.0360346 71.1729723  73.639  < 2e-16 ***
    ## AgeCent     -0.0294152  0.0049665 89.1973705  -5.923 5.82e-08 ***
    ## AgeCentSq    0.0011172  0.0007708 69.6475189   1.449    0.152    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##           (Intr) AgeCnt
    ## AgeCent    0.055       
    ## AgeCentSq -0.539 -0.103

``` r
summary(cubModvlPFC <- lmer(parsopercularis_rh ~ AgeCent + AgeCentSq + AgeCentCu + (1|fsid), data=appAvgLongROIs))
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: parsopercularis_rh ~ AgeCent + AgeCentSq + AgeCentCu + (1 | fsid)
    ##    Data: appAvgLongROIs
    ## 
    ## REML criterion at convergence: -40.9
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.62302 -0.48828 -0.03118  0.52141  1.63765 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  fsid     (Intercept) 0.040741 0.20184 
    ##  Residual             0.007631 0.08736 
    ## Number of obs: 98, groups:  fsid, 49
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)  2.654e+00  3.595e-02  7.123e+01  73.842  < 2e-16 ***
    ## AgeCent     -2.431e-02  8.541e-03  9.370e+01  -2.847  0.00543 ** 
    ## AgeCentSq    1.215e-03  7.958e-04  6.747e+01   1.527  0.13148    
    ## AgeCentCu   -9.784e-05  1.380e-04  6.980e+01  -0.709  0.48063    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##           (Intr) AgeCnt AgCntS
    ## AgeCent    0.037              
    ## AgeCentSq -0.530  0.115       
    ## AgeCentCu -0.004 -0.813 -0.216

``` r
AIC(nullModvlPFC, linModvlPFC, quadModvlPFC, cubModvlPFC)
```

|              |   df|        AIC|
|--------------|----:|----------:|
| nullModvlPFC |    3|  -41.57940|
| linModvlPFC  |    4|  -58.90453|
| quadModvlPFC |    5|  -46.34736|
| cubModvlPFC  |    6|  -28.89453|

### Rates of change

Does change in thickness predict change in behavior?
----------------------------------------------------

#### rACC

``` r
## Thickness and behav rate change
summary(changethickbehav <- lm(FarAnn ~ rostralanteriorcingulate_lh_diffAnn, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = FarAnn ~ rostralanteriorcingulate_lh_diffAnn, data = appWideROIRates)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.78507 -0.38156 -0.00946  0.27904  1.18270 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)                         -0.15052    0.06799  -2.214   0.0317 *
    ## rostralanteriorcingulate_lh_diffAnn -0.56156    0.77304  -0.726   0.4712  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4615 on 47 degrees of freedom
    ## Multiple R-squared:  0.0111, Adjusted R-squared:  -0.009937 
    ## F-statistic: 0.5277 on 1 and 47 DF,  p-value: 0.4712

``` r
## Age and thickness rate change? 
summary(changethickAge <- lm(rostralanteriorcingulate_lh_diffAnn ~ Age_T1, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = rostralanteriorcingulate_lh_diffAnn ~ Age_T1, data = appWideROIRates)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.15411 -0.04339 -0.00622  0.02372  0.43285 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  0.041635   0.037342   1.115    0.271
    ## Age_T1      -0.001437   0.002508  -0.573    0.569
    ## 
    ## Residual standard error: 0.08678 on 47 degrees of freedom
    ## Multiple R-squared:  0.006935,   Adjusted R-squared:  -0.01419 
    ## F-statistic: 0.3282 on 1 and 47 DF,  p-value: 0.5695

``` r
### Age X Rate interactions on behavior?
summary(changeInt <- lm(FarAnn ~  rostralanteriorcingulate_lh_diffAnn*AgeCent_T1, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = FarAnn ~ rostralanteriorcingulate_lh_diffAnn * AgeCent_T1, 
    ##     data = appWideROIRates)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.8126 -0.3625  0.0106  0.2919  1.0415 
    ## 
    ## Coefficients:
    ##                                                Estimate Std. Error t value
    ## (Intercept)                                    -0.14626    0.06828  -2.142
    ## rostralanteriorcingulate_lh_diffAnn            -0.88403    0.82237  -1.075
    ## AgeCent_T1                                     -0.01149    0.01343  -0.856
    ## rostralanteriorcingulate_lh_diffAnn:AgeCent_T1 -0.16805    0.17053  -0.985
    ##                                                Pr(>|t|)  
    ## (Intercept)                                      0.0376 *
    ## rostralanteriorcingulate_lh_diffAnn              0.2881  
    ## AgeCent_T1                                       0.3967  
    ## rostralanteriorcingulate_lh_diffAnn:AgeCent_T1   0.3297  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4626 on 45 degrees of freedom
    ## Multiple R-squared:  0.04885,    Adjusted R-squared:  -0.01456 
    ## F-statistic: 0.7704 on 3 and 45 DF,  p-value: 0.5167

#### dmPFC

``` r
## Thickness and behav rate change
summary(changethickbehav <- lm(FarAnn ~ superiorfrontal_rh_diffAnn, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = FarAnn ~ superiorfrontal_rh_diffAnn, data = appWideROIRates)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.8214 -0.3479 -0.0689  0.2961  1.1195 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)                 -0.1710     0.0729  -2.346   0.0232 *
    ## superiorfrontal_rh_diffAnn   0.4481     1.6109   0.278   0.7821  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4637 on 47 degrees of freedom
    ## Multiple R-squared:  0.001643,   Adjusted R-squared:  -0.0196 
    ## F-statistic: 0.07737 on 1 and 47 DF,  p-value: 0.7821

``` r
## Age and thickness rate change? 
summary(changethickAge <- lm(superiorfrontal_rh_diffAnn ~ Age_T1, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = superiorfrontal_rh_diffAnn ~ Age_T1, data = appWideROIRates)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.069712 -0.023667 -0.008206  0.023395  0.123273 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  0.031114   0.017968   1.732   0.0899 .
    ## Age_T1      -0.000871   0.001207  -0.722   0.4741  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.04176 on 47 degrees of freedom
    ## Multiple R-squared:  0.01096,    Adjusted R-squared:  -0.01008 
    ## F-statistic: 0.5208 on 1 and 47 DF,  p-value: 0.4741

``` r
### Age X Rate interactions on behavior?
summary(changeInt <- lm(FarAnn ~  superiorfrontal_rh_diffAnn*AgeCent_T1, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = FarAnn ~ superiorfrontal_rh_diffAnn * AgeCent_T1, 
    ##     data = appWideROIRates)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.81063 -0.29751 -0.06015  0.26851  1.09958 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                           -0.16606    0.07412  -2.241    0.030
    ## superiorfrontal_rh_diffAnn             0.43533    1.67426   0.260    0.796
    ## AgeCent_T1                            -0.01152    0.01374  -0.839    0.406
    ## superiorfrontal_rh_diffAnn:AgeCent_T1  0.13105    0.34718   0.377    0.708
    ##                                        
    ## (Intercept)                           *
    ## superiorfrontal_rh_diffAnn             
    ## AgeCent_T1                             
    ## superiorfrontal_rh_diffAnn:AgeCent_T1  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4698 on 45 degrees of freedom
    ## Multiple R-squared:  0.01877,    Adjusted R-squared:  -0.04665 
    ## F-statistic: 0.2869 on 3 and 45 DF,  p-value: 0.8346

#### dlPFC

``` r
## Thickness and behav rate change
summary(changethickbehav <- lm(FarAnn ~ rostralmiddlefrontal_lh_diffAnn, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = FarAnn ~ rostralmiddlefrontal_lh_diffAnn, data = appWideROIRates)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.8703 -0.3074 -0.0215  0.2833  1.1427 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)                     -0.17637    0.06813  -2.589   0.0128 *
    ## rostralmiddlefrontal_lh_diffAnn  0.67612    0.85311   0.793   0.4320  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.461 on 47 degrees of freedom
    ## Multiple R-squared:  0.01319,    Adjusted R-squared:  -0.007808 
    ## F-statistic: 0.6281 on 1 and 47 DF,  p-value: 0.432

``` r
## Age and thickness rate change? 
summary(changethickAge <- lm(rostralmiddlefrontal_lh_diffAnn ~ Age_T1, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = rostralmiddlefrontal_lh_diffAnn ~ Age_T1, data = appWideROIRates)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.208659 -0.034981  0.002486  0.039677  0.175878 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  0.054947   0.033496   1.640    0.108
    ## Age_T1      -0.002459   0.002250  -1.093    0.280
    ## 
    ## Residual standard error: 0.07785 on 47 degrees of freedom
    ## Multiple R-squared:  0.02479,    Adjusted R-squared:  0.004043 
    ## F-statistic: 1.195 on 1 and 47 DF,  p-value: 0.2799

``` r
### Age X Rate interactions on behavior?
summary(changeInt <- lm(FarAnn ~  rostralmiddlefrontal_lh_diffAnn*AgeCent_T1, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = FarAnn ~ rostralmiddlefrontal_lh_diffAnn * AgeCent_T1, 
    ##     data = appWideROIRates)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.8660 -0.3109 -0.0046  0.2914  1.0753 
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error t value
    ## (Intercept)                                -0.16714    0.06981  -2.394
    ## rostralmiddlefrontal_lh_diffAnn             0.57164    0.87484   0.653
    ## AgeCent_T1                                 -0.01036    0.01370  -0.756
    ## rostralmiddlefrontal_lh_diffAnn:AgeCent_T1  0.08749    0.15413   0.568
    ##                                            Pr(>|t|)  
    ## (Intercept)                                  0.0209 *
    ## rostralmiddlefrontal_lh_diffAnn              0.5168  
    ## AgeCent_T1                                   0.4536  
    ## rostralmiddlefrontal_lh_diffAnn:AgeCent_T1   0.5731  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4669 on 45 degrees of freedom
    ## Multiple R-squared:  0.03119,    Adjusted R-squared:  -0.03339 
    ## F-statistic: 0.483 on 3 and 45 DF,  p-value: 0.6958

#### vlPFC

``` r
## Thickness and behav rate change
summary(changethickbehav <- lm(FarAnn ~ parsopercularis_rh_diffAnn, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = FarAnn ~ parsopercularis_rh_diffAnn, data = appWideROIRates)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.89490 -0.30572 -0.07489  0.29154  1.03561 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)                -0.19441    0.07672  -2.534   0.0147 *
    ## parsopercularis_rh_diffAnn  0.82732    1.02349   0.808   0.4230  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4609 on 47 degrees of freedom
    ## Multiple R-squared:  0.01371,    Adjusted R-squared:  -0.007273 
    ## F-statistic: 0.6534 on 1 and 47 DF,  p-value: 0.423

``` r
## Age and thickness rate change? 
summary(changethickAge <- lm(parsopercularis_rh_diffAnn ~ Age_T1, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = parsopercularis_rh_diffAnn ~ Age_T1, data = appWideROIRates)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.192359 -0.042838 -0.007311  0.044286  0.132042 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  0.076075   0.027661   2.750  0.00843 **
    ## Age_T1      -0.002677   0.001858  -1.441  0.15622   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06428 on 47 degrees of freedom
    ## Multiple R-squared:  0.04231,    Adjusted R-squared:  0.02193 
    ## F-statistic: 2.076 on 1 and 47 DF,  p-value: 0.1562

``` r
### Age X Rate interactions on behavior?
summary(changeInt <- lm(FarAnn ~  parsopercularis_rh_diffAnn*AgeCent_T1, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = FarAnn ~ parsopercularis_rh_diffAnn * AgeCent_T1, 
    ##     data = appWideROIRates)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.89008 -0.28616 -0.02664  0.27813  1.02907 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                           -0.18574    0.07884  -2.356   0.0229
    ## parsopercularis_rh_diffAnn             0.76142    1.10328   0.690   0.4936
    ## AgeCent_T1                            -0.01122    0.01530  -0.733   0.4671
    ## parsopercularis_rh_diffAnn:AgeCent_T1  0.06504    0.23387   0.278   0.7822
    ##                                        
    ## (Intercept)                           *
    ## parsopercularis_rh_diffAnn             
    ## AgeCent_T1                             
    ## parsopercularis_rh_diffAnn:AgeCent_T1  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4683 on 45 degrees of freedom
    ## Multiple R-squared:  0.02539,    Adjusted R-squared:  -0.03958 
    ## F-statistic: 0.3908 on 3 and 45 DF,  p-value: 0.7602

#### What is the shape of the rate changes?

``` r
## rACC
rACCchangethickAgeNull <- lm(rostralanteriorcingulate_lh_diffAnn ~ 1, data=appWideROIRates)
rACCchangethickAgeLin <- lm(rostralanteriorcingulate_lh_diffAnn ~ AgeCent_T1, data=appWideROIRates)
rACCchangethickAgeSq <- lm(rostralanteriorcingulate_lh_diffAnn ~ AgeCent_T1 + AgedCentSq_T1, data=appWideROIRates)
rACCchangethickAgeCu <- lm(rostralanteriorcingulate_lh_diffAnn ~ Age_T1 + AgedCentSq_T1 + AgedCentCu_T1, data=appWideROIRates)

AIC(rACCchangethickAgeNull, rACCchangethickAgeLin, rACCchangethickAgeSq, rACCchangethickAgeCu)
```

|                        |   df|        AIC|
|------------------------|----:|----------:|
| rACCchangethickAgeNull |    2|  -98.19017|
| rACCchangethickAgeLin  |    3|  -96.53115|
| rACCchangethickAgeSq   |    4|  -96.58303|
| rACCchangethickAgeCu   |    5|  -95.69997|

``` r
## dmPFC
dmPFCchangethickAgeNull <- lm(superiorfrontal_rh_diffAnn ~ 1, data=appWideROIRates)
dmPFCchangethickAgeLin <- lm(superiorfrontal_rh_diffAnn ~ AgeCent_T1, data=appWideROIRates)
dmPFCchangethickAgeSq <- lm(superiorfrontal_rh_diffAnn ~ AgeCent_T1 + AgedCentSq_T1, data=appWideROIRates)
dmPFCchangethickAgeCu <- lm(superiorfrontal_rh_diffAnn ~ Age_T1 + AgedCentSq_T1 + AgedCentCu_T1, data=appWideROIRates)

AIC(dmPFCchangethickAgeNull, dmPFCchangethickAgeLin, dmPFCchangethickAgeSq, dmPFCchangethickAgeCu)
```

|                         |   df|        AIC|
|-------------------------|----:|----------:|
| dmPFCchangethickAgeNull |    2|  -169.6798|
| dmPFCchangethickAgeLin  |    3|  -168.2198|
| dmPFCchangethickAgeSq   |    4|  -174.3851|
| dmPFCchangethickAgeCu   |    5|  -172.4441|

``` r
## dlPFC
dlPFCchangethickAgeNull <- lm(rostralmiddlefrontal_lh_diffAnn ~ 1, data=appWideROIRates)
dlPFCchangethickAgeLin <- lm(rostralmiddlefrontal_lh_diffAnn ~ AgeCent_T1, data=appWideROIRates)
dlPFCchangethickAgeSq <- lm(rostralmiddlefrontal_lh_diffAnn ~ AgeCent_T1 + AgedCentSq_T1, data=appWideROIRates)
dlPFCchangethickAgeCu <- lm(rostralmiddlefrontal_lh_diffAnn ~ Age_T1 + AgedCentSq_T1 + AgedCentCu_T1, data=appWideROIRates)

AIC(dlPFCchangethickAgeNull, dlPFCchangethickAgeLin, dlPFCchangethickAgeSq, dlPFCchangethickAgeCu)
```

|                         |   df|        AIC|
|-------------------------|----:|----------:|
| dlPFCchangethickAgeNull |    2|  -107.9529|
| dlPFCchangethickAgeLin  |    3|  -107.1831|
| dlPFCchangethickAgeSq   |    4|  -106.6535|
| dlPFCchangethickAgeCu   |    5|  -107.9159|

``` r
## vlPFC
vlPFCchangethickAgeNull <- lm(parsopercularis_rh_diffAnn ~ 1, data=appWideROIRates)
vlPFCchangethickAgeLin <- lm(parsopercularis_rh_diffAnn ~ AgeCent_T1, data=appWideROIRates)
vlPFCchangethickAgeSq <- lm(parsopercularis_rh_diffAnn ~ AgeCent_T1 + AgedCentSq_T1, data=appWideROIRates)
vlPFCchangethickAgeCu <- lm(parsopercularis_rh_diffAnn ~ AgeCent_T1 + AgedCentSq_T1 + AgedCentCu_T1, data=appWideROIRates)

AIC(vlPFCchangethickAgeNull, vlPFCchangethickAgeLin, vlPFCchangethickAgeSq, vlPFCchangethickAgeCu)
```

|                         |   df|        AIC|
|-------------------------|----:|----------:|
| vlPFCchangethickAgeNull |    2|  -125.8227|
| vlPFCchangethickAgeLin  |    3|  -125.9410|
| vlPFCchangethickAgeSq   |    4|  -124.0794|
| vlPFCchangethickAgeCu   |    5|  -123.5906|

#### Plot Rates of Change by ROI

``` r
AparcRename <- appWideROIRates %>% 
    dplyr::select(fsid, Age_T1, dmPFC=superiorfrontal_rh_diffAnn,
                  dlPFC=rostralmiddlefrontal_lh_diffAnn,
                  vlPFC=parsopercularis_rh_diffAnn,
                  rACC=rostralanteriorcingulate_lh_diffAnn) %>% 
    gather(key=ROI, value=Thickness, -fsid, -Age_T1)



ggplot(AparcRename, aes(x=Age_T1, y=Thickness)) +
    geom_point(aes(y=Thickness), shape=1, size=1, alpha=.4) +
    geom_smooth(method='lm', color="black") + # linear change
    #geom_smooth(color="black") +  # let's look nonlinear
    geom_hline(yintercept = 0) +
    facet_wrap(~ROI, ncol=6) + 
    beckys.theme.conference +
    #theme_bw() +
    ylab("Annualized Thickness Change") +
    xlab("Age at T1") +
    xlim(5, 26) +
    ylim(-.2,.2)
```

    ## Warning: Removed 2 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](ROIResults_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
#ggsave("figures/farRateROI4RandR.pdf", width=10, height=6, dpi=300)
```

#### Do Rates of Change in behav correlate with rates of change in neural regions?

``` r
summary(rACCchangethickBehav <- lm(rostralanteriorcingulate_lh_diffAnn ~ FarAnn, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = rostralanteriorcingulate_lh_diffAnn ~ FarAnn, data = appWideROIRates)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.16299 -0.03464 -0.00794  0.03018  0.43748 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  0.01824    0.01314   1.388    0.172
    ## FarAnn      -0.01977    0.02722  -0.726    0.471
    ## 
    ## Residual standard error: 0.0866 on 47 degrees of freedom
    ## Multiple R-squared:  0.0111, Adjusted R-squared:  -0.009937 
    ## F-statistic: 0.5277 on 1 and 47 DF,  p-value: 0.4712

``` r
summary(dmPFCchangethickBehav <- lm(superiorfrontal_rh_diffAnn ~ FarAnn, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = superiorfrontal_rh_diffAnn ~ FarAnn, data = appWideROIRates)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.079181 -0.026270 -0.007078  0.020391  0.127286 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) 0.019478   0.006365   3.060  0.00365 **
    ## FarAnn      0.003668   0.013186   0.278  0.78212   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.04195 on 47 degrees of freedom
    ## Multiple R-squared:  0.001643,   Adjusted R-squared:  -0.0196 
    ## F-statistic: 0.07737 on 1 and 47 DF,  p-value: 0.7821

``` r
summary(dlPFCchangethickBehav <- lm(rostralmiddlefrontal_lh_diffAnn ~ FarAnn, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = rostralmiddlefrontal_lh_diffAnn ~ FarAnn, data = appWideROIRates)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.207222 -0.039512  0.006334  0.048597  0.171925 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  0.02358    0.01188   1.985    0.053 .
    ## FarAnn       0.01951    0.02461   0.793    0.432  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07831 on 47 degrees of freedom
    ## Multiple R-squared:  0.01319,    Adjusted R-squared:  -0.007808 
    ## F-statistic: 0.6281 on 1 and 47 DF,  p-value: 0.432

``` r
summary(vlPFCchangethickBehav <- lm(parsopercularis_rh_diffAnn ~ FarAnn, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = parsopercularis_rh_diffAnn ~ FarAnn, data = appWideROIRates)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.189954 -0.046331  0.000891  0.038100  0.122604 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.041171   0.009898   4.160 0.000134 ***
    ## FarAnn      0.016574   0.020503   0.808 0.422972    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06524 on 47 degrees of freedom
    ## Multiple R-squared:  0.01371,    Adjusted R-squared:  -0.007273 
    ## F-statistic: 0.6534 on 1 and 47 DF,  p-value: 0.423

``` r
# Nope rates of change in behav don't relate to rates of change in structure

## What about controlling for age? 
summary(rACCchangethickBehavAge <- lm(rostralanteriorcingulate_lh_diffAnn ~ FarAnn + AgeCent_T1, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = rostralanteriorcingulate_lh_diffAnn ~ FarAnn + AgeCent_T1, 
    ##     data = appWideROIRates)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.14595 -0.04056 -0.00951  0.02930  0.43009 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  0.018240   0.013218   1.380    0.174
    ## FarAnn      -0.022005   0.027587  -0.798    0.429
    ## AgeCent_T1  -0.001684   0.002537  -0.664    0.510
    ## 
    ## Residual standard error: 0.08712 on 46 degrees of freedom
    ## Multiple R-squared:  0.02048,    Adjusted R-squared:  -0.0221 
    ## F-statistic: 0.481 on 2 and 46 DF,  p-value: 0.6213

``` r
summary(dmPFCchangethickBehavAge <- lm(superiorfrontal_rh_diffAnn ~ FarAnn + AgeCent_T1, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = superiorfrontal_rh_diffAnn ~ FarAnn + AgeCent_T1, 
    ##     data = appWideROIRates)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.070658 -0.024794 -0.007438  0.023446  0.123593 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  0.0194775  0.0064015   3.043  0.00387 **
    ## FarAnn       0.0025503  0.0133606   0.191  0.84946   
    ## AgeCent_T1  -0.0008424  0.0012287  -0.686  0.49640   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.04219 on 46 degrees of freedom
    ## Multiple R-squared:  0.01174,    Adjusted R-squared:  -0.03123 
    ## F-statistic: 0.2733 on 2 and 46 DF,  p-value: 0.7621

``` r
summary(dlPFCchangethickBehavAge <- lm(rostralmiddlefrontal_lh_diffAnn ~ FarAnn + AgeCent_T1, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = rostralmiddlefrontal_lh_diffAnn ~ FarAnn + AgeCent_T1, 
    ##     data = appWideROIRates)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.208560 -0.036993  0.007269  0.040870  0.167169 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  0.023578   0.011881   1.984   0.0532 .
    ## FarAnn       0.016488   0.024798   0.665   0.5094  
    ## AgeCent_T1  -0.002275   0.002281  -0.997   0.3238  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07831 on 46 degrees of freedom
    ## Multiple R-squared:  0.03408,    Adjusted R-squared:  -0.007921 
    ## F-statistic: 0.8114 on 2 and 46 DF,  p-value: 0.4505

``` r
summary(vlPFCchangethickBehavAge <- lm(parsopercularis_rh_diffAnn ~ FarAnn + AgeCent_T1, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = parsopercularis_rh_diffAnn ~ FarAnn + AgeCent_T1, 
    ##     data = appWideROIRates)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.197754 -0.040794 -0.003324  0.043685  0.117901 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.041169   0.009814   4.195 0.000123 ***
    ## FarAnn       0.013219   0.020483   0.645 0.521912    
    ## AgeCent_T1  -0.002529   0.001884  -1.343 0.185994    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06469 on 46 degrees of freedom
    ## Multiple R-squared:  0.0509, Adjusted R-squared:  0.009637 
    ## F-statistic: 1.234 on 2 and 46 DF,  p-value: 0.3007

``` r
# Nope. 

## Interaction with age? 
## What about controlling for age? 
summary(rACCchangethickBehaXvAge <- lm(rostralanteriorcingulate_lh_diffAnn ~ FarAnn*AgeCent_T1, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = rostralanteriorcingulate_lh_diffAnn ~ FarAnn * AgeCent_T1, 
    ##     data = appWideROIRates)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.13698 -0.04748 -0.00624  0.02637  0.43182 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)        0.017339   0.013430   1.291    0.203
    ## FarAnn            -0.022417   0.027816  -0.806    0.425
    ## AgeCent_T1        -0.002187   0.002727  -0.802    0.427
    ## FarAnn:AgeCent_T1 -0.003049   0.005737  -0.532    0.598
    ## 
    ## Residual standard error: 0.08781 on 45 degrees of freedom
    ## Multiple R-squared:  0.02659,    Adjusted R-squared:  -0.0383 
    ## F-statistic: 0.4098 on 3 and 45 DF,  p-value: 0.7467

``` r
summary(dmPFCchangethickBehavXAge <- lm(superiorfrontal_rh_diffAnn ~ FarAnn*AgeCent_T1, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = superiorfrontal_rh_diffAnn ~ FarAnn * AgeCent_T1, 
    ##     data = appWideROIRates)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.075699 -0.021986 -0.005626  0.024041  0.122620 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)        0.0199842  0.0064970   3.076  0.00356 **
    ## FarAnn             0.0027819  0.0134566   0.207  0.83715   
    ## AgeCent_T1        -0.0005595  0.0013191  -0.424  0.67345   
    ## FarAnn:AgeCent_T1  0.0017148  0.0027754   0.618  0.53979   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.04248 on 45 degrees of freedom
    ## Multiple R-squared:  0.02006,    Adjusted R-squared:  -0.04527 
    ## F-statistic: 0.307 on 3 and 45 DF,  p-value: 0.8202

``` r
summary(dlPFCchangethickBehavXAge <- lm(rostralmiddlefrontal_lh_diffAnn ~ FarAnn*AgeCent_T1, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = rostralmiddlefrontal_lh_diffAnn ~ FarAnn * AgeCent_T1, 
    ##     data = appWideROIRates)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.210754 -0.029489  0.002397  0.045705  0.172078 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)        0.024869   0.012013   2.070   0.0442 *
    ## FarAnn             0.017078   0.024882   0.686   0.4960  
    ## AgeCent_T1        -0.001554   0.002439  -0.637   0.5273  
    ## FarAnn:AgeCent_T1  0.004368   0.005132   0.851   0.3992  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07855 on 45 degrees of freedom
    ## Multiple R-squared:  0.04938,    Adjusted R-squared:  -0.01399 
    ## F-statistic: 0.7792 on 3 and 45 DF,  p-value: 0.5118

``` r
summary(vlPFCchangethickBehavXAge <- lm(parsopercularis_rh_diffAnn ~ FarAnn*AgeCent_T1, data=appWideROIRates))
```

    ## 
    ## Call:
    ## lm(formula = parsopercularis_rh_diffAnn ~ FarAnn * AgeCent_T1, 
    ##     data = appWideROIRates)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.196682 -0.042435  0.001424  0.043701  0.118293 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        0.0414265  0.0099982   4.143 0.000149 ***
    ## FarAnn             0.0133365  0.0207083   0.644 0.522835    
    ## AgeCent_T1        -0.0023855  0.0020299  -1.175 0.246110    
    ## FarAnn:AgeCent_T1  0.0008707  0.0042710   0.204 0.839381    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06537 on 45 degrees of freedom
    ## Multiple R-squared:  0.05178,    Adjusted R-squared:  -0.01144 
    ## F-statistic: 0.8191 on 3 and 45 DF,  p-value: 0.4902
