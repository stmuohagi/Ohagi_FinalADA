Ohagi\_Final3
================
Stephanie Ohagi
12/11/2020

Packages Used

``` r
library(tidyverse) # data import
```

    ## Warning: package 'tidyverse' was built under R version 4.0.2

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ dplyr   1.0.0
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## Warning: package 'ggplot2' was built under R version 4.0.2

    ## Warning: package 'tidyr' was built under R version 4.0.2

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(arsenal) # descriptives table
library(pastecs) # descriptives
```

    ## Warning: package 'pastecs' was built under R version 4.0.2

    ## 
    ## Attaching package: 'pastecs'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
library(formattable) # formatting tables
```

    ## Warning: package 'formattable' was built under R version 4.0.2

``` r
library(reshape) # melt function for histogram matrix
```

    ## Warning: package 'reshape' was built under R version 4.0.2

    ## 
    ## Attaching package: 'reshape'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     rename

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, smiths

``` r
pacman::p_load(tidyverse, ROCR, odds.n.ends, blorr, lmtest, car) # vif is in blorr
```

Import Breast Cancer Wisconsin Data Set and Data Cleaning

``` r
BCW <- read.csv("/Users/stephanieohagi/Downloads/data.csv", header=TRUE, sep=",", na.strings=FALSE) # reads BCW data file into R
BCW <- subset(BCW, select = -1) # remove the ID column
BCW <- subset(BCW, select = -c(12:32)) # remove SE, worst, and best columns
BCW <- BCW %>%
  mutate(diagnosis_bin = case_when(diagnosis %in% c("M") ~ 1,
                                     diagnosis %in% c("B") ~ 0)) # recode diagnosis to binary

class(BCW$diagnosis_bin) # diagnosis_bin is numeric
```

    ## [1] "numeric"

``` r
table(BCW$diagnosis, BCW$diagnosis_bin) # recoding was successful 
```

    ##    
    ##       0   1
    ##   B 357   0
    ##   M   0 212

``` r
missing(BCW) # no missing values; no need for case analysis
```

    ## [1] FALSE

Descriptives

``` r
descrip_gen <- stat.desc(BCW[, c(2:11)]) # general descriptives
formattable(descrip_gen)
```

<table class="table table-condensed">

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

radius\_mean

</th>

<th style="text-align:right;">

texture\_mean

</th>

<th style="text-align:right;">

perimeter\_mean

</th>

<th style="text-align:right;">

area\_mean

</th>

<th style="text-align:right;">

smoothness\_mean

</th>

<th style="text-align:right;">

compactness\_mean

</th>

<th style="text-align:right;">

concavity\_mean

</th>

<th style="text-align:right;">

concave.points\_mean

</th>

<th style="text-align:right;">

symmetry\_mean

</th>

<th style="text-align:right;">

fractal\_dimension\_mean

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

nbr.val

</td>

<td style="text-align:right;">

569.0000000

</td>

<td style="text-align:right;">

5.690000e+02

</td>

<td style="text-align:right;">

5.690000e+02

</td>

<td style="text-align:right;">

5.690000e+02

</td>

<td style="text-align:right;">

5.690000e+02

</td>

<td style="text-align:right;">

5.690000e+02

</td>

<td style="text-align:right;">

5.690000e+02

</td>

<td style="text-align:right;">

5.690000e+02

</td>

<td style="text-align:right;">

5.690000e+02

</td>

<td style="text-align:right;">

5.690000e+02

</td>

</tr>

<tr>

<td style="text-align:left;">

nbr.null

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.000000e+00

</td>

<td style="text-align:right;">

0.000000e+00

</td>

<td style="text-align:right;">

0.000000e+00

</td>

<td style="text-align:right;">

0.000000e+00

</td>

<td style="text-align:right;">

0.000000e+00

</td>

<td style="text-align:right;">

1.300000e+01

</td>

<td style="text-align:right;">

1.300000e+01

</td>

<td style="text-align:right;">

0.000000e+00

</td>

<td style="text-align:right;">

0.000000e+00

</td>

</tr>

<tr>

<td style="text-align:left;">

nbr.na

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.000000e+00

</td>

<td style="text-align:right;">

0.000000e+00

</td>

<td style="text-align:right;">

0.000000e+00

</td>

<td style="text-align:right;">

0.000000e+00

</td>

<td style="text-align:right;">

0.000000e+00

</td>

<td style="text-align:right;">

0.000000e+00

</td>

<td style="text-align:right;">

0.000000e+00

</td>

<td style="text-align:right;">

0.000000e+00

</td>

<td style="text-align:right;">

0.000000e+00

</td>

</tr>

<tr>

<td style="text-align:left;">

min

</td>

<td style="text-align:right;">

6.9810000

</td>

<td style="text-align:right;">

9.710000e+00

</td>

<td style="text-align:right;">

4.379000e+01

</td>

<td style="text-align:right;">

1.435000e+02

</td>

<td style="text-align:right;">

5.263000e-02

</td>

<td style="text-align:right;">

1.938000e-02

</td>

<td style="text-align:right;">

0.000000e+00

</td>

<td style="text-align:right;">

0.000000e+00

</td>

<td style="text-align:right;">

1.060000e-01

</td>

<td style="text-align:right;">

4.996000e-02

</td>

</tr>

<tr>

<td style="text-align:left;">

max

</td>

<td style="text-align:right;">

28.1100000

</td>

<td style="text-align:right;">

3.928000e+01

</td>

<td style="text-align:right;">

1.885000e+02

</td>

<td style="text-align:right;">

2.501000e+03

</td>

<td style="text-align:right;">

1.634000e-01

</td>

<td style="text-align:right;">

3.454000e-01

</td>

<td style="text-align:right;">

4.268000e-01

</td>

<td style="text-align:right;">

2.012000e-01

</td>

<td style="text-align:right;">

3.040000e-01

</td>

<td style="text-align:right;">

9.744000e-02

</td>

</tr>

<tr>

<td style="text-align:left;">

range

</td>

<td style="text-align:right;">

21.1290000

</td>

<td style="text-align:right;">

2.957000e+01

</td>

<td style="text-align:right;">

1.447100e+02

</td>

<td style="text-align:right;">

2.357500e+03

</td>

<td style="text-align:right;">

1.107700e-01

</td>

<td style="text-align:right;">

3.260200e-01

</td>

<td style="text-align:right;">

4.268000e-01

</td>

<td style="text-align:right;">

2.012000e-01

</td>

<td style="text-align:right;">

1.980000e-01

</td>

<td style="text-align:right;">

4.748000e-02

</td>

</tr>

<tr>

<td style="text-align:left;">

sum

</td>

<td style="text-align:right;">

8038.4290000

</td>

<td style="text-align:right;">

1.097581e+04

</td>

<td style="text-align:right;">

5.233038e+04

</td>

<td style="text-align:right;">

3.726319e+05

</td>

<td style="text-align:right;">

5.482900e+01

</td>

<td style="text-align:right;">

5.937002e+01

</td>

<td style="text-align:right;">

5.052681e+01

</td>

<td style="text-align:right;">

2.783499e+01

</td>

<td style="text-align:right;">

1.030811e+02

</td>

<td style="text-align:right;">

3.573184e+01

</td>

</tr>

<tr>

<td style="text-align:left;">

median

</td>

<td style="text-align:right;">

13.3700000

</td>

<td style="text-align:right;">

1.884000e+01

</td>

<td style="text-align:right;">

8.624000e+01

</td>

<td style="text-align:right;">

5.511000e+02

</td>

<td style="text-align:right;">

9.587000e-02

</td>

<td style="text-align:right;">

9.263000e-02

</td>

<td style="text-align:right;">

6.154000e-02

</td>

<td style="text-align:right;">

3.350000e-02

</td>

<td style="text-align:right;">

1.792000e-01

</td>

<td style="text-align:right;">

6.154000e-02

</td>

</tr>

<tr>

<td style="text-align:left;">

mean

</td>

<td style="text-align:right;">

14.1272917

</td>

<td style="text-align:right;">

1.928965e+01

</td>

<td style="text-align:right;">

9.196903e+01

</td>

<td style="text-align:right;">

6.548891e+02

</td>

<td style="text-align:right;">

9.636028e-02

</td>

<td style="text-align:right;">

1.043410e-01

</td>

<td style="text-align:right;">

8.879932e-02

</td>

<td style="text-align:right;">

4.891915e-02

</td>

<td style="text-align:right;">

1.811619e-01

</td>

<td style="text-align:right;">

6.279761e-02

</td>

</tr>

<tr>

<td style="text-align:left;">

SE.mean

</td>

<td style="text-align:right;">

0.1477358

</td>

<td style="text-align:right;">

1.803088e-01

</td>

<td style="text-align:right;">

1.018666e+00

</td>

<td style="text-align:right;">

1.475301e+01

</td>

<td style="text-align:right;">

5.895989e-04

</td>

<td style="text-align:right;">

2.214026e-03

</td>

<td style="text-align:right;">

3.342028e-03

</td>

<td style="text-align:right;">

1.626700e-03

</td>

<td style="text-align:right;">

1.149266e-03

</td>

<td style="text-align:right;">

2.959858e-04

</td>

</tr>

<tr>

<td style="text-align:left;">

CI.mean.0.95

</td>

<td style="text-align:right;">

0.2901752

</td>

<td style="text-align:right;">

3.541534e-01

</td>

<td style="text-align:right;">

2.000813e+00

</td>

<td style="text-align:right;">

2.897711e+01

</td>

<td style="text-align:right;">

1.158060e-03

</td>

<td style="text-align:right;">

4.348678e-03

</td>

<td style="text-align:right;">

6.564242e-03

</td>

<td style="text-align:right;">

3.195081e-03

</td>

<td style="text-align:right;">

2.257331e-03

</td>

<td style="text-align:right;">

5.813603e-04

</td>

</tr>

<tr>

<td style="text-align:left;">

var

</td>

<td style="text-align:right;">

12.4189201

</td>

<td style="text-align:right;">

1.849891e+01

</td>

<td style="text-align:right;">

5.904405e+02

</td>

<td style="text-align:right;">

1.238436e+05

</td>

<td style="text-align:right;">

1.977997e-04

</td>

<td style="text-align:right;">

2.789187e-03

</td>

<td style="text-align:right;">

6.355248e-03

</td>

<td style="text-align:right;">

1.505661e-03

</td>

<td style="text-align:right;">

7.515428e-04

</td>

<td style="text-align:right;">

4.984872e-05

</td>

</tr>

<tr>

<td style="text-align:left;">

std.dev

</td>

<td style="text-align:right;">

3.5240488

</td>

<td style="text-align:right;">

4.301036e+00

</td>

<td style="text-align:right;">

2.429898e+01

</td>

<td style="text-align:right;">

3.519141e+02

</td>

<td style="text-align:right;">

1.406413e-02

</td>

<td style="text-align:right;">

5.281276e-02

</td>

<td style="text-align:right;">

7.971981e-02

</td>

<td style="text-align:right;">

3.880284e-02

</td>

<td style="text-align:right;">

2.741428e-02

</td>

<td style="text-align:right;">

7.060363e-03

</td>

</tr>

<tr>

<td style="text-align:left;">

coef.var

</td>

<td style="text-align:right;">

0.2494497

</td>

<td style="text-align:right;">

2.229712e-01

</td>

<td style="text-align:right;">

2.642083e-01

</td>

<td style="text-align:right;">

5.373645e-01

</td>

<td style="text-align:right;">

1.459536e-01

</td>

<td style="text-align:right;">

5.061555e-01

</td>

<td style="text-align:right;">

8.977525e-01

</td>

<td style="text-align:right;">

7.932036e-01

</td>

<td style="text-align:right;">

1.513248e-01

</td>

<td style="text-align:right;">

1.124304e-01

</td>

</tr>

</tbody>

</table>

``` r
# Differences in IVs between diagnosis groups
table_descrip <- tableby(diagnosis_bin ~., data = BCW) # creates table based on diagnosis group
descrip_sum_diag <- summary(table_descrip, title = "Descriptive Table") # summary of all other IVs

# pie chart for diagnosis
diagnosis_freq <- table(BCW$diagnosis) # freq table
color_diagnosis <- terrain.colors(2)
diagnosis_prop <- prop.table(diagnosis_freq)*100
diagnosis_prop_df <- as.data.frame(diagnosis_prop)
pielabels_diagnosis <- sprintf("%s - %3.1f%s", diagnosis_prop_df[,1], diagnosis_prop, "%")

pie(diagnosis_prop,
  labels=pielabels_diagnosis,  
  clockwise=TRUE,
  col=color_diagnosis,
  border="gray",
  radius=0.8,
  cex=0.8, 
  main="Proportions of Cancer Diagnosis")
legend(1, .4, legend=diagnosis_prop_df[,1], cex = 0.7, fill = color_diagnosis)
```

![](Ohagi_Final3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# histograms of all IVs between diagnosis groups
dataframe_hist <- BCW[ ,c("diagnosis", "radius_mean", "texture_mean","perimeter_mean", "area_mean", "smoothness_mean", "compactness_mean", "concavity_mean", "concave.points_mean", "symmetry_mean", "fractal_dimension_mean")]

ggplot(data = melt(dataframe_hist, id.var = "diagnosis"), mapping = aes(x = value)) + 
    geom_histogram(bins = 15, aes(fill = diagnosis), alpha=0.5) + facet_wrap(~ variable, scales = 'free_x')
```

![](Ohagi_Final3_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

Assumptions testing: Logistic regression

``` r
# LINEARITY

## radius_mean: passes
BCW <- BCW %>%
  mutate(rad.times.lograd = radius_mean * log(radius_mean)) #term for linearity testing

boxTidwell_radius <- glm(diagnosis_bin ~ radius_mean + rad.times.lograd, data = BCW, family="binomial")
summary(boxTidwell_radius) # p = 0.348
```

    ## 
    ## Call:
    ## glm(formula = diagnosis_bin ~ radius_mean + rad.times.lograd, 
    ##     family = "binomial", data = BCW)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.6843  -0.4685  -0.2086   0.1071   2.7045  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)       -1.5023    14.4884  -0.104    0.917
    ## radius_mean       -2.5398     3.7910  -0.670    0.503
    ## rad.times.lograd   0.9799     1.0433   0.939    0.348
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 751.44  on 568  degrees of freedom
    ## Residual deviance: 329.14  on 566  degrees of freedom
    ## AIC: 335.14
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
## texture_mean: FAILS
BCW <- BCW %>%
  mutate(text.times.logtext = texture_mean * log(texture_mean)) 

boxTidwell_texture <- glm(diagnosis_bin ~ texture_mean + text.times.logtext, data = BCW, family="binomial")
summary(boxTidwell_texture) # p< 0.05
```

    ## 
    ## Call:
    ## glm(formula = diagnosis_bin ~ texture_mean + text.times.logtext, 
    ##     family = "binomial", data = BCW)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5399  -0.8980  -0.4305   0.9975   3.3157  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -24.5753     4.1987  -5.853 4.82e-09 ***
    ## texture_mean         4.0358     0.7959   5.071 3.96e-07 ***
    ## text.times.logtext  -0.9391     0.1951  -4.814 1.48e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 751.44  on 568  degrees of freedom
    ## Residual deviance: 620.56  on 566  degrees of freedom
    ## AIC: 626.56
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
## perimeter_mean: passes
BCW <- BCW %>%
  mutate(peri.times.logperi = perimeter_mean * log(perimeter_mean)) 

boxTidwell_peri <- glm(diagnosis_bin ~ perimeter_mean + peri.times.logperi, data = BCW, family="binomial")
summary(boxTidwell_peri) # p = 0.652
```

    ## 
    ## Call:
    ## glm(formula = diagnosis_bin ~ perimeter_mean + peri.times.logperi, 
    ##     family = "binomial", data = BCW)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.58278  -0.42502  -0.17571   0.09515   2.73937  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)         -6.0061    15.4454  -0.389    0.697
    ## perimeter_mean      -0.4224     0.9367  -0.451    0.652
    ## peri.times.logperi   0.1062     0.1699   0.625    0.532
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 751.44  on 568  degrees of freedom
    ## Residual deviance: 304.10  on 566  degrees of freedom
    ## AIC: 310.1
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
## area_mean: passes
BCW <- BCW %>%
  mutate(area.times.logarea = area_mean * log(area_mean))

boxTidwell_area <- glm(diagnosis_bin ~ area_mean + area.times.logarea, data = BCW, family="binomial")
summary(boxTidwell_area) # p = 0.8986
```

    ## 
    ## Call:
    ## glm(formula = diagnosis_bin ~ area_mean + area.times.logarea, 
    ##     family = "binomial", data = BCW)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.75201  -0.46426  -0.20872   0.09285   2.72201  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)        -7.4986131  3.8565263  -1.944   0.0518 .
    ## area_mean           0.0059436  0.0466527   0.127   0.8986  
    ## area.times.logarea  0.0007839  0.0062801   0.125   0.9007  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 751.44  on 568  degrees of freedom
    ## Residual deviance: 325.64  on 566  degrees of freedom
    ## AIC: 331.64
    ## 
    ## Number of Fisher Scoring iterations: 8

``` r
## smoothness_mean: FAILS
BCW <- BCW %>%
  mutate(smooth.times.logsmooth = smoothness_mean * log(smoothness_mean)) 

boxTidwell_smooth <- glm(diagnosis_bin ~ radius_mean + smooth.times.logsmooth, data = BCW, family="binomial")
summary(boxTidwell_smooth)
```

    ## 
    ## Call:
    ## glm(formula = diagnosis_bin ~ radius_mean + smooth.times.logsmooth, 
    ##     family = "binomial", data = BCW)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.20439  -0.29094  -0.09322   0.09118   2.57291  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)            -36.1976     3.6292  -9.974  < 2e-16 ***
    ## radius_mean              1.2103     0.1166  10.382  < 2e-16 ***
    ## smooth.times.logsmooth -81.4516    10.9143  -7.463 8.47e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 751.44  on 568  degrees of freedom
    ## Residual deviance: 249.81  on 566  degrees of freedom
    ## AIC: 255.81
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
## compactness_mean: FAILS
BCW <- BCW %>%
  mutate(compact.times.logcompact = compactness_mean * log(compactness_mean))

boxTidwell_compact <- glm(diagnosis_bin ~ compactness_mean + compact.times.logcompact, data = BCW, family="binomial")
summary(boxTidwell_compact) # p = 0.0062
```

    ## 
    ## Call:
    ## glm(formula = diagnosis_bin ~ compactness_mean + compact.times.logcompact, 
    ##     family = "binomial", data = BCW)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3167  -0.6577  -0.3164   0.6786   2.6098  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                -7.848      1.394  -5.629 1.82e-08 ***
    ## compactness_mean           -1.051     13.187  -0.080   0.9365    
    ## compact.times.logcompact  -31.921     11.664  -2.737   0.0062 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 751.44  on 568  degrees of freedom
    ## Residual deviance: 502.38  on 566  degrees of freedom
    ## AIC: 508.38
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
## concavity_mean: FAILS
BCW <- BCW %>%
  mutate(concav.times.logconcav = concavity_mean * log(concavity_mean)) 

boxTidwell_concav <- glm(diagnosis_bin ~ concavity_mean + concav.times.logconcav, data = BCW, family="binomial")
summary(boxTidwell_concav) # p < 0.05
```

    ## 
    ## Call:
    ## glm(formula = diagnosis_bin ~ concavity_mean + concav.times.logconcav, 
    ##     family = "binomial", data = BCW)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.4539  -0.3742  -0.1225   0.4344   2.9582  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)              -7.732      0.765 -10.107  < 2e-16 ***
    ## concavity_mean          -14.549      5.242  -2.775  0.00551 ** 
    ## concav.times.logconcav  -41.561      5.468  -7.600 2.95e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 739.14  on 555  degrees of freedom
    ## Residual deviance: 342.49  on 553  degrees of freedom
    ##   (13 observations deleted due to missingness)
    ## AIC: 348.49
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
## concave.points_mean: passes
BCW <- BCW %>%
  mutate(cp.times.logcp = concave.points_mean * log(concave.points_mean))

boxTidwell_cp <- glm(diagnosis_bin ~ concave.points_mean + cp.times.logcp, data = BCW, family="binomial")
summary(boxTidwell_cp) # p = 0.647154
```

    ## 
    ## Call:
    ## glm(formula = diagnosis_bin ~ concave.points_mean + cp.times.logcp, 
    ##     family = "binomial", data = BCW)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.5246  -0.3245  -0.1415   0.1869   2.7574  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           -6.572      1.711  -3.841 0.000123 ***
    ## concave.points_mean   73.304     73.227   1.001 0.316799    
    ## cp.times.logcp       -16.477     35.997  -0.458 0.647154    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 739.14  on 555  degrees of freedom
    ## Residual deviance: 258.64  on 553  degrees of freedom
    ##   (13 observations deleted due to missingness)
    ## AIC: 264.64
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
## symmetry_mean: FAILS
BCW <- BCW %>%
  mutate(sym.times.logsym = symmetry_mean * log(symmetry_mean)) 

boxTidwell_sym <- glm(diagnosis_bin ~ symmetry_mean + sym.times.logsym, data = BCW, family="binomial")
summary(boxTidwell_sym) # p = 0.04640
```

    ## 
    ## Call:
    ## glm(formula = diagnosis_bin ~ symmetry_mean + sym.times.logsym, 
    ##     family = "binomial", data = BCW)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6311  -0.9526  -0.6442   1.1275   2.3006  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)       -18.569      6.651  -2.792  0.00524 **
    ## symmetry_mean     -17.426     22.431  -0.777  0.43724   
    ## sym.times.logsym  -68.688     34.486  -1.992  0.04640 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 751.44  on 568  degrees of freedom
    ## Residual deviance: 683.00  on 566  degrees of freedom
    ## AIC: 689
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
## fractal_dimension_mean: passes
BCW <- BCW %>%
  mutate(fd.times.logfd = fractal_dimension_mean * log(fractal_dimension_mean)) 

boxTidwell_fd <- glm(diagnosis_bin ~ fractal_dimension_mean + fd.times.logfd, data = BCW, family="binomial")
summary(boxTidwell_fd) # p = 0.0531
```

    ## 
    ## Call:
    ## glm(formula = diagnosis_bin ~ fractal_dimension_mean + fd.times.logfd, 
    ##     family = "binomial", data = BCW)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.4618  -0.9604  -0.9150   1.3772   1.4733  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)              16.970      8.943   1.898   0.0577 .
    ## fractal_dimension_mean  439.528    229.749   1.913   0.0557 .
    ## fd.times.logfd          260.013    134.452   1.934   0.0531 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 751.44  on 568  degrees of freedom
    ## Residual deviance: 747.51  on 566  degrees of freedom
    ## AIC: 753.51
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# Linearity tests meant removal of texture, smoothness, compactness, symmetry, and concavity for the sake of the logistic regression model. No available means of categorization
```

Multivariate Logistic model

``` r
#IVs: perimeter, area, radius, concave points, and fractal dimensions
logit_diagnosis <- glm(diagnosis_bin ~ perimeter_mean + area_mean + radius_mean + concave.points_mean + fractal_dimension_mean, data = BCW, family="binomial")
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
summary(logit_diagnosis) # only area and concave points are significant predictors for tumor diagnosis
```

    ## 
    ## Call:
    ## glm(formula = diagnosis_bin ~ perimeter_mean + area_mean + radius_mean + 
    ##     concave.points_mean + fractal_dimension_mean, family = "binomial", 
    ##     data = BCW)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3952  -0.2462  -0.1322   0.0127   2.6308  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             13.23465    8.98333   1.473  0.14068    
    ## perimeter_mean           0.17570    0.26499   0.663  0.50730    
    ## area_mean                0.03760    0.01244   3.023  0.00251 ** 
    ## radius_mean             -3.86222    2.22175  -1.738  0.08215 .  
    ## concave.points_mean     91.43337   15.91221   5.746 9.13e-09 ***
    ## fractal_dimension_mean -52.32658   46.96536  -1.114  0.26521    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 751.44  on 568  degrees of freedom
    ## Residual deviance: 199.14  on 563  degrees of freedom
    ## AIC: 211.14
    ## 
    ## Number of Fisher Scoring iterations: 8

``` r
#calculate and print ORs and 95% CIs  
ORdiagnosis<-exp(cbind(OR = coef(logit_diagnosis), confint(logit_diagnosis)))
```

    ## Waiting for profiling to be done...

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
    
    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
ORdiagnosis #print
```

    ##                                  OR        2.5 %       97.5 %
    ## (Intercept)            5.594163e+05 1.040938e-02 2.201857e+13
    ## perimeter_mean         1.192083e+00 7.144105e-01 2.024701e+00
    ## area_mean              1.038317e+00 1.013761e+00 1.064375e+00
    ## radius_mean            2.102125e-02 2.412791e-04 1.511699e+00
    ## concave.points_mean    5.116912e+39 5.640538e+26 1.073303e+54
    ## fractal_dimension_mean 1.883018e-23 2.741552e-64 6.328303e+16

Multicolinearity and Outliers (Full Model)

``` r
# multicolinearity
vif(logit_diagnosis) # radius, area, and perimeter have incredibly large VIFs; probably should not be included in the model
```

    ##         perimeter_mean              area_mean            radius_mean 
    ##             257.129015              93.600021             430.159371 
    ##    concave.points_mean fractal_dimension_mean 
    ##               2.512446               4.299540

``` r
# new model with multicolinear variables removed
logit_diagnosis2 <- glm(diagnosis_bin ~ concave.points_mean + fractal_dimension_mean + area_mean, data = BCW, family="binomial")
summary(logit_diagnosis2) #  concave points and area are sign. predictors
```

    ## 
    ## Call:
    ## glm(formula = diagnosis_bin ~ concave.points_mean + fractal_dimension_mean + 
    ##     area_mean, family = "binomial", data = BCW)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.45704  -0.24291  -0.09586   0.04621   2.61431  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             -8.357145   2.982228  -2.802  0.00507 ** 
    ## concave.points_mean     87.449411  13.889367   6.296 3.05e-10 ***
    ## fractal_dimension_mean -14.294649  40.051138  -0.357  0.72116    
    ## area_mean                0.007097   0.001793   3.959 7.54e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 751.44  on 568  degrees of freedom
    ## Residual deviance: 205.65  on 565  degrees of freedom
    ## AIC: 213.65
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
#NEW calculation of ORs and 95% CIs  
ORdiagnosis2 <-exp(cbind(OR = coef(logit_diagnosis2), confint(logit_diagnosis2)))
```

    ## Waiting for profiling to be done...

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
ORdiagnosis2 #print
```

    ##                                  OR        2.5 %       97.5 %
    ## (Intercept)            2.347134e-04 6.103738e-07 7.804891e-02
    ## concave.points_mean    9.523497e+37 4.683944e+26 2.871084e+50
    ## fractal_dimension_mean 6.193165e-07 6.793542e-42 2.367109e+27
    ## area_mean              1.007122e+00 1.003704e+00 1.010820e+00

``` r
# multicolinearity again
vif(logit_diagnosis2) # YAY!
```

    ##    concave.points_mean fractal_dimension_mean              area_mean 
    ##               1.938746               2.908722               2.021710

Model Fit

``` r
lrtest(logit_diagnosis, logit_diagnosis2) # model without perimeter and radius is a better fit
```

    ## Likelihood ratio test
    ## 
    ## Model 1: diagnosis_bin ~ perimeter_mean + area_mean + radius_mean + concave.points_mean + 
    ##     fractal_dimension_mean
    ## Model 2: diagnosis_bin ~ concave.points_mean + fractal_dimension_mean + 
    ##     area_mean
    ##   #Df   LogLik Df  Chisq Pr(>Chisq)  
    ## 1   6  -99.572                       
    ## 2   4 -102.824 -2 6.5048    0.03868 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
logit_diagnosis3 <- glm(diagnosis_bin ~ concave.points_mean + fractal_dimension_mean, data = BCW, family="binomial") 
# logit model without area as a predictor

lrtest(logit_diagnosis2, logit_diagnosis3) # model WITHOUT area is a better fit
```

    ## Likelihood ratio test
    ## 
    ## Model 1: diagnosis_bin ~ concave.points_mean + fractal_dimension_mean + 
    ##     area_mean
    ## Model 2: diagnosis_bin ~ concave.points_mean + fractal_dimension_mean
    ##   #Df  LogLik Df  Chisq Pr(>Chisq)    
    ## 1   4 -102.82                         
    ## 2   3 -111.71 -1 17.771  2.491e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Various pseudo R squares, log likelihood, deviance, AIC, BIC
blr_model_fit_stats(logit_diagnosis3)
```

    ##                               Model Fit Statistics                                
    ## ---------------------------------------------------------------------------------
    ## Log-Lik Intercept Only:      -375.720    Log-Lik Full Model:             -111.710 
    ## Deviance(566):                223.420    LR(2):                           528.020 
    ##                                          Prob > LR:                         0.000 
    ## MCFadden's R2                   0.703    McFadden's Adj R2:                 0.695 
    ## ML (Cox-Snell) R2:              0.605    Cragg-Uhler(Nagelkerke) R2:        0.825 
    ## McKelvey & Zavoina's R2:        0.876    Efron's R2:                        0.745 
    ## Count R2:                       0.923    Adj Count R2:                      0.792 
    ## BIC:                          242.451    AIC:                             229.420 
    ## ---------------------------------------------------------------------------------

``` r
#Hosmer lemeshow goodness of fit test: a significant p value indicates a bad fit
blr_test_hosmer_lemeshow(logit_diagnosis3) # p = 0.765; good model fit, chi-square = 4.9290
```

    ##            Partition for the Hosmer & Lemeshow Test            
    ## --------------------------------------------------------------
    ##                         def = 1                 def = 0        
    ## Group    Total    Observed    Expected    Observed    Expected 
    ## --------------------------------------------------------------
    ##   1       57         0          0.15         57        56.85   
    ##   2       57         0          0.43         57        56.57   
    ##   3       57         0          0.90         57        56.10   
    ##   4       57         1          1.83         56        55.17   
    ##   5       57         6          3.72         51        53.28   
    ##   6       56         11         9.01         45        46.99   
    ##   7       57         29        31.37         28        25.63   
    ##   8       57         51        51.21         6          5.79   
    ##   9       57         57        56.42         0          0.58   
    ##  10       57         57        56.98         0          0.02   
    ## --------------------------------------------------------------
    ## 
    ##      Goodness of Fit Test      
    ## ------------------------------
    ## Chi-Square    DF    Pr > ChiSq 
    ## ------------------------------
    ##   4.9290      8       0.7651   
    ## ------------------------------

Evaluate Model Performance

``` r
table(BCW$diagnosis) # M: 212 cases
```

    ## 
    ##   B   M 
    ## 357 212

``` r
#new dataset with only benign diagnoses = 0
BCW_benign <- BCW %>%
  filter(diagnosis == 'B')

#create a new dataset where the number of malignant and benign cases are the same
BCW_malignant<- BCW %>%
  filter(diagnosis=='M') #keep only malignant cases

set.seed(1) 
BCW_malignant <- sample_n(BCW_malignant, size=212) # sample from smaller number of the two groups

BCW_balance <-BCW_malignant %>%
  rbind(BCW_benign) #Combine malignant with benign

multi_logit_diagnosis <- glm(diagnosis_bin ~ concave.points_mean + fractal_dimension_mean, data = BCW_balance, family = "binomial")
summary(multi_logit_diagnosis)
```

    ## 
    ## Call:
    ## glm(formula = diagnosis_bin ~ concave.points_mean + fractal_dimension_mean, 
    ##     family = "binomial", data = BCW_balance)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.74068  -0.27546  -0.11359   0.08979   2.55107  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)               2.223      1.535   1.448    0.148    
    ## concave.points_mean     126.200     11.399  11.071  < 2e-16 ***
    ## fractal_dimension_mean -143.202     27.736  -5.163 2.43e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 751.44  on 568  degrees of freedom
    ## Residual deviance: 223.42  on 566  degrees of freedom
    ## AIC: 229.42
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
#b.
pred_diag <- predict(multi_logit_diagnosis, newdata = BCW_balance, type = 'response')
hist(pred_diag) # histogram of predicted probabilities for diagnosis status
```

![](Ohagi_Final3_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# creating a dataframe of predicted VALUES for diagnosis (number of patients)
df1 <-as.data.frame(pred_diag)
df2 <-cbind(df1, BCW_balance$diagnosis)

# plot distribution for predicted values
ggplot(data=df2) +
  geom_density(aes(x=pred_diag, color=BCW_balance$diagnosis, linetype=BCW_balance$diagnosis))
```

![](Ohagi_Final3_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
odds.n.ends(multi_logit_diagnosis)
```

    ## Waiting for profiling to be done...

    ## $`Logistic regression model significance`
    ## Chi-squared        d.f.           p 
    ##      528.02        2.00        0.00 
    ## 
    ## $`Contingency tables (model fit): percent predicted`
    ##                  Percent observed
    ## Percent predicted          1          0        Sum
    ##               1   0.32864675 0.03339192 0.36203866
    ##               0   0.04393673 0.59402460 0.63796134
    ##               Sum 0.37258348 0.62741652 1.00000000
    ## 
    ## $`Contingency tables (model fit): frequency predicted`
    ##                 Number observed
    ## Number predicted   1   0 Sum
    ##              1   187  19 206
    ##              0    25 338 363
    ##              Sum 212 357 569
    ## 
    ## $`Predictor odds ratios and 95% CI`
    ##                                  OR        2.5 %       97.5 %
    ## (Intercept)            9.234962e+00 4.925029e-01 2.038742e+02
    ## concave.points_mean    6.425958e+54 7.157120e+45 2.211902e+65
    ## fractal_dimension_mean 6.427071e-63 7.011132e-88 1.465623e-40
    ## 
    ## $`Model sensitivity`
    ## [1] 0.8820755
    ## 
    ## $`Model specificity`
    ## [1] 0.9467787

``` r
# sensitivity: 0.8820755
# Specificity:0.9467787


# d.
ROCR_diagnosis = prediction(as.numeric(pred_diag), as.numeric(BCW_balance$diagnosis_bin)) # predicted probability values for outcome variables


perf_diag = performance(ROCR_diagnosis,"tpr", "fpr") #tpr on y axis and fpr on x axis

# ROC curve
plot(perf_diag, colorsize=T, color="dark green", print.cutoffs.at=seq(0,1,0.1))
abline(a = 0, b = 1)
```

![](Ohagi_Final3_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
# AUC
auc_diag = performance(ROCR_diagnosis, measure = "auc")
auc_diag@y.values # AUC is 0.9751467
```

    ## [[1]]
    ## [1] 0.9751467
