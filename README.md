
<!-- README.md is generated from README.Rmd. Please edit that file -->
<h1 align="center">

FTM - Flexible Transfer Models

</h1>
<p align="center">

<i>Seamlessly transfer omics models between datasets with varying
predictors.</i>

</p>

<div align="center">

<a href="https://www.r-project.org/"><img src="https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&amp;logo=r&amp;logoColor=white" alt="R"/></a>
<a href="https://www.baker.edu.au/"><img src="https://img.shields.io/badge/Baker-%23276DC3?style=for-the-badge" alt="Baker Institute"/></a>
<a href="https://doi.org/10.1016/j.jlr.2025.100800"><img src="https://img.shields.io/badge/DOI-10.1016/j.jlr.2025.100800-%23276DC3?style=for-the-badge&amp;logo=doi&amp;logoColor=white" alt="https://doi.org/10.1016/j.jlr.2025.100800"/></a>
<br>

<p align="center">

<i>Love our work? Visit our
<a href="https://metabolomics.baker.edu.au/">WebPortal</a>.</i>

</p>

</div>

# Overview of the FTM package

Often, with ’omics data, we have a model that we have trained on one
dataset and we want to apply it to another dataset. However, the
predictors in the new dataset may not be entirely the same as the
predictors in the original dataset. This causes an issue, which can
traditionally be solved through 2 options:

- Ignore the missing predictors (effectively setting them to 0)
- Recreate the model using a subset of variables that are present in
  both datasets

FTM overcomes this challenge by allowing the model to be evaluated in
datasets without exact overlap in the predictor variables, without loss
in generalisability, re-optimization, or access to individual-level
data. This is achieved by storing intermediate matrices that allow beta
coefficients to be recalculated using a subset of original predictors.

## Installation

You can install the development version of FTM from
[GitHub](https://github.com/) with:

``` r
if (!require("devtools")) install.packages("devtools")
if (!require("glmnet")) install.packages("glmnet")
devtools::install_github("BakerMetabolomics/FlexibleTransferModels", build_vignettes = TRUE)
```

Load the package. You’ll need to have
[glmnet](https://glmnet.stanford.edu/articles/glmnet.html) installed to
use the FTM package.

``` r
library(FTM)
```

## Example

This is a basic example which shows you how to convert an existing model
to a FTM object and predict outcomes in new dataset:

``` r
# Set up the predictors (glmnet requires a matrix)
predictors <- as.matrix(mtcars[, c("hp", "wt", "cyl")])

# 1. Fitting a glmnet model to the mtcars dataset
glmnet_model <- glmnet::cv.glmnet(predictors, mtcars$am, family = "binomial", alpha = 0)

# 2. Create FTM object using createFTM
ftmglmnet_model <- createFTM(glmnet_model, x = predictors, y = mtcars$am)

# A quick look at the model
show(ftmglmnet_model)
#> Flexible Transfer Model - Generalized Linear Model
#> ------------------------------------------------------
#>   Number of predictors: 3
#> 
#> Formula:
#> am ~ `(Intercept)` + hp + wt + cyl
#> 
#> Coefficients:
#> (Intercept)           hp           wt          cyl  
#>      6.2245       0.0116      -2.0526      -0.3230

# Extract the beta coefficients from a FTM object
coef(ftmglmnet_model)
#> (Intercept)          hp          wt         cyl 
#>  6.22451499  0.01164029 -2.05263254 -0.32296485


# 3. Predict on "new" data
new_data <- mtcars[1:10, c("hp", "wt", "cyl")]

# Predict using the FTM from glmnet model
predictions <- predict(ftmglmnet_model, newdata = new_data, type = "response")
predictions
#>                          am
#> Mazda RX4         0.5471743
#> Mazda RX4 Wag     0.4172285
#> Datsun 710        0.7778351
#> Hornet 4 Drive    0.2626857
#> Hornet Sportabout 0.2004940
#> Valiant           0.1689408
#> Duster 360        0.3025400
#> Merc 240D         0.2903789
#> Merc 230          0.3947689
#> Merc 280          0.2070870

# 4. Predict on "new" data that has missing predictors
new_data_missing <- mtcars[1:10, c("hp", "wt")] # missing cyl

# Predict using the FTM from glmnet model
predictions_missing <- predict(ftmglmnet_model, newdata = new_data_missing, type = "response")
predictions_missing
#>                          am
#> Mazda RX4         0.6216695
#> Mazda RX4 Wag     0.4695754
#> Datsun 710        0.7498350
#> Hornet 4 Drive    0.2795889
#> Hornet Sportabout 0.2672955
#> Valiant           0.1710853
#> Duster 360        0.3094615
#> Merc 240D         0.2238860
#> Merc 230          0.2889472
#> Merc 280          0.1985376
```

## Documentation

For more detailed information and advanced usage, please refer to the
package vignettes available within R:

``` r
browseVignettes("FTM")
```

## Contributing

Contributions to the FTM package are welcome from the community. Bug
reports, documentation enhancements, and feature requests can be filed
on the [GitHub issues
page](https://github.com/BakerMetabolomics/FlexibleTransferModels/issues).

## Contact

For any inquiries, please contact:

- Corey Giles: <Corey.Giles@Baker.edu.au>
- Changyu Yi: <Joe.Yi@Baker.edu.au>

## Version History

For a detailed list of changes, improvements, and bug fixes, please
refer to the [NEWS.md](NEWS.md) file.

## Citation

If you use this package in your work, please cite our publication.  
Yi C, Huynh K, Schooneveldt Y, Olshansky G, Liang A, Wang T, et
al. Statin effects on the lipidome: Predicting statin usage and
implications for cardiovascular risk prediction. Journal of Lipid
Research. 2025;66(5):100800.
<https://doi.org/10.1016/j.jlr.2025.100800>.
