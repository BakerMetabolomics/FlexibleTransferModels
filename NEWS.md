# FlexibleTransferModels

# Version 0.1.3 (2025-05-14)
## Bug Fixes and Enhancements
- Switching from solve and (Moore-Penrose) psuedoinverse to MASS::ginv.
- Should provide (practically) identical results, but catches a few specific edge cases.
  - It was possible that the ridge penalty was very small and XtX would still be singular.

# Version 0.1.2 (2025-05-07)
## Bug Fixes and minor features
- Fixed a bug where the model's "s" parameter wasn't always respected when using predict with ftmlm objects.
- Added the ability to estimate model performance (R2) for ftmlm objects (see: rsq function).
  - Provides estimates of model performance given a subset of variables.
  - Allows one to determine the approximate drop in performance for the proposed feature list.
- Estimating model performance requires storing a few extra parameters:
  - n: sample size used in model fitting
  - yty: sum of squared outcome values
  - y_mean: mean of the outcome variable

# Version 0.1.1 (2025-05-02)
## Bug Fixes and minor features
- Fixed a small bug that limited glmnet models to just cv.glmnet objects.
- Added the ability to explicitly label the outcome name.

# Version 0.1.0 (2024-05-29)
## Initial Release
- Introduced the Flexible Transfer Models (FTM) package.
- Enabled adaptation of glm, lm, and glmnet models to the FTM framework.
- Implemented a matrix-based approach for re-estimating model parameters.