<h1 align="center"> Kaggle Tabular Playground - October 2021 </h1>

This is a part of the Kaggle Tabular Playground Series (TPS). Each month, Kaggle releases a dataset we can work on to improve our skills in Data Analysis, Cleaning, Visualization and Machine Learning.

In the August dataset, we find 99 features which all seem to be continuous and no categorical features. Our predictions are to be done on the feature called "loss". We start by plotting a correlation matrix between all variables (using seaborn heatmap on the corr functions) and then finding the distributions of each variable (using the seaborn kdeplots).

There doesn't seem to be any outliers we need to remove yet but we can revisit this later if we need to. For now, my approach to the data is going to be as follows:
- *Regression Plots* - Now that we have looked at the distributions of each variable, let's plot the features against the target and see the regression of each.  Do we need to use splines or some form of non-parametric regression? Let's test with the following:
    - *splines*
    - *lowess*
    - *GAM*
- *Other Regression Models* - If the variables don't show any non-parametric features, we can look at other models that might help us predict the values better like:
    - *XGBoost*
    - *LightGBM*
    - *SVM*
