```R
# Reading the cleaned data
MPI <- read.csv("MPI.csv")
CPI <- read.csv("CPI.csv")
UEM <- read.csv("UEM.csv")
GDP <- read.csv("GDP.csv")
PPP <- read.csv("PPP.csv")

# Removing unwanted columns
MPI <- MPI[,2:18]
CPI <- CPI[,2:18]
UEM <- UEM[,2:18]
GDP <- GDP[,2:18]
PPP <- PPP[,2:18]

# Scaling the data
MPI <- as.data.frame(scale(MPI))
CPI <- as.data.frame(scale(CPI))
UEM <- as.data.frame(scale(UEM))
GDP <- as.data.frame(scale(GDP))
PPP <- as.data.frame(scale(PPP))
```


```R
# Creating the countries data frame from their respective columns
Bangladesh <- data.frame(UEM$Bangladesh, MPI$Bangladesh, CPI$Bangladesh, GDP$Bangladesh, PPP$Bangladesh)

# Creating and running our MLR with MPI as the response and all other predictors
Lmod_Bangladesh = lm(MPI.Bangladesh ~ ., data = Bangladesh)
summary(Lmod_Bangladesh)

# Examing the the variance of each predictor to identify 
anova_Bangladesh = aov(Lmod_Bangladesh)
summary(anova_Bangladesh)
# The same is applied to every country below
```


    
    Call:
    lm(formula = MPI.Bangladesh ~ ., data = Bangladesh)
    
    Residuals:
          1       2       3       4       5       6 
     0.0661  0.2709 -0.6390  0.3433 -0.1339  0.0925 
    
    Coefficients:
                     Estimate Std. Error t value Pr(>|t|)
    (Intercept)     2.793e-15  3.242e-01   0.000    1.000
    UEM.Bangladesh -2.215e-01  9.110e-01  -0.243    0.848
    CPI.Bangladesh -2.924e-01  1.465e+00  -0.200    0.875
    GDP.Bangladesh  4.825e+00  1.241e+01   0.389    0.764
    PPP.Bangladesh -5.739e+00  1.403e+01  -0.409    0.753
    
    Residual standard error: 0.794 on 1 degrees of freedom
    Multiple R-squared:  0.8739,	Adjusted R-squared:  0.3695 
    F-statistic: 1.733 on 4 and 1 DF,  p-value: 0.5103




                   Df Sum Sq Mean Sq F value Pr(>F)
    UEM.Bangladesh  1  3.867   3.867   6.133  0.244
    CPI.Bangladesh  1  0.309   0.309   0.490  0.611
    GDP.Bangladesh  1  0.089   0.089   0.140  0.772
    PPP.Bangladesh  1  0.105   0.105   0.167  0.753
    Residuals       1  0.630   0.630               



```R
D.R.Congo <- data.frame(UEM$D.R.Congo, MPI$D.R.Congo, CPI$D.R.Congo, GDP$D.R.Congo, PPP$D.R.Congo)
Lmod_D.R.Congo = lm(MPI.D.R.Congo ~ ., data = D.R.Congo)
summary(Lmod_D.R.Congo)

anova_D.R.Congo = aov(Lmod_D.R.Congo)
summary(anova_D.R.Congo)
```


    
    Call:
    lm(formula = MPI.D.R.Congo ~ ., data = D.R.Congo)
    
    Residuals:
            1         2         3         4         5         6 
    -0.006313  0.028300  0.159321 -0.218970  0.086275 -0.048613 
    
    Coefficients:
                    Estimate Std. Error t value Pr(>|t|)
    (Intercept)   -2.758e-16  1.183e-01   0.000    1.000
    UEM.D.R.Congo  8.484e-02  2.148e-01   0.395    0.761
    CPI.D.R.Congo -3.126e-01  1.828e-01  -1.710    0.337
    GDP.D.R.Congo -8.360e-01  5.363e-01  -1.559    0.363
    PPP.D.R.Congo -3.535e-01  4.364e-01  -0.810    0.567
    
    Residual standard error: 0.2898 on 1 degrees of freedom
    Multiple R-squared:  0.9832,	Adjusted R-squared:  0.916 
    F-statistic: 14.63 on 4 and 1 DF,  p-value: 0.1933




                  Df Sum Sq Mean Sq F value Pr(>F)  
    UEM.D.R.Congo  1  0.354   0.354   4.218 0.2885  
    CPI.D.R.Congo  1  0.209   0.209   2.491 0.3595  
    GDP.D.R.Congo  1  4.297   4.297  51.173 0.0884 .
    PPP.D.R.Congo  1  0.055   0.055   0.656 0.5665  
    Residuals      1  0.084   0.084                 
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



```R
Dominican.Republic <- data.frame(UEM$Dominican.Republic, MPI$Dominican.Republic, CPI$Dominican.Republic, GDP$Dominican.Republic, PPP$Dominican.Republic)
Lmod_Dominican.Republic = lm(MPI.Dominican.Republic ~ ., data = Dominican.Republic)
summary(Lmod_Dominican.Republic)

anova_Dominican.Republic = aov(Lmod_Dominican.Republic)
summary(anova_Dominican.Republic)
```


    
    Call:
    lm(formula = MPI.Dominican.Republic ~ ., data = Dominican.Republic)
    
    Residuals:
           1        2        3        4        5        6 
    -0.03864  0.06158 -0.07910  0.11084 -0.04013 -0.01455 
    
    Coefficients:
                             Estimate Std. Error t value Pr(>|t|)
    (Intercept)             9.455e-16  6.538e-02   0.000    1.000
    UEM.Dominican.Republic -5.686e-01  1.720e-01  -3.306    0.187
    CPI.Dominican.Republic -3.570e-01  1.511e-01  -2.363    0.255
    GDP.Dominican.Republic -2.005e-02  2.734e-01  -0.073    0.953
    PPP.Dominican.Republic -7.198e-01  4.418e-01  -1.629    0.350
    
    Residual standard error: 0.1602 on 1 degrees of freedom
    Multiple R-squared:  0.9949,	Adjusted R-squared:  0.9744 
    F-statistic: 48.48 on 4 and 1 DF,  p-value: 0.1073




                           Df Sum Sq Mean Sq F value Pr(>F)  
    UEM.Dominican.Republic  1  4.458   4.458 173.821 0.0482 *
    CPI.Dominican.Republic  1  0.030   0.030   1.176 0.4742  
    GDP.Dominican.Republic  1  0.418   0.418  16.287 0.1546  
    PPP.Dominican.Republic  1  0.068   0.068   2.654 0.3505  
    Residuals               1  0.026   0.026                 
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



```R
Jordan <- data.frame(UEM$Jordan, MPI$Jordan, CPI$Jordan, GDP$Jordan, PPP$Jordan)
Lmod_Jordan = lm(MPI.Jordan ~ ., data = Jordan)
summary(Lmod_Jordan)

anova_Jordan = aov(Lmod_Jordan)
summary(anova_Jordan)
```


    
    Call:
    lm(formula = MPI.Jordan ~ ., data = Jordan)
    
    Residuals:
          1       2       3       4       5       6 
    -0.2830  0.3037 -0.2724  0.3816  0.1517 -0.2815 
    
    Coefficients:
                  Estimate Std. Error t value Pr(>|t|)
    (Intercept)  5.389e-16  2.871e-01   0.000    1.000
    UEM.Jordan  -1.546e+00  1.107e+00  -1.396    0.396
    CPI.Jordan  -3.410e-01  4.389e-01  -0.777    0.579
    GDP.Jordan   8.784e-01  1.369e+00   0.642    0.637
    PPP.Jordan  -7.698e-01  6.423e-01  -1.198    0.443
    
    Residual standard error: 0.7031 on 1 degrees of freedom
    Multiple R-squared:  0.9011,	Adjusted R-squared:  0.5056 
    F-statistic: 2.278 on 4 and 1 DF,  p-value: 0.4561




                Df Sum Sq Mean Sq F value Pr(>F)
    UEM.Jordan   1  3.630   3.630   7.343  0.225
    CPI.Jordan   1  0.077   0.077   0.155  0.761
    GDP.Jordan   1  0.088   0.088   0.179  0.745
    PPP.Jordan   1  0.710   0.710   1.436  0.443
    Residuals    1  0.494   0.494               



```R
Lesotho <- data.frame(UEM$Lesotho, MPI$Lesotho, CPI$Lesotho, GDP$Lesotho, PPP$Lesotho)
Lmod_Lesotho = lm(MPI.Lesotho ~ ., data = Lesotho)
summary(Lmod_Lesotho)

anova_Lesotho = aov(Lmod_Lesotho)
summary(anova_Lesotho)
```


    
    Call:
    lm(formula = MPI.Lesotho ~ ., data = Lesotho)
    
    Residuals:
            1         2         3         4         5         6 
     0.004436  0.009241 -0.012939 -0.015937  0.027764 -0.012564 
    
    Coefficients:
                  Estimate Std. Error t value Pr(>|t|)  
    (Intercept)  9.054e-16  1.557e-02   0.000   1.0000  
    UEM.Lesotho  3.948e-01  3.340e-02  11.820   0.0537 .
    CPI.Lesotho  5.411e-03  1.996e-02   0.271   0.8315  
    GDP.Lesotho -4.432e-01  2.393e-02 -18.523   0.0343 *
    PPP.Lesotho -5.898e-01  3.194e-02 -18.464   0.0344 *
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    Residual standard error: 0.03815 on 1 degrees of freedom
    Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9985 
    F-statistic: 858.7 on 4 and 1 DF,  p-value: 0.02559




                Df Sum Sq Mean Sq  F value Pr(>F)  
    UEM.Lesotho  1 2.1816  2.1816 1499.188 0.0164 *
    CPI.Lesotho  1 0.0027  0.0027    1.853 0.4033  
    GDP.Lesotho  1 2.3182  2.3182 1593.054 0.0159 *
    PPP.Lesotho  1 0.4961  0.4961  340.904 0.0344 *
    Residuals    1 0.0015  0.0015                  
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



```R
Malawi <- data.frame(UEM$Malawi, MPI$Malawi, CPI$Malawi, GDP$Malawi, PPP$Malawi)
Lmod_Malawi = lm(MPI.Malawi ~ ., data = Malawi)
summary(Lmod_Malawi)

anova_Malawi = aov(Lmod_Malawi)
summary(anova_Malawi)
```


    
    Call:
    lm(formula = MPI.Malawi ~ ., data = Malawi)
    
    Residuals:
            1         2         3         4         5         6 
    -0.017854  0.104105 -0.153894  0.066665 -0.003921  0.004899 
    
    Coefficients:
                  Estimate Std. Error t value Pr(>|t|)
    (Intercept)  2.107e-16  8.096e-02   0.000    1.000
    UEM.Malawi  -1.920e-01  1.719e-01  -1.117    0.465
    CPI.Malawi  -2.202e-01  2.120e-01  -1.039    0.488
    GDP.Malawi  -5.754e-02  2.863e-01  -0.201    0.874
    PPP.Malawi  -1.069e+00  2.088e-01  -5.119    0.123
    
    Residual standard error: 0.1983 on 1 degrees of freedom
    Multiple R-squared:  0.9921,	Adjusted R-squared:  0.9607 
    F-statistic: 31.54 on 4 and 1 DF,  p-value: 0.1327




                Df Sum Sq Mean Sq F value Pr(>F)  
    UEM.Malawi   1  0.365   0.365   9.275 0.2020  
    CPI.Malawi   1  0.000   0.000   0.010 0.9374  
    GDP.Malawi   1  3.565   3.565  90.665 0.0666 .
    PPP.Malawi   1  1.030   1.030  26.200 0.1228  
    Residuals    1  0.039   0.039                 
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



```R
Mongolia <- data.frame(UEM$Mongolia, MPI$Mongolia, CPI$Mongolia, GDP$Mongolia, PPP$Mongolia)
Lmod_Mongolia = lm(MPI.Mongolia ~ ., data = Mongolia)
summary(Lmod_Mongolia)

anova_Mongolia = aov(Lmod_Mongolia)
summary(anova_Mongolia)
```


    
    Call:
    lm(formula = MPI.Mongolia ~ ., data = Mongolia)
    
    Residuals:
           1        2        3        4        5        6 
    -0.06479 -0.09850  0.29095 -0.02585 -0.02075 -0.08106 
    
    Coefficients:
                   Estimate Std. Error t value Pr(>|t|)
    (Intercept)   5.939e-16  1.331e-01   0.000    1.000
    UEM.Mongolia  1.688e+00  5.715e-01   2.953    0.208
    CPI.Mongolia  1.450e+00  6.707e-01   2.161    0.276
    GDP.Mongolia  1.451e-01  3.770e-01   0.385    0.766
    PPP.Mongolia -9.752e-02  5.019e-01  -0.194    0.878
    
    Residual standard error: 0.3259 on 1 degrees of freedom
    Multiple R-squared:  0.9788,	Adjusted R-squared:  0.8938 
    F-statistic: 11.52 on 4 and 1 DF,  p-value: 0.2171




                 Df Sum Sq Mean Sq F value Pr(>F)
    UEM.Mongolia  1  0.937   0.937   8.819  0.207
    CPI.Mongolia  1  3.941   3.941  37.100  0.104
    GDP.Mongolia  1  0.012   0.012   0.114  0.793
    PPP.Mongolia  1  0.004   0.004   0.038  0.878
    Residuals     1  0.106   0.106               



```R
Morocco <- data.frame(UEM$Morocco, MPI$Morocco, CPI$Morocco, GDP$Morocco, PPP$Morocco)
Lmod_Morocco = lm(MPI.Morocco ~ ., data = Morocco)
summary(Lmod_Morocco)

anova_Morocco = aov(Lmod_Morocco)
summary(anova_Morocco)
```


    
    Call:
    lm(formula = MPI.Morocco ~ ., data = Morocco)
    
    Residuals:
          1       2       3       4       5       6 
     0.1952 -0.1059 -0.4642  0.3376 -0.1405  0.1778 
    
    Coefficients:
                  Estimate Std. Error t value Pr(>|t|)
    (Intercept) -4.802e-16  2.677e-01   0.000    1.000
    UEM.Morocco -3.228e-01  5.251e-01  -0.615    0.649
    CPI.Morocco -6.489e-01  3.084e-01  -2.104    0.282
    GDP.Morocco  1.306e-02  4.182e-01   0.031    0.980
    PPP.Morocco -6.328e-01  5.525e-01  -1.145    0.457
    
    Residual standard error: 0.6558 on 1 degrees of freedom
    Multiple R-squared:  0.914,	Adjusted R-squared:  0.5699 
    F-statistic: 2.656 on 4 and 1 DF,  p-value: 0.4273




                Df Sum Sq Mean Sq F value Pr(>F)
    UEM.Morocco  1 2.4363  2.4363   5.665  0.253
    CPI.Morocco  1 1.5109  1.5109   3.513  0.312
    GDP.Morocco  1 0.0586  0.0586   0.136  0.775
    PPP.Morocco  1 0.5641  0.5641   1.312  0.457
    Residuals    1 0.4301  0.4301               



```R
Nigeria <- data.frame(UEM$Nigeria, MPI$Nigeria, CPI$Nigeria, GDP$Nigeria, PPP$Nigeria)
Lmod_Nigeria = lm(MPI.Nigeria ~ ., data = Nigeria)
summary(Lmod_Nigeria)

anova_Nigeria = aov(Lmod_Nigeria)
summary(anova_Nigeria)
```


    
    Call:
    lm(formula = MPI.Nigeria ~ ., data = Nigeria)
    
    Residuals:
           1        2        3        4        5        6 
     0.02209 -0.00307 -0.01378 -0.01608 -0.03036  0.04120 
    
    Coefficients:
                  Estimate Std. Error t value Pr(>|t|)  
    (Intercept)  2.320e-15  2.437e-02    0.00   1.0000  
    UEM.Nigeria -5.724e+00  3.860e-01  -14.83   0.0429 *
    CPI.Nigeria  5.351e-01  7.881e-02    6.79   0.0931 .
    GDP.Nigeria -2.119e+00  9.562e-02  -22.16   0.0287 *
    PPP.Nigeria  3.505e+00  3.195e-01   10.97   0.0579 .
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    Residual standard error: 0.0597 on 1 degrees of freedom
    Multiple R-squared:  0.9993,	Adjusted R-squared:  0.9964 
    F-statistic: 350.4 on 4 and 1 DF,  p-value: 0.04004




                Df Sum Sq Mean Sq F value Pr(>F)  
    UEM.Nigeria  1 1.0838  1.0838   304.1 0.0365 *
    CPI.Nigeria  1 2.0844  2.0844   584.8 0.0263 *
    GDP.Nigeria  1 1.3992  1.3992   392.5 0.0321 *
    PPP.Nigeria  1 0.4290  0.4290   120.4 0.0579 .
    Residuals    1 0.0036  0.0036                 
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



```R
Peru <- data.frame(UEM$Peru, MPI$Peru, CPI$Peru, GDP$Peru, PPP$Peru)
Lmod_Peru = lm(MPI.Peru ~ ., data = Peru)
summary(Lmod_Peru)

anova_Peru = aov(Lmod_Peru)
summary(anova_Peru)
```


    
    Call:
    lm(formula = MPI.Peru ~ ., data = Peru)
    
    Residuals:
             1          2          3          4          5          6 
    -0.0020999  0.1682355 -0.4147765  0.3012716 -0.0002682 -0.0523625 
    
    Coefficients:
                  Estimate Std. Error t value Pr(>|t|)
    (Intercept) -4.591e-16  2.213e-01   0.000    1.000
    UEM.Peru    -3.350e-01  3.518e-01  -0.952    0.516
    CPI.Peru     6.105e-02  2.995e-01   0.204    0.872
    GDP.Peru    -1.825e-01  4.909e-01  -0.372    0.773
    PPP.Peru    -5.377e-01  5.456e-01  -0.985    0.505
    
    Residual standard error: 0.5421 on 1 degrees of freedom
    Multiple R-squared:  0.9412,	Adjusted R-squared:  0.7061 
    F-statistic: 4.004 on 4 and 1 DF,  p-value: 0.3565




                Df Sum Sq Mean Sq F value Pr(>F)
    UEM.Peru     1 3.0610  3.0610  10.417  0.191
    CPI.Peru     1 0.0466  0.0466   0.159  0.759
    GDP.Peru     1 1.3131  1.3131   4.469  0.281
    PPP.Peru     1 0.2853  0.2853   0.971  0.505
    Residuals    1 0.2939  0.2939               



```R
Philippines <- data.frame(UEM$Philippines, MPI$Philippines, CPI$Philippines, GDP$Philippines, PPP$Philippines)
Lmod_Philippines = lm(MPI.Philippines ~ ., data = Philippines)
summary(Lmod_Philippines)

anova_Philippines = aov(Lmod_Philippines)
summary(anova_Philippines)
```


    
    Call:
    lm(formula = MPI.Philippines ~ ., data = Philippines)
    
    Residuals:
           1        2        3        4        5        6 
     0.01008 -0.08643  0.15853 -0.10038  0.05667 -0.03847 
    
    Coefficients:
                      Estimate Std. Error t value Pr(>|t|)
    (Intercept)      1.067e-15  8.895e-02   0.000    1.000
    UEM.Philippines -1.685e-01  1.406e-01  -1.199    0.443
    CPI.Philippines -1.911e-01  1.133e-01  -1.687    0.341
    GDP.Philippines -2.159e-01  4.536e-01  -0.476    0.717
    PPP.Philippines -8.104e-01  4.147e-01  -1.954    0.301
    
    Residual standard error: 0.2179 on 1 degrees of freedom
    Multiple R-squared:  0.9905,	Adjusted R-squared:  0.9525 
    F-statistic: 26.08 on 4 and 1 DF,  p-value: 0.1457




                    Df Sum Sq Mean Sq F value Pr(>F)  
    UEM.Philippines  1  0.307   0.307   6.466 0.2385  
    CPI.Philippines  1  0.544   0.544  11.450 0.1829  
    GDP.Philippines  1  3.921   3.921  82.588 0.0698 .
    PPP.Philippines  1  0.181   0.181   3.819 0.3011  
    Residuals        1  0.047   0.047                 
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



```R
Senegal <- data.frame(UEM$Senegal, MPI$Senegal, CPI$Senegal, GDP$Senegal, PPP$Senegal)
Lmod_Senegal = lm(MPI.Senegal ~ ., data = Senegal)
summary(Lmod_Senegal)

anova_Senegal = aov(Lmod_Senegal)
summary(anova_Senegal)
```


    
    Call:
    lm(formula = MPI.Senegal ~ ., data = Senegal)
    
    Residuals:
          1       2       3       4       5       6 
    -0.5153  1.0161 -0.4208 -0.2364  0.4997 -0.3434 
    
    Coefficients:
                  Estimate Std. Error t value Pr(>|t|)
    (Intercept) -3.359e-16  5.625e-01   0.000    1.000
    UEM.Senegal  7.256e-01  9.085e-01   0.799    0.571
    CPI.Senegal -1.559e-01  8.930e-01  -0.175    0.890
    GDP.Senegal -2.568e-01  8.727e-01  -0.294    0.818
    PPP.Senegal  9.362e-03  7.679e-01   0.012    0.992
    
    Residual standard error: 1.378 on 1 degrees of freedom
    Multiple R-squared:  0.6203,	Adjusted R-squared:  -0.8986 
    F-statistic: 0.4084 on 4 and 1 DF,  p-value: 0.8073




                Df Sum Sq Mean Sq F value Pr(>F)
    UEM.Senegal  1 2.6169  2.6169   1.378  0.449
    CPI.Senegal  1 0.2585  0.2585   0.136  0.775
    GDP.Senegal  1 0.2257  0.2257   0.119  0.789
    PPP.Senegal  1 0.0003  0.0003   0.000  0.992
    Residuals    1 1.8986  1.8986               



```R
Sierra.Leone <- data.frame(UEM$Sierra.Leone, MPI$Sierra.Leone, CPI$Sierra.Leone, GDP$Sierra.Leone, PPP$Sierra.Leone)
Lmod_Sierra.Leone = lm(MPI.Sierra.Leone ~ ., data = Sierra.Leone)
summary(Lmod_Sierra.Leone)

anova_Sierra.Leone = aov(Lmod_Sierra.Leone)
summary(anova_Sierra.Leone)
```


    
    Call:
    lm(formula = MPI.Sierra.Leone ~ ., data = Sierra.Leone)
    
    Residuals:
           1        2        3        4        5        6 
    -0.19181  0.26038 -0.09823  0.07347  0.18982 -0.23362 
    
    Coefficients:
                       Estimate Std. Error t value Pr(>|t|)
    (Intercept)      -1.425e-15  1.872e-01   0.000    1.000
    UEM.Sierra.Leone -4.158e-01  4.546e-01  -0.915    0.528
    CPI.Sierra.Leone  1.301e+00  1.018e+00   1.278    0.423
    GDP.Sierra.Leone  1.666e-01  7.492e-01   0.222    0.861
    PPP.Sierra.Leone -1.735e+00  1.074e+00  -1.615    0.353
    
    Residual standard error: 0.4585 on 1 degrees of freedom
    Multiple R-squared:  0.958,	Adjusted R-squared:  0.7898 
    F-statistic: 5.695 on 4 and 1 DF,  p-value: 0.3033




                     Df Sum Sq Mean Sq F value Pr(>F)
    UEM.Sierra.Leone  1 2.8254  2.8254  13.439  0.170
    CPI.Sierra.Leone  1 0.2309  0.2309   1.098  0.485
    GDP.Sierra.Leone  1 1.1848  1.1848   5.635  0.254
    PPP.Sierra.Leone  1 0.5487  0.5487   2.610  0.353
    Residuals         1 0.2102  0.2102               



```R
South.Africa <- data.frame(UEM$South.Africa, MPI$South.Africa, CPI$South.Africa, GDP$South.Africa, PPP$South.Africa)
Lmod_South.Africa = lm(MPI.South.Africa ~ ., data = South.Africa)
summary(Lmod_South.Africa)

anova_South.Africa = aov(Lmod_South.Africa)
summary(anova_South.Africa)
```


    
    Call:
    lm(formula = MPI.South.Africa ~ ., data = South.Africa)
    
    Residuals:
           1        2        3        4        5        6 
    -0.06659  0.19301 -0.38464  0.10829  0.66762 -0.51769 
    
    Coefficients:
                       Estimate Std. Error t value Pr(>|t|)
    (Intercept)      -1.124e-15  3.905e-01   0.000    1.000
    UEM.South.Africa -1.538e+00  2.348e+00  -0.655    0.631
    CPI.South.Africa  3.966e-01  6.581e-01   0.603    0.655
    GDP.South.Africa  1.123e-01  1.248e+00   0.090    0.943
    PPP.South.Africa  1.173e+00  2.942e+00   0.398    0.759
    
    Residual standard error: 0.9566 on 1 degrees of freedom
    Multiple R-squared:  0.817,	Adjusted R-squared:  0.08491 
    F-statistic: 1.116 on 4 and 1 DF,  p-value: 0.6026




                     Df Sum Sq Mean Sq F value Pr(>F)
    UEM.South.Africa  1 2.3282  2.3282   2.544  0.356
    CPI.South.Africa  1 1.4494  1.4494   1.584  0.427
    GDP.South.Africa  1 0.1620  0.1620   0.177  0.746
    PPP.South.Africa  1 0.1453  0.1453   0.159  0.759
    Residuals         1 0.9151  0.9151               



```R
Suriname <- data.frame(UEM$Suriname, MPI$Suriname, CPI$Suriname, GDP$Suriname, PPP$Suriname)
Lmod_Suriname = lm(MPI.Suriname ~ ., data = Suriname)
summary(Lmod_Suriname)

anova_Suriname = aov(Lmod_Suriname)
summary(anova_Suriname)
```


    
    Call:
    lm(formula = MPI.Suriname ~ ., data = Suriname)
    
    Residuals:
           1        2        3        4        5        6 
     0.10552  0.35595 -0.43482 -0.06864  0.11124 -0.06926 
    
    Coefficients:
                   Estimate Std. Error t value Pr(>|t|)
    (Intercept)   2.448e-16  2.411e-01   0.000    1.000
    UEM.Suriname -4.693e-01  3.211e-01  -1.462    0.382
    CPI.Suriname -9.232e-01  1.244e+00  -0.742    0.594
    GDP.Suriname -6.695e-01  4.899e-01  -1.367    0.402
    PPP.Suriname -1.113e-01  1.259e+00  -0.088    0.944
    
    Residual standard error: 0.5906 on 1 degrees of freedom
    Multiple R-squared:  0.9302,	Adjusted R-squared:  0.6512 
    F-statistic: 3.334 on 4 and 1 DF,  p-value: 0.387




                 Df Sum Sq Mean Sq F value Pr(>F)
    UEM.Suriname  1  3.238   3.238   9.285  0.202
    CPI.Suriname  1  0.754   0.754   2.162  0.380
    GDP.Suriname  1  0.656   0.656   1.881  0.401
    PPP.Suriname  1  0.003   0.003   0.008  0.944
    Residuals     1  0.349   0.349               



```R
Togo <- data.frame(UEM$Togo, MPI$Togo, CPI$Togo, GDP$Togo, PPP$Togo)
Lmod_Togo = lm(MPI.Togo ~ ., data = Togo)
summary(Lmod_Togo)

anova_Togo = aov(Lmod_Togo)
summary(anova_Togo)
```


    
    Call:
    lm(formula = MPI.Togo ~ ., data = Togo)
    
    Residuals:
           1        2        3        4        5        6 
    -0.02747  0.06029 -0.04654  0.01534  0.02137 -0.02299 
    
    Coefficients:
                  Estimate Std. Error t value Pr(>|t|)  
    (Intercept) -8.296e-17  3.600e-02   0.000   1.0000  
    UEM.Togo     4.814e+00  7.531e-01   6.393   0.0988 .
    CPI.Togo     2.325e-01  1.247e-01   1.865   0.3133  
    GDP.Togo    -6.011e+00  7.685e-01  -7.822   0.0810 .
    PPP.Togo     7.259e-01  1.567e-01   4.633   0.1353  
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    Residual standard error: 0.08818 on 1 degrees of freedom
    Multiple R-squared:  0.9984,	Adjusted R-squared:  0.9922 
    F-statistic: 160.5 on 4 and 1 DF,  p-value: 0.05912




                Df Sum Sq Mean Sq F value Pr(>F)  
    UEM.Togo     1  3.643   3.643  468.57 0.0294 *
    CPI.Togo     1  0.354   0.354   45.47 0.0937 .
    GDP.Togo     1  0.829   0.829  106.57 0.0615 .
    PPP.Togo     1  0.167   0.167   21.47 0.1353  
    Residuals    1  0.008   0.008                 
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



```R
Zimbabwe <- data.frame(UEM$Zimbabwe, MPI$Zimbabwe, CPI$Zimbabwe, GDP$Zimbabwe, PPP$Zimbabwe)
Lmod_Zimbabwe = lm(MPI.Zimbabwe ~ ., data = Zimbabwe)
summary(Lmod_Zimbabwe)

anova_Zimbabwe = aov(Lmod_Zimbabwe)
summary(anova_Zimbabwe)
```


    
    Call:
    lm(formula = MPI.Zimbabwe ~ ., data = Zimbabwe)
    
    Residuals:
            1         2         3         4         5         6 
    -0.000143  0.001711  0.004362 -0.005897  0.001147 -0.001181 
    
    Coefficients:
                   Estimate Std. Error t value Pr(>|t|)   
    (Intercept)  -9.029e-16  3.148e-03    0.00  1.00000   
    UEM.Zimbabwe -5.597e-01  4.144e-03 -135.06  0.00471 **
    CPI.Zimbabwe  1.132e+00  9.255e-03  122.29  0.00521 **
    GDP.Zimbabwe  6.290e-02  5.926e-03   10.61  0.05981 . 
    PPP.Zimbabwe -1.365e+00  8.088e-03 -168.81  0.00377 **
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    Residual standard error: 0.007711 on 1 degrees of freedom
    Multiple R-squared:      1,	Adjusted R-squared:  0.9999 
    F-statistic: 2.102e+04 on 4 and 1 DF,  p-value: 0.005173




                 Df Sum Sq Mean Sq F value  Pr(>F)   
    UEM.Zimbabwe  1 1.7879  1.7879 30070.0 0.00367 **
    CPI.Zimbabwe  1 0.0093  0.0093   156.1 0.05085 . 
    GDP.Zimbabwe  1 1.5085  1.5085 25371.8 0.00400 **
    PPP.Zimbabwe  1 1.6942  1.6942 28495.1 0.00377 **
    Residuals     1 0.0001  0.0001                   
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

