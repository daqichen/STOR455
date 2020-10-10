# ---
# title: "Amsterdam Housing Basic Data"
# author: "Daqi Chen"
# date: "9/14/2020"
# Updated: "10/10/2020
# ---
  
library(readr)
library(leaps)
source("R Scripts/VIF.R")
source("R Scripts/ShowSubsets.R")
source("R Scripts/HistNormal.R")


Ames5 <- read.csv("Datasets/AmsterdamData/AmesTrain5.csv", header = TRUE, sep = ",")
sapply(Ames5, is.numeric) #Find the list of numeric variables in the dataset.

##########################################
# Part 1: Build an initial "basic" model #
##########################################

## Inital model with all numerical variables
Ames_1 = lm(Price~., data=Ames5[c(2:4, 7:10, 17:19, 23:30, 32, 33, 35, 36, 39:42)])
summary(Ames_1)
# plot(Ames_1)


# Building an initial model with all of the numeric variables.
# + Problematic numerical variables
# ++ _YearBuilt_ and _YearRemodel_ are actually categorical
# ++ _BasementSF_ = _BasementFinSF_ + _BasementUnFinSF_
# ++ _GroundSF_ = _FirstSF_ + _SecondSF_
# ++ _TotalRooms_ is the sum of all the rooms besides bathrooms
# ++ _GarageCars_ represent almost the same thing _GarageSF_ represent


#______________________________________________________________________________________________
## Without the problematic variables

Ames = lm(Price~LotFrontage+LotArea+Quality+Condition+
            BasementFinSF+BasementUnFinSF+FirstSF+
            SecondSF+BasementFBath+BasementHBath+
            FullBath+HalfBath+Bedroom+Fireplaces+
            GarageSF+WoodDeckSF+OpenPorchSF+
            EnclosedPorchSF+ScreenPorchSF
          , data=Ames5)
summary(Ames)
# Residual standard error: 29.44 on 580 degrees of freedom
# Multiple R-squared:  0.8535,	Adjusted R-squared:  0.8487 
# F-statistic: 177.8 on 19 and 580 DF,  p-value: < 2.2e-16

plot(Ames)
# + With the numerical variables in the dataset, R^2 is 0.8487, hence roughly 85% of the data is explained.

#______________________________________________________________________________________________
vif(Ames)
# LotFrontage         LotArea         Quality       Condition   BasementFinSF BasementUnFinSF 
# 1.099754        1.180547        2.571154        1.188768        3.798558        3.435426 
# FirstSF        SecondSF   BasementFBath   BasementHBath        FullBath        HalfBath 
# 3.556326        3.849100        2.156316        1.159904        2.493375        1.984103 
# Bedroom      Fireplaces        GarageSF      WoodDeckSF     OpenPorchSF EnclosedPorchSF 
# 1.801298        1.581352        1.736276        1.181585        1.184168        1.233239 
# ScreenPorchSF 
# 1.095330 

hist.normal(Ames$residuals)


#______________________________________________________________________________________________
## Using All Subsets to select predictors

AmesAll = regsubsets(Price~LotFrontage+LotArea+Quality+Condition+
                       BasementFinSF+BasementUnFinSF+FirstSF+
                       SecondSF+BasementFBath+BasementHBath+
                       FullBath+HalfBath+Bedroom+Fireplaces+
                       GarageSF+WoodDeckSF+OpenPorchSF+
                       EnclosedPorchSF+ScreenPorchSF
                     , data=Ames5, nbest = 2)
summary(AmesAll)

ShowSubsets(AmesAll)
# LotFrontage LotArea Quality Condition BasementFinSF BasementUnFinSF FirstSF SecondSF
# 1  ( 1 )                           *                                                         
#   1  ( 2 )                                                                                     
# 2  ( 1 )                           *                                               *         
#   2  ( 2 )                           *                                                         
#   3  ( 1 )                           *                                               *        *
#   3  ( 2 )                           *                                               *         
#   4  ( 1 )                           *                       *                       *        *
#   4  ( 2 )                           *                                               *        *
#   5  ( 1 )                           *                       *                       *        *
#   5  ( 2 )                   *       *                       *                       *        *
#   6  ( 1 )                   *       *                       *                       *        *
#   6  ( 2 )           *               *                       *                       *        *
#   7  ( 1 )           *       *       *                       *                       *        *
#   7  ( 2 )                   *       *                       *                       *        *
#   8  ( 1 )           *       *       *                       *                       *        *
#   8  ( 2 )           *       *       *                       *                       *        *
#   BasementFBath BasementHBath FullBath HalfBath Bedroom Fireplaces GarageSF WoodDeckSF
# 1  ( 1 )                                                                                     
# 1  ( 2 )                                                                         *           
#   2  ( 1 )                                                                                     
# 2  ( 2 )                                                                         *           
#   3  ( 1 )                                                                                     
# 3  ( 2 )                                             *                                       
#   4  ( 1 )                                                                                     
# 4  ( 2 )                                                                         *           
#   5  ( 1 )                                                                         *           
#   5  ( 2 )                                                                                     
# 6  ( 1 )                                                                         *           
#   6  ( 2 )                                                                         *           
#   7  ( 1 )                                                                         *           
#   7  ( 2 )                                                                         *           
#   8  ( 1 )                                                                         *           
#   8  ( 2 )                                                                         *          *
#   OpenPorchSF EnclosedPorchSF ScreenPorchSF   Rsq adjRsq      Cp
# 1  ( 1 )                                           65.64  65.59  763.83
# 1  ( 2 )                                           42.72  42.62 1671.16
# 2  ( 1 )                                           73.53  73.44  453.71
# 2  ( 2 )                                           71.40  71.30  538.16
# 3  ( 1 )                                           78.92  78.82  242.20
# 3  ( 2 )                                           76.70  76.58  330.27
# 4  ( 1 )                                           81.55  81.42  140.40
# 4  ( 2 )                                           80.44  80.31  184.16
# 5  ( 1 )                                           82.90  82.76   88.76
# 5  ( 2 )                                           82.17  82.02  117.77
# 6  ( 1 )                                           83.54  83.37   65.55
# 6  ( 2 )                                           83.28  83.11   75.95
# 7  ( 1 )                                           83.88  83.69   53.83
# 7  ( 2 )           *                               83.83  83.64   55.92
# 8  ( 1 )           *                               84.14  83.93   45.67
# 8  ( 2 )                                           84.11  83.90   46.76

# + The smallest Cp was 45.67
# + Predictors: LotFrontage + LotArea + Quality + BasementFinSF + FirstSF + SecondSF + GarageSF + OpenPorchSF 


#______________________________________________________________________________________________
modAmes1 = lm(Price~LotFrontage + LotArea + Quality + BasementFinSF + FirstSF + SecondSF + GarageSF + OpenPorchSF, data=Ames5)
summary(modAmes1)
# Residual standard error: 30.34 on 591 degrees of freedom
# Multiple R-squared:  0.8414,	Adjusted R-squared:  0.8393 
# F-statistic:   392 on 8 and 591 DF,  p-value: < 2.2e-16

# + R^2 was 0.8487, and here in this model after selecting predictors using All Subsets, R^2 is 0.8393, so it's a good set of predictors
# + Among the predictors selected, __OpenPorchSF__ has a p-value > 5%


plot(modAmes1)
vif(modAmes1)
# LotFrontage       LotArea       Quality BasementFinSF       FirstSF      SecondSF      GarageSF 
# 1.043859      1.130386      1.784098      1.209725      1.968110      1.395500      1.669701 
# OpenPorchSF 
# 1.138768 

# + No VIF > 5, so __Multicollinearity__ shouldn't be an issue.

#______________________________________________________________________________________________
## Using Backward Elimination to select predictors

AmesFull = lm(Price~LotFrontage+LotArea+Quality+Condition+
                BasementFinSF+BasementUnFinSF+FirstSF+
                SecondSF+BasementFBath+BasementHBath+
                FullBath+HalfBath+Bedroom+Fireplaces+
                GarageSF+WoodDeckSF+OpenPorchSF+
                EnclosedPorchSF+ScreenPorchSF
              , data=Ames5)
MSE = (summary(AmesFull)$sigma)^2

step(AmesFull, scale=MSE)
# lm(formula = Price ~ LotFrontage + LotArea + Quality + Condition + 
#      BasementFinSF + BasementUnFinSF + FirstSF + SecondSF + FullBath + 
#      HalfBath + Bedroom + Fireplaces + GarageSF + WoodDeckSF + 
#      OpenPorchSF + EnclosedPorchSF, data = Ames5)

# + Cp = 17.179

modAmes2 = lm(Price ~ LotFrontage + LotArea + Quality + Condition + 
                BasementFinSF + BasementUnFinSF + FirstSF + SecondSF + FullBath + 
                HalfBath + Bedroom + Fireplaces + GarageSF + WoodDeckSF + 
                OpenPorchSF + EnclosedPorchSF, data = Ames5)
summary(modAmes2)
# Residual standard error: 29.44 on 583 degrees of freedom
# Multiple R-squared:  0.8527,	Adjusted R-squared:  0.8486 
# F-statistic: 210.9 on 16 and 583 DF,  p-value: < 2.2e-16

# + R^2 was 0.8487, and here in this model after selecting predictors using All Subsets, R^2 is 0.8486, so it's a really good set of predictors
# + Among the predictors selected, __FullBath__ and __EnclosedPorchSF__ has a p-value > 5%
 
plot(modAmes2)
vif(modAmes2)
# LotFrontage         LotArea         Quality       Condition   BasementFinSF BasementUnFinSF 
# 1.097061        1.158470        2.510262        1.149547        3.321385        3.314696 
# FirstSF        SecondSF        FullBath        HalfBath         Bedroom      Fireplaces 
# 3.538575        3.827829        2.380986        1.949575        1.790606        1.545089 
# GarageSF      WoodDeckSF     OpenPorchSF EnclosedPorchSF 
# 1.698365        1.171513        1.178533        1.220391 

# + No VIF > 5, so __Multicollinearity__ shouldn't be an issue.


#______________________________________________________________________________________________
## Using Forward Selection to select predictors

none = lm(Price~1, data=Ames5)

step(none, scope=list(upper=AmesFull), scale=MSE, direction = "forward")

# lm(formula = Price ~ Quality + FirstSF + SecondSF + BasementFinSF + 
#      GarageSF + LotArea + LotFrontage + OpenPorchSF + WoodDeckSF + 
#      Bedroom + BasementUnFinSF + HalfBath + Condition + FullBath + 
#      Fireplaces + EnclosedPorchSF, data = Ames5)
# 
# 
# + Cp = 17.179


modAmes3 = lm(Price ~ Quality + FirstSF + SecondSF + BasementFinSF + 
                GarageSF + LotArea + LotFrontage + OpenPorchSF + WoodDeckSF + 
                Bedroom + BasementUnFinSF + HalfBath + Condition + FullBath + 
                Fireplaces + EnclosedPorchSF, data = Ames5)
summary(modAmes3)
# Residual standard error: 29.44 on 583 degrees of freedom
# Multiple R-squared:  0.8527,	Adjusted R-squared:  0.8486 
# F-statistic: 210.9 on 16 and 583 DF,  p-value: < 2.2e-16

# + R^2 was 0.8487, and here in this model after selecting predictors using All Subsets, R^2 is 0.8486, so it's a really good set of predictors
# + Among the predictors selected, __FullBath__ and __EnclosedPorchSF__ has a p-value > 5%
# + Both of the above observations were present in the backward selection model


plot(modAmes3)
vif(modAmes3)
# Quality         FirstSF        SecondSF   BasementFinSF        GarageSF         LotArea 
# 2.510262        3.538575        3.827829        3.321385        1.698365        1.158470 
# LotFrontage     OpenPorchSF      WoodDeckSF         Bedroom BasementUnFinSF        HalfBath 
# 1.097061        1.178533        1.171513        1.790606        3.314696        1.949575 
# Condition        FullBath      Fireplaces EnclosedPorchSF 
# 1.149547        2.380986        1.545089        1.220391

# + No VIF > 5, so __Multicollinearity__ shouldn't be an issue.

#______________________________________________________________________________________________
#### Chosen Set of Predictors:
# + modAmes3
# + Predictors: LotFrontage + LotArea + Quality + Condition +
#   BasementFinSF + BasementUnFinSF + FirstSF + SecondSF + FullBath + 
#   HalfBath + Bedroom + Fireplaces + GarageSF + WoodDeckSF + 
#   OpenPorchSF + EnclosedPorchSF 


#########################################################
# Part 2: Residual analysis for your chosen basic model #
#########################################################

plot(modAmes3, 1:2)
summary(modAmes3)


#______________________________________________________________________________________________
### Conditions:

# + Linearity:
#   - Looking at Residual vs. Fitted plot, the red line is curved
# - Problematic
# 
# + Zero Mean:
#   - Always Fit by the nature of the model
# 
# + Constant Variance:
#   - Looking at Residual vs. Fitted plot, some fanning patterns towards the right
# - Problematic
# 
# + Normality
# - Looking at qq plot, the data fit this line
# - But right tail is slightly above the line, could be problematic
# 
# + Independence
# - Looking at Residual vs. Fitted plot, the curve pattern
# - Problematic


#______________________________________________________________________________________________
## Looking at Standardized and Studentized Residuals

head(sort(modAmes3$residuals, decreasing=TRUE), n=20)
# 406        88       377       457        32       380        82       330       418       368 
# 156.45973 114.95053 108.82904 103.72011  97.33149  93.59933  91.59550  91.18175  90.71266  83.16931 
# 331       571       394       178       477        97       111         9       366       420 
# 76.26954  72.99692  62.58662  62.10794  60.41587  58.15600  57.66423  57.64484  57.14438  56.63910 

rstandard(modAmes3)[c(406,88,377,457,32,380,82,330,418,368)]
# 406       88      377      457       32      380       82      330      418      368 
# 5.489127 3.963868 3.812871 3.581949 3.346246 3.246301 3.172902 3.139275 3.154286 2.872677

rstudent(modAmes3)[c(406,88,377,457,32,380,82,330,418,368)]
# 406       88      377      457       32      380       82      330      418      368 
# 5.631880 4.014940 3.858006 3.618919 3.375953 3.273234 3.197911 3.163433 3.178821 2.890744 

# + Entry 406,88,377,457,32,380,82,330,418 are possibly influential, because both their standardized and studentized residuals are more than 3.

#______________________________________________________________________________________________
## Looking at Leverages

# number of predictors: 16
# number of samples: 600
2*(17/600) #[1] 0.05666667
3*(17/600) #[1] 0.085

head(sort(hatvalues(modAmes3),decreasing = TRUE),20)
# 559        278        463        556        271        467        459        165        416 
# 0.44183448 0.30328792 0.27387041 0.25336636 0.18795420 0.13358307 0.11358048 0.10967858 0.10681735 
# 431        185        230        425        149        356        510        128        340 
# 0.09770426 0.09299308 0.09165514 0.08949614 0.08762060 0.08252468 0.08137616 0.08083696 0.07814958 
# 187        587 
# 0.07760347 0.07423185 

# + Hat values for entry 559, 278, 463, 556, 271, 467, 459, 165, 416, 431, 185, 230, 425, 149 are high and shows those entries to have high leverage, or high potential to be influential on the model.

#______________________________________________________________________________________________
## Take subset of Ames5 without entries with high standardized/studentized residuals

AmesRes1 <- Ames5[-c(406,88,377,457,32,380,82,330,418),] 

modAmes3Res1 = lm(Price ~ Quality + FirstSF + SecondSF + BasementFinSF + 
                    GarageSF + LotArea + LotFrontage + OpenPorchSF + WoodDeckSF + 
                    Bedroom + BasementUnFinSF + HalfBath + Condition + FullBath + 
                    Fireplaces + EnclosedPorchSF, data = AmesRes1)
summary(modAmes3Res1)
# Residual standard error: 26.05 on 574 degrees of freedom
# Multiple R-squared:  0.8612,	Adjusted R-squared:  0.8574 
# F-statistic: 222.7 on 16 and 574 DF,  p-value: < 2.2e-16
plot(modAmes3Res1, 1:2)

# + After excluding entries with high standardized or studentized residuals, R^2 increased from 0.8486 to 0.8574, and now only _WoodDeckSF_ has an insignificant p-value. Linearity conditions wise, the problems with Linearity, Constant Variance and Independence persist.

#______________________________________________________________________________________________
## Take subset of Ames5 without entries with high leverages

AmesRes2 <- Ames5[-c(559, 278, 463, 556, 271, 467, 459, 165, 416, 431, 185, 230, 425, 149),] 

modAmes3Res2 = lm(Price ~ Quality + FirstSF + SecondSF + BasementFinSF + 
                    GarageSF + LotArea + LotFrontage + OpenPorchSF + WoodDeckSF + 
                    Bedroom + BasementUnFinSF + HalfBath + Condition + FullBath + 
                    Fireplaces + EnclosedPorchSF, data = AmesRes2)
summary(modAmes3Res2)
# Residual standard error: 28.73 on 569 degrees of freedom
# Multiple R-squared:  0.8586,	Adjusted R-squared:  0.8546 
# F-statistic: 215.9 on 16 and 569 DF,  p-value: < 2.2e-16
plot(modAmes3Res2, 1:2)

# + After excluding entries with high leverage, R^2 increased from 0.8486 to 0.8546, and now only _EnclosedPorchSF_ has an insignificant p-value. Linearity conditions wise, the problems with Linearity, Constant Variance and Independence persist. Worth noted, the model with high residuals excluded is more effective due to its higher R^2 value.

#______________________________________________________________________________________________
## Take subset of Ames5 without entries with high residuals or leverage

AmesRes3 <- Ames5[-c(406,88,377,457,32,380,82,330,418, 559, 278, 463, 556, 271, 467, 459, 165, 416, 431, 185, 230, 425, 149),] 

modAmes3Res3 = lm(Price ~ Quality + FirstSF + SecondSF + BasementFinSF + 
                    GarageSF + LotArea + LotFrontage + OpenPorchSF + WoodDeckSF + 
                    Bedroom + BasementUnFinSF + HalfBath + Condition + FullBath + 
                    Fireplaces + EnclosedPorchSF, data = AmesRes3)
summary(modAmes3Res3)
# Residual standard error: 25.6 on 560 degrees of freedom
# Multiple R-squared:  0.8639,	Adjusted R-squared:   0.86 
# F-statistic: 222.1 on 16 and 560 DF,  p-value: < 2.2e-16
plot(modAmes3Res3, 1:2)

#______________________________________________________________________________________________
### Conclusion: 

# + After excluding entries with both high standardized or studentized residuals and high leverage, R^2 increased from 0.8486 to 0.86, and now all predictors have a significant p-value. Linearity conditions wise, the problems with Linearity, Constant Variance and Independence persist. It does, however, have the highest R^2 value and have a significant p-value for all predictors, hence the most effective adjusted model.

