# Best-Subset-Selection-On-Simulated-Dataset

## Dataset: 
Here, I've created my own simulated dataset of of matrix 1000*20. I’ve also made 4 of the beta values zero so that i can make some estimations later on about my model . I’ve made my beta = 4,7,12 and 16 as zero. I’ll also create epsilon (Noise) value so that i don’t create a perfect model but i’ll scale this so that i don’t have the same scale as my data.

## Libraries: 
library(ISLR)
library(corrplot)
library(MASS)
library(klaR)
library(leaps)
library(lattice)
library(ggplot2)
library(corrplot)
library(car)
library(caret)
library(class)

## Density Plot:

![densityplot_ques1](https://user-images.githubusercontent.com/46763031/147999785-e2a8e9d1-e8ff-432d-b08e-6004f51ff590.png)

The density plot of the data tells that all the classes in our data are in normal distribution.

## Best Subset Selection:
I’ll perform best subset selection to see which is the best model. As i've made my beta’s zero for 4 classes that were 4,7,12 and 16, So the best 16 variable model should exclude those classes for me. After analysis, The best 16 variable model excludes 4,7,12 and 16. So our model is fitted perfectly.


## Training Error: 

![trainingerror_ques1](https://user-images.githubusercontent.com/46763031/148000031-65a46783-beef-4a50-b50e-74964b1354ea.png)


## Testing Error:

![testingerror_ques1](https://user-images.githubusercontent.com/46763031/148000048-973eb4e7-2b7b-499f-a4a0-26ce4ecd153a.png)

We have our lowest testing error for model with 16 variables. Our the best fit selection has worked perfectly on the test data because I took beta values zero for 4 variables (4,7,12,16) and best fit model gives the lowest testing error for model with 16 variable excluding those four variables.



