# required libraries 
library(dplyr)
library(corrplot)

# correlation between continous variables
num_col <- dplyr::select_if(trsf, is.numeric)
summary(num_col)

corrplot(cor(num_col), type = "upper", method = "number", tl.cex = 1)
cor(num_col > 0.9) 
