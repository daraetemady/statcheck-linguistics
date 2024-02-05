library(statcheck)
library(tidyverse)

#Might produce a warning message, this can be ignored.
stat <- as.data.frame(checkdir("C:/Users/darae/Documents/ILN2200/articles-test-pdf"))

names(stat)[names(stat) == 'source'] <- 'Article'
articles <- as.data.frame(table(stat$Article))
names(articles)[names(articles) == 'Var1'] <- 'Article'
num.articles <- length(articles$Article)

#Prints the Statcheck table
stat

#List of articles, name and p-value frequency
articles

#Number of articles
num.articles




