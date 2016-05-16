library(xtable)
# Functions for descriptives and printing 
# FUNCTIONS
mylongtable <- function(x){
    count <- table(x)
    perc <- round(prop.table(table(x))*100, digits = 1)
    Overall <- paste(count, " (", perc, ")", sep = "")
    return(cbind(as.character(levels(x)), Overall))
}

mywidetable <- function(x,label){
    count <- table(x, useNA = "always")
    perc <- round(prop.table(table(x, useNA = "always"))*100, digits = 2)
    final <- paste(count, " (", perc, ")", sep = "")
    return(c(label, final))
}
