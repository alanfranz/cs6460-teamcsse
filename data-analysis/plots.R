library(ggplot2)
#source("answers_pare.R")


age.group.chart <- ggplot(age.group.bsc.achieves.table, aes(x="", y=total, fill=age.group)) + geom_bar(stat="identity")
degree.highest.chart <- ggplot(degree.highest.bsc.achieves.table, aes(x="", y=total, fill=degree.highest)) + geom_bar(stat="identity")
degree.country.chart <- ggplot(degree.country.bsc.achieves.table, aes(x="", y=total, fill=degree.country)) + geom_bar(stat="identity")
employed.country.chart <- ggplot(employed.country.bsc.achieves.table, aes(x="", y=total, fill=employed.country)) + geom_bar(stat="identity")
