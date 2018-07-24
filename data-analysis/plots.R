library(ggplot2)
#source("answers_pare.R")


age.group.chart <- ggplot(age.group.bsc.achieves.table, aes(x="", y=total, fill=age.group)) + geom_bar(stat="identity") + labs(title="Age group count", x="", y="count") + guides(fill=guide_legend(title=NULL))
  


#degree.highest.chart <- ggplot(degree.highest.bsc.achieves.table, aes(x="", y=total, fill=degree.highest)) + geom_bar(stat="identity")
#degree.country.chart <- ggplot(degree.country.bsc.achieves.table, aes(x="", y=total, fill=degree.country)) + geom_bar(stat="identity")
#employed.country.chart <- ggplot(employed.country.bsc.achieves.table, aes(x="", y=total, fill=employed.country)) + geom_bar(stat="identity")

asd <- gather(tibble::rownames_to_column(categories.bsc.achieves.table), achievement, percentage, bsc.achieves.programming_pc:bsc.achieves.dontknow_pc, factor_key=TRUE)

# we need to find a better way to put the names on the bottom, but the graph should be ok
chart <- ggplot(asd, aes(x=achievement, y=percentage, fill=rowname)) +  geom_bar(position="dodge", stat="identity")



