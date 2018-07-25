library(ggplot2)
library(dplyr)
#source("answers_pare.R")


age.group.chart <- ggplot(age.group.bsc.achieves.table, aes(x="", y=total, fill=age.group)) + geom_bar(stat="identity") + labs(title="Age group count", x="", y="count") + guides(fill=guide_legend(title=NULL))
  


#degree.highest.chart <- ggplot(degree.highest.bsc.achieves.table, aes(x="", y=total, fill=degree.highest)) + geom_bar(stat="identity")
#degree.country.chart <- ggplot(degree.country.bsc.achieves.table, aes(x="", y=total, fill=degree.country)) + geom_bar(stat="identity")
#employed.country.chart <- ggplot(employed.country.bsc.achieves.table, aes(x="", y=total, fill=employed.country)) + geom_bar(stat="identity")

asd <- gather(tibble::rownames_to_column(categories.bsc.achieves.table %>% select(contains("_pc")), var="category"), achievement, percentage, bsc.achieves.programming_pc:bsc.achieves.dontknow_pc, factor_key=TRUE)
asd <- asd %>% mutate(achievement = gsub("_pc", "", gsub("bsc.achieves.", "", achievement, fixed=TRUE), fixed=TRUE),
                      category = gsub(".", " ", category, fixed = TRUE))


chart <- ggplot(asd, aes(x=achievement, y=percentage, fill=category)) +  geom_bar(position="dodge", stat="identity")
ggsave("/tmp/something.eps", plot=chart, device="eps", width=50, height=10, units="cm", dpi=72)



