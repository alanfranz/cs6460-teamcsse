library(ggplot2)
library(dplyr)
#source("answers_pare.R")


age.group.chart <- ggplot(age.group.bsc.achieves.table, aes(x="", y=total, fill=age.group)) + geom_bar(stat="identity") + labs(title="Age group count", x="", y="count") + guides(fill=guide_legend(title=NULL))
  


#degree.highest.chart <- ggplot(degree.highest.bsc.achieves.table, aes(x="", y=total, fill=degree.highest)) + geom_bar(stat="identity")
#degree.country.chart <- ggplot(degree.country.bsc.achieves.table, aes(x="", y=total, fill=degree.country)) + geom_bar(stat="identity")
#employed.country.chart <- ggplot(employed.country.bsc.achieves.table, aes(x="", y=total, fill=employed.country)) + geom_bar(stat="identity")

categories_plot <- function(mytable, title, replacesth) {

 asd <- gather(tibble::rownames_to_column(mytable %>% select(contains("_pc")), var="category"), achievement, percentage, matches(".*_pc$"), factor_key=TRUE)
 asd <- asd %>% mutate(achievement = gsub("_pc", "", gsub(replacesth, "", achievement, fixed=TRUE), fixed=TRUE),
                      category = gsub("$", "?", gsub(".", " ", category, fixed = TRUE)))


 chart <- ggplot(asd, aes(x=achievement, y=percentage, fill=category)) +  geom_bar(position="dodge", stat="identity") + labs(title=title, y="percentage of category respondents")
 ggsave(sprintf("%s/%s.eps", "plots_output", gsub(" ", "_", title, fixed=TRUE)), plot=chart, device="eps", width=50, height=10, units="cm", dpi=72)
}

categories_plot(categories.bsc.achieves.table, "BSc graduate reported achievements by category", "bsc.achieves.")
categories_plot(categories.bsc.shouldachieve.table, "BSc graduate would-like achievements by category", "bsc.shouldachieve.")
categories_plot(categories.msc.achieves.table, "MSc graduate reported achievements by category", "msc.achieves.")
categories_plot(categories.msc.shouldachieve.table, "MSc graduate would-like achievements by category", "msc.shouldachieve.")


