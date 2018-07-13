suppressMessages(library(dplyr))

answers <- read.csv(file="answers2.csv",
                        header=TRUE, sep=",",
                        stringsAsFactors=FALSE)

asd <- answers %>% as_tibble() %>% mutate(
     are.you.a.graduate.student = grepl("Graduate student", Please.select.any.categories.that.best.describe.your.current.position., fixed = TRUE),
     are.you.an.undergrad.student = grepl("Undergraduate student", Please.select.any.categories.that.best.describe.your.current.position., fixed = TRUE),
     are.you.a.teacher = grepl("Undergraduate student", Please.select.any.categories.that.best.describe.your.current.position., fixed = TRUE),
     are.you.an.industry.professional = grepl("Industry professional", Please.select.any.categories.that.best.describe.your.current.position., fixed = TRUE),
             )

