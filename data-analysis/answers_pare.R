suppressMessages(library(dplyr))
library(tidyr)
library(gmodels)
library(tables)

# we don't read answers with factors directly because I had some issues later on with multiple answers analysis,
# and we want to rename the columns, by the way
answers <- read.csv(file="misaligned-20180720-inferred-categories.csv",
                        header=TRUE, sep=",",
                        stringsAsFactors=FALSE)

# for this part of the analysis, we ignore the 'Other' answers, as well as the open ones.
# in this part of the analysis, we:
# - convert multiple answers (checkbox style) to yes/no answers, for the sake of analysis and proper correlation
# - pick better names for most columns, and factorize them
normalized_wide <- answers %>% as_tibble() %>% transmute(
     age.group = as.factor(What.s.your.age.),
     degree.highest = as.factor(What.is.the.highest.degree.you.earned.),
     degree.country = as.factor(If.you.hold.a.university.degree..what.country.you.received.your.degree.in.),
     employed.country = as.factor(If.you.are.employed..in.which.country.do.you.work.in.),
     company.size = as.factor(If.you.are.employed..what.is.the.size.of.employees.in.the.company.s.tech.department...if.mostly.a.tech.company..just.state.company.size.),
     are.you.a.graduate.student = grepl("Graduate student", Please.select.any.categories.that.best.describe.your.current.position., fixed = TRUE),
     are.you.an.undergrad.student = grepl("Undergraduate student", Please.select.any.categories.that.best.describe.your.current.position., fixed = TRUE),
     are.you.a.teacher = grepl("teacher", Please.select.any.categories.that.best.describe.your.current.position., fixed = TRUE),
     are.you.an.industry.professional = grepl("Industry professional", Please.select.any.categories.that.best.describe.your.current.position., fixed = TRUE),
     bsc.achieves.programming = grepl("He/She learns programming very well", Currently..what.do.you.think.an.UNDERGRADUATE.student.in.CS.or.SE.achieves..when.he.she.graduates., fixed = TRUE),
     bsc.achieves.computational = grepl("He/She learns computational thinking", Currently..what.do.you.think.an.UNDERGRADUATE.student.in.CS.or.SE.achieves..when.he.she.graduates., fixed = TRUE),
     bsc.achieves.projectmanagement = grepl("He/She learns project management", Currently..what.do.you.think.an.UNDERGRADUATE.student.in.CS.or.SE.achieves..when.he.she.graduates., fixed = TRUE),
     bsc.achieves.realworldproblemsolving = grepl("He/She learns real-world problem solving handling industry-type problems", Currently..what.do.you.think.an.UNDERGRADUATE.student.in.CS.or.SE.achieves..when.he.she.graduates., fixed = TRUE),
     bsc.achieves.research = grepl("He/She learns how to be a researcher", Currently..what.do.you.think.an.UNDERGRADUATE.student.in.CS.or.SE.achieves..when.he.she.graduates., fixed = TRUE),
     bsc.achieves.softskills = grepl("He/She gets very good soft skills (communication, teamwork, etc)", Currently..what.do.you.think.an.UNDERGRADUATE.student.in.CS.or.SE.achieves..when.he.she.graduates., fixed = TRUE),
     bsc.achieves.hireability = grepl("He/She gets credentials to get a good job", Currently..what.do.you.think.an.UNDERGRADUATE.student.in.CS.or.SE.achieves..when.he.she.graduates., fixed = TRUE),
     bsc.achieves.dontknow = grepl("I don't know/prefer not to disclose", Currently..what.do.you.think.an.UNDERGRADUATE.student.in.CS.or.SE.achieves..when.he.she.graduates., fixed = TRUE),
     bsc.shouldachieve.programming = grepl("Programming in different languages", Are.there.any.skills..that.you.think.an.UNDERGRADUATE.student.would.need.to.learn.at.university..where.it.is.not.taught.by.universities..and.are.essential.in.working.industry., fixed = TRUE),
     bsc.shouldachieve.computational = grepl("Computational Thinking skill", Are.there.any.skills..that.you.think.an.UNDERGRADUATE.student.would.need.to.learn.at.university..where.it.is.not.taught.by.universities..and.are.essential.in.working.industry., fixed = TRUE),
     bsc.shouldachieve.projectmanagement = grepl("Project Management skill", Are.there.any.skills..that.you.think.an.UNDERGRADUATE.student.would.need.to.learn.at.university..where.it.is.not.taught.by.universities..and.are.essential.in.working.industry., fixed = TRUE),
     bsc.shouldachieve.realworldproblemsolving = grepl("Real-world problem solving handling industry-type problems", Are.there.any.skills..that.you.think.an.UNDERGRADUATE.student.would.need.to.learn.at.university..where.it.is.not.taught.by.universities..and.are.essential.in.working.industry., fixed = TRUE),
     bsc.shouldachieve.research = grepl("Learn how to be a researcher", Are.there.any.skills..that.you.think.an.UNDERGRADUATE.student.would.need.to.learn.at.university..where.it.is.not.taught.by.universities..and.are.essential.in.working.industry., fixed = TRUE),
     bsc.shouldachieve.softskills = grepl("Soft skills (communication, teamwork, etc)", Are.there.any.skills..that.you.think.an.UNDERGRADUATE.student.would.need.to.learn.at.university..where.it.is.not.taught.by.universities..and.are.essential.in.working.industry., fixed = TRUE),
     bsc.shouldachieve.hireability = grepl("Get credentials and learn skills in finding a good job", Are.there.any.skills..that.you.think.an.UNDERGRADUATE.student.would.need.to.learn.at.university..where.it.is.not.taught.by.universities..and.are.essential.in.working.industry., fixed = TRUE),
     bsc.shouldachieve.dontknow = grepl("I don't know/prefer not to disclose", Are.there.any.skills..that.you.think.an.UNDERGRADUATE.student.would.need.to.learn.at.university..where.it.is.not.taught.by.universities..and.are.essential.in.working.industry., fixed = TRUE),
     
     bsc.hireability.gpa = as.factor(How.do.you.think.the.GPA.affects.a.fresh.BS.graduate.s.chances.to.get.hired.),
     bsc.proficiency.gpa = as.factor(How.do.you.think.the.GPA.affects.a.fresh.BS.graduate.s.professional.proficiency.),
     bsc.proficiency.topschool = as.factor(How.do.you.think.the.school.choice..e.g..Top.10.Ivy.League.university.vs.random.college..affects.a.fresh.BS.graduate.s.professional.proficiency.),
     bsc.landjob.delay = as.factor(How.long.do.you.think.it.will.take.for.a.fresh.BS.graduate.to.land.his.her.first.job..after.graduation.),
     bsc.proficiency.delay = as.factor(How.long.do.you.think.it.will.take.for.a.fresh.BS.graduate.to.become.fully.proficient.at.his.her.first.job..after.being.hired.),
     
    
     msc.achieves.programming = grepl("He/She learns programming very well", Currently..what.do.you.think.a.GRADUATE.student..for.a.MS.program..in.CS.or.SE.achieves..when.he.she.graduates., fixed = TRUE),
     msc.achieves.computational = grepl("He/She learns computational thinking", Currently..what.do.you.think.a.GRADUATE.student..for.a.MS.program..in.CS.or.SE.achieves..when.he.she.graduates., fixed = TRUE),
     msc.achieves.projectmanagement = grepl("He/She learns project management", Currently..what.do.you.think.a.GRADUATE.student..for.a.MS.program..in.CS.or.SE.achieves..when.he.she.graduates., fixed = TRUE),
     msc.achieves.realworldproblemsolving = grepl("He/She learns real-world problem solving handling industry-type problems", Currently..what.do.you.think.a.GRADUATE.student..for.a.MS.program..in.CS.or.SE.achieves..when.he.she.graduates., fixed = TRUE),
     msc.achieves.research = grepl("He/She learns how to be a researcher", Currently..what.do.you.think.a.GRADUATE.student..for.a.MS.program..in.CS.or.SE.achieves..when.he.she.graduates., fixed = TRUE),
     msc.achieves.softskills = grepl("He/She gets very good soft skills (communication, teamwork, etc)", Currently..what.do.you.think.a.GRADUATE.student..for.a.MS.program..in.CS.or.SE.achieves..when.he.she.graduates., fixed = TRUE),
     msc.achieves.hireability = grepl("He/She gets credentials to get a good job", Currently..what.do.you.think.a.GRADUATE.student..for.a.MS.program..in.CS.or.SE.achieves..when.he.she.graduates., fixed = TRUE),
     msc.achieves.dontknow = grepl("I don't know/prefer not to disclose", Currently..what.do.you.think.a.GRADUATE.student..for.a.MS.program..in.CS.or.SE.achieves..when.he.she.graduates., fixed = TRUE),
     msc.shouldachieve.programming = grepl("Programming in different languages", Are.there.any.skills..that.you.think.a.GRADUATE.student..for.a.MS.program..would.need.to.learn.at.university..that.it.is.not.taught.by.universities..and.are.essential.in.working.industry., fixed = TRUE),
     msc.shouldachieve.computational = grepl("Computational Thinking skill", Are.there.any.skills..that.you.think.a.GRADUATE.student..for.a.MS.program..would.need.to.learn.at.university..that.it.is.not.taught.by.universities..and.are.essential.in.working.industry., fixed = TRUE),
     msc.shouldachieve.projectmanagement = grepl("Project Management skill", Are.there.any.skills..that.you.think.a.GRADUATE.student..for.a.MS.program..would.need.to.learn.at.university..that.it.is.not.taught.by.universities..and.are.essential.in.working.industry., fixed = TRUE),
     msc.shouldachieve.realworldproblemsolving = grepl("Real-world problem solving handling industry-type problems", Are.there.any.skills..that.you.think.a.GRADUATE.student..for.a.MS.program..would.need.to.learn.at.university..that.it.is.not.taught.by.universities..and.are.essential.in.working.industry., fixed = TRUE),
     msc.shouldachieve.research = grepl("Learn how to be a researcher", Are.there.any.skills..that.you.think.a.GRADUATE.student..for.a.MS.program..would.need.to.learn.at.university..that.it.is.not.taught.by.universities..and.are.essential.in.working.industry., fixed = TRUE),
     msc.shouldachieve.softskills = grepl("Soft skills (communication, teamwork, etc)", Are.there.any.skills..that.you.think.a.GRADUATE.student..for.a.MS.program..would.need.to.learn.at.university..that.it.is.not.taught.by.universities..and.are.essential.in.working.industry., fixed = TRUE),
     msc.shouldachieve.hireability = grepl("Get the credentials and learn skills in finding a good job", Are.there.any.skills..that.you.think.a.GRADUATE.student..for.a.MS.program..would.need.to.learn.at.university..that.it.is.not.taught.by.universities..and.are.essential.in.working.industry., fixed = TRUE),
     msc.shouldachieve.dontknow = grepl("I don't know/prefer not to disclose", Are.there.any.skills..that.you.think.a.GRADUATE.student..for.a.MS.program..would.need.to.learn.at.university..that.it.is.not.taught.by.universities..and.are.essential.in.working.industry., fixed = TRUE),
     
     msc.hireability.gpa = as.factor(How.do.you.think.the.GPA.of.a.GRADUATE.student..for.a.MS.program..affects.a.fresh.graduate.to.get.hired.),
     msc.landjob.delay = as.factor(How.long.do.you.think.a.GRADUATE.student..for.a.MS.program..will.need.in.order.to.land.his.her.first.job..after.graduation.),
     msc.proficiency.delay = as.factor(How.long.do.you.think.a.GRADUATE.student..for.a.MS.program..will.need.in.order.to.become.fully.proficient.at.his.her.first.job..after.being.hired.),
     
     bsc.vs.jobexperience.hireability = as.factor(Consider.two.candidates.for.a.same.job..One.holds.a.4.year.BS.degree.and.has.no.job.experience..The.other.has.no.degree..but.has.4.years.of.job.experience.in.a.similar.role..What.do.you.think.about.the.candidates..chance.of.being.hired.),
     bsc.vs.jobexperience.shorttermproficiency = as.factor(Consider.two.fresh.hires.for.the.same.position.at.the.same.company..One.holds.a.4.year.BS.degree.and.no.job.experience..The.other.has.no.degree..but.has.4.years.of.job.experience.in.a.similar.role..What.do.you.think.about.the.candidates..skills.and.performance.RIGHT.AFTER.BEING.HIRED.),
     bsc.vs.jobexperience.longtermproficiency = as.factor(Consider.two.fresh.hires.for.the.same.position.at.the.same.company..One.holds.a.4.year.BS.degree.and.no.job.experience..The.other.has.no.degree..but.has.4.years.of.job.experience.in.a.similar.role..They.work.at.the.company..in.the.same.role..for.one.year..What.do.you.think.about.the.candidates..skills.and.career.at.that.time..after.1.year..),
     msc.vs.bscexperience.hireability = as.factor(Consider.two.candidates.for.the.same.job..One.holds.a.relevant.MS.degree.and.no.job.experience..The.other.has.a.BS.and.2.years.of.relevant.job.experience..What.do.you.think.about.the.candidates..chance.of.being.hired.),
     msc.vs.bscexperience.shorttermproficiency = as.factor(Consider.two.fresh.hires.for.the.same.position.at.the.same.company..One.holds.a.relevant.MS.degree.and.no.job.experience..The.other.has.a.BS.degree..and.a.couple.of.years.of.experience.in.a.similar.role..What.do.you.think.about.the.candidates..skills.and.performance.RIGHT.AFTER.BEING.HIRED.),
     msc.vs.bscexperience.longtermproficiency = as.factor(Consider.two.fresh.hires.for.the.same.role.at.the.same.company..One.holds.an.MS.degree.and.no.job.experience..The.other.has.a.BS.degree..and.2.years.of.job.experience.in.a.similar.role..They.work.at.the.company..in.the.same.role..for.one.year..What.do.you.think.about.the.candidates..skills.and.career.at.that.time..after.1.year..),
     retraining.alt.onthejob = grepl("Retraining on the job.", In.your.opinion..what.could.be.valid.alternatives.to.a.Master.s.degree.for.a.professional.in.need.of.retraining., fixed = TRUE),
     retraining.alt.mooc = grepl("Massive Open Online Courses (MOOCs)", In.your.opinion..what.could.be.valid.alternatives.to.a.Master.s.degree.for.a.professional.in.need.of.retraining., fixed = TRUE),
     retraining.alt.inperson = grepl("In-person classes in non-academic institutions (e.g. Bradfield school of computer science)", In.your.opinion..what.could.be.valid.alternatives.to.a.Master.s.degree.for.a.professional.in.need.of.retraining., fixed = TRUE),
     retraining.alt.dontknow = grepl("I don't know/prefer not to disclose", In.your.opinion..what.could.be.valid.alternatives.to.a.Master.s.degree.for.a.professional.in.need.of.retraining., fixed = TRUE),
     grad.online.benefits.time = grepl("Time. The online program gives the schedule flexibility.", In.your.opinion..what.would.be.the.benefits.of.joining.a..graduate.level.online..program., fixed = TRUE),
     grad.online.benefits.money = grepl("Money. The online program mostly are less expensive than on-campus programs", In.your.opinion..what.would.be.the.benefits.of.joining.a..graduate.level.online..program., fixed = TRUE),
     grad.online.benefits.motivation = grepl("Motivation. The topic should be of high interest, not just an imposed training experience.", In.your.opinion..what.would.be.the.benefits.of.joining.a..graduate.level.online..program., fixed = TRUE),
     grad.online.benefits.career = grepl("Career advancement. Success in the program should be a guarantee of a better position.", In.your.opinion..what.would.be.the.benefits.of.joining.a..graduate.level.online..program., fixed = TRUE),
     grad.online.benefits.dontknow = grepl("I don't know/prefer not to disclose", In.your.opinion..what.would.be.the.benefits.of.joining.a..graduate.level.online..program., fixed = TRUE),
     nojobexperience.opinion = as.factor(In.general..which.statement..in.your.opinion..is.more.accurate..if.we.assume.that.below.groups.have.no.previous.job.experience..)
             )

#add explicit row id, needed lather
ids <- rownames(normalized_wide)
normalized_wide <- cbind(id=ids, normalized_wide)

# now, each column is either a bool or a factor.
normalized_long <- gather(normalized_wide, question, measurement, age.group:nojobexperience.opinion,factor_key=TRUE)


 
tab_categories <- function(independent, dependent, mutate_what) {
mytbl <- table(normalized_wide %>% select(matches(independent)))
 p <- tabular(as.formula(sprintf("%s~%s", independent, dependent))
              , normalized_wide)
 
 q <- data.frame(matrix(p, nrow=nrow(p))) %>% mutate_all(as.integer)
 rownames(q) <- rowLabels(p)
 colnames(q) <- colLabels(p)[1,]
 q <- tibble::rownames_to_column(q, var=independent)
 q <- q %>% mutate(total=mytbl[!!sym(independent)])
 return (q %>% mutate_at(vars(contains(mutate_what)), .funs = funs(pc = round(./total*100, 1))))
}
 
tab_boolean <- function(independent, dependent, mutate_what) {

  p <- tabular(as.formula(sprintf("%s~%s", independent, dependent))
               , normalized_wide)
  
  q <- data.frame(matrix(p, nrow=nrow(p))) %>% mutate_all(as.integer)
 
  colnames(q) <- colLabels(p)[1,]
  #q <- tibble::rownames_to_column(q, var=independent)
  q <- q %>% mutate(total=nrow(normalized_wide %>% filter(!!sym(independent) == TRUE)))
  q <- q %>% mutate_at(vars(contains(mutate_what)), .funs = funs(pc = round(./total*100, 1)))
 rownames(q) <- c(independent)
  return (q)

}

tab_msq <- function(independent, dependent, mutate_what) {
  mytbl <- table(normalized_wide %>% select(matches(independent)))
  p <- tabular(as.formula(sprintf("%s~%s", independent, dependent))
               , normalized_wide)
  
  q <- data.frame(matrix(p, nrow=nrow(p))) %>% mutate_all(as.integer)
  
  colnames(q) <- colLabels(p)[2,]
  rownames(q) <- rowLabels(p)
  q <- tibble::rownames_to_column(q, var=independent)
  q <- q %>% mutate(total=mytbl[!!sym(independent)])
  q <- q %>% mutate_at(vars(-contains(independent),-contains("total")), .funs = funs(pc = round(./total*100, 1)))
  #rownames(q) <- c(independent)
  return (q)
  
}




age.group.bsc.achieves.table <- tab_categories("age.group", "bsc.achieves.programming + bsc.achieves.computational + bsc.achieves.projectmanagement + bsc.achieves.realworldproblemsolving + bsc.achieves.research + bsc.achieves.softskills + bsc.achieves.hireability + bsc.achieves.dontknow", "bsc")
age.group.bsc.shouldachieve.table <- tab_categories("age.group", "bsc.shouldachieve.programming + bsc.shouldachieve.computational + bsc.shouldachieve.projectmanagement + bsc.shouldachieve.realworldproblemsolving + bsc.shouldachieve.research + bsc.shouldachieve.softskills + bsc.shouldachieve.hireability + bsc.shouldachieve.dontknow", "bsc")

age.group.msc.achieves.table <- tab_categories("age.group", "msc.achieves.programming + msc.achieves.computational + msc.achieves.projectmanagement + msc.achieves.realworldproblemsolving + msc.achieves.research + msc.achieves.softskills + msc.achieves.hireability + msc.achieves.dontknow", "msc")
age.group.msc.shouldachieve.table <- tab_categories("age.group", "msc.shouldachieve.programming + msc.shouldachieve.computational + msc.shouldachieve.projectmanagement + msc.shouldachieve.realworldproblemsolving + msc.shouldachieve.research + msc.shouldachieve.softskills + msc.shouldachieve.hireability + msc.shouldachieve.dontknow", "msc")
 
degree.highest.bsc.achieves.table <- tab_categories("degree.highest", "bsc.achieves.programming + bsc.achieves.computational + bsc.achieves.projectmanagement + bsc.achieves.realworldproblemsolving + bsc.achieves.research + bsc.achieves.softskills + bsc.achieves.hireability + bsc.achieves.dontknow", "bsc")
degree.highest.bsc.shouldachieve.table <- tab_categories("degree.highest", "bsc.shouldachieve.programming + bsc.shouldachieve.computational + bsc.shouldachieve.projectmanagement + bsc.shouldachieve.realworldproblemsolving + bsc.shouldachieve.research + bsc.shouldachieve.softskills + bsc.shouldachieve.hireability + bsc.shouldachieve.dontknow", "bsc")

degree.highest.msc.achieves.table <- tab_categories("degree.highest", "msc.achieves.programming + msc.achieves.computational + msc.achieves.projectmanagement + msc.achieves.realworldproblemsolving + msc.achieves.research + msc.achieves.softskills + msc.achieves.hireability + msc.achieves.dontknow", "msc")
degree.highest.msc.shouldachieve.table <- tab_categories("degree.highest", "msc.shouldachieve.programming + msc.shouldachieve.computational + msc.shouldachieve.projectmanagement + msc.shouldachieve.realworldproblemsolving + msc.shouldachieve.research + msc.shouldachieve.softskills + msc.shouldachieve.hireability + msc.shouldachieve.dontknow", "msc")

degree.country.bsc.achieves.table <- tab_categories("degree.country", "bsc.achieves.programming + bsc.achieves.computational + bsc.achieves.projectmanagement + bsc.achieves.realworldproblemsolving + bsc.achieves.research + bsc.achieves.softskills + bsc.achieves.hireability + bsc.achieves.dontknow", "bsc")
degree.country.bsc.shouldachieve.table <- tab_categories("degree.country", "bsc.shouldachieve.programming + bsc.shouldachieve.computational + bsc.shouldachieve.projectmanagement + bsc.shouldachieve.realworldproblemsolving + bsc.shouldachieve.research + bsc.shouldachieve.softskills + bsc.shouldachieve.hireability + bsc.shouldachieve.dontknow", "bsc")

degree.country.msc.achieves.table <- tab_categories("degree.country", "msc.achieves.programming + msc.achieves.computational + msc.achieves.projectmanagement + msc.achieves.realworldproblemsolving + msc.achieves.research + msc.achieves.softskills + msc.achieves.hireability + msc.achieves.dontknow", "msc")
degree.country.msc.shouldachieve.table <- tab_categories("degree.country", "msc.shouldachieve.programming + msc.shouldachieve.computational + msc.shouldachieve.projectmanagement + msc.shouldachieve.realworldproblemsolving + msc.shouldachieve.research + msc.shouldachieve.softskills + msc.shouldachieve.hireability + msc.shouldachieve.dontknow", "msc")


employed.country.bsc.achieves.table <- tab_categories("employed.country", "bsc.achieves.programming + bsc.achieves.computational + bsc.achieves.projectmanagement + bsc.achieves.realworldproblemsolving + bsc.achieves.research + bsc.achieves.softskills + bsc.achieves.hireability + bsc.achieves.dontknow", "bsc")
employed.country.bsc.shouldachieve.table <- tab_categories("employed.country", "bsc.shouldachieve.programming + bsc.shouldachieve.computational + bsc.shouldachieve.projectmanagement + bsc.shouldachieve.realworldproblemsolving + bsc.shouldachieve.research + bsc.shouldachieve.softskills + bsc.shouldachieve.hireability + bsc.shouldachieve.dontknow", "bsc")

employed.country.msc.achieves.table <- tab_categories("employed.country", "msc.achieves.programming + msc.achieves.computational + msc.achieves.projectmanagement + msc.achieves.realworldproblemsolving + msc.achieves.research + msc.achieves.softskills + msc.achieves.hireability + msc.achieves.dontknow", "msc")
employed.country.msc.shouldachieve.table <- tab_categories("employed.country", "msc.shouldachieve.programming + msc.shouldachieve.computational + msc.shouldachieve.projectmanagement + msc.shouldachieve.realworldproblemsolving + msc.shouldachieve.research + msc.shouldachieve.softskills + msc.shouldachieve.hireability + msc.shouldachieve.dontknow", "msc")

company.size.bsc.achieves.table <- tab_categories("company.size", "bsc.achieves.programming + bsc.achieves.computational + bsc.achieves.projectmanagement + bsc.achieves.realworldproblemsolving + bsc.achieves.research + bsc.achieves.softskills + bsc.achieves.hireability + bsc.achieves.dontknow", "bsc")
company.size.bsc.shouldachieve.table <- tab_categories("company.size", "bsc.shouldachieve.programming + bsc.shouldachieve.computational + bsc.shouldachieve.projectmanagement + bsc.shouldachieve.realworldproblemsolving + bsc.shouldachieve.research + bsc.shouldachieve.softskills + bsc.shouldachieve.hireability + bsc.shouldachieve.dontknow", "bsc")

company.size.msc.achieves.table <- tab_categories("company.size", "msc.achieves.programming + msc.achieves.computational + msc.achieves.projectmanagement + msc.achieves.realworldproblemsolving + msc.achieves.research + msc.achieves.softskills + msc.achieves.hireability + msc.achieves.dontknow", "msc")
company.size.msc.shouldachieve.table <- tab_categories("company.size", "msc.shouldachieve.programming + msc.shouldachieve.computational + msc.shouldachieve.projectmanagement + msc.shouldachieve.realworldproblemsolving + msc.shouldachieve.research + msc.shouldachieve.softskills + msc.shouldachieve.hireability + msc.shouldachieve.dontknow", "msc")

is.undergraduate.student.bsc.achieves.table <- tab_boolean("are.you.an.undergrad.student", "bsc.achieves.programming + bsc.achieves.computational + bsc.achieves.projectmanagement + bsc.achieves.realworldproblemsolving + bsc.achieves.research + bsc.achieves.softskills + bsc.achieves.hireability + bsc.achieves.dontknow", "bsc")
is.graduate.student.bsc.achieves.table <- tab_boolean("are.you.a.graduate.student", "bsc.achieves.programming + bsc.achieves.computational + bsc.achieves.projectmanagement + bsc.achieves.realworldproblemsolving + bsc.achieves.research + bsc.achieves.softskills + bsc.achieves.hireability + bsc.achieves.dontknow", "bsc")
teacher.bsc.achieves.table <- tab_boolean("are.you.a.teacher", "bsc.achieves.programming + bsc.achieves.computational + bsc.achieves.projectmanagement + bsc.achieves.realworldproblemsolving + bsc.achieves.research + bsc.achieves.softskills + bsc.achieves.hireability + bsc.achieves.dontknow", "bsc")
industry.professional.bsc.achieves.table <- tab_boolean("are.you.an.industry.professional", "bsc.achieves.programming + bsc.achieves.computational + bsc.achieves.projectmanagement + bsc.achieves.realworldproblemsolving + bsc.achieves.research + bsc.achieves.softskills + bsc.achieves.hireability + bsc.achieves.dontknow", "bsc")

categories.bsc.achieves.table <- rbind(is.undergraduate.student.bsc.achieves.table, is.graduate.student.bsc.achieves.table, teacher.bsc.achieves.table, industry.professional.bsc.achieves.table)

is.undergraduate.student.bsc.shouldachieve.table <- tab_boolean("are.you.an.undergrad.student", "bsc.shouldachieve.programming + bsc.shouldachieve.computational + bsc.shouldachieve.projectmanagement + bsc.shouldachieve.realworldproblemsolving + bsc.shouldachieve.research + bsc.shouldachieve.softskills + bsc.shouldachieve.hireability + bsc.shouldachieve.dontknow", "bsc")
is.graduate.student.bsc.shouldachieve.table <- tab_boolean("are.you.a.graduate.student", "bsc.shouldachieve.programming + bsc.shouldachieve.computational + bsc.shouldachieve.projectmanagement + bsc.shouldachieve.realworldproblemsolving + bsc.shouldachieve.research + bsc.shouldachieve.softskills + bsc.shouldachieve.hireability + bsc.shouldachieve.dontknow", "bsc")
teacher.bsc.shouldachieve.table <- tab_boolean("are.you.a.teacher", "bsc.shouldachieve.programming + bsc.shouldachieve.computational + bsc.shouldachieve.projectmanagement + bsc.shouldachieve.realworldproblemsolving + bsc.shouldachieve.research + bsc.shouldachieve.softskills + bsc.shouldachieve.hireability + bsc.shouldachieve.dontknow", "bsc")
industry.professional.bsc.shouldachieve.table <- tab_boolean("are.you.an.industry.professional", "bsc.shouldachieve.programming + bsc.shouldachieve.computational + bsc.shouldachieve.projectmanagement + bsc.shouldachieve.realworldproblemsolving + bsc.shouldachieve.research + bsc.shouldachieve.softskills + bsc.shouldachieve.hireability + bsc.shouldachieve.dontknow", "bsc")

categories.bsc.shouldachieve.table <- rbind(is.undergraduate.student.bsc.shouldachieve.table, is.graduate.student.bsc.shouldachieve.table, teacher.bsc.shouldachieve.table, industry.professional.bsc.shouldachieve.table)

is.undergraduate.student.msc.achieves.table <- tab_boolean("are.you.an.undergrad.student", "msc.achieves.programming + msc.achieves.computational + msc.achieves.projectmanagement + msc.achieves.realworldproblemsolving + msc.achieves.research + msc.achieves.softskills + msc.achieves.hireability + msc.achieves.dontknow", "msc")
is.graduate.student.msc.achieves.table <- tab_boolean("are.you.a.graduate.student", "msc.achieves.programming + msc.achieves.computational + msc.achieves.projectmanagement + msc.achieves.realworldproblemsolving + msc.achieves.research + msc.achieves.softskills + msc.achieves.hireability + msc.achieves.dontknow", "msc")
teacher.msc.achieves.table <- tab_boolean("are.you.a.teacher", "msc.achieves.programming + msc.achieves.computational + msc.achieves.projectmanagement + msc.achieves.realworldproblemsolving + msc.achieves.research + msc.achieves.softskills + msc.achieves.hireability + msc.achieves.dontknow", "msc")
industry.professional.msc.achieves.table <- tab_boolean("are.you.an.industry.professional", "msc.achieves.programming + msc.achieves.computational + msc.achieves.projectmanagement + msc.achieves.realworldproblemsolving + msc.achieves.research + msc.achieves.softskills + msc.achieves.hireability + msc.achieves.dontknow", "msc")

categories.msc.achieves.table <- rbind(is.undergraduate.student.msc.achieves.table, is.graduate.student.msc.achieves.table, teacher.msc.achieves.table, industry.professional.msc.achieves.table)

is.undergraduate.student.msc.shouldachieve.table <- tab_boolean("are.you.an.undergrad.student", "msc.shouldachieve.programming + msc.shouldachieve.computational + msc.shouldachieve.projectmanagement + msc.shouldachieve.realworldproblemsolving + msc.shouldachieve.research + msc.shouldachieve.softskills + msc.shouldachieve.hireability + msc.shouldachieve.dontknow", "msc")
is.graduate.student.msc.shouldachieve.table <- tab_boolean("are.you.a.graduate.student", "msc.shouldachieve.programming + msc.shouldachieve.computational + msc.shouldachieve.projectmanagement + msc.shouldachieve.realworldproblemsolving + msc.shouldachieve.research + msc.shouldachieve.softskills + msc.shouldachieve.hireability + msc.shouldachieve.dontknow", "msc")
teacher.msc.shouldachieve.table <- tab_boolean("are.you.a.teacher", "msc.shouldachieve.programming + msc.shouldachieve.computational + msc.shouldachieve.projectmanagement + msc.shouldachieve.realworldproblemsolving + msc.shouldachieve.research + msc.shouldachieve.softskills + msc.shouldachieve.hireability + msc.shouldachieve.dontknow", "msc")
industry.professional.msc.shouldachieve.table <- tab_boolean("are.you.an.industry.professional", "msc.shouldachieve.programming + msc.shouldachieve.computational + msc.shouldachieve.projectmanagement + msc.shouldachieve.realworldproblemsolving + msc.shouldachieve.research + msc.shouldachieve.softskills + msc.shouldachieve.hireability + msc.shouldachieve.dontknow", "msc")

categories.msc.shouldachieve.table <- rbind(is.undergraduate.student.msc.shouldachieve.table, is.graduate.student.msc.shouldachieve.table, teacher.msc.shouldachieve.table, industry.professional.msc.shouldachieve.table)

#bsc.hireability.gpa = as.factor(How.do.you.think.the.GPA.affects.a.fresh.BS.graduate.s.chances.to.get.hired.),
#bsc.proficiency.gpa = as.factor(How.do.you.think.the.GPA.affects.a.fresh.BS.graduate.s.professional.proficiency.),
#bsc.proficiency.topschool = as.factor(How.do.you.think.the.school.choice..e.g..Top.10.Ivy.League.university.vs.random.college..affects.a.fresh.BS.graduate.s.professional.proficiency.),
#bsc.landjob.delay = as.factor(How.long.do.you.think.it.will.take.for.a.fresh.BS.graduate.to.land.his.her.first.job..after.graduation.),
#bsc.proficiency.delay = as.factor(How.long.do.you.think.it.will.take.fwor.a.fresh.BS.graduate.to.become.fully.proficient.at.his.her.first.job..after.being.hired.),

#age.group.bsc.hireability = tab_categories("age.group", "bsc.hireability.gpa + bsc.proficiency.gpa + bsc.proficiency.topschool + bsc.landjob.delay + bsc.proficiency.delay", "bsc")
age.group.bsc.hireability <- tab_msq("age.group", "bsc.hireability.gpa", "bsc")



 #explore
 #age.group.msc.achieves.table %>% select("age.group", "total", contains("_pc"), )