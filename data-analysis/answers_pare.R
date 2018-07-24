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

tab_boolean_categories <- function(dependent, mutate_what) {
 gradstudent <- tab_boolean("are.you.a.graduate.student", dependent, mutate_what)
 undergrad <- tab_boolean("are.you.an.undergrad.student", dependent, mutate_what)
 teacher <- tab_boolean("are.you.a.teacher", dependent, mutate_what)
 industry.professional <- tab_boolean("are.you.an.industry.professional", dependent, mutate_what)

 return (rbind(undergrad, gradstudent, teacher, industry.professional))
}

tab_msq <- function(independent, dependent) {
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

tab_msq_no_index <- function(independent, dependent) {
  mytbl <- table(normalized_wide %>% select(matches(independent)))
  p <- tabular(as.formula(sprintf("%s~%s", independent, dependent))
               , normalized_wide)
  
  q <- data.frame(matrix(p, nrow=nrow(p))) %>% mutate_all(as.integer)
  
  colnames(q) <- colLabels(p)[2,]
  rownames(q) <- rowLabels(p)
  q <- tibble::rownames_to_column(q, var=independent)
  q <- q %>% mutate(total=nrow(normalized_wide %>% filter(!!sym(independent) == TRUE)))
  q <- q %>% mutate_at(vars(-contains(independent),-contains("total")), .funs = funs(pc = round(./total*100, 1)))
  q <- q %>% select(-contains(independent))
  rownames(q) <- rowLabels(p)
  #rownames(q) <- c(independent)
  return (q)
  
}

tab_msq_no_index_categories <- function(dependent) {
 gradstudent <- tab_msq_no_index("are.you.a.graduate.student", dependent)
 undergrad <- tab_msq_no_index("are.you.an.undergrad.student", dependent)
 teacher <- tab_msq_no_index("are.you.a.teacher", dependent)
 industry.professional <- tab_msq_no_index("are.you.an.industry.professional", dependent)

 return (rbind(undergrad, gradstudent, teacher, industry.professional))
 
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
age.group.bsc.hireability.gpa.table <- tab_msq("age.group", "bsc.hireability.gpa")
age.group.bsc.proficiency.gpa.table <- tab_msq("age.group", "bsc.proficiency.gpa")
age.group.bsc.proficiency.topschool.table <- tab_msq("age.group", "bsc.proficiency.topschool")
age.group.bsc.landjob.delay.table <- tab_msq("age.group", "bsc.landjob.delay")
age.group.bsc.proficiency.delay.table <- tab_msq("age.group", "bsc.proficiency.delay")

degree.highest.bsc.hireability.gpa.table <- tab_msq("degree.highest", "bsc.hireability.gpa")
degree.highest.bsc.proficiency.gpa.table <- tab_msq("degree.highest", "bsc.proficiency.gpa")
degree.highest.bsc.proficiency.topschool.table <- tab_msq("degree.highest", "bsc.proficiency.topschool")
degree.highest.bsc.landjob.delay.table <- tab_msq("degree.highest", "bsc.landjob.delay")
degree.highest.bsc.proficiency.delay.table <- tab_msq("degree.highest", "bsc.proficiency.delay")

degree.country.bsc.hireability.gpa.table <- tab_msq("degree.country", "bsc.hireability.gpa")
degree.country.bsc.proficiency.gpa.table <- tab_msq("degree.country", "bsc.proficiency.gpa")
degree.country.bsc.proficiency.topschool.table <- tab_msq("degree.country", "bsc.proficiency.topschool")
degree.country.bsc.landjob.delay.table <- tab_msq("degree.country", "bsc.landjob.delay")
degree.country.bsc.proficiency.delay.table <- tab_msq("degree.country", "bsc.proficiency.delay")

employed.country.bsc.hireability.gpa.table <- tab_msq("employed.country", "bsc.hireability.gpa")
employed.country.bsc.proficiency.gpa.table <- tab_msq("employed.country", "bsc.proficiency.gpa")
employed.country.bsc.proficiency.topschool.table <- tab_msq("employed.country", "bsc.proficiency.topschool")
employed.country.bsc.landjob.delay.table <- tab_msq("employed.country", "bsc.landjob.delay")
employed.country.bsc.proficiency.delay.table <- tab_msq("employed.country", "bsc.proficiency.delay")

company.size.bsc.hireability.gpa.table <- tab_msq("company.size", "bsc.hireability.gpa")
company.size.bsc.proficiency.gpa.table <- tab_msq("company.size", "bsc.proficiency.gpa")
company.size.bsc.proficiency.topschool.table <- tab_msq("company.size", "bsc.proficiency.topschool")
company.size.bsc.landjob.delay.table <- tab_msq("company.size", "bsc.landjob.delay")
company.size.bsc.proficiency.delay.table <- tab_msq("company.size", "bsc.proficiency.delay")

gradstudent.bsc.hireability.gpa.tmp <- tab_msq_no_index("are.you.a.graduate.student", "bsc.hireability.gpa")
undergrad.bsc.hireability.gpa.tmp <- tab_msq_no_index("are.you.an.undergrad.student", "bsc.hireability.gpa")
teacher.bsc.hireability.gpa.tmp <- tab_msq_no_index("are.you.a.teacher", "bsc.hireability.gpa")
industry.professional.bsc.hireability.gpa.tmp <- tab_msq_no_index("are.you.an.industry.professional", "bsc.hireability.gpa")

categories.bsc.hireability.gpa.table = rbind(undergrad.bsc.hireability.gpa.tmp, gradstudent.bsc.hireability.gpa.tmp,
                                             teacher.bsc.hireability.gpa.tmp, industry.professional.bsc.hireability.gpa.tmp)

gradstudent.bsc.proficiency.gpa.tmp <- tab_msq_no_index("are.you.a.graduate.student", "bsc.proficiency.gpa")
undergrad.bsc.proficiency.gpa.tmp <- tab_msq_no_index("are.you.an.undergrad.student", "bsc.proficiency.gpa")
teacher.bsc.proficiency.gpa.tmp <- tab_msq_no_index("are.you.a.teacher", "bsc.proficiency.gpa")
industry.professional.bsc.proficiency.gpa.tmp <- tab_msq_no_index("are.you.an.industry.professional", "bsc.proficiency.gpa")

categories.bsc.proficiency.gpa.table = rbind(undergrad.bsc.proficiency.gpa.tmp, gradstudent.bsc.proficiency.gpa.tmp,
                                             teacher.bsc.proficiency.gpa.tmp, industry.professional.bsc.proficiency.gpa.tmp)


gradstudent.bsc.proficiency.topschool.tmp <- tab_msq_no_index("are.you.a.graduate.student", "bsc.proficiency.topschool")
undergrad.bsc.proficiency.topschool.tmp <- tab_msq_no_index("are.you.an.undergrad.student", "bsc.proficiency.topschool")
teacher.bsc.proficiency.topschool.tmp <- tab_msq_no_index("are.you.a.teacher", "bsc.proficiency.topschool")
industry.professional.bsc.proficiency.topschool.tmp <- tab_msq_no_index("are.you.an.industry.professional", "bsc.proficiency.topschool")

categories.bsc.proficiency.topschool.table = rbind(undergrad.bsc.proficiency.topschool.tmp, gradstudent.bsc.proficiency.topschool.tmp,
                                             teacher.bsc.proficiency.topschool.tmp, industry.professional.bsc.proficiency.topschool.tmp)


gradstudent.bsc.landjob.delay.tmp <- tab_msq_no_index("are.you.a.graduate.student", "bsc.landjob.delay")
undergrad.bsc.landjob.delay.tmp <- tab_msq_no_index("are.you.an.undergrad.student", "bsc.landjob.delay")
teacher.bsc.landjob.delay.tmp <- tab_msq_no_index("are.you.a.teacher", "bsc.landjob.delay")
industry.professional.bsc.landjob.delay.tmp <- tab_msq_no_index("are.you.an.industry.professional", "bsc.landjob.delay")

categories.bsc.landjob.delay.table = rbind(undergrad.bsc.landjob.delay.tmp, gradstudent.bsc.landjob.delay.tmp,
                                             teacher.bsc.landjob.delay.tmp, industry.professional.bsc.landjob.delay.tmp)

gradstudent.bsc.proficiency.delay.tmp <- tab_msq_no_index("are.you.a.graduate.student", "bsc.proficiency.delay")
undergrad.bsc.proficiency.delay.tmp <- tab_msq_no_index("are.you.an.undergrad.student", "bsc.proficiency.delay")
teacher.bsc.proficiency.delay.tmp <- tab_msq_no_index("are.you.a.teacher", "bsc.proficiency.delay")
industry.professional.bsc.proficiency.delay.tmp <- tab_msq_no_index("are.you.an.industry.professional", "bsc.proficiency.delay")

categories.bsc.proficiency.delay.table = rbind(undergrad.bsc.proficiency.delay.tmp, gradstudent.bsc.proficiency.delay.tmp,
                                             teacher.bsc.proficiency.delay.tmp, industry.professional.bsc.proficiency.delay.tmp)



age.group.msc.hireability.gpa.table <- tab_msq("age.group", "msc.hireability.gpa")
age.group.msc.landjob.delay.table <- tab_msq("age.group", "msc.landjob.delay")
age.group.msc.proficiency.delay.table <- tab_msq("age.group", "msc.proficiency.delay")

degree.highest.msc.hireability.gpa.table <- tab_msq("degree.highest", "msc.hireability.gpa")
degree.highest.msc.landjob.delay.table <- tab_msq("degree.highest", "msc.landjob.delay")
degree.highest.msc.proficiency.delay.table <- tab_msq("degree.highest", "msc.proficiency.delay")

degree.country.msc.hireability.gpa.table <- tab_msq("degree.country", "msc.hireability.gpa")
degree.country.msc.landjob.delay.table <- tab_msq("degree.country", "msc.landjob.delay")
degree.country.msc.proficiency.delay.table <- tab_msq("degree.country", "msc.proficiency.delay")

employed.country.msc.hireability.gpa.table <- tab_msq("employed.country", "msc.hireability.gpa")
employed.country.msc.landjob.delay.table <- tab_msq("employed.country", "msc.landjob.delay")
employed.country.msc.proficiency.delay.table <- tab_msq("employed.country", "msc.proficiency.delay")

company.size.msc.hireability.gpa.table <- tab_msq("company.size", "msc.hireability.gpa")
company.size.msc.landjob.delay.table <- tab_msq("company.size", "msc.landjob.delay")
company.size.msc.proficiency.delay.table <- tab_msq("company.size", "msc.proficiency.delay")

gradstudent.msc.hireability.gpa.tmp <- tab_msq_no_index("are.you.a.graduate.student", "msc.hireability.gpa")
undergrad.msc.hireability.gpa.tmp <- tab_msq_no_index("are.you.an.undergrad.student", "msc.hireability.gpa")
teacher.msc.hireability.gpa.tmp <- tab_msq_no_index("are.you.a.teacher", "msc.hireability.gpa")
industry.professional.msc.hireability.gpa.tmp <- tab_msq_no_index("are.you.an.industry.professional", "msc.hireability.gpa")

categories.msc.hireability.gpa.table = rbind(undergrad.msc.hireability.gpa.tmp, gradstudent.msc.hireability.gpa.tmp,
                                             teacher.msc.hireability.gpa.tmp, industry.professional.msc.hireability.gpa.tmp)

gradstudent.msc.landjob.delay.tmp <- tab_msq_no_index("are.you.a.graduate.student", "msc.landjob.delay")
undergrad.msc.landjob.delay.tmp <- tab_msq_no_index("are.you.an.undergrad.student", "msc.landjob.delay")
teacher.msc.landjob.delay.tmp <- tab_msq_no_index("are.you.a.teacher", "msc.landjob.delay")
industry.professional.msc.landjob.delay.tmp <- tab_msq_no_index("are.you.an.industry.professional", "msc.landjob.delay")

categories.msc.landjob.delay.table = rbind(undergrad.msc.landjob.delay.tmp, gradstudent.msc.landjob.delay.tmp,
                                             teacher.msc.landjob.delay.tmp, industry.professional.msc.landjob.delay.tmp)

gradstudent.msc.proficiency.delay.tmp <- tab_msq_no_index("are.you.a.graduate.student", "msc.proficiency.delay")
undergrad.msc.proficiency.delay.tmp <- tab_msq_no_index("are.you.an.undergrad.student", "msc.proficiency.delay")
teacher.msc.proficiency.delay.tmp <- tab_msq_no_index("are.you.a.teacher", "msc.proficiency.delay")
industry.professional.msc.proficiency.delay.tmp <- tab_msq_no_index("are.you.an.industry.professional", "msc.proficiency.delay")

categories.msc.proficiency.delay.table = rbind(undergrad.msc.proficiency.delay.tmp, gradstudent.msc.proficiency.delay.tmp,
                                             teacher.msc.proficiency.delay.tmp, industry.professional.msc.proficiency.delay.tmp)


age.group.bsc.vs.jobexperience.hireability.table <- tab_msq("age.group", "bsc.vs.jobexperience.hireability")
age.group.bsc.vs.jobexperience.shorttermproficiency.table <- tab_msq("age.group", "bsc.vs.jobexperience.shorttermproficiency")
age.group.bsc.vs.jobexperience.longtermproficiency.table <- tab_msq("age.group", "bsc.vs.jobexperience.longtermproficiency")
age.group.msc.vs.bscexperience.hireability.table <- tab_msq("age.group", "msc.vs.bscexperience.hireability")
age.group.msc.vs.bscexperience.shorttermproficiency.table <- tab_msq("age.group", "msc.vs.bscexperience.shorttermproficiency")
age.group.msc.vs.bscexperience.longtermproficiency.table <- tab_msq("age.group", "msc.vs.bscexperience.longtermproficiency")

degree.highest.bsc.vs.jobexperience.hireability.table <- tab_msq("degree.highest", "bsc.vs.jobexperience.hireability")
degree.highest.bsc.vs.jobexperience.shorttermproficiency.table <- tab_msq("degree.highest", "bsc.vs.jobexperience.shorttermproficiency")
degree.highest.bsc.vs.jobexperience.longtermproficiency.table <- tab_msq("degree.highest", "bsc.vs.jobexperience.longtermproficiency")
degree.highest.msc.vs.bscexperience.hireability.table <- tab_msq("degree.highest", "msc.vs.bscexperience.hireability")
degree.highest.msc.vs.bscexperience.shorttermproficiency.table <- tab_msq("degree.highest", "msc.vs.bscexperience.shorttermproficiency")
degree.highest.bsc.vs.jobexperience.hireability.table <- tab_msq("degree.highest", "msc.vs.bscexperience.longtermproficiency")


degree.country.bsc.vs.jobexperience.hireability.table <- tab_msq("degree.country", "bsc.vs.jobexperience.hireability")
degree.country.bsc.vs.jobexperience.shorttermproficiency.table <- tab_msq("degree.country", "bsc.vs.jobexperience.shorttermproficiency")
degree.country.bsc.vs.jobexperience.longtermproficiency.table <- tab_msq("degree.country", "bsc.vs.jobexperience.longtermproficiency")
degree.country.msc.vs.bscexperience.hireability.table <- tab_msq("degree.country", "msc.vs.bscexperience.hireability")
degree.country.msc.vs.bscexperience.shorttermproficiency.table <- tab_msq("degree.country", "msc.vs.bscexperience.shorttermproficiency")
degree.country.msc.vs.bscexperience.longtermproficiency.table <- tab_msq("degree.country", "msc.vs.bscexperience.longtermproficiency")


employed.country.bsc.vs.jobexperience.hireability.table <- tab_msq("employed.country", "bsc.vs.jobexperience.hireability")
employed.country.bsc.vs.jobexperience.shorttermproficiency.table <- tab_msq("employed.country", "bsc.vs.jobexperience.shorttermproficiency")
employed.country.bsc.vs.jobexperience.longtermproficiency.table <- tab_msq("employed.country", "bsc.vs.jobexperience.longtermproficiency")
employed.country.msc.vs.bscexperience.hireability.table <- tab_msq("employed.country", "msc.vs.bscexperience.hireability")
employed.country.msc.vs.bscexperience.shorttermproficiency.table <- tab_msq("employed.country", "msc.vs.bscexperience.shorttermproficiency")
employed.country.msc.vs.bscexperience.longtermproficiency.table <- tab_msq("employed.country", "msc.vs.bscexperience.longtermproficiency")


company.size.bsc.vs.jobexperience.hireability.table <- tab_msq("company.size", "bsc.vs.jobexperience.hireability")
company.size.bsc.vs.jobexperience.shorttermproficiency.table <- tab_msq("company.size", "bsc.vs.jobexperience.shorttermproficiency")
company.size.bsc.vs.jobexperience.longtermproficiency.table <- tab_msq("company.size", "bsc.vs.jobexperience.longtermproficiency")
company.size.msc.vs.bscexperience.hireability.table <- tab_msq("company.size", "msc.vs.bscexperience.hireability")
company.size.msc.vs.bscexperience.shorttermproficiency.table <- tab_msq("company.size", "msc.vs.bscexperience.shorttermproficiency")
company.size.msc.vs.bscexperience.longtermproficiency.table <- tab_msq("company.size", "msc.vs.bscexperience.longtermproficiency")


categories.bsc.vs.jobexperience.hireability.table <- tab_msq_no_index_categories("bsc.vs.jobexperience.hireability")
categories.bsc.vs.jobexperience.shorttermproficiency.table <- tab_msq_no_index_categories("bsc.vs.jobexperience.shorttermproficiency")
categories.bsc.vs.jobexperience.longtermproficiency.table <- tab_msq_no_index_categories("bsc.vs.jobexperience.longtermproficiency")
categories.msc.vs.bscexperience.hireability.table <- tab_msq_no_index_categories("bsc.vs.jobexperience.hireability")
categories.msc.vs.bscexperience.shorttermproficiency.table <- tab_msq_no_index_categories("bsc.vs.jobexperience.shorttermproficiency")
categories.msc.vs.bscexperience.longtermproficiency.table <- tab_msq_no_index_categories("bsc.vs.jobexperience.longtermproficiency")

age.group.retraining.table <- tab_categories("age.group", "retraining.alt.onthejob + retraining.alt.mooc + retraining.alt.inperson + retraining.alt.dontknow", "retraining")
degree.country.retraining.table <- tab_categories("age.group", "retraining.alt.onthejob + retraining.alt.mooc + retraining.alt.inperson + retraining.alt.dontknow", "retraining")
degree.highest.retraining.table <- tab_categories("degree.highest", "retraining.alt.onthejob + retraining.alt.mooc + retraining.alt.inperson + retraining.alt.dontknow", "retraining")
employed.country.retraining.table <- tab_categories("employed.country", "retraining.alt.onthejob + retraining.alt.mooc + retraining.alt.inperson + retraining.alt.dontknow", "retraining")
company.size.retraining.table <- tab_categories("company.size", "retraining.alt.onthejob + retraining.alt.mooc + retraining.alt.inperson + retraining.alt.dontknow", "retraining")

categories.retraining.table <- tab_boolean_categories("retraining.alt.onthejob + retraining.alt.mooc + retraining.alt.inperson + retraining.alt.dontknow", "retraining")

age.group.grad.online.benefits.table <- tab_categories("age.group", "grad.online.benefits.time + grad.online.benefits.money + grad.online.benefits.motivation + grad.online.benefits.career + grad.online.benefits.dontknow", "benefits")
degree.country.grad.online.benefits.table <- tab_categories("degree.country", "grad.online.benefits.time + grad.online.benefits.money + grad.online.benefits.motivation + grad.online.benefits.career + grad.online.benefits.dontknow", "benefits")
degree.highest.grad.online.benefits.table <- tab_categories("degree.highest", "grad.online.benefits.time + grad.online.benefits.money + grad.online.benefits.motivation + grad.online.benefits.career + grad.online.benefits.dontknow", "benefits")
employed.country.grad.online.benefits.table <- tab_categories("employed.country", "grad.online.benefits.time + grad.online.benefits.money + grad.online.benefits.motivation + grad.online.benefits.career + grad.online.benefits.dontknow", "benefits")
company.size.grad.online.benefits.table <- tab_categories("company.size", "grad.online.benefits.time + grad.online.benefits.money + grad.online.benefits.motivation + grad.online.benefits.career + grad.online.benefits.dontknow", "benefits")


categories.grad.online.benefits.table <- tab_boolean_categories("grad.online.benefits.time + grad.online.benefits.money + grad.online.benefits.motivation + grad.online.benefits.career + grad.online.benefits.dontknow", "benefits")

















 #explore
 #age.group.msc.achieves.table %>% select("age.group", "total", contains("_pc"), )
