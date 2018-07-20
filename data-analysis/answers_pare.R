suppressMessages(library(dplyr))
library(tidyr)
library(gmodels)

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
     are.you.a.teacher = grepl("Undergraduate student", Please.select.any.categories.that.best.describe.your.current.position., fixed = TRUE),
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
     bsc.proficiency.topchool = as.factor(How.do.you.think.the.school.choice..e.g..Top.10.Ivy.League.university.vs.random.college..affects.a.fresh.BS.graduate.s.professional.proficiency.),
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

#print(summary(lm(bsc.achieves.programming ~ are.you.a.graduate.student, normalized_wide)))
#print(summary(lm(bsc.achieves.programming ~ are.you.an.undergrad.student, normalized_wide)))
#print(summary(lm(bsc.achieves.programming ~ are.you.a.teacher, normalized_wide)))
#print(summary(lm(bsc.achieves.programming ~ are.you.an.industry.professional, normalized_wide)))

# we'll do the regression later on, let's start with the simpler things.
#print(summary(lm(bsc.achieves.research ~ are.you.a.graduate.student, normalized_wide)))
#print(summary(lm(bsc.achieves.research ~ are.you.an.industry.professional, normalized_wide)))
a
# I'm quite sure there's a better way to do this.
industry_programming <- prop.table(table(normalized_wide %>% 
                        select(are.you.an.industry.professional, bsc.achieves.programming) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_programming <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.undergrad.student, bsc.achieves.programming) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_programming <- prop.table(table(normalized_wide %>% 
                                            select(are.you.a.graduate.student, bsc.achieves.programming) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_programming <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.teacher, bsc.achieves.programming) %>% filter(are.you.a.teacher == TRUE)))


df <- data.frame(category=character(), programming=logical(), stringsAsFactors = FALSE)
df[nrow(df) + 1,] = list("Industry professional", round(industry_programming[1, "TRUE"]*100,1))
df[nrow(df) + 1,] = list("Undergrad student", round(undergrad_programming[1, "TRUE"]*100,1))
df[nrow(df) + 1,] = list("Graduate student", round(grad_programming[1, "TRUE"]*100,1))
df[nrow(df) + 1,] = list("Teacher", round(teacher_programming[1, "TRUE"]*100,1))

