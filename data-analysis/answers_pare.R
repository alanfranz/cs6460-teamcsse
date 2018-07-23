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

# I'm quite sure there's a better way to do this.
# FIX: missing soft skills!!!
industry_bsc_programming <- prop.table(table(normalized_wide %>% 
                        select(are.you.an.industry.professional, bsc.achieves.programming) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_bsc_programming <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.undergrad.student, bsc.achieves.programming) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_bsc_programming <- prop.table(table(normalized_wide %>% 
                                            select(are.you.a.graduate.student, bsc.achieves.programming) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_bsc_programming <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.teacher, bsc.achieves.programming) %>% filter(are.you.a.teacher == TRUE)))

industry_bsc_computational <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, bsc.achieves.computational) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_bsc_computational <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, bsc.achieves.computational) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_bsc_computational <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, bsc.achieves.computational) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_bsc_computational <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, bsc.achieves.computational) %>% filter(are.you.a.teacher == TRUE)))

industry_bsc_hireability <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, bsc.achieves.hireability) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_bsc_hireability <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, bsc.achieves.hireability) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_bsc_hireability <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, bsc.achieves.hireability) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_bsc_hireability <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, bsc.achieves.hireability) %>% filter(are.you.a.teacher == TRUE)))


industry_bsc_projectmanagement <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, bsc.achieves.projectmanagement) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_bsc_projectmanagement <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, bsc.achieves.projectmanagement) %>% filter(are.you.an.undergrad.student == TRUE)))

graduate_bsc_projectmanagement <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, bsc.achieves.projectmanagement) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_bsc_projectmanagement <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, bsc.achieves.projectmanagement) %>% filter(are.you.a.teacher == TRUE)))


industry_bsc_realworldproblemsolving <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, bsc.achieves.realworldproblemsolving) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_bsc_realworldproblemsolving <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, bsc.achieves.realworldproblemsolving) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_bsc_realworldproblemsolving <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, bsc.achieves.realworldproblemsolving) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_bsc_realworldproblemsolving <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, bsc.achieves.realworldproblemsolving) %>% filter(are.you.a.teacher == TRUE)))


industry_bsc_research <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, bsc.achieves.research) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_bsc_research <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, bsc.achieves.research) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_bsc_research <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, bsc.achieves.research) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_bsc_research <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, bsc.achieves.research) %>% filter(are.you.a.teacher == TRUE)))


bsc.achievements.by.category <- data.frame(category=character(), bsc.achieves.programming=numeric(), bsc.achieves.computational=numeric(), bsc.achieves.hireability=numeric(),
		 bsc.achieves.realworldproblemsolving=numeric(), bsc.achieves.research=numeric(), bsc.achieves.projectmanagement=numeric(),
		 stringsAsFactors = FALSE)
bsc.achievements.by.category[nrow(bsc.achievements.by.category) + 1,] = list("Industry professional", round(industry_bsc_programming[1, "TRUE"]*100,1), 
                         round(industry_bsc_computational[1, "TRUE"]*100,1), round(industry_bsc_hireability[1, "TRUE"]*100,1),  round(industry_bsc_realworldproblemsolving[1, "TRUE"]*100,1),
			 round(industry_bsc_research[1, "TRUE"]*100,1), round(industry_bsc_projectmanagement[1, "TRUE"]*100,1))
bsc.achievements.by.category[nrow(bsc.achievements.by.category) + 1,] = list("Undergrad", round(undergrad_bsc_programming[1, "TRUE"]*100,1), 
                         round(undergrad_bsc_computational[1, "TRUE"]*100,1), round(undergrad_bsc_hireability[1, "TRUE"]*100,1),  round(undergrad_bsc_realworldproblemsolving[1, "TRUE"]*100,1),
			 round(undergrad_bsc_research[1, "TRUE"]*100,1), 0.0)
bsc.achievements.by.category[nrow(bsc.achievements.by.category) + 1,] = list("Graduate student", round(grad_bsc_programming[1, "TRUE"]*100,1), 
                         round(grad_bsc_computational[1, "TRUE"]*100,1), round(grad_bsc_hireability[1, "TRUE"]*100,1),  round(grad_bsc_realworldproblemsolving[1, "TRUE"]*100,1),
			 round(grad_bsc_research[1, "TRUE"]*100,1), round(graduate_bsc_projectmanagement[1, "TRUE"]*100,1))

bsc.achievements.by.category[nrow(bsc.achievements.by.category) + 1,] = list("Teacher", round(teacher_bsc_programming[1, "TRUE"]*100,1), 
                         round(teacher_bsc_computational[1, "TRUE"]*100,1), round(teacher_bsc_hireability[1, "TRUE"]*100,1),  round(teacher_bsc_realworldproblemsolving[1, "TRUE"]*100,1),
			 round(teacher_bsc_research[1, "TRUE"]*100,1), 0.0)

# start bsc expectations

industry_bscexpect_programming <- prop.table(table(normalized_wide %>% 
                        select(are.you.an.industry.professional, bsc.shouldachieve.programming) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_bscexpect_programming <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.undergrad.student, bsc.shouldachieve.programming) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_bscexpect_programming <- prop.table(table(normalized_wide %>% 
                                            select(are.you.a.graduate.student, bsc.shouldachieve.programming) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_bscexpect_programming <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.teacher, bsc.shouldachieve.programming) %>% filter(are.you.a.teacher == TRUE)))

industry_bscexpect_computational <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, bsc.shouldachieve.computational) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_bscexpect_computational <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, bsc.shouldachieve.computational) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_bscexpect_computational <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, bsc.shouldachieve.computational) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_bscexpect_computational <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, bsc.shouldachieve.computational) %>% filter(are.you.a.teacher == TRUE)))

industry_bscexpect_hireability <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, bsc.shouldachieve.hireability) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_bscexpect_hireability <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, bsc.shouldachieve.hireability) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_bscexpect_hireability <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, bsc.shouldachieve.hireability) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_bscexpect_hireability <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, bsc.shouldachieve.hireability) %>% filter(are.you.a.teacher == TRUE)))


industry_bscexpect_projectmanagement <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, bsc.shouldachieve.projectmanagement) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_bscexpect_projectmanagement <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, bsc.shouldachieve.projectmanagement) %>% filter(are.you.an.undergrad.student == TRUE)))

graduate_bscexpect_projectmanagement <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, bsc.shouldachieve.projectmanagement) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_bscexpect_projectmanagement <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, bsc.shouldachieve.projectmanagement) %>% filter(are.you.a.teacher == TRUE)))


industry_bscexpect_realworldproblemsolving <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, bsc.shouldachieve.realworldproblemsolving) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_bscexpect_realworldproblemsolving <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, bsc.shouldachieve.realworldproblemsolving) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_bscexpect_realworldproblemsolving <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, bsc.shouldachieve.realworldproblemsolving) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_bscexpect_realworldproblemsolving <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, bsc.shouldachieve.realworldproblemsolving) %>% filter(are.you.a.teacher == TRUE)))


industry_bscexpect_research <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, bsc.shouldachieve.research) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_bscexpect_research <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, bsc.shouldachieve.research) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_bscexpect_research <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, bsc.shouldachieve.research) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_bscexpect_research <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, bsc.shouldachieve.research) %>% filter(are.you.a.teacher == TRUE)))


bscexpect.achievements.by.category <- data.frame(category=character(), bsc.shouldachieve.programming=numeric(), bsc.shouldachieve.computational=numeric(), bsc.shouldachieve.hireability=numeric(),
		 bsc.shouldachieve.realworldproblemsolving=numeric(), bsc.shouldachieve.research=numeric(), bsc.shouldachieve.projectmanagement=numeric(),
		 stringsAsFactors = FALSE)
bscexpect.achievements.by.category[nrow(bscexpect.achievements.by.category) + 1,] = list("Industry professional", round(industry_bscexpect_programming[1, "TRUE"]*100,1), 
                         round(industry_bscexpect_computational[1, "TRUE"]*100,1), round(industry_bscexpect_hireability[1, "TRUE"]*100,1),  round(industry_bscexpect_realworldproblemsolving[1, "TRUE"]*100,1),
			 round(industry_bscexpect_research[1, "TRUE"]*100,1), round(industry_bscexpect_projectmanagement[1, "TRUE"]*100,1))
bscexpect.achievements.by.category[nrow(bscexpect.achievements.by.category) + 1,] = list("Undergrad", round(undergrad_bscexpect_programming[1, "TRUE"]*100,1), 
                         round(undergrad_bscexpect_computational[1, "TRUE"]*100,1), round(undergrad_bscexpect_hireability[1, "TRUE"]*100,1),  round(undergrad_bscexpect_realworldproblemsolving[1, "TRUE"]*100,1),
			 0.0, round(undergrad_bscexpect_projectmanagement[1, "TRUE"]*100, 1))
bscexpect.achievements.by.category[nrow(bscexpect.achievements.by.category) + 1,] = list("Graduate student", round(grad_bscexpect_programming[1, "TRUE"]*100,1), 
                         round(grad_bscexpect_computational[1, "TRUE"]*100,1), round(grad_bscexpect_hireability[1, "TRUE"]*100,1),  round(grad_bscexpect_realworldproblemsolving[1, "TRUE"]*100,1),
			 round(grad_bscexpect_research[1, "TRUE"]*100,1), round(graduate_bscexpect_projectmanagement[1, "TRUE"]*100,1))

bscexpect.achievements.by.category[nrow(bscexpect.achievements.by.category) + 1,] = list("Teacher", round(teacher_bscexpect_programming[1, "TRUE"]*100,1), 
                         round(teacher_bscexpect_computational[1, "TRUE"]*100,1), round(teacher_bscexpect_hireability[1, "TRUE"]*100,1),  round(teacher_bscexpect_realworldproblemsolving[1, "TRUE"]*100,1),
			 0.0, round(teacher_bscexpect_projectmanagement[1, "TRUE"]*100,1))



# start msc

industry_msc_programming <- prop.table(table(normalized_wide %>% 
                        select(are.you.an.industry.professional, msc.achieves.programming) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_msc_programming <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.undergrad.student, msc.achieves.programming) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_msc_programming <- prop.table(table(normalized_wide %>% 
                                            select(are.you.a.graduate.student, msc.achieves.programming) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_msc_programming <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.teacher, msc.achieves.programming) %>% filter(are.you.a.teacher == TRUE)))

industry_msc_computational <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, msc.achieves.computational) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_msc_computational <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, msc.achieves.computational) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_msc_computational <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, msc.achieves.computational) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_msc_computational <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, msc.achieves.computational) %>% filter(are.you.a.teacher == TRUE)))

industry_msc_hireability <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, msc.achieves.hireability) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_msc_hireability <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, msc.achieves.hireability) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_msc_hireability <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, msc.achieves.hireability) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_msc_hireability <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, msc.achieves.hireability) %>% filter(are.you.a.teacher == TRUE)))


industry_msc_projectmanagement <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, msc.achieves.projectmanagement) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_msc_projectmanagement <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, msc.achieves.projectmanagement) %>% filter(are.you.an.undergrad.student == TRUE)))

graduate_msc_projectmanagement <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, msc.achieves.projectmanagement) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_msc_projectmanagement <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, msc.achieves.projectmanagement) %>% filter(are.you.a.teacher == TRUE)))


industry_msc_realworldproblemsolving <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, msc.achieves.realworldproblemsolving) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_msc_realworldproblemsolving <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, msc.achieves.realworldproblemsolving) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_msc_realworldproblemsolving <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, msc.achieves.realworldproblemsolving) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_msc_realworldproblemsolving <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, msc.achieves.realworldproblemsolving) %>% filter(are.you.a.teacher == TRUE)))


industry_msc_research <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, msc.achieves.research) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_msc_research <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, msc.achieves.research) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_msc_research <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, msc.achieves.research) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_msc_research <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, msc.achieves.research) %>% filter(are.you.a.teacher == TRUE)))


msc.achievements.by.category <- data.frame(category=character(), msc.achieves.programming=numeric(), msc.achieves.computational=numeric(), msc.achieves.hireability=numeric(),
		 msc.achieves.realworldproblemsolving=numeric(), msc.achieves.research=numeric(), msc.achieves.projectmanagement=numeric(),
		 stringsAsFactors = FALSE)
msc.achievements.by.category[nrow(msc.achievements.by.category) + 1,] = list("Industry professional", round(industry_msc_programming[1, "TRUE"]*100,1), 
                         round(industry_msc_computational[1, "TRUE"]*100,1), round(industry_msc_hireability[1, "TRUE"]*100,1),  round(industry_msc_realworldproblemsolving[1, "TRUE"]*100,1),
			 round(industry_msc_research[1, "TRUE"]*100,1), round(industry_msc_projectmanagement[1, "TRUE"]*100,1))
msc.achievements.by.category[nrow(msc.achievements.by.category) + 1,] = list("Undergrad", round(undergrad_msc_programming[1, "TRUE"]*100,1), 
                         round(undergrad_msc_computational[1, "TRUE"]*100,1), round(undergrad_msc_hireability[1, "TRUE"]*100,1),  round(undergrad_msc_realworldproblemsolving[1, "TRUE"]*100,1),
			 round(undergrad_msc_research[1, "TRUE"]*100,1), round(undergrad_msc_projectmanagement[1, "TRUE"]*100,1))
msc.achievements.by.category[nrow(msc.achievements.by.category) + 1,] = list("Graduate student", round(grad_msc_programming[1, "TRUE"]*100,1), 
                         round(grad_msc_computational[1, "TRUE"]*100,1), round(grad_msc_hireability[1, "TRUE"]*100,1),  round(grad_msc_realworldproblemsolving[1, "TRUE"]*100,1),
			 round(grad_msc_research[1, "TRUE"]*100,1), round(graduate_msc_projectmanagement[1, "TRUE"]*100,1))

msc.achievements.by.category[nrow(msc.achievements.by.category) + 1,] = list("Teacher", round(teacher_msc_programming[1, "TRUE"]*100,1), 
                         round(teacher_msc_computational[1, "TRUE"]*100,1), round(teacher_msc_hireability[1, "TRUE"]*100,1),  round(teacher_msc_realworldproblemsolving[1, "TRUE"]*100,1),
			 round(teacher_msc_research[1, "TRUE"]*100,1), round(teacher_msc_projectmanagement[1, "TRUE"]*100,1))



# start msc expectations

industry_mscexpect_programming <- prop.table(table(normalized_wide %>% 
                        select(are.you.an.industry.professional, msc.shouldachieve.programming) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_mscexpect_programming <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.undergrad.student, msc.shouldachieve.programming) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_mscexpect_programming <- prop.table(table(normalized_wide %>% 
                                            select(are.you.a.graduate.student, msc.shouldachieve.programming) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_mscexpect_programming <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.teacher, msc.shouldachieve.programming) %>% filter(are.you.a.teacher == TRUE)))

industry_mscexpect_computational <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, msc.shouldachieve.computational) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_mscexpect_computational <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, msc.shouldachieve.computational) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_mscexpect_computational <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, msc.shouldachieve.computational) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_mscexpect_computational <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, msc.shouldachieve.computational) %>% filter(are.you.a.teacher == TRUE)))

industry_mscexpect_hireability <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, msc.shouldachieve.hireability) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_mscexpect_hireability <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, msc.shouldachieve.hireability) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_mscexpect_hireability <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, msc.shouldachieve.hireability) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_mscexpect_hireability <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, msc.shouldachieve.hireability) %>% filter(are.you.a.teacher == TRUE)))


industry_mscexpect_projectmanagement <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, msc.shouldachieve.projectmanagement) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_mscexpect_projectmanagement <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, msc.shouldachieve.projectmanagement) %>% filter(are.you.an.undergrad.student == TRUE)))

graduate_mscexpect_projectmanagement <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, msc.shouldachieve.projectmanagement) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_mscexpect_projectmanagement <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, msc.shouldachieve.projectmanagement) %>% filter(are.you.a.teacher == TRUE)))


industry_mscexpect_realworldproblemsolving <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, msc.shouldachieve.realworldproblemsolving) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_mscexpect_realworldproblemsolving <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, msc.shouldachieve.realworldproblemsolving) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_mscexpect_realworldproblemsolving <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, msc.shouldachieve.realworldproblemsolving) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_mscexpect_realworldproblemsolving <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, msc.shouldachieve.realworldproblemsolving) %>% filter(are.you.a.teacher == TRUE)))


industry_mscexpect_research <- prop.table(table(normalized_wide %>% 
                                           select(are.you.an.industry.professional, msc.shouldachieve.research) %>% filter(are.you.an.industry.professional == TRUE)))

undergrad_mscexpect_research <- prop.table(table(normalized_wide %>% 
                                            select(are.you.an.undergrad.student, msc.shouldachieve.research) %>% filter(are.you.an.undergrad.student == TRUE)))

grad_mscexpect_research <- prop.table(table(normalized_wide %>% 
                                       select(are.you.a.graduate.student, msc.shouldachieve.research) %>% filter(are.you.a.graduate.student == TRUE)))

teacher_mscexpect_research <- prop.table(table(normalized_wide %>% 
                                          select(are.you.a.teacher, msc.shouldachieve.research) %>% filter(are.you.a.teacher == TRUE)))


mscexpect.achievements.by.category <- data.frame(category=character(), msc.shouldachieve.programming=numeric(), msc.shouldachieve.computational=numeric(), msc.shouldachieve.hireability=numeric(),
		 msc.shouldachieve.realworldproblemsolving=numeric(), msc.shouldachieve.research=numeric(), msc.shouldachieve.projectmanagement=numeric(),
		 stringsAsFactors = FALSE)
mscexpect.achievements.by.category[nrow(mscexpect.achievements.by.category) + 1,] = list("Industry professional", round(industry_mscexpect_programming[1, "TRUE"]*100,1), 
                         round(industry_mscexpect_computational[1, "TRUE"]*100,1), round(industry_mscexpect_hireability[1, "TRUE"]*100,1),  round(industry_mscexpect_realworldproblemsolving[1, "TRUE"]*100,1),
			 round(industry_mscexpect_research[1, "TRUE"]*100,1), round(industry_mscexpect_projectmanagement[1, "TRUE"]*100,1))
mscexpect.achievements.by.category[nrow(mscexpect.achievements.by.category) + 1,] = list("Undergrad", round(undergrad_mscexpect_programming[1, "TRUE"]*100,1), 
                         round(undergrad_mscexpect_computational[1, "TRUE"]*100,1), round(undergrad_mscexpect_hireability[1, "TRUE"]*100,1),  round(undergrad_mscexpect_realworldproblemsolving[1, "TRUE"]*100,1),
			 round(undergrad_mscexpect_research[1, "TRUE"]*100,1), round(undergrad_mscexpect_projectmanagement[1, "TRUE"]*100, 1))
mscexpect.achievements.by.category[nrow(mscexpect.achievements.by.category) + 1,] = list("Graduate student", round(grad_mscexpect_programming[1, "TRUE"]*100,1), 
                         round(grad_mscexpect_computational[1, "TRUE"]*100,1), round(grad_mscexpect_hireability[1, "TRUE"]*100,1),  round(grad_mscexpect_realworldproblemsolving[1, "TRUE"]*100,1),
			 round(grad_mscexpect_research[1, "TRUE"]*100,1), round(graduate_mscexpect_projectmanagement[1, "TRUE"]*100,1))

mscexpect.achievements.by.category[nrow(mscexpect.achievements.by.category) + 1,] = list("Teacher", round(teacher_mscexpect_programming[1, "TRUE"]*100,1), 
                         round(teacher_mscexpect_computational[1, "TRUE"]*100,1), round(teacher_mscexpect_hireability[1, "TRUE"]*100,1),  round(teacher_mscexpect_realworldproblemsolving[1, "TRUE"]*100,1),
			 round(teacher_mscexpect_research[1, "TRUE"]*100,1), round(teacher_mscexpect_projectmanagement[1, "TRUE"]*100,1))


#x <- normalized_wide %>% select(bsc.achieves.programming, bsc.achieves.computational, age.group)
#x$bsc.achieves.programming <- as.factor(x$bsc.achieves.programming)
#x$bsc.achieves.computational <- as.factor(x$bsc.achieves.computational)


agegroup.tbl <- table(normalized_wide %>% select(age.group))
# age.group vs bsc achievements - START
p <- tabular(age.group~bsc.achieves.programming + bsc.achieves.computational + bsc.achieves.projectmanagement
             + bsc.achieves.realworldproblemsolving + bsc.achieves.research + bsc.achieves.softskills + bsc.achieves.hireability
             + bsc.achieves.dontknow
             , normalized_wide)

 q <- data.frame(matrix(p, nrow=nrow(p))) %>% mutate_all(as.integer)
 rownames(q) <- rowLabels(p)
 colnames(q) <- colLabels(p)[1,]
  q <- add_rownames(q, var="age.group")
 q <- q %>% mutate(total = agegroup.tbl[age.group])
 age.group.bsc.achieves.table <- q %>% mutate_at(vars(contains('bsc')), .funs = funs(pc = round(./total*100, 1)))
 # age.group vs bsc achievements - END
 
 # age.group vs bsc desiderata - START
 p <- tabular(age.group~bsc.shouldachieve.programming + bsc.shouldachieve.computational + bsc.shouldachieve.projectmanagement
              + bsc.shouldachieve.realworldproblemsolving + bsc.shouldachieve.research + bsc.shouldachieve.softskills + bsc.shouldachieve.hireability
              + bsc.shouldachieve.dontknow
              , normalized_wide)
 
 q <- data.frame(matrix(p, nrow=nrow(p))) %>% mutate_all(as.integer)
 rownames(q) <- rowLabels(p)
 colnames(q) <- colLabels(p)[1,]
 q <- add_rownames(q, var="age.group")
 q <- q %>% mutate(total = agegroup.tbl[age.group])
 age.group.bsc.shouldachieve.table <- q %>% mutate_at(vars(contains('bsc')), .funs = funs(pc = round(./total*100, 1)))
 # age.group vs bsc desiderata - END

 # age.group vs msc achievements - START
 p <- tabular(age.group~msc.achieves.programming + msc.achieves.computational + msc.achieves.projectmanagement
              + msc.achieves.realworldproblemsolving + msc.achieves.research + msc.achieves.softskills + msc.achieves.hireability
              + msc.achieves.dontknow
              , normalized_wide)
 
 q <- data.frame(matrix(p, nrow=nrow(p))) %>% mutate_all(as.integer)
 rownames(q) <- rowLabels(p)
 colnames(q) <- colLabels(p)[1,]
 q <- add_rownames(q, var="age.group")
 q <- q %>% mutate(total = agegroup.tbl[age.group])
 age.group.msc.achieves.table <- q %>% mutate_at(vars(contains('msc')), .funs = funs(pc = round(./total*100, 1)))
 # age.group vs msc achievements - END
 
 # age.group vs msc achievements - START
 p <- tabular(age.group~msc.shouldachieve.programming + msc.shouldachieve.computational + msc.shouldachieve.projectmanagement
              + msc.shouldachieve.realworldproblemsolving + msc.shouldachieve.research + msc.shouldachieve.softskills + msc.shouldachieve.hireability
              + msc.shouldachieve.dontknow
              , normalized_wide)
 
 q <- data.frame(matrix(p, nrow=nrow(p))) %>% mutate_all(as.integer)
 rownames(q) <- rowLabels(p)
 colnames(q) <- colLabels(p)[1,]
 q <- add_rownames(q, var="age.group")
 q <- q %>% mutate(total = agegroup.tbl[age.group])
 age.group.msc.shouldachieve.table <- q %>% mutate_at(vars(contains('msc')), .funs = funs(pc = round(./total*100, 1)))
 # age.group vs msc achievements - END
 
 
mytabfunc <- function(independent, dependent, mutate_what) {
 p <- tabular(as.formula(sprintf("%s~%s", independent, dependent))
              , normalized_wide)
 
 q <- data.frame(matrix(p, nrow=nrow(p))) %>% mutate_all(as.integer)
 rownames(q) <- rowLabels(p)
 colnames(q) <- colLabels(p)[1,]
 q <- add_rownames(q, var=independent)
 q <- q %>% mutate(total = agegroup.tbl[age.group])
 return (q %>% mutate_at(vars(mutate_what), .funs = funs(pc = round(./total*100, 1))))
}
 

xxx <- mytabfunc("age.group", "msc.shouldachieve.programming + msc.shouldachieve.computational + msc.shouldachieve.projectmanagement
              + msc.shouldachieve.realworldproblemsolving + msc.shouldachieve.research + msc.shouldachieve.softskills + msc.shouldachieve.hireability
              + msc.shouldachieve.dontknow", contains('msc'))
 
 
 
 
 
 #explore
 #age.group.msc.achieves.table %>% select("age.group", "total", contains("_pc"), )