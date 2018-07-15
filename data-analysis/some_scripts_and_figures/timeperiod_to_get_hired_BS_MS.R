
cat("\014")
####################
survey <- read.csv("MisalignedExpectations-114.csv")
levels(survey$undergraduate.timeperiod.hired)
levels(survey$graduate.timeperiod.hired)
####################
# Time period to et hired
summary(survey$undergraduate.timeperiod.hired)
summary(survey$graduate.timeperiod.hired)

timeperiod_hired <- read.csv("timeperiod_to_get_hired.csv")
col_names_timeoeriod <- c("0-3 months","3-6 months","6-12 months","more than 1 year","already have a job","don't know")
colnames(timeperiod_hired) <- col_names_timeoeriod

percent_timeperiod <- (as.matrix(timeperiod_hired) / 114) * 100 #scale(data, FALSE, colSums(data)) /114 
bp_timeperiod <- barplot(percent_timeperiod, names=col_names_timeoeriod, beside=TRUE, axes=FALSE, main ="Timeperiod to get hired for new graduates" , xlab="Timeperiod Options", ylab="Frequency of Reponses for each Option (%)", col=colours, ylim=c(0,100))
axis(2, at=seq(0,100,10))
legend("top", legend=c("BS","MS"), bty="n", fill=colours)
text(bp_timeperiod, 0, round(percent_timeperiod,0), cex=1, pos=3, xpd=TRUE)

#####################
# time period to become proficient
p1 <- summary(survey$undergraduate.timeperiod.proficiency)
p2 <- summary(survey$graduate.timeperiod.proficiency)
timeperiod_proficiency <- c(p1,p2)

timeperiod_proficiency <- matrix(timeperiod_proficiency, nrow = 2, ncol=6, byrow = TRUE)
colnames(timeperiod_proficiency) <- c("0-3 months","12-24 months","3-6 months","6-12 months","don't know","more than 2 years")

orderedColumns <- c("0-3 months", "3-6 months", "6-12 months", "12-24 months", "more than 2 years", "don't know")
timeperiod_proficiency <- timeperiod_proficiency[ , orderedColumns]

percent_timeperiod_prof <- (timeperiod_proficiency / 114) * 100 
bp_timeperiod_prof <- barplot(percent_timeperiod_prof, beside=TRUE, axes=FALSE, main ="Timeperiod for new graduates to become proficient" , xlab="Timeperiod Options", ylab="Percent of Reponses for each Option (%)", col=colours, ylim=c(0,100))

lines(x = bp_timeperiod_prof[1,], y = percent_timeperiod_prof[1,], col="blue")
points(x = bp_timeperiod_prof[1,], y = percent_timeperiod_prof[1,], col="blue")

lines(x = bp_timeperiod_prof[2,], y = percent_timeperiod_prof[2,], col="green")
points(x = bp_timeperiod_prof[2,], y = percent_timeperiod_prof[2,], col="green")

axis(2, at=seq(0,100,10))
legend("topright", legend=c("BS","MS"), bty="n", fill=colours)
text(bp_timeperiod_prof, 0, round(percent_timeperiod_prof,0), cex=1, pos=3, xpd=TRUE)

#####################
# Compare BS MS and experience land a job 

compare_colors = c("red","orange")
job_chance_compare <- read.csv("compare_BS_MS_exp_job_chance.csv")

colnames(job_chance_compare) <- c("new graduate","experienced candidate","more or less equal chances","depends on position","don't know")

percent_job_chance_compare <- (as.matrix(job_chance_compare) / 114) * 100 
bp_job_chance_cmpare <- barplot(percent_job_chance_compare, beside=TRUE, axes=FALSE, main ="Compare chances of being hired" , xlab="Chance options", ylab="Percent of Reponses for each Option (%)", col=compare_colors, ylim=c(0,100))
axis(2, at=seq(0,100,10))
legend("topleft", legend=c("new BS graduate vs. 4 years of experience","new MS graduate vs. BS degree + 2 years of experience"), bty="n", fill=compare_colors)
text(bp_timeperiod_prof, 0, round(percent_job_chance_compare,0), cex=1, pos=3)

#####################
# Compare BS MS and experience on become proficient RIGHT AFTER BEING HIRED

compare_colors = c("red","orange")
job_chance_compare <- read.csv("compare_BS_MS_exp_job_chance.csv")

colnames(job_chance_compare) <- c("new graduate","experienced candidate","more or less equal chances","depends on position","don't know")

percent_job_chance_compare <- (as.matrix(job_chance_compare) / 114) * 100 
bp_job_chance_cmpare <- barplot(percent_job_chance_compare, beside=TRUE, axes=FALSE, main ="Compare chances of being hired" , xlab="Chance options", ylab="Percent of Reponses for each Option (%)", col=compare_colors, ylim=c(0,100))
axis(2, at=seq(0,100,10))
legend("topleft", legend=c("new BS graduate vs. 4 years of experience","new MS graduate vs. BS degree + 2 years of experience"), bty="n", fill=compare_colors)
text(bp_timeperiod_prof, 0, round(percent_job_chance_compare,0), cex=1, pos=3)




