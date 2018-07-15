
cat("\014")

data <- read.csv("gpa_effect_hiring_BS_MS_new2.csv")
barplot(as.matrix(data))

colours <- c("blue", "green")
barplot(as.matrix(data), main="My Barchart", ylab = "Numbers", cex.lab = 1.8, cex.main = 1.4, beside=TRUE, col=colours)
legend("topright", c("BS","MS"), cex=1.3, bty="n", fill=colours)

col_names <- c("not relevant at all","high gpa is essential","low gpa discourages employer","don't know","other")
colnames(data) <- col_names

percent <- (as.matrix(data) / 114) * 100 #scale(data, FALSE, colSums(data)) /114 
bp_gpa <- barplot(percent, names=col_names, beside=TRUE, axes=FALSE, main ="GPA effect on getting hired for new graduates" , xlab="Answer Options", ylab="Number of Reponses for each Option (%)", col=colours, ylim=c(0,100))
axis(2, at=seq(0,100,10))
legend("topright", legend=c("BS","MS"), bty="n", fill=colours)
text(bp_gpa, 0, round(percent, 0), cex=1, pos=3, xpd=TRUE)
