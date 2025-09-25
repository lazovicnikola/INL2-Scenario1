# =============================
# INL2 – Scenario 1 
# =============================


prog1 <- c(104,102,159,168,150,151,111,105,137,124)
prog2 <- c(71.3,110,178,153,120,174,94.9,86.1,115,175)


A <- c(1,3,4,7,9)
ideA <- c(prog1[A], prog2[-A])
ideB <- c(prog2[A], prog1[-A])


cat("IDE-A mean:", mean(ideA), "median:", median(ideA), "\n")
cat("IDE-B mean:", mean(ideB), "median:", median(ideB), "\n")


diffs <- ideA - ideB
sh <- shapiro.test(diffs)
print(sh)  # p > 0.05 => normal


if (sh$p.value > 0.05) {
  # paired t-test
  tt <- t.test(ideA, ideB, paired=TRUE)
  print(tt)

  dz <- mean(diffs) / sd(diffs)
  cat("Cohen's dz:", dz, "\n")
} else {

  wt <- wilcox.test(ideA, ideB, paired=TRUE, exact=FALSE)
  print(wt)
}


boxplot(ideA, ideB, names=c("IDE-A","IDE-B"),
        main="Box plot of development times", ylab="Time (minutes)")

hist(ideA, main="Histogram IDE-A", xlab="Time (minutes)")

hist(ideB, main="Histogram IDE-B", xlab="Time (minutes)")

plot(ideA, ideB, main="Scatter: IDE-A vs IDE-B",
     xlab="IDE-A time (minutes)", ylab="IDE-B time (minutes)")
abline(0,1,col="red")  # referentna linija y=x
