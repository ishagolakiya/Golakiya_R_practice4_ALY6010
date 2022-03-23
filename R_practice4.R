print("R- Practice 4")
install.packages("MASS")
library(MASS)

#getting familiar with data set
data("cats")
View(cats)    
summary(cats)
headtail(cats, 3)


#separate vector for male and female
male <- subset(cats, subset=(cats$Sex=="M"))
male
female <- subset(cats, subset=(cats$Sex=="F"))
female

#Graph for normalized
ggqqplot(male$Bwt,  ylab = "Male Body Weight Distribution",
         ggtheme = theme_minimal())

ggqqplot(female$Bwt,  ylab = "Female Body Weight Distribution",
         ggtheme = theme_minimal())

normale<- as.data.frame(log(male$Bwt))
norfemale<- as.data.frame(log(female$Bwt))

Male_sample <- sample_n(normale, 30)
Male_sample

Female_sample <- sample_n(norfemale,30)
Female_sample

mean(normale$`log(male$Bwt)`)

mean(norfemale$`log(female$Bwt)`)


t.test(Male_sample$`log(male$Bwt)`, Female_sample$`log(female$Bwt)`, conf.level = 0.95, paired = TRUE)

# Critical value
qt(p= 0.05/2, df=29, lower.tail = TRUE)


######################################## Part 2 ###########################################


before_workshop<-c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
before_workshop

after_workshop <-c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)
after_workshop

Change <- after_workshop - before_workshop
Change

shapiro.test(Change)

# Paired-T Hypothesis testing alpha = 0.95

t.test(before_workshop , after_workshop, conf.level = 0.95, paired = TRUE, alternative = "less")


# Paired-T Hypothesis testing alpha = 0.90

t.test(before_workshop , after_workshop, conf.level = 0.90, paired = TRUE, alternative = "less")

