###Question 1###
#Difference: Vector is one dimensional, matrix is two dimensional data structure where it additionally contains the dimension attribute.
#Similarity: Contains elements of same type.

###Question 2###
#Difference: Data frame is used for different types of variables. Matrix contains same data type.
#Similarity: Both are two dimensional data structure.

###Question 3###
x<-c(15,TRUE,"World") 
x
class(x)
#All 15, TRUE and World are classified as character by R.

###Question 4###
John_score<-c(95,91,88)
John_score
subject<-c("Statistics","Linear Algebra","Calculus")
subject
names(John_score)<-(subject)
John_score
###Question 5###
class(John_score)
class(subject)
###Question 6###
Amy_score<-c(95,91,88)
Ben_score<-c(96,94,97)
Catherine_score<-c(88,98,85)
student_score<-c(Amy_score,Ben_score,Catherine_score)
student_score
student_matrix<-matrix(student_score,nrow=3,byrow = TRUE)
student_matrix
colnames(student_matrix)<-c(subject)
rownames(student_matrix)<-c("Amy","Ben","Catherine")
student_matrix
###Question 7###
Student<-c("Amy","Amy","Amy","Ben","Ben","Ben","Catherine","Catherine","Catherine")
student_df<-data.frame(Student,subject,student_score)
student_df
###Question 8###
Country<-c("USA","India","Brazil","France","Russia")
Total_number_of_cases<-c(32608235,16003820,14122795,5374288,4736121)
Total_number_of_deaths<-c(583427,184986,381687,101881,107103)
Top5_country_df<-data.frame(Country,Total_number_of_cases,Total_number_of_deaths)
Top5_country_df
###Question 9###
head(mtcars)
tail(mtcars)
str(mtcars)
mtcars$cyl<-factor(mtcars$cyl)
mtcars[8:11]<-lapply(mtcars[8:11],as.factor)
write.table(mtcars,file="New_mtcars.csv",
            row.names = TRUE,
            col.names = TRUE,
            sep = "\t",
            quote=FALSE)


