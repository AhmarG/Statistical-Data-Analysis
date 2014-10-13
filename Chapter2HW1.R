#Ahmar Gordon
#STT-3850

#1
Berkeley <- read.csv("C:\\Users\\admin\\AppData\\Local\\Temp\\RtmpoHSgfA\\data71c46664cab")
sex = Berkeley$Sex
dep = Berkeley$Department
stat = Berkeley$Status

#2
table(sex)
barplot(table(sex))
#3
table(dep)
barplot(table(dep))
#4
table(dep,sex)
barplot(table(dep,sex),main=" number of applicants to each department by gender",ylab="Count")
#5
prop.table(table(sex,stat))*100
#6
barplot(prop.table(table(sex,stat))*100,main="% of men and women who were accepted or rejected",ylab="percentages",beside=TRUE)

#7
DeptA = subset(Berkeley,Department=="A")
DeptB = subset(Berkeley,Department=="B")
DeptC = subset(Berkeley,Department=="C")
DeptD = subset(Berkeley,Department=="D")
DeptE = subset(Berkeley,Department=="E")
DeptF = subset(Berkeley,Department=="F")
#8
prop.table(table(DeptA$Sex,DeptA$Status))*100
barplot(prop.table(table(DeptA$Sex,DeptA$Status))*100,main="% of Men and Women accepted or rejected in DeptA",ylab="Percentage",beside=TRUE)

prop.table(table(DeptB$Sex,DeptB$Status))*100
barplot(prop.table(table(DeptB$Sex,DeptB$Status))*100,main="% of Men and Women accepted or rejected in DeptB",ylab="Percentage",beside=TRUE)

prop.table(table(DeptC$Sex,DeptC$Status))*100
barplot(prop.table(table(DeptC$Sex,DeptC$Status))*100,main="% of Men and Women accepted or rejected in DeptC",ylab="Percentage",beside=TRUE)

prop.table(table(DeptD$Sex,DeptD$Status))*100
barplot(prop.table(table(DeptD$Sex,DeptD$Status))*100,main="% of Men and Women accepted or rejected in DeptD",ylab="Percentage",beside=TRUE)

prop.table(table(DeptE$Sex,DeptE$Status))*100
barplot(prop.table(table(DeptE$Sex,DeptE$Status))*100,main="% of Men and Women accepted or rejected in DeptE",ylab="Percentage",beside=TRUE)

prop.table(table(DeptF$Sex,DeptF$Status))*100
barplot(prop.table(table(DeptF$Sex,DeptF$Status))*100,main="% of Men and Women accepted or rejected in DeptF",ylab="Percentage",beside=TRUE)
