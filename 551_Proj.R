setwd("C:\\Users\\Amit Patney\\Desktop\\UBC\\551\\Project")
getwd()

install.packages("dplyr")
library(dplyr)

viz <- read.csv("Employee_Compensation.csv")
colnames(viz)


org <- aggregate(Employee.Identifier~Organization.Group.Code+Organization.Group,data=viz,FUN = length)
anyDuplicated(org$Organization.Group.Code)
# No duplicates or blank descriptions in org

dept_all <- aggregate(Employee.Identifier~Department.Code+Department,data=viz,FUN = length)
dept <- dept_all[dept_all$Department != "",]
anyDuplicated(dept$Department.Code)
# Duplicates present due to blank descriptions in dept_all. Accounted for in the main data file below.
viz <- left_join(viz,subset(dept,select = -c(Employee.Identifier)), by = "Department.Code")
viz <- subset(viz, select = -c(Department.x))
colnames(viz)[colnames(viz) == 'Department.y'] <- 'Department'



union <- aggregate(Employee.Identifier~Union.Code+Union,data=viz,FUN = length)
anyDuplicated(union$Union.Code)
# Duplicates present due to misspelled descriptions in union. Accounted for in the main data file below.
temp <- aggregate(Employee.Identifier~Union.Code,data=union,FUN = max)
union <- merge(union,temp,by = c("Union.Code","Employee.Identifier"))
anyDuplicated(union$Union.Code)
viz <- left_join(viz,subset(union,select = -c(Employee.Identifier)), by = "Union.Code")
viz <- subset(viz, select = -c(Union.x))
colnames(viz)[colnames(viz) == 'Union.y'] <- 'Union'



viz <- viz[viz$Job.Family.Code != "__UNASSIGNED__" & viz$Job.Family.Code != 0,]
jfam_all <- aggregate(Employee.Identifier~Job.Family.Code+Job.Family,data=viz,FUN = length)
jfam <- jfam_all[jfam_all$Job.Family != "",]
anyDuplicated(jfam$Job.Family.Code)
# Duplicates present due to blank descriptions in jfam_all. Accounted for in the main data file below.
viz <- left_join(viz,subset(jfam,select = -c(Employee.Identifier)), by = "Job.Family.Code")
viz <- subset(viz, select = -c(Job.Family.x))
colnames(viz)[colnames(viz) == 'Job.Family.y'] <- 'Job.Family'



job <- aggregate(Employee.Identifier~Job.Code+Job,data=viz,FUN = length)
anyDuplicated(job$Job.Code)
# The duplicated job codes have very different job descriptions. Hence, these were not treated as duplicates.


viz <- viz[,c("Year.Type","Year","Organization.Group.Code","Organization.Group","Department.Code","Department","Union.Code","Union",                  
              "Job.Family.Code","Job.Family","Job.Code","Job","Employee.Identifier","Salaries","Overtime","Other.Salaries","Total.Salary",
              "Retirement","Health.and.Dental","Other.Benefits","Total.Benefits","Total.Compensation")]

sum(is.na(viz))
viz <- na.omit(viz)


sal <- viz[viz$Salaries>1,]
sal <- sal[sal$Total.Compensation>1,]
table(sal$Year.Type)
table(sal$Year)

cal <- sal[sal$Year.Type == "Calendar",]
fisc <- sal[sal$Year.Type == "Fiscal",]

cal <- cal[cal$Year < "2018",]
fisc <- fisc[fisc$Year < "2018",]


write.table(cal, "cal.csv", sep=",",row.names=FALSE)

#__UNASSIGNED__,0