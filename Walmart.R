Wmt <- read.table("/Users/Richard/CloudStation/Docs/Learning/Machine_Learning/Kaggle Competition/Walmart/data/train.csv",
                   header = TRUE, sep = ',')
Test <- read.table("/Users/Richard/CloudStation/Docs/Learning/Machine_Learning/Kaggle Competition/Walmart/data/test.csv",
                   header = TRUE, sep = ',')


library(sqldf)
library(reshape2)
library(data.table)
library(plyr)
library(rpart)

# Check the uniqueness of TripType for each VisitNumber
uniq1 <- sqldf('select distinct TripType, VisitNumber from Wmt')

uniq2 <- sqldf('select VisitNumber, count(TripType) as cnt from uniq1 group by VisitNumber')
table (uniq2$cnt)

#
data30 <- Wmt[Wmt$TripType == 30,]
uniq_fl <- sqldf('select distinct FinelineNumber from Wmt')

# Unique Dept Name
Dept <- sqldf('select distinct DepartmentDescription from Wmt')
Dept$Dept_No <- (1: dim(Dept)[1])

# Derive Features at Trip Level 
# 1. Number of different items
# 2. Day of the trip
# 3. Return only
# 4. Department indicator
# 5. Fine Line number indicator?

VisitData1 <- sqldf('select distinct visitNumber, TripType, Weekday, sum(scancount) as tot_item, count(scancount) as uniq_item
                    from Wmt group by visitNumber, TripType, Weekday')

VisitDept <- sqldf('select visitNumber, DepartmentDescription, sum(scancount) as tot_item
                   from Wmt group by visitNumber, DepartmentDescription')
VisitDept <- sqldf('select a.VisitNumber, a.tot_item, b.Dept_No from VisitDept a left join Dept b on a.DepartmentDescription = b.DepartmentDescription')

VisitData2 <- data.frame(acast(VisitDept,VisitNumber ~ Dept_No, fill = 0))

setDT(VisitData2, keep.rownames = TRUE)
rename(VisitData2, c("rn" = "VisitNumber"))

VisitDataF <- sqldf('select a.TripType, a.Weekday, a.tot_item, a.uniq_item
                    , b.* from VisitData1 a join VisitData2 b on a.VisitNumber = b.VisitNumber')

# Decision Tree
VisitDataF$TripTypeF <- factor(VisitDataF$TripType)

xnam <- paste0("X", 1:69)
(fmla <- as.formula(paste("TripTypeF ~ Weekday + tot_item +", paste(xnam, collapse= "+"))))

DTresult <- rpart(fmla, VisitDataF, control = rpart.control(minsplit = 50, maxdepth = 30, cp = 0.001))

pred <- pmax(pmin(predict(DTresult), 1-1E-15), 1E-15)

# Post Processing and Scoring Calculation
y_mat <- matrix(0,dim(pred)[1], dim(pred)[2])
colnames(y_mat) <- colnames(pred)
for (i in (1:dim(pred)[1])){
  y_mat[i, toString(VisitDataF$TripType[i])] <- 1
}

-sum(log(pred) * y_mat)/(dim(pred)[1] * dim(pred)[2])


