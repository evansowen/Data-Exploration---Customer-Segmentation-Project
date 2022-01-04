library(openxlsx)
library(tidyverse)
library(expss)
library(cluster)    # clustering algorithms
library(factoextra)
library(rpart)
library(rpart.plot)
library(tidyverse)

setwd("~/Desktop/Segmentation Project/Working Folder")
df <- read.xlsx(file.choose(), 1)  # df2.w_clusters
df <- select(df, -c("cluster2"))
df$IncomeInd <- df$HHIncome/df$HouseholdSize

# Add Labels
# Low ARPU <12
# Mid   12 < ARPU < 35
# High  ARPU > 35

df$Value <- ifelse(df$ARPU<12,"Low",ifelse(df$ARPU<35,"Medium","High"))
df$HighValue <- ifelse(df$Value=="High", "High", "Not High") # 0 is yes, 1 is no
df$MidValue <- ifelse(df$Value=="Medium","Mid", "Not Mid") # 0 is yes, 1 is no
df$LowValue <- ifelse(df$Value=="Low", "Low", "Not Low") # 0 is yes, 1 is no
df$CollegeEd <- ifelse(df$EducationYears>14, "CollegeEducated", "Not College")
df$EmployStatus <- ifelse(df$EmploymentLength>5, "Experienced", "Not Experienced")
df$IncomeClass <- ifelse(df$IncomeInd<12500, "LowIncome", "HighIncome")

# Run Tree for High Value Segment
treehigh <- rpart(HighValue ~ Age + IncomeClass + 
                EmployStatus + CollegeEd, data =df, 
              control = rpart.control(cp = 0.0001))
bestcp <- treehigh$cptable[which.min(treehigh$cptable[,"xerror"]),"CP"]
treehigh.pruned <- prune(treehigh, cp = bestcp)

# visualize tree
prp(treehigh.pruned, faclen = 0, cex = 0.8, extra = 1)

# Run Tree for Low Value Segment
treelow <- rpart(LowValue ~ Age + IncomeInd + 
                    EmploymentLength + EducationYears, data =df, 
                  control = rpart.control(cp = 0.0001))
bestcp1 <- treelow$cptable[which.min(treelow$cptable[,"xerror"]),"CP"]
treelow.pruned <- prune(treelow, cp = bestcp1)

# visualize tree
prp(treelow.pruned, faclen = 0, cex = 0.8, extra = 1)

# Run Tree for Mid Value Segment
treemid <- rpart(MidValue ~ IncomeClass + 
                   EmployStatus + AgeBin + CollegeEd, data =df, 
                 control = rpart.control(cp = 0.0001))
bestcp2 <- treemid$cptable[which.min(treemid$cptable[,"xerror"]),"CP"]
treemid.pruned <- prune(treemid, cp = bestcp2)

# visualize tree
prp(treemid.pruned, faclen = 0, cex = 0.8, extra = 1)

str(treemid.pruned)
treelow.pruned$cptable
