rm(list=ls())
library("devtools")
load_all("SourceCode")
library("ggenealogy")
data("sbGeneal")
data("statGeneal")
library(stringr) # for str_count

v1="Tokyo";geneal=sbGeneal;colName="devYear";gen=15
v1="David Cox";geneal=statGeneal;colName="gradYear";gen=15

TokyoBranches <- getBranchQuant("Tokyo", sbGeneal, "devYear", 15)
AKBranches <- getBranchQuant("A.K.", sbGeneal, "yield", 15)
CoxBranches <- getBranchQuant("David Cox", statGeneal, "gradYear", 15)

##############################################################################
##############################################################################

v1="David Cox";geneal=statGeneal;colName="thesis";gen=15
rExpr = "grepl('Stochastic', geneal$colName)"
DCBranches_Sto <- getBranchQual(v1, geneal, colName, rExpr, gen)

rExpr = "grepl('(?i)Stochastic', geneal$colName)"
DCBranches_Stochastic <- getBranchQual(v1, geneal, colName, rExpr, gen)

colName = "country"
rExpr = "geneal$colName=='UnitedKingdom'" #works
DCBranches_UK <- getBranchQual(v1, geneal, colName, rExpr, gen)

desDC <- getDescendants("David Cox", statGeneal,15)
tail(sort(table(statGeneal[match(desDC$label,statGeneal$child),]$school)),5)
sum(statGeneal[match(getChild("David Cox",statGeneal),statGeneal$child),]$school=="University of London")

colName = "school"
rExpr = "geneal$colName=='University of London'" #works
DCBranches_UL <- getBranchQual(v1, geneal, colName, rExpr, gen)

rExpr = "geneal$colName=='Universidade de São Paulo'"
DCBranches_USP <- getBranchQual(v1, geneal, colName, rExpr, gen)
