# Load and examine the structure of the example soybean genealogy dataset (called sbGeneal)
rm(list=ls())
library("devtools")
load_all("Code")
library("ggenealogy")
data("sbGeneal")
str(sbGeneal)

# Load and examine the structure of the example academic statistician genealogy dataset (called statGeneal)
data("statGeneal")
dim(statGeneal)
colnames(statGeneal)

# Convert our example soybean genealogy dataset (called sbGeneal) into an igraph object:
sbIG <- dfToIG(sbGeneal)
sbIG

# Obtain basic summary statistics about the igraph object
getBasicStatistics(sbIG)

# Determine shortest path of parent-child relationships between two labels of interest ("Tokyo" and "Narow")
pathTN <- getPath("Tokyo", "Narow", sbIG, sbGeneal, "devYear")
pathTN

# Plot returned path
plotPath(pathTN, sbGeneal, "devYear")

# Do the same for a different pair of two labels of interest ("Bedford" and "Zane"). First, we can determine the years these two labels were identified
getVariable("Bedford", sbGeneal, "devYear")
getVariable("Zane", sbGeneal, "devYear")

# Next, we can determine the shortest path of parent-child relationships between these two labels of interest and plot it.
pathBZ <- getPath("Bedford", "Zane", sbIG, sbGeneal, "devYear")
plotPath(pathBZ, sbGeneal, "devYear", fontFace = 2) + ggplot2::xlab("Development Year")

# Here, we demonstrate plotting paths on a filtered dataset, where all observations contain a non-NA value for yield. We show one example where there is one variable plotted for the path (yield) and another example where there are two variables plotted for the path (yield and devYear)
sbFilt <- sbGeneal[complete.cases(sbGeneal[1:3]),]
sbFiltIG <- dfToIG(sbFilt)
pathCL <- getPath("Clark", "Lawrence", sbFiltIG, sbFilt, "yield")
plotPath(pathCL, sbFilt, "yield") + ggplot2::xlab("Yield")
pathCL2 <- getPath("Clark", "Lawrence", sbFiltIG, sbFilt, "devYear")
plotPath(pathCL2, sbFilt, "devYear", "yield") + ggplot2::xlab("Development Year") + ggplot2::ylab("Yield")

# In the previous section, we obtained the shortest path between the the pair of labels "Tokyo" and "Narow" and saved it as a variable pathTN. Here, we can plot that path superimposed over all labels in the example soybean genealogy dataset.
plotPathOnAll(pathTN, sbGeneal, sbIG, "devYear", bin = 3, pathEdgeCol = "red", nodeSize = 2.5, pathNodeSize = 4) + ggplot2::theme(axis.text = ggplot2::element_text(size = 12), axis.title = ggplot2::element_text(size = 12))

# We can repeat this process, only now instead of setting the bin variable to 1:3 (as we did earlier), we can set it to 1:6.
plotPathOnAll(pathTN, sbGeneal, sbIG, "devYear", bin = 6, pathEdgeCol = "seagreen2", nodeSize = 1, pathNodeSize = 3) + ggplot2::xlab("Development Year")

# We now plot the path across all values in the filtered data frame. We again demonstrate this plotting tool with one variable plotted for the path (yield) and another example where there are two variables plotted for the path (yield and devYear)
plotPathOnAll(pathCL, sbFilt, sbFiltIG, "yield", bin = 3, pathEdgeCol = "purple") + ggplot2::xlab("Yield")
plotPathOnAll(pathCL, sbFilt, sbFiltIG, "yield", "devYear", pathEdgeCol = "orange") + ggplot2::xlab("Yield") + ggplot2::ylab("Development Year")

# As is explained in the article, only the top part of Figure 6 (the figure from this section) is produced by ggenealogy code. In contrast, the bottom part of Figure 6 was produced by tools outside of ggenealogy for didactic purposes. Below, we recreate the top part of Figure 6, which was to generate a plot of the ancestors and descendants of the label Lee.
plotAncDes("Lee", sbGeneal, mAnc = 6, mDes = 6, vCol = "blue")

# We can plot the distance matrix for a set of 10 varieties.
varieties <- c("Brim", "Bedford", "Calland", "Dillon", "Hood", "Narow", "Pella", "Tokyo", "Young", "Zane")
plotDegMatrix(varieties, sbIG, sbGeneal) + ggplot2::scale_fill_continuous(low = "white", high = "darkgreen") + ggplot2::theme(legend.title = ggplot2::element_text(size = 15), legend.text = ggplot2::element_text(size = 15)) + ggplot2::labs(x = "Variety", y = "Variety")

# We can now explore some of the plotting functions in ggenealogy, only now with the academic statistican genealogy dataset. This second example dataset is much larger than the first example dataset of soybean genealogy. For example purposes, we would like to view the ancestor and descendant plot for the individual who has the largest number of descendants. To identify the name of this individual, we run the following code:
library("dplyr")
indVec <- getNodes(statGeneal)
indVec <- indVec[which(indVec != "", )]
dFunc <- function(var) nrow(getDescendants(var, statGeneal, gen = 100))
numDesc <- sapply(indVec, dFunc)
table(numDesc)
which(numDesc == 159)

# We see the individual with the largest number of descendants is Sir David Cox, who has 159 descendants. Now, we can plot the "ancestors" and "descendants" of Sir David Cox.
plotAncDes("David Cox", statGeneal, mAnc = 6, mDes = 6, vCol = "blue")

# It seems that of the 42 "children" of Sir David Cox, the one who went on to have the largest number of "children" of his own was Peter Bloomfield. We can verify below that Peter Bloomfield had 26 "children" and 49 "descendants".
length(getChild("Peter Bloomfield", statGeneal))
nrow(getDescendants("Peter Bloomfield", statGeneal, gen = 100))

# It would be of interest now to examine the shortest path between Sir David Cox and one of his newest "descendants" Petra Buzkova. To do so, we first need to obtain the corresponding igraph object of the example academic statistican genealogy dataset.
statIG <- dfToIG(statGeneal)
pathCB <- getPath("David Cox", "Petra Buzkova", statIG, statGeneal, "gradYear", isDirected = FALSE)

# After doing so, we can now determine the shortest path between Sir David Cox and Petra Buzkova, and plot it.
plotPath(pathCB, statGeneal, "gradYear", fontFace = 4) + ggplot2::xlab("Graduation Year") + ggplot2::theme(axis.text = ggplot2::element_text(size = 10), axis.title = ggplot2::element_text(size = 10)) + ggplot2::scale_x_continuous(expand = c(.1, .2))

# We can now superimpose this shortest path between Sir David Cox and Petra Buzkova across the entire genealogical structure.
plotPathOnAll(pathCB, statGeneal, statIG, "gradYear", bin = 200) + ggplot2::theme(axis.text = ggplot2::element_text(size = 8), axis.title = ggplot2::element_text(size = 8)) + ggplot2::scale_x_continuous(expand = c(.1, .2)) + ggplot2::xlab("Graduation Year") 

# We notice, however, that we cannot read the text of the nodes on the path of interest. To solve this problem, we can create the same plot, only now specifying that any nodes that are not on our path of interest are deemphasized with smaller text.
plotPathOnAll(pathCB, statGeneal, statIG, "gradYear", bin = 200, nodeSize = .5, pathNodeSize = 2.5, nodeCol = "darkgray", edgeCol = "lightgray") + ggplot2::theme(axis.text = ggplot2::element_text(size = 8), axis.title = ggplot2::element_text(size = 8)) + ggplot2::scale_x_continuous(expand = c(.1, .2)) + ggplot2::xlab("Graduation Year")

# Even though we can now read the text labels on the path of interest, we lost the ability to read labels that are not on our path of interest. At this point, we can keep the best of both worlds: We can create a plot that demphasizes the nodes that are not on the path of interest by assigning them small text font size, but incoroporate an interactive function so that we can hover over these non-path nodes if we wish to obtain their label information. This is the plot we used and interacted with to create the video embedded in Figure 12.
plotPathOnAll(pathCB, statGeneal, statIG, "gradYear", bin = 200, nodeSize = .5, pathNodeSize = 2.5, nodeCol = "darkgray", edgeCol = "lightgray", animate = TRUE)

# Here, we plot the filtered data frame of the soy bean data with one and two variables, respectively, when the animation is turned on.
plotPathOnAll(pathCL, sbFilt, sbFiltIG, "yield", pathEdgeCol = "orange", animate = TRUE)
plotPathOnAll(pathCL, sbFilt, sbFiltIG, "yield", "devYear", pathEdgeCol = "orange", animate = TRUE)

# We can demonstrate the getBranchQuant() function by examining the quantitative variable ``yield" across the descendant branches of the soybean variety A.K.
AKBranchYield <- getBranchQuant("A.K.", sbGeneal, "yield", 15)
select(AKBranchYield, -DesNames)

# We can examine the mean graduation year for the ``descendant" branches of the academic statistician David Cox
CoxBranchYear <- getBranchQuant("David Cox", statGeneal, "gradYear", 15)
head(select(CoxBranchYear, -DesNames), 10)

# We can demonstrate the getBranchQual() function by examining the qualitative variable ``thesis" across the ``descendant" branches of the academic statistician David Cox
v1 = "David Cox"; geneal = statGeneal; colName = "thesis"; gen = 15
rExpr = "grepl('(?i)Stochastic', geneal$colName)"
CoxBranchStochastic <- getBranchQual(v1, geneal, colName, rExpr, gen)
head(select(CoxBranchStochastic, -DesNames))

# It may be interesting to examine the school that is represented the most among all descendants of David Cox. Below, we determine what school this is.
desDC <- getDescendants("David Cox", statGeneal, 15)
tableDC <- table(statGeneal[match(desDC$label, statGeneal$child), ]$school)
tail(sort(tableDC), 5)

# We can now determine which of the branches from the 42 ``children" of David Cox have the largest proportion of ``descendants" graduating from the University of London.
colName = "school"
rExpr = "geneal$colName=='University of London'"
DCBranchUL <- getBranchQual(v1, geneal, colName, rExpr, gen)
head(select(DCBranchUL, -DesNames))

# We show that 34 of the ``descendants" of David Cox that graduated from the University of London were direct ``children" of him
DCChild <- statGeneal[match(getChild("David Cox", statGeneal), statGeneal$child), ]
sum(DCChild$school == "University of London")
