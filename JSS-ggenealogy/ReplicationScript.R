# Load and examine the structure of the example soybean genealogy dataset (called sbGeneal)
rm(list=)
library("devtools")
load_all("SourceCode")
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
pathTN <- getPath("Tokyo", "Narow", sbIG, sbGeneal)
pathTN

# Plot returned path
plotPath(pathTN)

# Do the same for a different pair of two labels of interest ("Bedford" and "Zane"). First, we can determine the years these two labels were identified
getYear("Bedford", sbGeneal)
getYear("Zane", sbGeneal)

# Next, we can determine the shortest path of parent-child relationships between these two labels of interest and plot it.
pathBZ <- getPath("Bedford", "Zane", sbIG, sbGeneal)
plotPath(pathBZ, fontFace = 2)

# In the previous section, we obtained the shortest path between the the pair of labels "Tokyo" and "Narow" and saved it as a variable pathTN. Here, we can plot that path superimposed over all labels in the example soybean genealogy dataset.
plotPathOnAll(pathTN, sbGeneal, sbIG, binVector = 1:3, pathEdgeCol = "red", nodeSize = 2.5, pathNodeSize = 4) + ggplot2::theme(axis.text = ggplot2::element_text(size = 12), axis.title = ggplot2::element_text(size = 12))

# We can repeat this process, only now instead of setting the binVector variable to 1:3 (as we did earlier), we can set it to 1:6.
plotPathOnAll(pathTN, sbGeneal, sbIG, binVector = 1:6, pathEdgeCol = "seagreen2", nodeSize = 1, pathNodeSize = 3)

# As is explained in the article, only the top part of Figure 6 (the figure from this section) is produced by ggenealogy code. In contrast, the bottom part of Figure 6 was produced by tools outside of ggenealogy for didactic purposes. Below, we recreate the top part of Figure 6, which was to generate a plot of the ancestors and descendants of the label Lee.
plotAncDes("Lee", sbGeneal, mAnc = 6, mDes = 6, vCol = "blue")

# We can plot the distance matrix for a set of 10 varieties.
varieties <- c("Brim", "Bedford", "Calland", "Dillon", "Hood", "Narow", "Pella", "Tokyo", "Young", "Zane")
plotDegMatrix(varieties, sbIG, sbGeneal, "Variety", "Variety", "Degree") + ggplot2::scale_fill_continuous(low = "white", high = "darkgreen") + ggplot2::theme(legend.title = ggplot2::element_text(size = 15), legend.text = ggplot2::element_text(size = 15))

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

# After doing so, we can now determine the shortest path between Sir David Cox and Petra Buzkova, and plot it.
plotPath(pathCB, fontFace = 4) + ggplot2::theme(axis.text = ggplot2::element_text(size = 10), axis.title = ggplot2::element_text(size = 10)) + ggplot2::scale_x_continuous(expand = c(.1, .2))

# We can now superimpose this shortest path between Sir David Cox and Petra Buzkova across the entire genealogical structure.
plotPathOnAll(pathCB, statGeneal, statIG, binVector = 1:200) + ggplot2::theme(axis.text = ggplot2::element_text(size = 8), axis.title = ggplot2::element_text(size = 8)) + ggplot2::scale_x_continuous(expand = c(.1, .2))

# We notice, however, that we cannot read the text of the nodes on the path of interest. To solve this problem, we can create the same plot, only now specifying that any nodes that are not on our path of interest are deemphasized with smaller text.
plotPathOnAll(pathCB, statGeneal, statIG, binVector = 1:200, nodeSize = .5, pathNodeSize = 2.5, nodeCol = "darkgray", edgeCol = "lightgray") + ggplot2::theme(axis.text = ggplot2::element_text(size = 8), axis.title = ggplot2::element_text(size = 8)) + ggplot2::scale_x_continuous(expand = c(.1, .2))

# Even though we can now read the text labels on the path of interest, we lost the ability to read labels that are not on our path of interest. At this point, we can keep the best of both worlds: We can create a plot that demphasizes the nodes that are not on the path of interest by assigning them small text font size, but incoroporate an interactive function so that we can hover over these non-path nodes if we wish to obtain their label information. This is the plot we used and interacted with to create the video embedded in Figure 12.
plotPathOnAll(pathCB, statGeneal, statIG, binVector = 1:200, nodeSize = .5, pathNodeSize = 2.5, nodeCol = "darkgray", edgeCol = "lightgray", animate = TRUE)
