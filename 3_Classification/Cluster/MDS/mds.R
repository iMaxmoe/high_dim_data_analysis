# References
# https://www.r-statistics.com/2016/01/multidimensional-scaling-with-r-from-mastering-data-analysis-with-r/

# Example 1
mtcars = read.csv("mtcars.csv", header = TRUE)
mds = cmdscale(dist(mtcars))
plot(mds, type = 'n', axes = TRUE, xlab = '', ylab = '')
text(mds[, 1], mds[, 2], mtcars[, 1])
title('MDS Results')

# Example 2
as.matrix(eurodist)[1:5, 1:5]
mds = cmdscale(eurodist)
plot(mds, type = 'n')
text(mds[, 1], mds[, 2], labels(eurodist))
library(ggplot2)
ggplot(as.data.frame(mds), aes(V1, -V2, label = rownames(mds))) +
 geom_text(check_overlap = TRUE) + theme_minimal() + xlab('') + ylab('') +
  ggtitle('European Cities')+ scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL)

as.matrix(UScitiesD)[1:5, 1:5]
mds = cmdscale(UScitiesD)
plot(mds, type = 'n')
text(mds[, 1], mds[, 2], labels(UScitiesD))
library(ggplot2)
ggplot(as.data.frame(mds), aes(-V1, -V2, label = rownames(mds))) +
  geom_text(check_overlap = TRUE) + theme_minimal() + xlab('') + ylab('')+
  ggtitle('The US Cities')+scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL)