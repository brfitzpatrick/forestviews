# forestviews
## network flow visualisation of the paths through a random forest

Sankey diagrams and parallel coordinates plots of all paths from root to leaf nodes through the binary trees that constitute a random forest.

### Installation:

Install the `devtools` package and use it to install `forestviews`:
```r
install.packages('devtools')
library('devtools')
install_github(repo = 'brfitzpatrick/forestviews')
```

### Example Usage:

Load the data and packages we will use:
```r
library(mlbench)
data(Satellite)
library(randomForest)
library(networkD3)
library(forestviews)
```
Fit a random forest:
```r
rf.1 <- randomForest(classes ~ ., data = Satellite, mtry = 8, keep.forest = TRUE, ntree = 25, importance = TRUE)
```

Calculate all paths through the random forest:
```r
rf.1.all.paths <- rf_pathfinder(rf = rf.1, report.progress = FALSE, nodeeval=5)
```

#### Sankey Diagram:

Convert these paths to a d3network:
```r
nd3 <- rf_sankey(all.paths.out = rf.1.all.paths, all.nodes = FALSE, plot.node.lim = 6)
```

Plot the network as an interactive Sankey Diagram (this plot will open in your web browser):
```r
sankeyNetwork(Links = nd3$links, Nodes = nd3$nodes , Source = 'source', Target = 'target', Value = 'value', NodeID = 'name', units = 'Count', fontSize = 12, nodeWidth = 30, NodeGroup = NULL)
```

#### Parallel Coordinates Plots:

Simple usage:
```r
rf.pc <- rf_parcoor(all.paths.out = rf.1.all.paths, plot = TRUE, all.nodes = TRUE, plot.title = '', grey.scale = FALSE)
rf.pc
```

Grey scale version:
```r
rf.pc.grey <- rf_parcoor(all.paths.out = rf.1.all.paths, plot = TRUE, all.nodes = TRUE, plot.title = '', grey.scale = TRUE)
rf.pc.grey
```

### Project Website

See the [project website](http://brfitzpatrick.github.io/forestviews) for an example of an interactive Sankey diagram.

### Reference

The visualisation techniques implemented in `forestviews` are introduced in our manuscript on this topic.
A pre-print of this manuscript is available from [arXiv](https://arxiv.org/abs/1706.08702).