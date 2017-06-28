#' Parallel coordinates plots of the paths through a random forest
#'
#' \code{rf_parcoor} produces parallel coordinates plots of the paths through a random forest. These visualisations are introduced and explained in our manuscript, a preprint of which is available from arXiv at \url{https://arxiv.org/abs/1706.08702}.
#' 
#' @param all.paths.out the result of supplying a random forest to \code{\link{rf_pathfiner}}.
#' @param plot if \code{TRUE} a \code{\link[ggplot2]{ggplot2}} plot object is returned that is the parallel coordinates plot. If \code{FALSE} the dataframe is returned from which a parallel coordinates plot can be produced with \code{ggplot2} (see examples below).
#' @param all.nodes If \code{TRUE} the parallel coordinates plot depicts all nodes on all paths through the random forest. If \code{FALSE} the parallel coordinates plot depicts the first \code{plot.x.nodes} nodes along all paths through the random forest.
#' @param plot.title a character string specifying the title to display on the plot.
#' @param plot.x.nodes the number of nodes along all paths to represent with parallel vertical axes
#' @param colour.scale.log If \code{TRUE} the colour scale for the path selection frequencies uses a logarithmic scale. If \code{FALSE} the colour bar does not use a logarithmic scale.
#' @param colour.scale.lim The upper bound of the colour scale (if \code{colour.scale.log = TRUE} supply the logarithm of the upper bound desired to this argument).
#' @param grey.scale If \code{TRUE} a grey scale is used in place of a colour scale to represent the path selection frequencies. If \code{FALSE} a colour scale is used to represent the path selection frequencies.
#' @seealso \code{\link{rf_pathfinder}}
#' @examples
#' # example 1:
#' library(mlbench)
#' data(Satellite)
#' library(randomForest)
#' rf.1 <- randomForest(classes ~ ., data = Satellite, mtry = 8, keep.forest = TRUE, ntree = 25, importance = TRUE)
#' rf.1.all.paths <- rf_pathfinder(rf = rf.1)
#' rf.pc <- rf_parcoor(all.paths.out = rf.1.all.paths, plot = TRUE, all.nodes = TRUE, plot.title = '', grey.scale = FALSE)
#' rf.pc
#' # custom ordering of covariates on the vertical axis:
#' rf.pc + ggplot2::ylim(paste(c('Terminus', paste('x',1:36,sep = '.'))))
#'
#' # grey scale version:
#' rf.pc.grey <- rf_parcoor(all.paths.out = rf.1.all.paths, plot = TRUE, all.nodes = TRUE, plot.title = '', grey.scale = TRUE)
#' rf.pc.grey
#'
#' # interactive version with plotly (should work once geom_GeomCurve() is implemented in plotly)
#' # library(plotly)
#' # ggplotly(p = p1)
#' 
#' # example 2:
#' data(iris)
#' library(randomForest)
#' rf.2 <- randomForest(Species ~ ., data = iris, mtry = 2, keep.forest = TRUE, ntree = 100, importance = TRUE)
#' rf.2.all.paths <- rf_pathfinder(rf = rf.2)
#' rf.pc.2 <- rf_parcoor(all.paths.out = rf.2.all.paths, plot = TRUE, all.nodes = TRUE, plot.title = '', grey.scale = TRUE)
#' rf.pc.2
#' 
#' @export
rf_parcoor <- function(all.paths.out, plot = TRUE, all.nodes = TRUE, plot.title = '', plot.x.nodes = 7, colour.scale.log = TRUE, colour.scale.lim = 9, grey.scale = TRUE){
  n.a.c <- purrr::map_chr(.x = strsplit(x = c(unique(all.paths.out$Axis.Number)), split = 'X'), 2)
  max.h.grad <- max(as.numeric(n.a.c))
  all.paths.out$Axis.Number.F <- factor(x = all.paths.out$Axis.Number, levels = paste('X', 1:max.h.grad, sep = ''), ordered = TRUE)  
  all.paths.out %>%
    dplyr::select(Count, Seg.ID, Axis.Number.F, Node.ID) -> Plot.tb  
  Plot.tb %>%
    dplyr::group_by(Seg.ID) %>%
      dplyr::arrange(Axis.Number.F) %>%
        dplyr::mutate(Pairs = c('First', 'Second')) %>% 
          dplyr::ungroup() %>%
            dplyr::arrange(Seg.ID) -> out.2
  out.2 %>%
    dplyr::filter(Pairs == 'First') %>%
      dplyr::select(Count, Seg.ID, First.Axis.Number.F = Axis.Number.F, First.Node.ID = Node.ID) -> First.all.tb
  out.2 %>%
    dplyr::filter(Pairs == 'Second') %>%
      dplyr::select(Count, Seg.ID, Second.Axis.Number.F = Axis.Number.F, Second.Node.ID = Node.ID) -> Second.all.tb
  All.tb <- dplyr::left_join(x = First.all.tb, y = Second.all.tb, by = c('Seg.ID', 'Count'))
  All.df <- data.frame(All.tb) 
  node.names <- unique(unique(All.df$First.Node.ID), unique(All.df$Second.Node.ID))
  covar.names <- sort(grep(pattern = '^Terminus$', x = node.names, value = TRUE, invert = TRUE))
  All.df$First.Node.ID.F <- factor(x = All.df$First.Node.ID, levels = c('Terminus', covar.names, ordered = TRUE))    
  All.df$Second.Node.ID.F <- factor(x = All.df$Second.Node.ID, levels = c('Terminus', covar.names, ordered = TRUE))  
  All.df %>%
    dplyr::arrange(Count) -> Plot.tb
  XtoNode <- function(input){
    return(gsub(pattern = 'X', replacement = 'Node.', x = input))
  }
  Plot.tb$First.Node.F <- forcats::fct_relabel(f = Plot.tb$First.Axis.Number.F, fun = XtoNode)
  Plot.tb$Second.Node.F <- forcats::fct_relabel(f = Plot.tb$Second.Axis.Number.F, fun = XtoNode)
  h.grad.order <- paste('Node', 1:max.h.grad, sep = '.')  
  if(plot == TRUE){   
    if(colour.scale.log == TRUE){
      p1 <- ggplot2::ggplot(data = Plot.tb, ggplot2::aes(x = First.Node.F, xend = Second.Node.F, y = First.Node.ID.F, yend = Second.Node.ID.F, colour = log(Count))) + ggplot2::xlim(h.grad.order)
    } else{
       p1 <- ggplot2::ggplot(data = Plot.tb, ggplot2::aes(x = First.Node.F, xend = Second.Node.F, y = First.Node.ID.F, yend = Second.Node.ID.F, colour = Count)) + ggplot2::xlim(paste(levels(all.paths.out$Axis.Number.F)))
    }    
    if(grey.scale == TRUE){
      p1.all <- p1 + ggplot2::geom_curve(curvature = 0.05) + ggplot2::scale_colour_gradient(limits = c(0,colour.scale.lim), na.value = 'red', high = 'black', low = 'white') + ggplot2::labs(x = '', y = 'Covariate', title = plot.title) + ggplot2::theme_bw()
    } else{
        p1.all <- p1 + ggplot2::geom_curve(curvature = 0.05) + viridis::scale_colour_viridis(limits = c(0,colour.scale.lim), na.value = 'red') + ggplot2::labs(x = '', y = 'Covariate', title = plot.title) + ggplot2::theme_dark()
    }    
    if(all.nodes == TRUE){    
      return(p1.all + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust = 1)))
    } else{    
        p1.x.nodes <- p1.all + ggplot2::xlim(paste('Node', 1:plot.x.nodes,sep = '.')) + ggplot2::labs(x = '', y = 'Covariate', title = plot.title) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust = 1))
        return(p1.x.nodes)    
    }  
   } else{    
       return(Plot.tb)    
   }  
}
