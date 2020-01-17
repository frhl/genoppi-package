#' @name search_scatter
#' @rdname search_scatter
#' @title Search scatter plot
#' @author April/Frederik
#' @family search
#' @export

search_scatter <- function(p, found){
  if(nrow(found)==0){
    p
  } else{
    p <- add_markers(p, data = found, x = ~rep1, y = ~rep2,
                     marker = list(color = "#f7f4f9", size = 10, line = list(width=1.3, color = "#3f007d")),
                     textposition = ~ifelse(logFC>0, "middle right", "middle left"), textfont = list(color='black', size = 10),
                     hoverinfo="text+x+y", text = ~paste(gene), showlegend = FALSE)
  }
}

#' @rdname search_scatter
search_scatter_gg <- function(p, found){
  if(nrow(found)==0){
    p
  } else{
    p <- p + geom_point(data = found, mapping=aes(x = rep1, y = rep2), size = 2, alpha = 1, colour = "red") +
      geom_point(data = found, mapping=aes(x = rep1, y = rep2), size = 2, alpha = 1, colour = "black", shape=1) +
      geom_text_repel(data = found, aes(label = gene),
                      arrow = arrow(length = unit(0.5, 'npc')), box.padding = unit(0.15, "lines"),
                      point.padding = unit(0.2, "lines"), color = "black", size=2)
  }
  p
}
