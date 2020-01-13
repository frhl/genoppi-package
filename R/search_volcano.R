#volcano - search/found highlight
search_volcano <- function(p, found){
  if(nrow(found)==0){
    p
  } else{
    p <- add_markers(p, data = found, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#f7f4f9", size = 10, line = list(width=1.3, color = "#3f007d")),
                     textposition = ~ifelse(logFC>0, "middle right", "middle left"), textfont = list(color='black', size = 10),
                     hoverinfo="text+x+y", text = ~paste(gene), showlegend = FALSE)
  }
  p
}

search_volcano_gg <- function(p, found){
  if(nrow(found)==0){
    p
  } else{
    p <- p +
      geom_point(data = found, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, fill = "#ffffb3", colour = "black", shape=21) +
      # geom_point(data = found, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=1) +
      geom_text_repel(data = found, aes(label = gene),
                      arrow = arrow(length = unit(0.5, 'npc')), box.padding = unit(0.15, "lines"),
                      point.padding = unit(0.2, "lines"), color = "black", size=2)
  }
  p
}

search_volcano_gg_reg_label <- function(p, found){
  if(nrow(found)==0){
    p
  } else{
    p <- p +
      geom_point(data = found, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, fill = "#ffffb3", colour = "black", shape=21) +
      # geom_point(data = found, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=1) +
      geom_text(data = found, aes(label = gene), color = "black", size=2, position=position_jitter(1L))
  }
  p
}


