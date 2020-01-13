#' @name scatter
#' @rdname plot_scatter
#' @title Scatter plots for proteomics data
#' @description Make scatter
#' @author April/Frederik
#' @family statistics
#' @export

plot_scatter <- function(d){
  
  stop('still needs work..')
  
  require(ggplot2)
  require(ggrepel)
  
  # inital checks of input
  stopifnot(any(grepl('rep', colnames(d))))
  colRep <- as.vector(grepl('rep', colnames(d)) & unlist(lapply(d, is.numeric)))
  nRep <- sum(as.numeric(colRep))

  for (i in 1:(nRep-1)) {
    for (j in (i+1):nRep) {
      
      # set columns of replicates
      col1 <- paste("rep",i,sep="")
      col2 <- paste("rep",j,sep="")
      r <- cor(d[,col1],d[,col2]) # Pearson correlation
      temp_d <- data.frame(gene=d$gene,temp_rep1=d[,col1],temp_rep2=d[,col2],significant=d$significant)
      
      # start scatterplot
      p <- ggplot(temp_d, aes(x=temp_rep1, y=temp_rep2)) +
        
        # plot all proteins (green = significant, blue = not significant)
        geom_point(alpha=0.5, size=1.5, color=ifelse(d$significant, "springgreen3", "royalblue2")) +
        
        # label bait (red = signficant, orange = not significant)
        geom_point(subset(temp_d, gene==bait & significant), mapping=aes(x=temp_rep1, y=temp_rep2),size=2, color="red") + 
        geom_point(subset(temp_d, gene==bait & !significant), mapping=aes(x=temp_rep1, y=temp_rep2),size=2, color="orange") +
        geom_point(subset(temp_d, gene==bait), mapping=aes(x=temp_rep1, y=temp_rep2), size=2, color="black", shape=1) +	
        geom_text_repel(subset(temp_d, gene==bait), mapping=aes(label=gene),
                        arrow=arrow(length=unit(0.015, 'npc')), box.padding=unit(0.15, "lines"),
                        point.padding=unit(0.2, "lines"), color="black", size=3) +
        
        # identity line, title (with correlation), theme
        geom_abline(intercept=0, slope=1, linetype="longdash", size=0.2) +
        labs(title = title, subtitle = paste("correlation:",format(r,digits=3))) + xlab(col1) + ylab(col2) +
        theme_bw() + theme_genoppi() + ggstamp()
      
      p
    }
  }
}

#' @rdname plot_scatter
plot_scatter_qc_gg <- function(d){
  p <- ggplot(data = d, aes(x = rep1, y = rep2, text = gene)) +
    geom_point(alpha = 0.5, size = 1.5, colour = d$col) +
    geom_abline(intercept = 0, slope = 1, linetype = "longdash", size=0.2) +
    xlab("rep1") + ylab("rep2") + theme_genoppi()
  p
}

#' @rdname plot_scatter
plot_scatter_exac_gg <- function(b, a, n){
  d <- rbind(b, a, n)
  p <- ggplot(data = d, aes(x = rep1, y = rep2, text = gene)) +
    geom_point(data = b, alpha = 0.5, size = 1.5, colour = "#66c2a5") +
    geom_point(data = a, alpha = 0.5, size = 1.5, colour = "#fc8d62") +
    geom_point(data = n, alpha = 0.5, size = 1.5, colour = "#8da0cb") +
    xlab("rep1") + ylab("rep2") +
    theme_minimal() + theme_genoppi()
  p
}

#' @rdname plot_scatter
#scatter - opacity = 1 colorscale
plot_scatter_qc <- function(orig, d){
  p <- plot_ly(showlegend = FALSE, width = 550, height = 550)
  p <- add_markers(p, x = 0, y = 0, opacity = 0)
  p <- add_lines(p, data = orig, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                 text = "x=y", hoverinfo = "text",
                 line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
  for(i in nrow(d)){
    p <- add_markers(p, data = d, x = ~rep1, y = ~rep2,
                     marker = list(size = 7, cmin = 0, cmax = 1, color = ~col, line = list(width=0.2, color = "grey89")),
                     opacity = 0.9,
                     text = ~paste0(gene, ", rep1=", round(rep1, digits = 2), ", rep2=", round(rep2, digits = 2)), hoverinfo = "text", name = "pull down")
  }
  p
}

#' @rdname plot_scatter
#scatter - lower opacity to highlight different layers
plot_scatter_multiple_cond <- function(orig, d){
  p <- plot_ly(showlegend = FALSE, width = 320, height = 320)
  p <- add_lines(p, data = orig, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                 text = "x=y", hoverinfo = "text",
                 line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
  for(i in nrow(d)){
    p <- add_markers(p, data = d, x = ~rep1, y = ~rep2,
                     marker = list(size = 7, cmin = 0, cmax = 1, color = ~col, line = list(width=0.2, color = "grey89")),
                     opacity = 0.8,
                     text = ~paste0(gene, ", rep1=", round(rep1, digits = 2), ", rep2=", round(rep2, digits = 2)), hoverinfo = "text", name = "pull down")
  }
  p
}

#' @rdname plot_scatter
#scatter - lower opacity to highlight different layers
plot_scatter_white <- function(orig){
  p <- plot_ly(data = orig, x = ~c(min(rep1), max(rep1)), y = 0, marker = list(color = "white"), hoverinfo = "none", showlegend = FALSE, width = 320, height = 320)
  p %>% layout(xaxis = list(fixedrange=TRUE), yaxis = list(fixedrange=TRUE))
}

#' @rdname plot_scatter
#scatter - exac colorscale
plot_scatter_exac <- function(orig, b, a, n){
  p <- plot_ly(showlegend = F, width = 550, height = 550)
  p <- add_lines(p, data = orig, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                 line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
  p <- add_markers(p, data = b, x = ~rep1, y = ~rep2,
                   marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#66c2a5"),
                   opacity = 0.7,
                   text = ~paste(gene), hoverinfo = "text") #, name = paste0("pLI<0.9 (", nrow(b), ")")
  p <- add_markers(p, data = a, x = ~rep1, y = ~rep2,
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#fc8d62"),
                   opacity = 0.7,
                   text = ~paste(gene), hoverinfo = "text") #, name = paste0("pLI>=0.9 (", nrow(a), ")")
  p <- add_markers(p, data = n, x = ~rep1, y = ~rep2,
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#8da0cb"),
                   opacity = 0.7,
                   text = ~paste(gene), hoverinfo = "text") #, name = paste0("not in ExAC (", nrow(n), ")")
}

#' @rdname plot_scatter
plot_scatter_exac_multi <- function(orig, b, a, n){
  p <- plot_ly(showlegend = F, width = 320, height = 320)
  p <- add_lines(p, data = orig, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                 line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
  p <- add_markers(p, data = b, x = ~rep1, y = ~rep2,
                   marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#66c2a5"),
                   opacity = 0.7,
                   text = ~paste(gene), hoverinfo = "text") #, name = paste0("pLI<0.9 (", nrow(b), ")")
  p <- add_markers(p, data = a, x = ~rep1, y = ~rep2,
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#fc8d62"),
                   opacity = 0.7,
                   text = ~paste(gene), hoverinfo = "text") #, name = paste0("pLI>=0.9 (", nrow(a), ")")
  p <- add_markers(p, data = n, x = ~rep1, y = ~rep2,
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#8da0cb"),
                   opacity = 0.7,
                   text = ~paste(gene), hoverinfo = "text") #, name = paste0("not in ExAC (", nrow(n), ")")
}
