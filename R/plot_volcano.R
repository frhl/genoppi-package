
#volcano - gg

plot_volcano_qc_gg <- function(d){
  p <- ggplot(data = d, aes(x = logFC, y = -log10(pvalue), text = gene)) +
    geom_point(alpha = 0.5, size = 1.5, colour = d$col) +
    xlab("log2FC") + ylab("-log10(P)") +
    theme_minimal() +
    theme(axis.text.x = element_text(size=7),
          axis.title.x=element_text(size=8),
          axis.text.y=element_text(size=7),
          axis.title.y=element_text(size=8),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(1,1,1,1), "pt"))
  p
}

plot_volcano_qc_gg_1 <- function(data, data1, col, col1, logfc1, logfc2, pval){
  d <- rbind(data, data1)
  p <- ggplot(data = d, aes(x = logFC, y = -log10(pvalue), text=sprintf("Gene: %s<br>FDR: %s", gene, FDR))) +
    geom_point(data = data1, aes(x = logFC, y = -log10(pvalue)),alpha = 0.5, size = 1.5, colour = col1) +
    geom_point(data = data, aes(x = logFC, y = -log10(pvalue)),alpha = 0.5, size = 1.5, colour = col) +
    geom_vline(xintercept = logfc1, size = 0.5, linetype = "dotted", colour = "#737373") +
    geom_vline(xintercept = logfc2, size = 0.5, linetype = "dotted", colour = "#737373") +
    geom_hline(yintercept = pval, size = 0.5, linetype = "dotted", colour = "#737373") +
    # xlab("log2FC") + ylab("-log10(P)") +
    theme_minimal() +
    theme(axis.text.x = element_text(size=7),
          axis.title.x=element_text(size=8),
          axis.text.y=element_text(size=7),
          axis.title.y=element_text(size=8),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(1,1,1,1), "pt"),
          aspect.ratio=1)
  p
}

plot_volcano_exac_gg <- function(b, a, n){
  d <- rbind(b, a, n)
  p <- ggplot(data = d, aes(x = logFC, y = -log10(pvalue), text = gene)) +
    geom_point(data = b, alpha = 0.5, size = 1.5, colour = "#66c2a5") +
    geom_point(data = a, alpha = 0.5, size = 1.5, colour = "#fc8d62") +
    geom_point(data = n, alpha = 0.5, size = 1.5, colour = "#8da0cb") +
    xlab(bquote(log[2]*"(Fold change)")) + ylab(bquote(-log[10]*"("*italic(.("P"))*"-value)")) +
    theme_minimal() +
    theme(axis.text.x = element_text(size=7),
          axis.title.x=element_text(size=8),
          axis.text.y=element_text(size=7),
          axis.title.y=element_text(size=8),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(1,1,1,1), "pt"))
  p
}


#volcano - opacity = 1 colorscale
plot_volcano_qc <- function(d){
  p <- plot_ly(showlegend = FALSE, width = 550, height = 550)
  for(i in nrow(d)){
    p <- add_markers(p, data = d, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(size = 7, cmin = 0, cmax = 1, color = ~col, line = list(width=0.2, color = "grey89")),
                     opacity = 0.9,
                     text = ~paste0(all_gene_names, ", FDR=", signif(FDR, digits = 3)), hoverinfo = "text", name = "pull down")
  }
  p
}

plot_volcano_user_color <- function(n_exist, y_exist){
  p <- plot_ly(showlegend = T, width = 550, height = 550)
  p <- add_markers(p, data = n_exist, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#f7f7f7"),
                   opacity = 0.8,
                   text = ~paste(gene), hoverinfo = "text", name = paste0("not in data (", nrow(df$no_exist), ")"))
  for(i in nrow(y_exist)){
    p <- add_markers(p, data = y_exist, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(size = 7, cmin = 0, cmax = 1, color = ~col, line = list(width=0.2, color = "grey89")),
                     opacity = 0.9,
                     text = ~paste0(gene, ", FDR=", signif(FDR, digits = 3)), hoverinfo = "text") #, name = "pull down"
  }
  p
}

#volcano - lower opacity to highlight different layers
plot_volcano_multiple_cond <- function(d){
  p <- plot_ly(showlegend = FALSE, height = 320, width = 320) #
  for(i in nrow(d)){
    p <- add_markers(p, data = d, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(size = 7, cmin = 0, cmax = 1, color = ~col, line = list(width=0.2, color = "grey89")),
                     opacity = 0.8,
                     text = ~paste0(gene, ", FDR=", signif(FDR, digits = 3)), hoverinfo = "text", name = "pull down")
  }
  p
}

#volcano - lower opacity to highlight different layers WITH legend
plot_volcano_multiple_cond_for_goi <- function(d){
  p <- plot_ly(showlegend = TRUE, height = 350) #, height = 320
  for(i in nrow(d)){
    p <- add_markers(p, data = d, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(size = 7, cmin = 0, cmax = 1, color = ~col, line = list(width=0.2, color = "grey89")),
                     opacity = 0.8,
                     text = ~paste0(gene, ", FDR=", signif(FDR, digits = 3)), hoverinfo = "text", name = "pull down")
  }
  p
}

#volcano - exac colorscale
plot_volcano_exac <- function(b, a, n){
  p <- plot_ly(showlegend = T, width = 650, height = 550)
  p <- add_markers(p, data = b, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#66c2a5"),
                   opacity = 0.8,
                   text = ~paste(gene), hoverinfo = "text", name = paste0("pLI<0.9 (", nrow(b), ")"))
  p <- add_markers(p, data = a, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#fc8d62"),
                   opacity = 0.8,
                   text = ~paste(gene), hoverinfo = "text", name = paste0("pLI>=0.9 (", nrow(a), ")"))
  p <- add_markers(p, data = n, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#8da0cb"),
                   opacity = 0.8,
                   text = ~paste(gene), hoverinfo = "text", name = paste0("not in ExAC (", nrow(n), ")"))
}

#volcano - exac colorscale
plot_volcano_exac_multi <- function(b, a, n){
  p <- plot_ly(showlegend = T, width = 320, height = 390)
  p <- add_markers(p, data = b, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#66c2a5"),
                   opacity = 0.8,
                   text = ~paste(gene), hoverinfo = "text", name = paste0("pLI<0.9 (", nrow(b), ")"))
  p <- add_markers(p, data = a, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#fc8d62"),
                   opacity = 0.8,
                   text = ~paste(gene), hoverinfo = "text", name = paste0("pLI>=0.9 (", nrow(a), ")"))
  p <- add_markers(p, data = n, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#8da0cb"),
                   opacity = 0.8,
                   text = ~paste(gene), hoverinfo = "text", name = paste0("not in ExAC (", nrow(n), ")"))
}
