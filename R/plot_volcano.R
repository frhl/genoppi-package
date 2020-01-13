#' @name volcano
#' @rdname plot_volcano
#' @title Volcano plots for proteomics
#' @description Will generate a classic volcano plot. \code{plot_volcano} is 
#' intented for use in R, whereas the remaining functons are used in the shiny
#' application or intented for the user that requires additional control of 
#' plotting.
#' @author April/Frederik
#' @family plotting
#' @examples \dontrun{
#' # artificial data to illustrate how to use.
#' data <- data.frame(gene=LETTERS, rep1 = runif(1:26), rep2=rnorm(1:26))
#' result = calculate_moderated_ttest(data)
#' plot_volcano(result, bait = 'A', main = 'randomly generated data points')
#' }
#' @export

plot_volcano <- function(d, bait = NULL, main = NULL, sub=NULL){
  
  require(ggplot2)
  require(ggrepel)
  
  ## inital check
  cnames = colnames(d)
  if ('logFC' %nin% cnames) stop('significant not in column names')
  if ('pvalue' %nin% cnames) stop('significant not in column names')
  if ('significant' %in% cnames){
    total <- dim(d)[1]; significant <- sum(d$significant==TRUE)
    if (is.null(sub)) sub = paste0(significant, '/', total, ' signifcant')
  } else d$significant <- TRUE

  # start volcano plot
  p <- ggplot(d, aes(x = logFC, y = -log10(pvalue))) +
    geom_hline(yintercept=0, color="grey") + geom_vline(xintercept=0, color="grey") +
    xlab(bquote(log[2]*"(Fold change)")) + ylab(bquote(-log[10]*"("*italic(.("P"))*"-value)")) +
    # plot all proteins (green = significant, blue = not significant)
    geom_point(alpha=0.5, size=1.5, color=ifelse(d$significant, "springgreen3", "royalblue2")) +
    # label bait (red = signficant, orange = not significant)
    geom_point(subset(d, gene==bait & significant), mapping=aes(x=logFC, y=-log10(pvalue)), size=2, color="red") + 
    geom_point(subset(d, gene==bait & !significant), mapping=aes(x=logFC, y=-log10(pvalue)), size=2, color="orange") +
    geom_point(subset(d, gene==bait), mapping=aes(x=logFC, y=-log10(pvalue)), size=2, color="black", shape=1) +	
    geom_text_repel(subset(d, gene==bait), mapping=aes(label=gene), arrow=arrow(length=unit(0.015, 'npc')),
                    box.padding=unit(0.15, "lines"), point.padding=unit(0.2, "lines"), color="black", size=3) +
    labs(title = main, subtitle = sub) + theme_genoppi()
  p
}


#' @rdname plot_volcano
plot_volcano_qc_gg <- function(d){
  p <- ggplot(data = d, aes(x = logFC, y = -log10(pvalue), text = gene)) +
    geom_point(alpha = 0.5, size = 1.5, colour = d$col) +
    xlab("log2FC") + ylab("-log10(P)") + theme_genoppi()
  p
}


#' @rdname plot_volcano
plot_volcano_qc_gg_1 <- function(data, data1, col, col1, logfc1, logfc2, pval){
  d <- rbind(data, data1)
  p <- ggplot(data = d, aes(x = logFC, y = -log10(pvalue), text=sprintf("Gene: %s<br>FDR: %s", gene, FDR))) +
    geom_point(data = data1, aes(x = logFC, y = -log10(pvalue)),alpha = 0.5, size = 1.5, colour = col1) +
    geom_point(data = data, aes(x = logFC, y = -log10(pvalue)),alpha = 0.5, size = 1.5, colour = col) +
    geom_vline(xintercept = logfc1, size = 0.5, linetype = "dotted", colour = "#737373") +
    geom_vline(xintercept = logfc2, size = 0.5, linetype = "dotted", colour = "#737373") +
    geom_hline(yintercept = pval, size = 0.5, linetype = "dotted", colour = "#737373") +
    # xlab("log2FC") + ylab("-log10(P)") +
    theme_minimal() + theme_genoppi()
  p
}

#' @rdname plot_volcano
plot_volcano_exac_gg <- function(b, a, n){
  d <- rbind(b, a, n)
  p <- ggplot(data = d, aes(x = logFC, y = -log10(pvalue), text = gene)) +
    geom_point(data = b, alpha = 0.5, size = 1.5, colour = "#66c2a5") +
    geom_point(data = a, alpha = 0.5, size = 1.5, colour = "#fc8d62") +
    geom_point(data = n, alpha = 0.5, size = 1.5, colour = "#8da0cb") +
    xlab(bquote(log[2]*"(Fold change)")) + ylab(bquote(-log[10]*"("*italic(.("P"))*"-value)")) +
    theme_minimal() + + theme_genoppi()
  p
}


#volcano - opacity = 1 colorscale
#' @rdname plot_volcano
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

#' @rdname plot_volcano
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
#' @rdname plot_volcano
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
#' @rdname plot_volcano
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
#' @rdname plot_volcano
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
#' @rdname plot_volcano
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
