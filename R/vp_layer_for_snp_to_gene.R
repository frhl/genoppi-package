vp_layer_for_snp_to_gene_sgl <- function(p, snp_sgl, marker_col){
  if(nrow(snp_sgl)==0){
    p
  } else if(nrow(snp_sgl)>0){
    p <- add_markers(p, data = snp_sgl, x = ~logFC, y= ~-log10(pvalue),
                     marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = "square"
                     mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
                     text = ~paste(gene, snpid, sep = "<br>"), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
                     name = "SGL gene")
  }
  p
}

vp_layer_for_snp_to_gene_sgl_gg <- function(p, snp_sgl, marker_col, lab){
  if(nrow(snp_sgl)==0){
    p
  } else if(nrow(snp_sgl)>0){
    p <- p + geom_point(data = snp_sgl, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = marker_col) +
      geom_point(data = snp_sgl, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=1) +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = snp_sgl, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')),
                               color = "black", size=2)
    }
    p
  }
  p
}

vp_layer_for_snp_to_gene_sgl_no_text <- function(p, snp_sgl, marker_col){
  if(nrow(snp_sgl)==0){
    p
  } else if(nrow(snp_sgl)>0){
    p <- add_markers(p, data = snp_sgl, x = ~logFC, y= ~-log10(pvalue),
                     marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = "square"
                     mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
                     # text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
                     name = "SGL gene")
  }
  p
}

vp_layer_for_snp_to_gene_sgl_cbf <- function(p, snp_sgl){
  if(nrow(snp_sgl)==0){
    p
  } else if(nrow(snp_sgl)>0){
    p <- add_markers(p, data = snp_sgl, x = ~logFC, y= ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = "square"),
                     mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
                     text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
                     name = "SGL gene")
  }
  p
}

vp_layer_for_snp_to_gene_sgl_cbf_gg <- function(p, snp_sgl, lab){
  if(nrow(snp_sgl)==0){
    p
  } else if(nrow(snp_sgl)>0){
    p <- p + geom_point(data = snp_sgl, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "#ffffff", shape=15) +
      geom_point(data = snp_sgl, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=0) +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = snp_sgl, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')),
                               color = "black", size=2)
    }
    p
  }
  p
}

vp_layer_for_snp_to_gene_sgl_cbf_no_text <- function(p, snp_sgl){
  if(nrow(snp_sgl)==0){
    p
  } else if(nrow(snp_sgl)>0){
    p <- add_markers(p, data = snp_sgl, x = ~logFC, y= ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = "square"),
                     mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
                     name = "SGL gene")
  }
  p
}

vp_layer_for_snp_to_gene_mgl <- function(p, snp_mgl, marker_col){
  if(nrow(snp_mgl)==0){
    p
  } else if(nrow(snp_mgl)>0){
    p <- add_markers(p, data = snp_mgl, x = ~logFC, y= ~-log10(pvalue),
                     marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = 2
                     mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
                     text = ~paste(gene, snpid, sep = "\n"), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
                     name = "MGL gene")
  }
  p
}

vp_layer_for_snp_to_gene_mgl_gg <- function(p, snp_mgl, marker_col, lab){
  if(nrow(snp_mgl)==0){
    p
  } else if(nrow(snp_mgl)>0){
    p <- p + geom_point(data = snp_mgl, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = marker_col) +
      geom_point(data = snp_mgl, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=1) +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = snp_mgl, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')),
                               color = "black", size=2)
    }
    p
  }
  p
}

vp_layer_for_snp_to_gene_mgl_no_text <- function(p, snp_mgl, marker_col){
  if(nrow(snp_mgl)==0){
    p
  } else if(nrow(snp_mgl)>0){
    p <- add_markers(p, data = snp_mgl, x = ~logFC, y= ~-log10(pvalue),
                     marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = 2
                     mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
                     name = "MGL gene")
  }
  p
}

vp_layer_for_snp_to_gene_mgl_cbf <- function(p, snp_mgl){
  if(nrow(snp_mgl)==0){
    p
  } else if(nrow(snp_mgl)>0){
    p <- add_markers(p, data = snp_mgl, x = ~logFC, y= ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = 2),
                     mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
                     text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
                     name = "MGL gene")
  }
  p
}

vp_layer_for_snp_to_gene_mgl_cbf_gg <- function(p, snp_mgl, lab){
  if(nrow(snp_mgl)==0){
    p
  } else if(nrow(snp_mgl)>0){
    p <- p + geom_point(data = snp_mgl, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=23, fill="#ffffff") +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = snp_mgl, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')),
                               color = "black", size=2)
    }
    p
  }
  p
}

vp_layer_for_snp_to_gene_mgl_cbf_no_text <- function(p, snp_mgl){
  if(nrow(snp_mgl)==0){
    p
  } else if(nrow(snp_mgl)>0){
    p <- add_markers(p, data = snp_mgl, x = ~logFC, y= ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = 2),
                     mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
                     name = "MGL gene")
  }
  p
}

vp_layer_for_snp_to_gene_none <- function(p, d){
  p <- add_markers(p, data = d, x = 0, y= ~-log10(max(pvalue)),
                   marker = list(opacity = 0),
                   mode = "markers", legendgroup = "group3", hoverinfo = "none",
                   name = "no SNP to gene")
}

vp_layer_for_snp_to_gene_none_cbf <- function(p, d){
  p <- add_markers(p, data = d, x = 0, y= ~-log10(max(pvalue)),
                   marker = list(opacity = 0),
                   mode = "markers", legendgroup = "group3", hoverinfo = "none",
                   name = "no SNP to gene")
}
