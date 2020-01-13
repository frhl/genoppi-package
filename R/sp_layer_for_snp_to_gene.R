
sp_layer_for_snp_to_gene_sgl <- function(p, snp_sgl){
  p <- add_markers(p, data = snp_sgl, x = ~rep1, y= ~rep2,
                   marker = list(color = "#80cdc1", size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = "square"
                   mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
                   text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
                   name = "SGL gene")
}

sp_layer_for_snp_to_gene_sgl_no_text <- function(p, snp_sgl){
  p <- add_markers(p, data = snp_sgl, x = ~rep1, y= ~rep2,
                   marker = list(color = "#80cdc1", size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = "square"
                   mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
                   name = "SGL gene")
}

sp_layer_for_snp_to_gene_sgl_cbf <- function(p, snp_sgl){
  p <- add_markers(p, data = snp_sgl, x = ~rep1, y= ~rep2,
                   marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = "square"),
                   mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
                   text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
                   name = "SGL gene")
}

sp_layer_for_snp_to_gene_sgl_cbf_no_text <- function(p, snp_sgl){
  p <- add_markers(p, data = snp_sgl, x = ~rep1, y= ~rep2,
                   marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = "square"),
                   mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
                   name = "SGL gene")
}


sp_layer_for_snp_to_gene_mgl <- function(p, snp_mgl){
  p <- add_markers(p, data = snp_mgl, x = ~rep1, y= ~rep2,
                   marker = list(color = "#c2a5cf", size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = 2
                   mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
                   text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
                   name = "MGL gene")
}

sp_layer_for_snp_to_gene_mgl_no_text <- function(p, snp_mgl){
  p <- add_markers(p, data = snp_mgl, x = ~rep1, y= ~rep2,
                   marker = list(color = "#c2a5cf", size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = 2
                   mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
                   name = "MGL gene")
}

sp_layer_for_snp_to_gene_mgl_cbf <- function(p, snp_mgl){
  p <- add_markers(p, data = snp_mgl, x = ~rep1, y= ~rep2,
                   marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = 2),
                   mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
                   text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
                   name = "MGL gene")
}

sp_layer_for_snp_to_gene_mgl_cbf_no_text <- function(p, snp_mgl){
  p <- add_markers(p, data = snp_mgl, x = ~rep1, y= ~rep2,
                   marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = 2),
                   mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
                   name = "MGL gene")
}

sp_layer_for_snp_to_gene_none <- function(p, d){
  p <- add_markers(p, data = d, x = ~min(rep1), y= ~min(rep2),
                   marker = list(opacity = 0),
                   mode = "markers", legendgroup = "group3", hoverinfo = "none",
                   name = "no SNP to gene")
}

sp_layer_for_snp_to_gene_none_cbf <- function(p, d){
  p <- add_markers(p, data = d, x = ~min(rep1), y= ~min(rep2),
                   marker = list(opacity = 0),
                   mode = "markers", legendgroup = "group3", hoverinfo = "none",
                   name = "no SNP to gene")
}
