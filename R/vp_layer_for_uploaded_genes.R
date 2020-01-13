
vp_layer_for_uploaded_genes_gg <- function(p, d_g2s, usr_palette, lab){
  if(nrow(d_g2s)==0){
    validate(
      need(nrow(d_g2s)>0, "No overlapping GOIs identified")
    )
  } else if(nrow(d_g2s)>0){
    p <- p + geom_point(data = d_g2s, mapping=aes(x = logFC, y = -log10(pvalue), colour = factor(.id)), size = 2, alpha = 1) +
      geom_point(data = d_g2s, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=1) +
      scale_color_brewer(name=" ", palette = usr_palette) +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = d_g2s, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')),
                               color = "black", size=2)
    }
    p
  }
}

vp_layer_for_uploaded_genes_cbf_gg <- function(p, d_g2s, lab){
  if(nrow(d_g2s)==0){
    validate(
      need(nrow(d_g2s)>0, "No overlapping GOIs identified")
    )
  } else if(nrow(d_g2s)>0){
    p <- p + geom_point(data = d_g2s, mapping=aes(x = logFC, y = -log10(pvalue), colour = factor(.id)), size = 2, alpha = 1) +
      geom_point(data = d_g2s, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=1) +
      scale_color_brewer(name=" ", palette = "Greys") +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = d_g2s, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')),
                               color = "black", size=2)
    }
    p
  }
}

vp_layer_for_uploaded_genes <- function(p, d_g2s){
  if(nrow(d_g2s)==0){
    validate(
      need(nrow(d_g2s)>0, "No overlapping GOIs identified")
    )
  } else if(nrow(d_g2s)>0){
    print(d_g2s)
    p <- add_markers(p, data = d_g2s, x = ~logFC, y= ~-log10(pvalue),
                     marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7), #symbol = 15,
                     color = ~factor(.id),
                     mode = "markers+text", hoverinfo = "text",
                     text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
  }
}

vp_layer_for_uploaded_genes_no_text <- function(p, d_g2s){
  if(nrow(d_g2s)==0){
    validate(
      need(nrow(d_g2s)>0, "No overlapping GOIs identified")
    )
  } else if(nrow(d_g2s)>0){
    p <- add_markers(p, data = d_g2s, x = ~logFC, y= ~-log10(pvalue),
                     marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7), #symbol = 15,
                     color = ~factor(.id),
                     mode = "markers+text", hoverinfo = "text")
  }
}

vp_layer_for_uploaded_genes_cbf <- function(p, d_g2s){
  if(nrow(d_g2s)==0){
    validate(
      need(nrow(d_g2s)>0, "No overlapping GOIs identified")
    )
  } else if(nrow(d_g2s)>0){
    p <- add_markers(p, data = d_g2s, x = ~logFC, y= ~-log10(pvalue),
                     marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7, symbol = 15),
                     color = ~factor(.id),
                     mode = "markers+text", hoverinfo = "text",
                     text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
  }
}

vp_layer_for_uploaded_genes_cbf_no_text <- function(p, d_g2s){
  if(nrow(d_g2s)==0){
    validate(
      need(nrow(d_g2s)>0, "No overlapping GOIs identified")
    )
  } else if(nrow(d_g2s)>0){
    p <- add_markers(p, data = d_g2s, x = ~logFC, y= ~-log10(pvalue),
                     marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7, symbol = 15),
                     color = ~factor(.id),
                     mode = "markers+text", hoverinfo = "text")
    # text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
  }
}

vp_layer_for_uploaded_genes_none <- function(p, d){
  p <- add_markers(p, data = d, x = 0, y= ~-log10(max(pvalue)),
                   marker = list(opacity = 0),
                   mode = "markers", legendgroup = "group1", hoverinfo = "text",
                   name = "no genes of interest")
}

vp_layer_for_uploaded_genes_none_cbf <- function(p, d){
  add_markers(p, data = d, x = 0, y= ~-log10(max(pvalue)),
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group2", hoverinfo = "none",
              name = "no genes of interest")
}
