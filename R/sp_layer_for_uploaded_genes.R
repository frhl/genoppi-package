
sp_layer_for_uploaded_genes <- function(p, d_g2s){
  p <- add_markers(p, data = d_g2s, x = ~rep1, y= ~rep2,
                   marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7),
                   color = ~factor(.id), colors = "Blues",
                   mode = "markers+text", hoverinfo = "text",
                   text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
}

sp_layer_for_uploaded_genes_gg <- function(p, d_g2s, usr_palette, lab){
  if(nrow(d_g2s)==0){
    validate(
      need(nrow(d_g2s)>0, "No overlapping GOIs identified")
    )
  } else if(nrow(d_g2s)>0){
    p <- p + geom_point(data = d_g2s, mapping=aes(x = rep1, y = rep2, colour = factor(.id)), size = 2, alpha = 1) +
      geom_point(data = d_g2s, mapping=aes(x = rep1, y = rep2), size = 2, alpha = 1, colour = "black", shape=1) +
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

sp_layer_for_uploaded_genes_cbf_gg <- function(p, d_g2s, lab){
  if(nrow(d_g2s)==0){
    validate(
      need(nrow(d_g2s)>0, "No overlapping GOIs identified")
    )
  } else if(nrow(d_g2s)>0){
    p <- p + geom_point(data = d_g2s, mapping=aes(x = rep1, y = rep2, colour = factor(.id)), size = 2, alpha = 1) +
      geom_point(data = d_g2s, mapping=aes(x = rep1, y = rep2), size = 2, alpha = 1, colour = "black", shape=1) +
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


sp_layer_for_uploaded_genes_no_text <- function(p, d_g2s){
  p <- add_markers(p, data = d_g2s, x = ~rep1, y= ~rep2,
                   marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7),
                   color = ~factor(.id), colors = "Blues",
                   mode = "markers+text", hoverinfo = "text")
}

sp_layer_for_uploaded_genes_cbf <- function(p, d_g2s){
  p <- add_markers(p, data = d_g2s, x = ~rep1, y= ~rep2,
                   marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7, symbol = 15),
                   color = ~factor(.id),
                   mode = "markers+text", hoverinfo = "text",
                   text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
}

sp_layer_for_uploaded_genes_cbf_no_text <- function(p, d_g2s){
  p <- add_markers(p, data = d_g2s, x = ~rep1, y= ~rep2,
                   marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7, symbol = 15),
                   color = ~factor(.id),
                   mode = "markers+text", hoverinfo = "text")
}

sp_layer_for_uploaded_genes_none <- function(p, d){
  p <- add_markers(p, data = d, x = ~min(rep1), y= ~min(rep2),
                   marker = list(opacity = 0),
                   mode = "markers", legendgroup = "group2", hoverinfo = "none",
                   name = "no genes of interest")
}

sp_layer_for_uploaded_genes_none_cbf <- function(p, d){
  p <- add_markers(p, data = d, x = ~min(rep1), y= ~min(rep2),
                   marker = list(opacity = 0),
                   mode = "markers", legendgroup = "group2", hoverinfo = "none",
                   name = "no genes of interest")
}
