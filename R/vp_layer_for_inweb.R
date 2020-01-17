#' @name vp_layer_for_inweb
#' @rdname vp_layer_for_inweb
#' @title InWeb Layers for ggplot2
#' @author April/Frederik
#' @family layers
#' @export

vp_layer_for_inweb <- function(p, d_in, marker_col, lab){
  if(nrow(d_in)==0){
    validate(
      need(nrow(d_in)>0, "No overlapping InWeb interactors identified")
    )
  } else if(nrow(d_in)>0){
    if(lab == "yes_label"){
      p <- add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
                       marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                       mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                       text = ~paste(gene), textposition =  ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
                       name = "InWeb")
    } else{
      p <- add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
                       marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                       mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                       name = "InWeb")
    }
  }
}

#' @rdname vp_layer_for_inweb
vp_layer_for_inweb_sf_gg <- function(p, d_in, marker_col, lab){
  if(nrow(d_in)==0){
    p
  } else if(nrow(d_in)>0){
    p <- p + geom_point(data = d_in, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = marker_col) +
      geom_point(data = d_in, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=1) +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = d_in, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')),
                               color = "black", size=2)
    }
    p
  }
  p
}


#' @rdname vp_layer_for_inweb
vp_layer_for_inweb_cbf <- function(p, d_in, lab){
  if(nrow(d_in)==0){
    validate(
      need(nrow(d_in)>0, "No overlapping InWeb interactors identified")
    )
  } else if(nrow(d_in)>0){
    if(lab == "yes_label"){
      p <- add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
                       marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                       # mode = "markers",
                       mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                       text = ~paste(gene), textposition = ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
                       name = "InWeb")
    } else{
      p <- add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
                       marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                       # mode = "markers",
                       mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                       name = "InWeb")
    }
  }
}



#' @rdname vp_layer_for_inweb
vp_layer_for_inweb_sf <- function(p, d_in, marker_col){
  if(nrow(d_in)==0){
    p
  } else if(nrow(d_in)>0){
    p <- add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
                     marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                     mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                     text = ~paste(gene), textposition =  ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
                     name = "InWeb")
  }
}

#' @rdname vp_layer_for_inweb
vp_layer_for_inweb_no_text_sf <- function(p, d_in, marker_col){
  if(nrow(d_in)==0){
    p
  } else if(nrow(d_in)>0){
    p <- add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
                     marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                     mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                     name = "InWeb")
  }
}

#' @rdname vp_layer_for_inweb
vp_layer_for_inweb_cbf_sf <- function(p, d_in){
  if(nrow(d_in)==0){
    p
  } else if(nrow(d_in)>0){
    p <- add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                     # mode = "markers",
                     mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                     text = ~paste(gene), textposition = ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
                     name = "InWeb")
  }
}

#' @rdname vp_layer_for_inweb
vp_layer_for_inweb_cbf_sf_gg <- function(p, d_in, lab){
  if(nrow(d_in)==0){
    p
  } else if(nrow(d_in)>0){
    p <- p + geom_point(data = d_in, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "#ffffff") +
      geom_point(data = d_in, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=1) +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = d_in, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')),
                               color = "black", size=2)
    }
    p
  }
  p
}

#' @rdname vp_layer_for_inweb
vp_layer_for_inweb_cbf_no_text_sf <- function(p, d_in){
  if(nrow(d_in)==0){
    p
  } else if(nrow(d_in)>0){
    p <- add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                     # mode = "markers",
                     mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                     name = "InWeb")
  }
}
