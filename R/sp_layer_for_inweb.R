
sp_layer_for_inweb <- function(p, d_in){
  p <- add_markers(p, data = d_in, x = ~rep1, y= ~rep2,
                   marker = list(color = "#ffff33", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                   mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                   text = ~paste(gene), textposition = ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
                   name = "InWeb")
}

sp_layer_for_inweb_no_text <- function(p, d_in){
  p <- add_markers(p, data = d_in, x = ~rep1, y= ~rep2,
                   marker = list(color = "#ffff33", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                   mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                   name = "InWeb")
}

sp_layer_for_inweb_cbf <- function(p, d_in){
  p <- add_markers(p, data = d_in, x = ~rep1, y= ~rep2,
                   marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                   mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                   text = ~paste(gene), textposition = ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
                   name = "InWeb")
}

sp_layer_for_inweb_cbf_no_text <- function(p, d_in){
  p <- add_markers(p, data = d_in, x = ~rep1, y= ~rep2,
                   marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                   mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                   name = "InWeb")
}
