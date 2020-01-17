#' @name compare_two_files
#' @rdname compare_two_files
#' @title Compare files
#' @author April/Frederik
#' @family infile
#' @export

compare_two_files_a <- function(orig, subset, overlaps){
  if(nrow(subset)==0 & nrow(overlaps)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps)>0){
    p <- plot_ly(showlegend = T, width = 300, height = 390)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(overlaps)>0){
      p <- add_markers(p, data = overlaps, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fdb462", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f12")
    }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#e41a1c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f1")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

#' @rdname compare_two_files
compare_two_files_a_scatter <- function(orig, subset, overlaps){
  if(nrow(subset)==0 & nrow(overlaps)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps)>0){
    p <- plot_ly(showlegend = T, width = 420, height = 390)
    p <- add_markers(p, data = orig, x = ~rep1, y = ~rep2,
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(overlaps)>0){
      p <- add_markers(p, data = overlaps, x = ~rep1, y = ~rep2,
                       marker = list(color = "#fdb462", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f12")
    }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~rep1, y = ~rep2,
                       marker = list(color = "#e41a1c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f1")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

#' @rdname compare_two_files
compare_two_files_b <- function(orig, subset, overlaps){
  if(nrow(subset)==0 & nrow(overlaps)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps)>0){
    p <- plot_ly(showlegend = T, width = 300, height = 390)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(overlaps)>0){
      p <- add_markers(p, data = overlaps, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fdb462", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f12")
    }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#ffff33", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f2")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

#' @rdname compare_two_files
compare_two_files_b_scatter <- function(orig, subset, overlaps){
  if(nrow(subset)==0 & nrow(overlaps)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps)>0){
    p <- plot_ly(showlegend = T, width = 420, height = 390)
    p <- add_markers(p, data = orig, x = ~rep1, y = ~rep2,
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(overlaps)>0){
      p <- add_markers(p, data = overlaps, x = ~rep1, y = ~rep2,
                       marker = list(color = "#fdb462", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f12")
    }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~rep1, y = ~rep2,
                       marker = list(color = "#ffff33", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f2")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

#' @rdname compare_two_files
compare_two_files_aa <- function(orig, subset, overlaps1, overlaps2, overlaps3){
  if(nrow(subset)==0 & nrow(overlaps1)==0 & nrow(overlaps2)==0 & nrow(overlaps3)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps1)>0 & nrow(overlaps2)>0 & nrow(overlaps3)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps1)>0 | nrow(overlaps2)>0 | nrow(overlaps3)>0){
    p <- plot_ly(showlegend = T, width = 300, height = 390)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#ef3b2c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f1")
    }
    if(nrow(overlaps1)>0){
      p <- add_markers(p, data = overlaps1, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fd8d3c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f12")
    }
    if(nrow(overlaps2)>0){
      p <- add_markers(p, data = overlaps2, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#8c6bb1", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f13")
    }
    if(nrow(overlaps3)>0){
      p <- add_markers(p, data = overlaps3, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f123")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

#' @rdname compare_two_files
compare_two_files_aa_scatter <- function(orig, subset, overlaps1, overlaps2, overlaps3){
  if(nrow(subset)==0 & nrow(overlaps1)==0 & nrow(overlaps2)==0 & nrow(overlaps3)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps1)>0 & nrow(overlaps2)>0 & nrow(overlaps3)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps1)>0 | nrow(overlaps2)>0 | nrow(overlaps3)>0){
    p <- plot_ly(showlegend = T, width = 300, height = 390)
    p <- add_markers(p, data = orig, x = ~rep1, y = ~rep2,
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~rep1, y = ~rep2,
                       marker = list(color = "#ef3b2c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f1")
    }
    if(nrow(overlaps1)>0){
      p <- add_markers(p, data = overlaps1, x = ~rep1, y = ~rep2,
                       marker = list(color = "#fd8d3c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f12")
    }
    if(nrow(overlaps2)>0){
      p <- add_markers(p, data = overlaps2, x = ~rep1, y = ~rep2,
                       marker = list(color = "#8c6bb1", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f13")
    }
    if(nrow(overlaps3)>0){
      p <- add_markers(p, data = overlaps3, x = ~rep1, y = ~rep2,
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f123")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

#' @rdname compare_two_files
compare_two_files_bb <- function(orig, subset, overlaps1, overlaps2, overlaps3){
  if(nrow(subset)==0 & nrow(overlaps1)==0 & nrow(overlaps2)==0 & nrow(overlaps3)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps1)>0 & nrow(overlaps2)>0 & nrow(overlaps3)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps1)>0 | nrow(overlaps2)>0 | nrow(overlaps3)>0){
    p <- plot_ly(showlegend = T, width = 300, height = 390)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#ffff33", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f2")
    }
    if(nrow(overlaps1)>0){
      p <- add_markers(p, data = overlaps1, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fd8d3c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f12")
    }
    if(nrow(overlaps2)>0){
      p <- add_markers(p, data = overlaps2, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#41ab5d", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f23")
    }
    if(nrow(overlaps3)>0){
      p <- add_markers(p, data = overlaps3, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f123")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

#' @rdname compare_two_files
compare_two_files_bb_scatter <- function(orig, subset, overlaps1, overlaps2, overlaps3){
  if(nrow(subset)==0 & nrow(overlaps1)==0 & nrow(overlaps2)==0 & nrow(overlaps3)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps1)>0 & nrow(overlaps2)>0 & nrow(overlaps3)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps1)>0 | nrow(overlaps2)>0 | nrow(overlaps3)>0){
    p <- plot_ly(showlegend = T, width = 300, height = 390)
    p <- add_markers(p, data = orig, x = ~rep1, y = ~rep2,
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~rep1, y = ~rep2,
                       marker = list(color = "#ffff33", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f2")
    }
    if(nrow(overlaps1)>0){
      p <- add_markers(p, data = overlaps1, x = ~rep1, y = ~rep2,
                       marker = list(color = "#fd8d3c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f12")
    }
    if(nrow(overlaps2)>0){
      p <- add_markers(p, data = overlaps2, x = ~rep1, y = ~rep2,
                       marker = list(color = "#41ab5d", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f23")
    }
    if(nrow(overlaps3)>0){
      p <- add_markers(p, data = overlaps3, x = ~rep1, y = ~rep2,
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f123")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

#' @rdname compare_two_files
compare_two_files_cc <- function(orig, subset, overlaps1, overlaps2, overlaps3){
  if(nrow(subset)==0 & nrow(overlaps1)==0 & nrow(overlaps2)==0 & nrow(overlaps3)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps1)>0 & nrow(overlaps2)>0 & nrow(overlaps3)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps1)>0 | nrow(overlaps2)>0 | nrow(overlaps3)>0){
    p <- plot_ly(showlegend = T, width = 300, height = 390)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#1f78b4", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f3")
    }
    if(nrow(overlaps1)>0){
      p <- add_markers(p, data = overlaps1, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#8c6bb1", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f13")
    }
    if(nrow(overlaps2)>0){
      p <- add_markers(p, data = overlaps2, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#41ab5d", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f23")
    }
    if(nrow(overlaps3)>0){
      p <- add_markers(p, data = overlaps3, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f123")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

#' @rdname compare_two_files
compare_two_files_cc_scatter <- function(orig, subset, overlaps1, overlaps2, overlaps3){
  if(nrow(subset)==0 & nrow(overlaps1)==0 & nrow(overlaps2)==0 & nrow(overlaps3)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps1)>0 & nrow(overlaps2)>0 & nrow(overlaps3)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps1)>0 | nrow(overlaps2)>0 | nrow(overlaps3)>0){
    p <- plot_ly(showlegend = T, width = 300, height = 390)
    p <- add_markers(p, data = orig, x = ~rep1, y = ~rep2,
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~rep1, y = ~rep2,
                       marker = list(color = "#1f78b4", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f3")
    }
    if(nrow(overlaps1)>0){
      p <- add_markers(p, data = overlaps1, x = ~rep1, y = ~rep2,
                       marker = list(color = "#8c6bb1", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f13")
    }
    if(nrow(overlaps2)>0){
      p <- add_markers(p, data = overlaps2, x = ~rep1, y = ~rep2,
                       marker = list(color = "#41ab5d", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f23")
    }
    if(nrow(overlaps3)>0){
      p <- add_markers(p, data = overlaps3, x = ~rep1, y = ~rep2,
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f123")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

#' @rdname compare_two_files
compare_two_files_pf_a <- function(orig, subset, overlaps){ #s_nga, , o_nga
  if(nrow(subset)==0 & nrow(overlaps)==0){ #nrow(s_nga)==0 &  & nrow(o_nga)==0
    validate(
      need(nrow(subset)>0& nrow(overlaps)>0, "No protein families identified") # & nrow(s_nga)>0 & nrow(o_nga)>0
    )
  } else if(nrow(subset)>0| nrow(overlaps)>0){ # | nrow(s_nga)>0  | nrow(o_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#e41a1c'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f1 unassigned")
    # }
    # if(nrow(o_nga)>0){
    #   p <- add_markers(p, data = o_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#fdb462'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f12 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#e41a1c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "none")
    }
    if(nrow(overlaps)>0){
      p <- add_markers(p, data = overlaps, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fdb462", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "none")
    }
  }
  p
}

#' @rdname compare_two_files
compare_two_files_pf_a_size <- function(orig, subset, overlaps, increase){ # s_nga, o_nga,
  if(nrow(subset)==0 & nrow(overlaps)==0){ #nrow(s_nga)==0 &  & nrow(o_nga)==0
    validate(
      need(nrow(subset)>0& nrow(overlaps)>0, "No protein families identified") # & nrow(s_nga)>0 & nrow(o_nga)>0
    )
  } else if(nrow(subset)>0| nrow(overlaps)>0){ # | nrow(s_nga)>0  | nrow(o_nga)>0
    print(head(orig))
    print(head(subset))
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     text = ~gene, hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#e41a1c'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f1 unassigned")
    # }
    # if(nrow(o_nga)>0){
    #   p <- add_markers(p, data = o_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#fdb462'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f12 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#e41a1c", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1,
                                     opacity = 0.8), color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(overlaps)>0){
      p <- add_markers(p, data = overlaps, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fdb462", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1,
                                     opacity = 0.8), color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}

#' @rdname compare_two_files
compare_two_files_pf_b <- function(orig, subset, overlaps){ #, s_nga , o_nga
  if(nrow(subset)==0 & nrow(overlaps)==0){ #nrow(s_nga)==0 &  & nrow(o_nga)==0
    validate(
      need(nrow(subset)>0& nrow(overlaps)>0, "No protein families identified") # & nrow(s_nga)>0 & nrow(o_nga)>0
    )
  } else if(nrow(subset)>0| nrow(overlaps)>0){ # | nrow(s_nga)>0  | nrow(o_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#ffff33'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f2 unassigned")
    # }
    # if(nrow(o_nga)>0){
    #   p <- add_markers(p, data = o_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#fdb462'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f12 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#ffff33", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(overlaps)>0){
      p <- add_markers(p, data = overlaps, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fdb462", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}

#' @rdname compare_two_files
compare_two_files_pf_b_size <- function(orig, subset, overlaps, increase){ #, s_nga, o_nga
  if(nrow(subset)==0 & nrow(overlaps)==0){ #nrow(s_nga)==0 &  & nrow(o_nga)==0
    validate(
      need(nrow(subset)>0& nrow(overlaps)>0, "No protein families identified") # & nrow(s_nga)>0 & nrow(o_nga)>0
    )
  } else if(nrow(subset)>0| nrow(overlaps)>0){ # | nrow(s_nga)>0  | nrow(o_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#ffff33'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f2 unassigned")
    # }
    # if(nrow(o_nga)>0){
    #   p <- add_markers(p, data = o_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#fdb462'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f12 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#ffff33", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1,
                                     opacity = 0.8), color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(overlaps)>0){
      p <- add_markers(p, data = overlaps, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fdb462", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1,
                                     opacity = 0.8), color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}

#' @rdname compare_two_files
compare_two_files_pf_aa <- function(orig, subset,o_12, o_13, o_123){ # s_nga, o_12_nga, o_13_nga, , o_123_nga
  if(nrow(subset)==0 & nrow(o_12)==0 & nrow(o_13)==0 #nrow(s_nga)==0 & nrow(o_12_nga)==0 &
     & nrow(o_123)==0){ #nrow(o_13_nga)==0 & & nrow(o_123_nga)==0
    validate(
      need(nrow(subset)>0 & nrow(o_12)>0 & nrow(o_13)>0 #& nrow(s_nga)>0 & nrow(o_12_nga)>0
           & nrow(o_123)>0, "No protein families identified") #nrow(o_13_nga)>0 & & nrow(o_123_nga)>0
    )
  } else if(nrow(subset)>0 | nrow(o_12)>0 | nrow(o_13)>0 #| nrow(s_nga)>0 | nrow(o_12_nga)>0
            | nrow(o_123)>0){ #nrow(o_13_nga)>0 | | nrow(o_123_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#e41a1c'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f1 unassigned")
    # }
    # if(nrow(o_12_nga)>0){
    #   p <- add_markers(p, data = o_12_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#fdb462'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f12 unassigned")
    # }
    # if(nrow(o_13_nga)>0){
    #   p <- add_markers(p, data = o_13_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#8c6bb1'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f13 unassigned")
    # }
    # if(nrow(o_123_nga)>0){
    #   p <- add_markers(p, data = o_123_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#d9d9d9'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f123 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#e41a1c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_12)>0){
      p <- add_markers(p, data = o_12, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fdb462", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_13)>0){
      p <- add_markers(p, data = o_13, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#8c6bb1", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_123)>0){
      p <- add_markers(p, data = o_123, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}

#' @rdname compare_two_files
compare_two_files_pf_aa_size <- function(orig, subset, o_12, o_13, o_123, increase){ #s_nga, o_12_nga, o_13_nga, o_123_nga,
  if(nrow(subset)==0 & nrow(o_12)==0 & nrow(o_13)==0 #nrow(s_nga)==0 & nrow(o_12_nga)==0 &
     & nrow(o_123)==0){ #nrow(o_13_nga)==0 & & nrow(o_123_nga)==0
    validate(
      need(nrow(subset)>0 & nrow(o_12)>0 & nrow(o_13)>0 #& nrow(s_nga)>0 & nrow(o_12_nga)>0
           & nrow(o_123)>0, "No protein families identified") #nrow(o_13_nga)>0 & & nrow(o_123_nga)>0
    )
  } else if(nrow(subset)>0 | nrow(o_12)>0 | nrow(o_13)>0 #| nrow(s_nga)>0 | nrow(o_12_nga)>0
            | nrow(o_123)>0){ #nrow(o_13_nga)>0 | | nrow(o_123_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#e41a1c'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f1 unassigned")
    # }
    # if(nrow(o_12_nga)>0){
    #   p <- add_markers(p, data = o_12_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#fdb462'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f12 unassigned")
    # }
    # if(nrow(o_13_nga)>0){
    #   p <- add_markers(p, data = o_13_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#8c6bb1'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f13 unassigned")
    # }
    # if(nrow(o_123_nga)>0){
    #   p <- add_markers(p, data = o_123_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#d9d9d9'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f123 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#e41a1c", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1,
                                     opacity = 0.8), color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_12)>0){
      p <- add_markers(p, data = o_12, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fdb462", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1,
                                     opacity = 0.8), color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_13)>0){
      p <- add_markers(p, data = o_13, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#8c6bb1", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1,
                                     opacity = 0.8), color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_123)>0){
      p <- add_markers(p, data = o_123, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1,
                                     opacity = 0.8), color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}

#' @rdname compare_two_files
compare_two_files_pf_bb <- function(orig, subset, o_21, o_213){ #s_nga, o_21_nga, o_23, o_23_nga, , o_213_nga
  if(nrow(subset)==0 & nrow(o_21)==0 & nrow(o_23)==0 #& nrow(s_nga)==0 & nrow(o_21_nga)==0
     & nrow(o_213)==0){ #nrow(o_23_nga)==0 & & nrow(o_213_nga)==0
    validate(
      need(nrow(subset)>0 & nrow(o_21)>0 & nrow(o_23)>0 # & nrow(s_nga)>0  & nrow(o_21_nga)>0
           & nrow(o_213)>0, "No protein families identified") # nrow(o_23_nga)>0 & & nrow(o_213_nga)>0
    )
  } else if(nrow(subset)>0 | nrow(o_21)>0 | nrow(o_23)>0 # | nrow(s_nga)>0| nrow(o_21_nga)>0
            | nrow(o_213)>0){ #nrow(o_23_nga)>0 | | nrow(o_213_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#ffff33'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f2 unassigned")
    # }
    # if(nrow(o_21_nga)>0){
    #   p <- add_markers(p, data = o_21_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#fd8d3c'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f2&1 unassigned")
    # }
    # if(nrow(o_23_nga)>0){
    #   p <- add_markers(p, data = o_23_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#41ab5d'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f2&3 unassigned")
    # }
    # if(nrow(o_213_nga)>0){
    #   p <- add_markers(p, data = o_213_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#d9d9d9'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f21&3 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#ffff33", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_21)>0){
      p <- add_markers(p, data = o_21, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fd8d3c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_23)>0){
      p <- add_markers(p, data = o_23, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#41ab5d", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_213)>0){
      p <- add_markers(p, data = o_213, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}

#' @rdname compare_two_files
compare_two_files_pf_bb_size <- function(orig, subset, o_21, o_23, o_213, increase){ # s_nga, o_21_nga, o_23_nga, o_213_nga,
  if(nrow(subset)==0 & nrow(o_21)==0 & nrow(o_23)==0 #& nrow(s_nga)==0 & nrow(o_21_nga)==0
     & nrow(o_213)==0){ #nrow(o_23_nga)==0 & & nrow(o_213_nga)==0
    validate(
      need(nrow(subset)>0 & nrow(o_21)>0 & nrow(o_23)>0 # & nrow(s_nga)>0  & nrow(o_21_nga)>0
           & nrow(o_213)>0, "No protein families identified") # nrow(o_23_nga)>0 & & nrow(o_213_nga)>0
    )
  } else if(nrow(subset)>0 | nrow(o_21)>0 | nrow(o_23)>0 # | nrow(s_nga)>0| nrow(o_21_nga)>0
            | nrow(o_213)>0){ #nrow(o_23_nga)>0 | | nrow(o_213_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#ffff33'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f2 unassigned")
    # }
    # if(nrow(o_21_nga)>0){
    #   p <- add_markers(p, data = o_21_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#fd8d3c'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f2&1 unassigned")
    # }
    # if(nrow(o_23_nga)>0){
    #   p <- add_markers(p, data = o_23_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#41ab5d'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f2&3 unassigned")
    # }
    # if(nrow(o_213_nga)>0){
    #   p <- add_markers(p, data = o_213_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#d9d9d9'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f21&3 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#ffff33", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_21)>0){
      p <- add_markers(p, data = o_21, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fd8d3c", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_23)>0){
      p <- add_markers(p, data = o_23, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#41ab5d", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_213)>0){
      p <- add_markers(p, data = o_213, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}

#' @rdname compare_two_files
compare_two_files_pf_cc <- function(orig, subset, o_31, o_32, o_312){ #s_nga, o_31_nga, o_32_nga, , o_312_nga
  if(nrow(subset)==0 & nrow(o_31)==0 & nrow(o_32)==0 # & nrow(s_nga)==0 & nrow(o_31_nga)==0
     & nrow(o_312)==0){#nrow(o_32_nga)==0 & & nrow(o_312_nga)==0
    validate(
      need(nrow(subset)>0 & nrow(o_31)>0 & nrow(o_32)>0 #& nrow(s_nga)>0  & nrow(o_31_nga)>0
           & nrow(o_312)>0, "No protein families identified") #nrow(o_32_nga)>0 & & nrow(o_312_nga)>0
    )
  } else if(nrow(subset)>0 | nrow(o_31)>0 | nrow(o_32)>0 #| nrow(s_nga)>0 | nrow(o_31_nga)>0
            | nrow(o_312)>0){ #nrow(o_32_nga)>0 |  | nrow(o_312_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#1f78b4'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f3 unassigned")
    # }
    # if(nrow(o_31_nga)>0){
    #   p <- add_markers(p, data = o_31_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#8c6bb1'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f3&1 unassigned")
    # }
    # if(nrow(o_32_nga)>0){
    #   p <- add_markers(p, data = o_32_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#41ab5d'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f3&2 unassigned")
    # }
    # if(nrow(o_312_nga)>0){
    #   p <- add_markers(p, data = o_312_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#d9d9d9'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f31&1 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#1f78b4", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_31)>0){
      p <- add_markers(p, data = o_31, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#8c6bb1", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_32)>0){
      p <- add_markers(p, data = o_32, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#41ab5d", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_312)>0){
      p <- add_markers(p, data = o_312, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}

#' @rdname compare_two_files
compare_two_files_pf_cc_size <- function(orig, subset, o_31, o_32, o_312, increase){ #s_nga, o_31_nga, o_32_nga, o_312_nga,
  if(nrow(subset)==0 & nrow(o_31)==0 & nrow(o_32)==0 # & nrow(s_nga)==0 & nrow(o_31_nga)==0
     & nrow(o_312)==0){#nrow(o_32_nga)==0 & & nrow(o_312_nga)==0
    validate(
      need(nrow(subset)>0 & nrow(o_31)>0 & nrow(o_32)>0 #& nrow(s_nga)>0  & nrow(o_31_nga)>0
           & nrow(o_312)>0, "No protein families identified") #nrow(o_32_nga)>0 & & nrow(o_312_nga)>0
    )
  } else if(nrow(subset)>0 | nrow(o_31)>0 | nrow(o_32)>0 #| nrow(s_nga)>0 | nrow(o_31_nga)>0
            | nrow(o_312)>0){ #nrow(o_32_nga)>0 |  | nrow(o_312_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#1f78b4'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f3 unassigned")
    # }
    # if(nrow(o_31_nga)>0){
    #   p <- add_markers(p, data = o_31_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#8c6bb1'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f3&1 unassigned")
    # }
    # if(nrow(o_32_nga)>0){
    #   p <- add_markers(p, data = o_32_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#41ab5d'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f3&2 unassigned")
    # }
    # if(nrow(o_312_nga)>0){
    #   p <- add_markers(p, data = o_312_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#d9d9d9'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f31&1 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#1f78b4", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_31)>0){
      p <- add_markers(p, data = o_31, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#8c6bb1", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_32)>0){
      p <- add_markers(p, data = o_32, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#41ab5d", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_312)>0){
      p <- add_markers(p, data = o_312, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}
