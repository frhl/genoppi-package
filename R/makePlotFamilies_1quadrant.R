
makePlotFamilies_1quadrant <- function(data_fam, data, sortPF){ #data_gna,
  ### replace logFC and pvalue in di* by these from dpm
  data_fam$logFC <- data$logFC[match(data_fam$gene, data$gene)]
  data_fam$pvalue <- data$pvalue[match(data_fam$gene, data$gene)]
  if(sortPF == "sort_f"){
    data_fam <- data_fam[order(-data_fam$frequency), ]
    if(nrow(data_fam)>=1){
      data_fam <- cbind(data_fam, new_f = paste0("(", data_fam$frequency, ") ", data_fam$overlay))
    }
    else {
      data_fam <- data.frame("id" = character(0), "rep1" = numeric(0), "rep2" = numeric(0),
                             "logFC" = numeric(0), "pvalue" = numeric(0), "FDR" = numeric(0),
                             "gene" = character(0), "overlay" = character(0), "frequency" = numeric(0),
                             "new_f" = character(0))
    }
  }
  else if(sortPF == "sort_a"){
    data_fam
    if(nrow(data_fam)>=1){
      data_fam <- cbind(data_fam, new_f = paste0(data_fam$overlay, " (", data_fam$frequency, ")"))
    }
    else {
      data_fam <- data.frame("id" = character(0), "rep1" = numeric(0), "rep2" = numeric(0),
                             "logFC" = numeric(0), "pvalue" = numeric(0), "FDR" = numeric(0),
                             "gene" = character(0), "overlay" = character(0), "frequency" = numeric(0),
                             "new_f" = character(0))
    }
  }
  data_fam$new_f <- factor(data_fam$new_f, levels = unique(data_fam$new_f))

  # data_gna$logFC <- data$logFC[match(data_gna$gene, data$gene)]
  # data_gna$pvalue <- data$pvalue[match(data_gna$gene, data$gene)]
  # pf_list <- list("list" = data_fam) #, "list" = data_gna
  pf_list <- data_fam
  pf_list
  #for download
  # families <- cbind(data_fam$gene, data_fam$family, data_fam$frequency)
  # colnames(families) <- c("gene", "family", "frequency")
}
