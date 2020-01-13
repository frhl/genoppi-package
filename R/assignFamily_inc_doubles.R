
assignFamily_inc_doubles <- function(d, overlay_d){
  if (nrow(d)<1){
    d_families <- data.frame("gene" = character(0), "id" = character(0), "rep1" = numeric(0), "rep2" = numeric(0), "logFC" = numeric(0), "pvalue" = numeric(0), "FDR" = numeric(0), "overlay" = character(0), "frequency" = numeric(0))
  }
  else{
    d1 <- d[FALSE,]
    d_families <- ldply(unique(c(d$gene)), function(name) {
      ix <- overlay_d[[name]]
      d_ix <- d[d$gene == name,]
      d_ix <- d_ix[rep(seq_len(nrow(d_ix)), each=length(ix)),]
      d_ix$overlay <- ix
      d1 <- rbindlist(list(d1,d_ix), use.names=T, fill = T)
    })
    if (nrow(d_families)<1){
      d_families <- data.frame("id" = character(0), "rep1" = numeric(0), "rep2" = numeric(0), "logFC" = numeric(0), "pvalue" = numeric(0), "FDR" = numeric(0), "gene" = character(0), "accession_number" = character(0), "overlay" = character(0), "frequency" = numeric(0))
      # }
    } else if (nrow(d_families) >= 1){
      d_families <- d_families[with(d_families, order(overlay, decreasing = FALSE)),]
      # howmany <- length(unique(d_families$overlay))
      tmp <- data.frame(table(d_families$overlay))
      d_families$frequency <- tmp$Freq[match(d_families$overlay, tmp$Var1)]
      ### uncomment the line below if you would like to show protein families with more than one member
      #d_families <- subset(d_families, frequency>1)
      ### in case it becomes empty after subsetting
    }
  }
  # print(d_families)
  d_families
}
