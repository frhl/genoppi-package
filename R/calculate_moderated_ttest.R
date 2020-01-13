
calculate_moderated_ttest_hgnc <- function(input_file){
  tmp <- data.frame(input_file$gene, rownames(input_file))
  calculated <- data.frame(rownames(input_file), input_file$rep1, input_file$rep2)
  ### apply median normalization
  medianNorm <- function(d){return(d - median(d, na.rm = TRUE))}
  calculated$input_file.rep1 <- medianNorm(calculated$input_file.rep1)
  calculated$input_file.rep2 <- medianNorm(calculated$input_file.rep2)
  ### calc moderated t test
  myfit <- lmFit(calculated[,-1], method="robust")
  myfit <- eBayes(myfit)
  # modtest <- topTable(myfit, number=nrow(m), sort.by='none')
  modtest <- topTable(myfit, number=nrow(myfit), sort.by='none')
  ### prepare final data to use in plots
  calculated <- data.frame(cbind(calculated, modtest))
  calculated <- calculated[,-c(5,6,9)]
  colnames(calculated) <- c("id", "rep1", "rep2","logFC","pvalue","FDR")
  ### add gene names
  tmp_g <- subset(tmp, rownames.input_file. %in% calculated$id)
  tmp_g[is.na(tmp_g$input_file.gene),] <- "NotAssigned"
  calculated$gene <- tmp_g$input_file.gene
  calculated
}


calculate_moderated_ttest_uniprot <- function(input_file){
  tmp <- data.frame(input_file$gene, rownames(input_file), input_file$accession_number, input_file$all_gene_names)
  calculated <- data.frame(rownames(input_file), input_file$rep1, input_file$rep2)
  ### apply median normalization
  medianNorm <- function(d){return(d - median(d, na.rm = TRUE))}
  calculated$input_file.rep1 <- medianNorm(calculated$input_file.rep1)
  calculated$input_file.rep2 <- medianNorm(calculated$input_file.rep2)
  ### calc moderated t test
  myfit <- lmFit(calculated[,-1], method="robust")
  myfit <- eBayes(myfit)
  # modtest <- topTable(myfit, number=nrow(m), sort.by='none')
  modtest <- topTable(myfit, number=nrow(myfit), sort.by='none')
  ### prepare final data to use in plots
  calculated <- data.frame(cbind(calculated, modtest))
  calculated <- calculated[,-c(5,6,9)]
  colnames(calculated) <- c("id", "rep1", "rep2","logFC","pvalue","FDR")
  ### add gene names
  tmp_g <- subset(tmp, rownames.input_file. %in% calculated$id)
  tmp_g[is.na(tmp_g$input_file.gene),] <- "NotAssigned"
  # calculated$gene <- tmp_g$input_file.gene
  calculated <- merge(calculated, tmp_g, by.x = "id", by.y = "rownames.input_file.")
  colnames(calculated) <- c("id", "rep1", "rep2","logFC","pvalue","FDR","gene","accession_number", "all_gene_names")
  calculated
}
