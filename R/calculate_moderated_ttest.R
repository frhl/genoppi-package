#' @name mttest
#' @rdname calculate_moderated_ttest
#' @aliases calculate_moderated_ttest_hgnc
#' @aliases calculate_moderated_ttest_uniprot
#' @aliases mttest
#' @title Moderated t-test for proteomics
#' @description Calculate the moderated ttest using limma package.
#' @param input_file a data.frame containing at least three columns, 
#' gene, replicate 1 and replicate 2. Triplicates and so on can be 
#' also  be included. The function assumes that the first column 
#' contains gene information.
#' @author April/Frederik
#' @family statistics
#' @export

calculate_moderated_ttest <- function(input_file){
  
  require(limma)
  ## check format
  if ('gene' %nin% colnames(input_file)) stop('Genename (gene) is not in column names.')
  if (!any(grepl('rep',colnames(input_file)))) stop(' Replicates (rep) could not be found in column names.')
  # isolate rep columns
  reps <- grepl('rep', colnames(input_file)) & unlist(lapply(input_file, is.numeric))
  calculated <- input_file[, reps]
  # calc moderated t-test
  myfit <- lmFit(calculated, method="robust")
  myfit <- eBayes(myfit)
  modtest <- topTable(myfit, number=nrow(myfit), sort.by='none')
  colnames(modtest)[4:5] <- c("pvalue","FDR")
  # return data frame with test results: gene, rep1, rep2, ..., logFC, pvalue, FDR 
  calculated <- data.frame(cbind(calculated, modtest[,-c(2,3,6)]))
  calculated <- cbind(input_file$gene, calculated)
  # rename columns
  colnames(calculated)[1] <- 'gene'
  calculated <- calculated[with(calculated, order(-logFC, FDR)),]
  
  return(calculated)
}

#' @rdname calculate_moderated_ttest
#' @export
calculate_moderated_ttest_hgnc <- function(input_file){
  tmp <- data.frame(input_file$gene, rownames(input_file))
  calculated <- data.frame(rownames(input_file), input_file$rep1, input_file$rep2)
  calculated <- calculate_moderated_ttest(calculated)
  ### add gene names
  tmp_g <- subset(tmp, rownames.input_file. %in% calculated$id)
  tmp_g[is.na(tmp_g$input_file.gene),] <- "NotAssigned"
  calculated$gene <- tmp_g$input_file.gene
  return(calculated)
}

#' @rdname calculate_moderated_ttest
#' @export
calculate_moderated_ttest_uniprot <- function(input_file){
  tmp <- data.frame(input_file$gene, rownames(input_file), input_file$accession_number, input_file$all_gene_names)
  calculated <- data.frame(rownames(input_file), input_file$rep1, input_file$rep2)
  calculated <- calculate_moderated_ttest(calculated)
  ### add gene names
  tmp_g <- subset(tmp, rownames.input_file. %in% calculated$id)
  tmp_g[is.na(tmp_g$input_file.gene),] <- "NotAssigned"
  calculated <- merge(calculated, tmp_g, by.x = "id", by.y = "rownames.input_file.")
  colnames(calculated) <- c("id", "rep1", "rep2","logFC","pvalue","FDR","gene","accession_number", "all_gene_names")
  return(calculated)
}
