
addNames <- function(d){
  if((file.exists("data/genes_naf_wnames.txt")) == TRUE){
    ### read genes that have no family assigned
    genesNotAs <- read.csv("data/genes_naf_wnames.txt", sep = "\t")
  }
  else {
    system("echo -e \"gene\"$'\t'\"name\" > data/genes_naf_secondary.txt")
    genesNotAs <- read.csv("data/genes_naf_secondary.txt", sep = "\t")
  }
  system("rm data/genes_naf_wnames.txt")
  system("rm data/genes_naf_secondary.txt")
  ### gna = gene not assigned
  d_gna <- subset(d, gene %in% genesNotAs$gene)
  d_gna$name <- genesNotAs$name[match(d_gna$gene, genesNotAs$gene)]
  d_gna
}
