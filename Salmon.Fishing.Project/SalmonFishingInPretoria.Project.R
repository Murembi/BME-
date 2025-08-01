getwd()
list.files()

install.packages("BiocManager") 
  BiocManager::install("tximport")
    BiocManager::install("GenomicFeatures")
    
library(tximport)
    library(GenomicFeatures)
      library(readr)

    BiocManager::install("txdbmaker")
txdb <- makeTxDbFromGFF("C:/Users/murem/Desktop/RNAseq/RNASeqCL-20250731T103301Z-1-001/RNASeqCL/Hooked/chr22_genes.gtf")
#create txdb for chr2 genes
 k <- keys(txdb, keytype = "TXNAME") #gets all transcript names (IDs)

tx2gene <- select(txdb, keys = k, columns = "GENEID", keytype = "TXNAME")
#transcript-gene mapping
  head(tx2gene)
samples <- read.table("C:/Users/murem/Desktop/RNAseq/RNASeqCL-20250731T103301Z-1-001/RNASeqCL/Hooked/samples.txt", header = TRUE)
head(samples)

files <- file.path("quant", samples$sample, "quant.sf")
  names(files) <- paste0(samples$sample)
  
file.exists(files)
print(files)
 getwd()
 setwd("C:/Users/murem/Desktop/RNAseq/RNASeqCL-20250731T103301Z-1-001/RNASeqCL")
 
 txi.salmon <- tximport(files, type = "salmon", tx2gene = tx2gene)
 
 head(txi.salmon$counts)
 
 BiocManager::install("DESeq2")
      library(DESeq2)
 dds <- DESeqDataSetFromTximport(txi.salmon, samples, ~condition)
 dds <- DESeq(dds)
 
 library(RColorBrewer)
 library(gplots)
 
 rld <- rlogTransformation(dds)
  head(assay(rld))
 
 (mycols <- brewer.pal(8, "Dark2")[1:length(unique(samples$condition))])
 sampleDists <- as.matrix(dist(t(assay(rld))))
 heatmap.2(as.matrix(sampleDists), key=F, trace="none",
           col=colorpanel(100, "black", "white"),
           ColSideColors=mycols[samples$condition],
           RowSideColors=mycols[samples$condition],
           margin=c(10, 10), main="Sample Distance Matrix")
 DESeq2::plotPCA(rld, intgroup="condition")
 res <- results(dds)
  table(res$padj<0.05)
    res <- res[order(res$padj), ]
resdata <- merge(as.data.frame(res), 
              as.data.frame(counts(dds, normalized=TRUE)), by="row.names", sort=FALSE)
names(resdata)[1] <- "Gene"

hist(res$padj, breaks=20, col="grey")

with(res, plot(log2FoldChange, -log10(pvalue), pch=20, main="Volcano plot", xlim=c(-2.5,2)))
  with(subset(res, padj<.05 ), points(log2FoldChange, -log10(pvalue), pch=20, col="red"))

BiocManager::install(c("AnnotationDbi", "org.Hs.eg.db",
                         "pathview", "gage", "gageData"))
  library(AnnotationDbi)
  library(org.Hs.eg.db)
  library(pathview)
  library(gage)
  library(gageData)
res$symbol <- mapIds(org.Hs.eg.db,
                                   keys=row.names(res),
                                   column="SYMBOL",
                                   keytype="ENSEMBL",
                                   multiVals="first")
res$entrez <- mapIds(org.Hs.eg.db,
                                   keys=row.names(res),
                                   column="ENTREZID",
                                   keytype="ENSEMBL",
                                   multiVals="first")
res$name <- mapIds(org.Hs.eg.db,
                                 keys=row.names(res),
                                 column="GENENAME",
                                 keytype="ENSEMBL",
                                 multiVals="first")
data(kegg.sets.hs)
data(sigmet.idx.hs)
kegg.sets.hs <- kegg.sets.hs[sigmet.idx.hs]
head(kegg.sets.hs, 3)

foldchanges <- res$log2FoldChange
names(foldchanges) <- res$entrez
keggres <- gage(foldchanges, gsets=kegg.sets.hs, same.dir = TRUE)
lapply(keggres, head)

library(dplyr)

keggrespathways <- data.frame(id=rownames(keggres$greater), keggres$greater) %>%
  tbl_df() %>%
  filter(row_number()<=5) %>%
  .$id %>%
  as.character()
keggrespathways
  keggresids <- substr(keggrespathways, start=1, stop=8)
    keggresids
plot_pathway <- function(pid) pathview(gene.data=foldchanges, 
                                        pathway.id=pid, species="hsa", new.signature=FALSE)
tmp <- sapply(keggresids, function(pid) pathview(gene.data=foldchanges, pathway.id=pid, species="hsa"))

    