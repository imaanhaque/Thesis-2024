#Heatmap construction

pheno=read_csv("/Users/Desktop/Data.csv")
# Define numbers of genes and samples
nGenes = ncol(combined); #combined=phenodata+genes
nSamples = nrow(combined);

# Recalculate MEs with color labels
MEs0 = moduleEigengenes(combined, mergedColors)$eigengenes
MEs = orderMEs(MEs0)
moduleTraitCor = cor(MEs, pheno, use = "p");
moduleTraitPvalue = corPvalueStudent(moduleTraitCor, nSamples);
sizeGrWindow(10,6)

# Will display correlations and their p-values
textMatrix = paste(signif(moduleTraitCor, 2), "\n(",
                   signif(moduleTraitPvalue, 1), ")", sep = "");
dim(textMatrix) = dim(moduleTraitCor)
par(mar = c(3,16, 1, 1));

# Display the correlation values within a heatmap plot
heatmap=labeledHeatmap(Matrix = moduleTraitCor,
               xLabels = names(pheno),
               yLabels = names(MEs),
               ySymbols = names(MEs),
               colorLabels = FALSE,
               colors = blueWhiteRed(50),
               textMatrix = textMatrix,
               setStdMargins = FALSE,
               cex.text = 0.5,
               zlim = c(-1,1),
               main = paste("Module-trait relationships"))

