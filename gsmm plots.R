#Load year table
library(readr)
library(WGCNA)
pheno=read_csv("/Users/Desktop/Data/pheno.csv")

#gene relationship to trait and important modules obtained from heatmap
trait = as.data.frame(pheno$Muscle_Mass2)
names(trait) = "MM"
#names (colors) of the modules
modNames = substring(names(MEs), 3)

geneModuleMembership = as.data.frame(cor(combined, MEs, use = "p"));
MMPvalue = as.data.frame(corPvalueStudent(as.matrix(geneModuleMembership), nSamples))

names(geneModuleMembership) = paste("MM", modNames, sep="");
names(MMPvalue) = paste("p.MM", modNames, sep="");

geneTraitSignificance = as.data.frame(cor(combined, trait, use = "p"));
GSPvalue = as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance), nSamples));

names(geneTraitSignificance) = paste("GS.", names(trait), sep="");
names(GSPvalue) = paste("p.GS.", names(trait), sep="")


######################################
#mm-gs for modules
#all the mm-gs done by replacing for each module with the respective colours

module = "darkgreen"
column = match(module, modNames);
moduleGenes = moduleColors==module;

par(mfrow = c(1,1));
verboseScatterplot(abs(geneModuleMembership[moduleGenes, column]),
                   abs(geneTraitSignificance[moduleGenes, 1]),
                   xlab = paste("Module Membership in", module, "module"),
                   ylab = "Gene significance for Muscle Mass",
                   main = paste("Module membership vs. gene significance\n"),
                   cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = module)
#display the gene names inside the modules
#1
darkgreen = colnames(combined)[moduleColors=="darkgreen"]
write.csv(darkgreen, file = "darkgreen2_signed.csv")

#identifying most impordarkgreent genes for one determined characteristic inside of the cluster
geneInfo0 = data.frame(hcjSamples = colnames(combined),
                       moduleColor = "darkgreen",
                       geneTraitSignificance,
                       GSPvalue)

#write information in csv file
write.csv(geneInfo0, file = "geneInfo0.csv")

######################################


modOrder = order(-abs(cor(MEs, trait, use = "p")))
for(mod in 1:ncol(geneModuleMembership))
{
  oldNames = names(geneInfo0)
  geneInfo0 = data.frame(geneInfo0, geneModuleMembership[, modOrder[mod]],
                         MMPvalue[, modOrder[mod]])
  names(geneInfo0) = c(oldNames, paste("MM.", modNames[modOrder[mod]], sep=""),
                       paste("p.MM.", modNames[modOrder[mod]], sep=""))
}
geneOrder = order(geneInfo0$moduleColor, -abs(geneInfo0$GS.MM))
geneInfo = geneInfo0[geneOrder,]
#write the information in csv file
write.csv(geneInfo, file = "geneInfo.csv")

#######################################

