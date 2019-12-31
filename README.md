# Shape analysis, phylogenetic analysis, phyPCA

Scripts to perform PGLS model (caper package), loops through to extract the coefficents and statistical tests, and puts the output in the table. Also automates the extraction of residuals from each PGLS and adds these as columns to the original dataframe. Also loops through to create plots mapping of trait values on a phylogeny (using the 'contmap' and 'phenogram' function of the phytools package), which are saved to one pdf to easily view in one place.
phyPCA.R-This performs and plots the phylogeneticPCA and plots, then ANOVA of PCA components according to groups, and plotting
ggtree.R– settings for plotting the annotated phylogeny with ggtree
