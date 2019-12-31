# shape-analysis-phylogenetic-analysis

Scripts to perform PGLS model (caper package) , and loops through to extract the coefficents and statistical tests, and puts the output in the table.
Also looping through to and takes the residuals from each PGLS and these as columns to the original dataframe, and
looping through to create plots mapping of trait values on a phylogeny (using the contmap function of the phytools package). It loops through to produce ‘cont maps’ and ‘phenogram’ (phytools package), which are saved to one pdf to easily view in one place.
phyPCA.R-This performs and plots the phylogeneticPCA and plots, then ANOVA of PCA components according to groups
ggtree.R– settings for plotting the annotated phylogeny with ggtree
