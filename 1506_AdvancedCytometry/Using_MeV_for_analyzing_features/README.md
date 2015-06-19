This part aims to get familiar with features analysis. A feature could be any information measured on the set of studied samples.

## CytoPrep: prepare features for MeV

CytoPrep is a R application

1. Start RStudio
2. If shiny library is not installed in RStudio, install it: `install.packages("shiny")`
3. Start CytPrep by typing `shiny::runGitHub('bioinfoxtra/cytometry', subdir='cytoPreparePercent')`
4. Scale interactively the feature data set

## MeV: visualize features, then apply statistical analyses

1. Import annotated features
2. Use Significance Analysis of Micro-arrays tool
3. Use ANOVA with two factors
