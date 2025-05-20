**DataGeneration.R** generates the visual field datasets, and adds noise and p-values.


The **Autoencoder** folder includes 4 different autoencoders: **Masked autoencoder with p-values**; **Masked autoencoder without p-values**; **Variational autoencoder with p-values**; **variational autoencoder without p-values**. The autoencoders read in VF datasets with noise and create denoised VF datasets.


**ProgressionEvaluation.Rmd** evaluates VF datasets using PLR, MD, and GRI methods to determine whether the VF are deteriorating.
