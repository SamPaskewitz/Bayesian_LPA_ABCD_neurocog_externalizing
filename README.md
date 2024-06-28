# Bayesian_LPA_ABCD_neurocog_externalizing
Analysis code for a paper using the Dirichlet process mixture model for latent profile analysis of neurocognitive data from the ABCD Study and exploring its relationship to externalizing behavior.

Here is an overview of the whole analysis:
1) Run 'process data (version 6).R' to combine ABCD data from various sources into several data tables, then export these as .csv files.
2) Run 'split_and_standardize_data.py' to further process the data files produced in step 1. This produces a lot of output that we didn't end up using (in particular separate training and test data sets), but importantly standardizes (z-scores) the data and exports it again as further .csv files.
3) Run 'fit_LPA_model_10_10_2023.py' to fit the DPM-LPA model to the standardized 2-year neurocognitive data (only selecting people with no missing neurocognitive data). The output is a DPM-LPA model object (saved by 'pickling' it) that is used for later analysis. NOTE: This uses the vbayesfa package, which has since been updated. Thus, the output of the model fit may be slightly different when running the most current version.
4) Run 'neurocog LPA.Rmd' to fit conventional LPA models using R.
5) Run 'similarity analysis.Rmd' to do a profile similarity analysis for conventional LPA models.
6) The file 'Bayesian ANOVA with partition post hoc analysis.R' defines R functions for doing the Bayesian ANOVA and post-hoc analysis (this functionality has since been added to the 'vbayesfa' Python package).
7) Run the Jupyter notebook 'ABCD DPM-LPA analysis.ipynb' for the main DPM-LPA analysis, producing plots etc.
8) Run 'DPM LPA outcome analysis.R' for Bayesian ANOVAs and post-hoc analysis of the DPM-LPA model.
9) Run 'conventional LPA (4 profile) analysis.R' for Bayesian ANOVAs and post-hoc analysis of the 4-profile conventional LPA model.
10) Run the Jupyter notebook 'extra ABCD DPM-LPA figures.ipynb' to produce additional figures for the paper.

I apologize that this analysis pipeline is a bit of a mess, e.g. in the way that it combines both R and Python code. Since doing this study, we have updated our Python package ('vbayesfa') in several important ways; in particular it now does the Bayesian ANOVA and post-hoc analysis. We have also written an R package ('dpm.lpa') that wraps the DPM-LPA Python code from 'vbayesfa' for the convenience of people who are more used to R than Python ('vbayesfa' is available on Pip; 'dpm.lpa' is available on GitHub and will be published to CRAN in the near future as of 6/28/2024). We recommend that future studies either use 'vbayesfa' in Python or 'dpm.lpa' in R.
