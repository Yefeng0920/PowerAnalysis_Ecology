# Low statistical power and overestimated anthropogenic impacts, exacerbated by publication bias, dominate field studies in global change biology

# Citation
Yefeng Yang, Helmut Hillebrand, Malgorzata Lagisz, Ian Cleasby, Shinichi Nakagawa. *Low statistical power and overestimated anthropogenic impacts, exacerbated by publication bias, dominate field studies in global change biolog*. Global Change Biology, in revision 2021.

# How to use
This repository houses the code used to reproduce the models, Figures, and Tables in the paper of 'Low statistical power and overestimated anthropogenic impacts, exacerbated by publication bias, dominate field studies in global change biology'.

To run the analysis and plot the graphs provided in the paper, use: 
#### PowerAnalysis_Global_Change_Bio.Rmd

The code has detailed annotations for our analysis pipelines. Briefly, the code includes *a three-step modelling procedure*.

### In the first step
We used multilevel meta-analytic approaches to estimate ‘true’ effects for each meta-analysis included in our dataset (for each of the effect sizes: lnRR*, lnRR, SMD, SMDH and their corrected versions). Of note, lnRR, SMD, SMDH quantify the response magnitude of ecological process to environmental stressor (mean difference between a stressor group and a control group).

### In the second step
we used the ‘true’ effects to calculate statistical power, Type M / S errors for each meta-analysis and each experiment included in meta-analysis (section 2.5.1). We termed the first and second steps as “within-meta-analysis modelling”. 

### Third
We statistically aggregated power, Type M / S errors across different meta-analyses (by doing so, we can obtain an estimate to represent the overall magnitude of power, Type M / S errors; section 2.5.2). Also, we conducted a secondary synthesis of the overall means (which were obtained from the first step meta-analyses) across different meta-analyses (i.e., a second-order meta-analysis, or meta-meta-analysis; section 2.6). We termed the step three as “between-meta-analysis modelling”.

### Additional analyses
We added three additional analyses which have not been included in our paper but have implications for future studies. They are: (i) 'stressor specific' results, which are the additional analysis required by one of Referee, (ii) the empirically-derived effect size interpretation guidelines in global change studies, and (3) false-positive report probability (FPRP) in global change studies. One can easily update our results if one has a “bigger” dataset. 

# data folder contains:
30 lnRR* datasets, 12 lnRR datasets, 12 SMD datasets, 12 SMDH datasets, 12 lnCVR datasets, 12 lnVR datasets

# Figures folder contains:
Figures 1 - 6  reported in the main text; funnel plots and model residuals in the online supplementary materials
