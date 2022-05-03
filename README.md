## Sick-leave due to COVID-19 during the first pandemic wave in France, 2020

<font size="-2">
Smith DRM<sup>1,2,3</sup>, Jijón S<sup>1,4,</sup>*, Oodally A<sup>1,2,3,</sup>*,  Shirreff G<sup>1,2,3,</sup>*, Aït Bouziad K<sup>1</sup>, Ante Testard P<sup>1</sup>, Bastard J<sup>1,2,3,4,5</sup>, Bouziri H<sup>1</sup>, Salama Daouda O<sup>1</sup>, Duchemin T<sup>1</sup>, Godon-Rensonnet AS<sup>1</sup>, Henriot P<sup>1,4</sup>, Houri Y<sup>1</sup>, Neynaud H<sup>1</sup>, Perozziello A<sup>1</sup>, Thonon F<sup>1</sup>, Crépey P<sup>6</sup>, Dab W<sup>1</sup>, Jean K<sup>1,4,7</sup>, Temime L<sup>1,4</sup>
</font>

<br>

<sup>1</sup> Laboratoire MESuRS, Conservatoire national des Arts et Métiers, Paris, France

<sup>2</sup> Institut Pasteur, Epidemiology and Modelling of Antibiotic Evasion (EMAE), Paris, France

<sup>3</sup> Université Paris-Saclay, UVSQ, Inserm, CESP, Anti-infective evasion and pharmacoepidemiology team, Montigny-Le-Bretonneux, France

<sup>4</sup> Unité PACRI, Institut Pasteur, Conservatoire national des Arts et Métiers, Paris, France

<sup>5</sup> Université Paris Diderot, Sorbonne Paris Cité, Paris, France

<sup>6</sup> Université de Rennes, EHESP, CNRS, Inserm, Arènes - UMR 6051, RSMS – U 1309, Rennes, France

<sup>7</sup>MRC Centre for Global Infectious Disease Analysis, Department of Infectious Disease Epidemiology, Imperial College London, United Kingdom

<sup>*</sup>These authors contributed equally

</br>

Corresponding author: Smith DRM (david.smith@pasteur.fr)

<!-- 
## Preprint
Preprint available at: <a href="" target="_blank"> doi: </a> 
-->

## Main contents
````
Covid-sick-leave
├── COVID19-sick-leaves_analysis.R
├── COVID19-sick-leaves_figures.R
├── Data
├── Plots
├── Plots_extras
├── Plots_uncorrected
├── Plots_uncorrected_extras
└── covid_absence.Rproj

````
- The main R files:
    - `COVID19-sick-leaves_analysis.R`
    <br> The main script where the analyses are conducted.
    - `COVID19-sick-leaves_figures.R`
    <br> Reproduces the figures in the manuscript.
    - `covid_absence.Rproj`
    <br> The R project.
- **Data**
<br> This folder contains the files regarding data on:
    - Working population by age
    - Age stratification of the population by region
    - Sick leaves
    - Symptomatic incidence
    - Probability of having a close contact by age and by region

<br> Of note, Socialcov data (the social contact survey) were shared via personal communication and thus we do not have the permission to share.
- **Plots**
<br> This folder contains the figures of the main text and and the Supplementary material.
- **Plots_extras**
<br> This folder contains figures concerning the baseline analyses, that were not included in the main text.
- **Plots_uncorrected** and **Plots_uncorrected_extras**
<br> These folders contain the figures obtained from the analyses *without the correction* of the bias regarding the proportion of survey respondents that work remote (see Supplementary material, Section b.).



