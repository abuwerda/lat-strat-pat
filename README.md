
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Mapping lateral stratigraphy at Palaeolithic surface sites: a case study from Dhofar,
Oman

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/abuwerda/lat-strat-pat/master?urlpath=rstudio)

This repository contains the data and code for our paper:

> Jeffrey Ian Rose, Yamandú H. Hilbert, Vitaly I. Usyk, Michelle R. Bebber, Amir Beshkani, Briggs Buchanan, João Cascalheira, Dominik Chlachula, Rudolf Dellmour, Metin I. Eren, Roman Garba, Emily Hallinan, Li Li, Robert S. Walker, Anthony E. Marks  (2024). *Mapping lateral stratigraphy at Palaeolithic surface sites: a case study from Dhofar,
Oman*. 

### How to cite

Please cite this compendium as:

> Jeffrey Ian Rose, Yamandú H. Hilbert, Vitaly I. Usyk, Michelle R. Bebber, Amir Beshkani, Briggs Buchanan, João Cascalheira, Dominik Chlachula, Rudolf Dellmour, Metin I. Eren, Roman Garba, Emily Hallinan, Li Li, Robert S. Walker, Anthony E. Marks  (2024). *Compendium of R code and data for Mapping lateral stratigraphy at Palaeolithic surface sites: a case study from Dhofar,
Oman*. Accessed 25 Jul 2024. Online at

## Contents

The **analysis** directory contains:

  - [:file\_folder: script](/analysis/script): Script to reproduce all plots and results in the paper
  - [:file\_folder: data](/analysis/data): Data used in the analysis.
  - [:file\_folder: figures](/analysis/figures): Plots and other
    illustrations

## How to run in your browser or download and run locally

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

You can download the compendium as a zip from from this URL:
[master.zip](/archive/master.zip). After unzipping: - open the `.Rproj`
file in RStudio - run `devtools::install()` to ensure you have the
packages this analysis depends on (also listed in the
[DESCRIPTION](/DESCRIPTION) file). - finally, open
`analysis/paper/paper.Rmd` and knit to produce the `paper.docx`, or run
`rmarkdown::render("analysis/paper/paper.qmd")` in the R console

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.
