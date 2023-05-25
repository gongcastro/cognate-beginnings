# cognate-beginnings [[report](https://gongcastro.github.io/cognate-beginnings)]

## Table of Contents
1. [Downloading this repository](#downloading)
2. [Reproducing this repository](#reproducing)<br>
2.1. [Installing package dependencies with {renv}](#packages)<br>
2.2. [Running the code with {targets}](#running)<br>
2.3. [Repository structure](#structure)<br>


## Download this repository <a name="downloading"></a>

1) Download and install [Git](https://git-scm.com/downloads) with default settings.
2) Clone this repository locally. You can do this in two ways:

* Using your Command Prompt/console, navigate to the folder you want to download the repository into, and run:

```console
git clone https://github.com/gongcastro/cognate-beginnings.git
```

* Clicking the green button "Code" in this page (upper-right corner), clicking "Download ZIP", and unzipping the downloaded repository.


## Installing package dependencies with {renv}

The code in this repository needs some packages to run, many of which might not have been already installed in your local machine. Instead of having to install them yourself one by one or updating already installed ones (which might change how they behave, possibly breaking some of your code in other projects) this repository uses the R package [{renv}](https://rstudio.github.io/renv/articles/renv.html) to deal with package libraries. You can just install `renv` by running `install.packages("renv")` and then run `renv::restore()` in your console. This command will install all necessary packages in the appropriate version (listed in the file `renv.lock` in a self-contained R package library. This process will not affect the packages that you had already installed in your machine. 

In case the installation of any package is giving you trouble, we recommend installing it globally in your machine in a different R session and then trying `renv::restore(rebuild = TRUE)`.

## Running the code with {targets}

This repository's workflow is based on [{targets}](https://books.ropensci.org/targets/). This means that the code is run in the appropriate order according to its internal dependencies.

The code is organised as *targets*, defined in the `_targets.R` file. Sometimes targets operate with the outcomes of other targets. For instance, one target might run a function that takes the outcome of a different target as argument. {targets} makes sure that the functions and objects needed to run each target have already been previously defined. You can visually explore this repository's targets and its dependency structure by running `targets::tar_visnetwork()`.

To run the code and generate the outputs of interest, run `make()` in your console. This function (defined in [utils.R](R/utils.R)) is a wrapper that invokes `targets::tar_make()` in the *jobs* tab in your RStudio session thanks to the {jobs} package. The code will run automatically while your R console will remain free for you to run code on it. You can stop the job at any point. If you are running R outside RStudio, we recommend you use the `targets::tar_make()` command.

You can explore the contents of the executed targets by running `tar_load_all()` (also defined in [utils.R](R/utils.R)), which is another wrapper function that calls `targets::tar_load()` on all defined targets to load them into your workspace. Mind that this process might take some time (~10 seconds) as brmsfit objects (Bayesian models) are heavy. Make sure that your RAM has equal or higher capacity than 

## Repository structure and files

This repository is organised as follows:

* **data**: processed data in CSV format
    - [items.csv](data/items.csv): information about words included in the analyses
    - [participants.csv](data/participants.csv): information about participants
    - [responses.csv](data/responses.csv): participant responses to the items. The model was fit on this dataset.
* **data-raw**: raw data from the [Barcelona Vocabulary Questionnaire, BVQ](https://gongcastro.github.io/bvq). This is a RDS file containing a list of data frames with all the information necessary to generate the datasets in the data/ directory.
* **docs**: source code to generate the documentation site of the project ([cognate-beginnings]([gongcastro.github.com/cognate-beginnings)].
* **manuscript**: Quarto document with the source code of the manuscript and appendix
* **R**: R functions used in the targets to process and analyse the data.
    - [items.R](R/items.R): to generate `items.csv`
    - [models.R](R/items.R): to fit the Bayesian model and extract posterior draws
    - [participants.R](R/participants.R): to generate `participants.csv`
    - [predictions.R](R/predictions.R): to generate posterior predictions from the model
    - [utils.R](R/utils..R): helper functions and wrappers used across the project
* **renv**: internal settings to ensure reproducibility of the computing environment.
* **results**: model outputs. You will need to run the code to generate the files that will be contained in this directoty.)
    - fits: RDS files with the brmsfit of the Bayesian models
    - posterior: CSV files with the posterior draws of the population-level and group-level coefficients
    - predictions: CSV files with the posterior predictions
* **src**: R functions to make programming tasks easier, not needed to reproduce the project.
* **stan**: Stan code of the models, as generated by `brms::stancode()`.
* **test**: testthat scripts used to test the functions used across the project.

    
    

RDS files with the contents of each model (e.g., MCMC samples), as generated by `brms::brm()`. Generating these files by running the R code might take long, depending on your set up. If you want to skip this step by loading already generated RDS files, please reach out to @gongcastro, who will send you the compressed `results` folder.


* **stan**: Stan code used to fit the models, as generated by `brms::makecode()`. This code is not involved in any step of the data analysis workflow, but may be helpful for those who want to take a look at what the models look like in Stan code.
* **tests**: unit tests used to make sure that the code has run without errors. These tests are run automatically as targets when `make()` is called.

