
# Trajectories [[report](https://github.com/gongcastro/trajectories/blob/master/Rmd/report.md)]

## Table of Contents
1. [Introduction](#introduction)
2. [Downloading this repository](#downloading)
3. [Reproducing this repository](#reproducing)
    3.1. [Installing package dependencies with {renv}](#packages)
    3.2. [Running the code with {targets}](#running)
    3.3. [Repository structure](#structure)


## What this repository is about<a name="introduction"></a>

Do bilingual children acquire cognates earlier than non-cognates? We collected vocabulary data from 10-to-32 month old infants exposed to a second language in varying degrees, and modelled the probability of acquisition of translation equivalents conditional participants' age, their degree of exposure to the second language, and the phonological overlap between word-forms across languages.

This repository hosts the code used to process and analyse the data, as well as the Rmarkdown files used to generate the preprint. The code and surveys used to collect and retrieve the data are hosted as a package in the [Multilex](github.org/gongcastro/multilex) repository. All other materials can be found in the [OSF](https://osf.io/hy984/) repository.


## Downloading this repository<a name="downloading"></a>

*NOTE*: We recommend using RStudio (as opposed to base R, the Command Prompt/console, or any other IDE). 

1) Download and install [Git](https://git-scm.com/downloads) with default settings.
2) Clone this repository locally. You can do this in two ways:

* Using your Command Prompt/console, navigate to the folder you want to download the repository into, and run:

```console
git clone https://github.com/bilingual-project/cognate-priming.git
```

If the repository is private at the time you try to clone it, you may have to enter your GitHub credentials)

* Clicking the green button "Code" in this page (upper-right corner), clicking "Download ZIP", and unzipping the downloaded repository.

## Reproducing this repository with {renv}<a name="reproducing"></a>

### Installing package dependencies with {targets}<a name="packages"></a>

The code in this repository needs some packages to run, many of which might not have been already installed in your local machine. Instead of having to install them yourself one by one or updating already installed ones (which might change how they behave, possibly breaking some of your code in other projects) this repository uses the R package [{renv}](https://rstudio.github.io/renv/articles/renv.html) to deal with package libraries. You can just install `renv` by running `install.packages("renv")` and then run `renv::restore()` in your console. This command will install all necessary packages in the appropriate version (listed in the file `renv.lock` in a self-contained R package library. This process will not affect the packages that you had already installed in your machine. 

In case the installation of any package is giving you trouble, we recommend installing it globally in your machine in a different R session and then trying `renv::restore(rebuild = TRUE)`.

### Running the code with {targets}<a name="running"></a>

This repository's workflow is based on [{targets}](https://books.ropensci.org/targets/). This means that code chunks and functions are run in the appropriate order according to their dependencies. Code chunks are organised as *targets*, as defined in the `_targets.R` file. Sometimes targets operate with the outcomes of other targets. For instance, one target might run a function that takes the outcome of a different target as argument. {targets} makes sure that when we run the repository code, lower-level targets have been defined before higher-level targets. You can visually explore this repository's targets and its dependency structure by running `targets::tar_visnetwork()`.

To run the code and generate the outputs of interest, run `make()` in your console. This function (defined in [utils.R](R/utils.R)) is a wrapper that invokes `targets::tar_make()` in the *jobs* tab in your RStudio session thanks to the {jobs} package. The code will run automatically while your R console will remain free for you to run code on it. You can stop the job at any point.

You can explore the contents of the executed targets by running `tar_load_all()` (also defined in [utils.R](R/utils.R)), which is another wrapper function that calls `targets::tar_load()` on all defined targets to load them into your workspace. Mind that this process might take some time (~10 seconds) as brmsfit objects (Bayesian models) are heavy. Make sure that your RAM has equal or higher capacity than 
## Repository structure<a name="structure"></a>

This repository is organised as follows:

* **data**: processed datasets in Parquet format. See R package {arrow} for more information.
* **docs**: Rmarkdown files used to generate the manuscript using the {papaja} R package, some manuscript by-products (TeX files), a `.bib` file that contains the references in a Rmarkdownd-friendly format, and some lab notes with extended details about the project.
* **img**: figures generated by the R code will be saved here.
* **R**: R functions used in the targets to import ([00_importing.R](R/00_importing.R)), preprocess ([00_preprocessing.R](R/01_preprocessing.R)) and model the data ([02_models.R](R/02_models.R)), along with some helper functions ([utils.R](R/utils.R)). You can check the documentation of each function as you would do in a normal R package (e.g., `?get_items`)
* **results**: RDS files with the contents of each model (e.g., MCMC samples), as generated by `brms::brm()`. Generating these files by running the R code might take long, depending on your set up. If you want to skip this step by loading already generated RDS files, please reach out to @gongcastro, who will send you the compressed `results` folder.
* **stan**: Stan code used to fit the models, as generated by `brms::makecode()`. This code is not involved in any step of the data analysis workflow, but may be helpful for those who want to take a look at what the models look like in Stan code.
* **tests**: unit tests used to make sure that the code has run without errors. These tests are run automatically as targets when `make()` is called.

