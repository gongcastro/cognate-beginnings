# Reproducing this repository

Follow these steps (see below for details):

0.  Download this repository
1.  Install software dependencies
2.  Install package dependencies with renv
3.  Running the code with targets
4.  Repository structure and files

## 0. Download this repository ⬇️

First, you will need to download this repository to your local machine.
We suggest three different ways:

a)  **Git terminal**: download and install
    [Git](https://git-scm.com/downloads) with default settings and clone
    this repository locally running the following command in your
    console:

``` bash
git clone https://github.com/gongcastro/cognate-beginnings.git
```

> 💡 You don't need to run this command from the Git console. Just open
> Command Prompt or Power Shell in Windows (just make sure that Git is
> included in your [PATH
> variables](https://www.delftstack.com/howto/git/add-git-to-path-on-windows/),
> which should be already done after the recommended Git installation),
> Terminal in MacOS, or command line in Linux and run this command.

b)  **GitHub releases**: download the latest
    [release](https://github.com/gongcastro/cognate-beginnings/releases)
    of the repository.

c)  **Direct download**: click the [green
    button](https://github.com/gongcastro/cognate-beginnings/archive/refs/heads/main.zip)
    "Code" in this page (upper-right corner), click "Download ZIP", and
    unzipping the downloaded repository.

## 1. Install software dependencies 💻

Here's a list of programs that you might need to install in your machine
to make the project work:

-   [R](https://www.r-project.org/) (4.2.2 or greater)

-   [RStudio](https://posit.co/download/rstudio-desktop/): although not
    strictly necessary, we encourage the use of Rstudio for reproducing
    the present project. We have not tested the reproducibility of the
    project in other IDEs.

-   [Quarto](https://quarto.org/docs/get-started/) (1.3.340 or greater):
    we use this software to generate the manuscript and lab notes. We
    recommend installing version 1.3.340 or higher, although previous
    version might work as well.

-   [Rtools](https://cran.r-project.org/bin/windows/Rtools/) (4.3 or
    greater).

-   [CmdStan](https://mc-stan.org/users/interfaces/cmdstan) (2.31.0 or
    greater): we use the CmdStan backend in brms to fit our Bayesian
    models. We recommend installing CmdStan using its R interface
    [CmdStanR](https://mc-stan.org/cmdstanr/) following the [*Getting
    started with
    CmdStanR*](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)
    vignette. You can also install it following the [CmdStan
    installation guide](https://mc-stan.org/users/interfaces/), and then
    letting CmdStanR know the path to the folder using
    `cmdstanr::set_cmdstan_path()`.

    <div>

    > 💡 **Installing CmdStan** is sometimes inconvenient, and might
    > require some time to set up, specially if it's your first time
    > doing so. Have patience and follow the CmdStan installation guide
    > closely. If you encounter problems and just don't have the time,
    > you can download the models from the [Open Science Framework
    > repository](https://osf.io/hy984/) and skip the model-fitting
    > steps.

    </div>

## 2. Install package dependencies with renv 📦

Open the downloaded repository folder and click on the
[cognate-beginnings.Rproj](cognate-beginnings.Rproj) to open an RStudio
session (recommended) or just open an R session and set the working
directory to the downloaded folder. Once open run:

``` r
install.packages("renv") # in case you need to install renv
renv::restore()
```

This might take a couple of minutes, depending on the number of packages
that need installation or an update.

> 💡 **What does this step do?** The code in this repository needs some
> packages to run, many of which might not have been already installed
> in your local machine. Instead of having to install them yourself one
> by one or updating already installed ones (which might change how they
> behave, possibly breaking some of your code in other projects) this
> repository uses the R package
> [renv](https://rstudio.github.io/renv/articles/renv.html) to deal with
> package libraries. The command `renv::restore()` will install all
> necessary packages in the appropriate version (listed in the file
> [renv.lock](renv.lock) in a self-contained R package library. This
> process will not affect the packages that you had already installed in
> your machine.
>
> 💡 **Something went wrong with renv. Now what?** Although renv
> facilitates the reproducibility of the repository, it is not
> guaranteed that things will work perfectly. If you encounter trouble
> installing the packages using renv, try installing them individually
> using (`install.packages()` or `renv::install()`) and try fixing
> whatever issues arise (they might depend on your particular setup).
> Some packages are used by RStudio itself, and might refiuse to update
> them while you're using RStudio. Try installing them from the R
> console outside of RStudio. Open an
> [issue](https://github.com/gongcastro/cognate-beginnings/issues) on
> this repository if you need further assistance! We'll get back to you
> ASAP. :smile:

## 3. Running the code with targets :rocket:

Once the package dependencies have been solved with renv, run the
following command:

``` r
targets::tar_make()
```

Mind that this process might take some time. Refitting the brms models
might take very long (days even, depending on your set-up). If you want
to skip this step, download the "results" folder in the [Open Science
Framework repository](https://osf.io/hy984/) and replace it in the
repository.

> 💡**What does this step do?** This repository's workflow is based on
> [targets](https://books.ropensci.org/targets/). This means that the
> code is run in the appropriate order according to its internal
> dependencies. The code is organised as *targets*, defined in the
> `_targets.R` file. Sometimes targets operate with the outcomes of
> other targets. For instance, one target might run a function that
> takes the outcome of a different target as argument. {targets} makes
> sure that the functions and objects needed to run each target have
> already been previously defined. You can visually explore this
> repository's targets and its dependency structure by running
> `targets::tar_visnetwork()`. You can explore the contents of the
> executed targets by running `targets::tar_load_everything()`, which
> will load all defined targets into your workspace.

## 4. Repository structure and files 📂

This repository is organised as follows:

-   **data**: processed data in CSV format
    -   [items.csv](data/items.csv): information about words included in
        the analyses
    -   [participants.csv](data/participants.csv): information about
        participants
    -   [responses.csv](data/responses.csv): participant responses to
        the items. The model was fit on this dataset.
-   **data-raw**: raw data from the [Barcelona Vocabulary Questionnaire,
    BVQ](https://gongcastro.github.io/bvq). This is a RDS file
    containing a list of data frames with all the information necessary
    to generate the datasets in the data/ directory.
-   **docs**: source code to generate the documentation site of the
    project
    ([cognate-beginnings](%5Bgongcastro.github.com/cognate-beginnings)].
-   **manuscript**: Quarto document with the source code of the
    manuscript and appendix
-   **R**: R functions used in the targets to process and analyse the
    data.
    -   [items.R](R/items.R): to generate `items.csv`
    -   [models.R](R/items.R): to fit the Bayesian model and extract
        posterior draws
    -   [participants.R](R/participants.R): to generate
        `participants.csv`
    -   [predictions.R](R/predictions.R): to generate posterior
        predictions from the model
    -   [utils.R](R/utils..R): helper functions and wrappers used across
        the project
-   **renv**: internal settings to ensure reproducibility of the
    computing environment.
-   **results**: model outputs. You will need to run the code to
    generate the files that will be contained in this directoty.)
    -   fits: RDS files with the brmsfit of the Bayesian models
    -   posterior: CSV files with the posterior draws of the
        population-level and group-level coefficients
    -   predictions: CSV files with the posterior predictions
-   **src**: R functions to make programming tasks easier, not needed to
    reproduce the project.
-   **stan**: Stan code of the models, as generated by
    `brms::stancode()`.
-   **test**: testthat scripts used to unit test the functions used
    across the project.
