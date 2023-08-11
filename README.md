# Cognate beginnings to bilingual lexical acquisition

| Link                                                             | Contents                                                       |
|------------------------------------------------------------------|----------------------------------------------------------------|
| [Website](https://gongcastro.github.io/cognate-beginnings)       | Instructions for reproducibility, data dictionaries, lab notes |
| PsyArxiv                                                         | Preprint and figures                                           |
| [GitHub](https://github.com/gongcastro/cognate-beginnings)       | Code, preprint and figures                                     |
| [Open Science Framework](https://osf.io/hy984/)                  | Code, preprint, and results (model outputs)                    |
| [Docker](https://hub.docker.com/r/gongcastro/cognate-beginnings) | Docker image with reproducible RStudio session                 |


# Repository structure and files 📂

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
    ([cognate-beginnings](https://gongcastro.github.com/cognate-beginnings)).
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
    -   [utils.R](R/utils.R): helper functions and wrappers used across
        the project
-   **renv**: internal settings to ensure reproducibility of the
    computing environment.
-   **results**: model outputs. You will need to run the code to
    generate the files that will be contained in this directoty.
    -   fits: RDS files with the brmsfit of the Bayesian models
    -   posterior: CSV files with the posterior draws of the
        population-level and group-level coefficients
    -   predictions: CSV files with the posterior predictions
-   **src**: R functions to make programming tasks easier, not needed to
    reproduce the project.
-   **Stan**: Stan code of the models, as generated by
    `brms::stancode()`.
-   **tests**: testthat scripts used to unit test the functions used
    across the project.
