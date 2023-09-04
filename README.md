OSE data science, Prof. Dr. Philipp Eisenhauer | Summer 2021, M.Sc. Economics, Bonn University

# Replication of Angrist and Lavy (1999)

> Angrist, J. and V. Lavy (1999). "[Using Maimonides' rule to estimate the effect of class size on scholastic achievement](https://economics.mit.edu/files/8273)". *The Quarterly Journal of Economics* 114 (2), pp. 533–575.

### Description of the project

This project replicates and analyzes the above cited paper by Angrist and Lavy (1999).

In Israeli public schools the division of enrollment cohorts into classes is officially determined by the Maimonides rule.
This rule, named after the medieval rabbinic scholar Maimonides, stipulates a maximum class size of 40 which induces a nonlinear and nonmonotonic relationship between grade enrollment and class size.
The authors exploit this administrative rule to construct instrumental variables estimates of the effect of class size on test scores for Israeli elementary school students.
The estimates show that class size reductions lead to a significant increase in scholastic achievement for fourth and fifth graders, although not for third graders.

In a recently published paper ("Maimonides' rule redux", 2019) Angrist et al. revisit the original results.
They redo the analysis for new data and investigate the validity of the former estimation results.
We, therefore, also replicate relevant parts of the follow-up study.
Moreover, we enrich the analysis by independent contributions, mainly in the form of visualization and robustness checks.

### Structure of the repository

The replication project is presented in the Jupyter notebook ``replication_notebook.ipynb``.
The folder [data](https://github.com/OpenSourceEconomics/ose-data-science-course-project-svjaco/tree/master/data) contains the original data provided by the authors (DTA format) as well as our cleaned versions (CSV format).
All coding was conducted in R, the scripts are stored in the directory [R_code](https://github.com/OpenSourceEconomics/ose-data-science-course-project-svjaco/tree/master/R_code).
Furthermore, in the notebook we embed figures from the folder [materials](https://github.com/OpenSourceEconomics/ose-data-science-course-project-svjaco/tree/master/materials) and causal graphs from the same-named folder (with the underlying TeX code).

### Accessing the project

#### Viewing notebook:

The recommended way to view ``replication_notebook.ipynb`` is by downloading the repository from GitHub and then open it locally via Jupyter Notebook.
This ensures that all contents are displayed properly and internal links do work.
However, the following badges allow to easily access the notebook.

[![nbviewer](https://github.com/jupyter/design/blob/master/logos/Badges/nbviewer_badge.svg)](https://nbviewer.jupyter.org/github/OpenSourceEconomics/ose-data-science-course-project-svjaco/blob/master/replication_notebook.ipynb)
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/OpenSourceEconomics/ose-data-science-course-project-svjaco/master?filepath=replication_notebook.ipynb)

Currently the build with binder fails due to a version dependency issue of some package.
Please use nbviewer for the time being.

#### Running notebook:

To run ``replication_notebook.ipynb`` locally, all packages that are listed in ``environment.yml`` have to be installed.
Using conda this can be achieved by running in the terminal:

```zsh
$ conda env create -f environment.yml
$ conda activate ose_project_sven_jacobs
```

Then it should be possible to launch and execute the notebook from inside the terminal session.

### Acknowledgement

I would like to thank Prof. Shany.
As one of the authors of "Maimonides' rule redux" (2019) she kindly helped me to clarify the meaning of one of the variables ("c_pik") in the original data.

Also, I acknowledge the fact that Prof. Angrist has published the data sets and some Stata code for the Maimonides paper on his
[data archive](https://economics.mit.edu/faculty/angrist/data1/data/anglavy99).

---

[![Continuous Integration](https://github.com/OpenSourceEconomics/ose-data-science-course-project-svjaco/actions/workflows/ci.yml/badge.svg)](https://github.com/OpenSourceEconomics/ose-data-science-course-project-svjaco/actions/workflows/ci.yml)
[![Licence](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/OpenSourceEconomics/ose-data-science-course-project-svjaco/blob/master/LICENSE)
