# autoxgboostMC - Multiple-Criteria tuning and fitting of [xgboost](https://github.com/dmlc/xgboost).

<img align="right" src="https://raw.githubusercontent.com/ja-thomas/autoxgboost/master/man/figures/hexagon.svg?sanitize=true" width="125px">


[![Build Status](https://travis-ci.org/ja-thomas/autoxgboost.svg?branch=master)](https://travis-ci.org/ja-thomas/autoxgboost)
[![Coverage Status](https://coveralls.io/repos/github/ja-thomas/autoxgboost/badge.svg?branch=master)](https://coveralls.io/github/ja-thomas/autoxgboost?branch=master)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/autoxgboost)](https://CRAN.R-project.org/package=autoxgboost)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/autoxgboost)](https://cran.rstudio.com/web/packages/autoxgboost/index.html)


* Installing the development version

    ```splus
    devtools::install_github("ja-thomas/autoxgboostMC")
    ```

# General overview

autoxgboost aims to find an optimal [xgboost](https://github.com/dmlc/xgboost) model automatically using the machine learning framework [mlr](https://github.com/mlr-org/mlr)
and the bayesian optimization framework [mlrMBO](https://github.com/mlr-org/mlrMBO).

**Work in progress**!

# autoxgboost - How to Cite

The **Automatic Gradient Boosting** framework was presented at the [ICML/IJCAI-ECAI 2018 AutoML Workshop](https://sites.google.com/site/automl2018icml/accepted-papers) ([poster](poster_2018.pdf)).
Please cite our [ICML AutoML workshop paper on arxiv](https://arxiv.org/abs/1807.03873v2).
You can get citation info via `citation("autoxgboost")` or copy the following BibTex entry:

```bibtex
@inproceedings{autoxgboost,
  title={Automatic Gradient Boosting},
  author={Thomas, Janek and Coors, Stefan and Bischl, Bernd},
  booktitle={International Workshop on Automatic Machine Learning at ICML},
  year={2018}
}
```
