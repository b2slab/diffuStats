# diffuStats: compute diffusion scores over networks

[![Travis-CI Build Status](https://travis-ci.org/b2slab/diffuStats.svg?branch=master)](https://travis-ci.org/b2slab/diffuStats)
[![codecov.io](https://codecov.io/github/b2slab/diffuStats/coverage.svg?branch=master)](https://codecov.io/github/b2slab/diffuStats?branch=master)

## Introduction

The general purpose `diffuStats` R package offers a collection of seven network propagation scores and five graph kernels.
Those find application in ubiquitous computational biology applications, being one representative example the propagation of genetic information (e.g. disease-associated genes) in a gene-gene or a protein-protein interaction network.
A distinctive feature of `diffuStats` is the implementation of statistically normalised scores, which address the recurrent question of how would the propagation of a randomised input look.
It offers parametric, exact z-scores as well as permutation-based empirical probabilities.

The `diffuStats` software was published in:

> Picart-Armada, S., Thompson, W. K., Buil, A., & Perera-Lluna, A. (2018). diffuStats: an R package to compute diffusion-based scores on biological networks. Bioinformatics, 34(3), 533-534.

General guidelines on how to choose the scores, along with mathematical properties of the normalised and unnormalised scores, were published in:

> Picart-Armada, S., Thompson, W. K., Buil, A., & Perera-Lluna, A. (2020). The effect of statistical normalisation on network propagation scores. Bioinformatics, btaa896.

## Installation

`diffuStats` is part of Bioconductor, and can be installed using

```{r}
BiocManager::install("diffuStats")
```

For the development version, you can also install the package through `R CMD INSTALL` or through `devtools::install_github("b2slab/diffuStats")`, which points to its [GitHub repository](https://github.com/b2slab/diffuStats).


## Getting started

`diffuStats` is suitable for medium-sized networks (thousands of nodes) and is conceived to be used in biological networks. 
Its limitations come from the kernel formalism: networks exceeding 20k nodes will start requiring large kernel matrices in memory.

Get started by looking at the package vignettes (`intro` for a quickstart, `diffuStats` for a complete documentation) or its help

```
?diffuStats
```

## News

File `NEWS.md` keeps track of the additions and bug fixes of each 
package version.
