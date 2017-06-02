# Version log

## Version 0.99.0

* Changes in both vignettes, using Bioconductor style
* Submitting to Bioconductor

## Version 0.97.0

* Changes in main vignette
* Ready to submit to Bioconductor

## Version 0.5.5

* Final adjustments in main vignette
* Added example to quickstart vignette
* **First internal review version**

## Version 0.5.4

* Changes in vignette
* Temporarily depends on R>=3.2 to build a zip for iPsych

## Version 0.5.3

* Modified main vignette: more proper introduction, more cites
* Added code chunk to generate the figure in the paper

## Version 0.5.2

* Added basic sanity checking
* Fixed small bug: `ml` could be wrongly codified
* Sanity also checks the variance in the input - useful to detect
pathological cases
* Reduced n.perm in vignettes and examples - they should build faster
* Added unit testing for sanity checkers

## Version 0.5.1

* Vignette has been added a "proper" introduction
* Now main vignette is in sweave

## Version 0.5.0

* Fixed devel version numbering x.y.z, y must be odd
* Added correct indentation, built indentation-fixing function for Rd files
* Fixed CRAN's NOTE on R 3.4 about the c++ code
* Added NEWS file

## Version 0.4.3

* Fixed small bug in `gm`
* Added unit testing for inputs using kernels

### TODO

* Some more unit testing (helpers...). Aim at 100% coverage
* Test installation on other systems 
* Sanity check for inputs
* Optional: one kernel wrapper with abbreviations

## Version 0.4.2

* Added examples to all functions. `BiocCheck` not complaining anymore!
* Fixed small bugs

### TODO

* Some more unit testing (helpers...). Aim at 100% coverage
* Test installation on other systems 
* Sanity check for inputs
* Optional: one kernel wrapper with abbreviations

## Version 0.4.0

* Added all the kernels and respective helpers, unit testing. 
* `diffuse`, `perf` now accept kernels as inputs as well. Also, the 
default Laplacian kernel is computed using kernel functions

### TODO

* Some more unit testing (helpers...). Aim at 100% coverage
* Add examples to each function
* Test installation on other systems 
* Sanity check for inputs
* Optional: one kernel wrapper with abbreviations

## Version 0.2.0

* Added performance wrapper: `perf` (and its core `perf_eval`) 
Can apply diffusion over a grid
of parameters and compute a metric using "target scores". 
Unit testing written as well.
* Dropped `pROC` dependency, added `Metrics`
* Helper functions to deal with the format of the files

### TODO

* Some more unit testing (helpers...). Aim at 100% coverage
* Add kernels
* Add examples to each function
* Test installation on other systems 
* Sanity check for inputs

## Version 0.0.10

* Added first 'wrapper': 
input scores can be given as vector, matrix or list of matrices. 
Whatever format was given will be returned. 
Contains unit testing.
This simplified vignettes a bit as well.
* Modified `graph_toy` to contain examples of input vector and matrix.
Updated doc and first vignette.
* Dense `Matrix` objects converted to base matrix in diffusion output.

### TODO

* Exhaustive unit testing
* Add kernels
* Add examples to each function
* Test installation on other systems 
* Add wrappers for several scores screening, AUC, CV(?)

## Version 0.0.8

* Added `largest_cc` helper
* First version of the `yeast` vignette. Takes 1:15 to build.

### TODO

* Exhaustive unit testing
* Add kernels
* Add examples to each function
* Test installation on other systems 
* Should we add wrappers? (cross-validation scores, AUC... 
We can use `try` to give `NA` as an AUC value for bad data splits)


## Version 0.0.6

* Fixed bug in `ber_s` (both matrices are from `Matrix`)
* Fixed bug in `gm` (missed `drop = FALSE` for the single-column case)
* Started second vignette with a bio example: yeast ppi. Added suggests.
* Now using `igraphdata`, the example is perfect!
* `raw` and `ber_s` will coincide if the tested nodes do not belong to 
the background

### TODO

* Exhaustive unit testing
* Add kernels
* End vignette

## Version 0.0.4

* Added synthetic input generator
* Fixed a bug in z-score - sums were computed over the raw scores 
and not the input
* Changed internal representation of default values for graph generation
* Added more unit testing (still missing some)
* Started synthetic example, see `data-raw`

### TODO

* Exhaustive unit testing
* Add kernels
* Add biological example - search for a Bioconductor package 
with a network in igraph format
* Add example
* Investigate: are `raw` and `ber_s` the same? 
They should be similar but not the same

## Version 0.0.2

First version of the package. Includes: 

* Synthetic network generation (`igraph` backend)
* Diffusion scores: `raw`, `ml`, `gm`, `z`, `mc`, `ber_s`, `ber_p`
* Monte carlo trials are in C++ and parallelised
* Support for diffusion in batchs with different backgrounds
* Unit testing of the majority of functions
* Example data
* Vignette
* Helper functions to plot colours and shapes

Results of `devtools::check()`: 1 NOTE 
(line *SystemRequirements: GNU make* in DESCRIPTION file)
