
# utils.add

<!-- badges: start -->

<!-- badges: end -->

`utils.add` is an R package designed to facilitate an eclectic and
evolving set of R operations commonly implemented by the author in the
course of scientific research and/or other R packages. These functions
generally fall into one of the following themes:

  - Extract data
  - Learn from data
  - Modify data
  - Common mathematical operations
  - Development tools

It is important to emphasise that `utils.add` is not a comprehensive
package designed to deliver any one of these themes: many R packages
already fill this space. `utils.add` simply makes a few eclectic
operations that can be loosely grouped into these themes a bit easier in
cases where this has proven useful in the course of the author’s
research.

## Installation

You can install the released version of utils.add from
[CRAN](https://CRAN.R-project.org) with:

``` r
# Install from CRAN
install.packages("utils.add")
```

``` r
# Or, install the development version from github:
devtools::install_github("edwardlavender/utils.add")
```

## Extract data

Some functions facilitate data extraction. These include:

  - `isnt_na()` – Extract observations which are not NA;
  - `substr_end()` – Extract the last n letters from a character;
  - `pos_first_unique()` – Extract the first position of each unique
    element in a vector;
  - `around()` – Extract the rows in a dataframe around specified
    position(s);
  - `sterm()` – Define `mgcv` model smooth term names (e.g. to extract
    values from `mgcv::predict.gam()`);

## Learn from data

Some functions which facilitate learning from data. These include:

  - `count_if()` – Count the number of observations which meet a
    specified condition;
  - `basic_stats()` – Compute basic statistics (e.g. `mean`, `mode`,
    etc.) for a sample;
  - `seq_range()` – Generate a sequence between two values;
  - `round_range()` – Round a range down and up at lower/upper limits
    respectively;
  - `clip_within_range()` – Clip a vector to lie within a range;

## Modify data

Some functions facilitate data modification. The include:

  - `add_list_null()` – add `list(NULL)` to a list in replacement of
    `NULL` elements;
  - `list_merge()` – merge lists, accounting for empty lists;

## Common mathematical functions

  - `linear()` – A linear function based on parameters `a` and `b`;
  - `quadratic()` – A quadratic function based on parameters `a`, `b`,
    `h` and `k`;
  - `sigmoid()` – A (centred) sigmoidal function based on parameters
    `x0`, `L` and `k`;

## Development tools

  - `check...()` – Check arguments supplied via `...` within a function;
  - `check_value()` – Check that the value of an input supplied to a
    parent function is appropriate;
  - `check_class()` – Check that the class of an inputted object
    supplied to a parent function is appropriate;
  - `check_tz()` – Check the time zone of an object and, if appropriate
    and necessary, force UTC;
  - `check_names()` – Check that required names are contained within an
    object;
  - `check_named_list()` – Check that a list is named;
  - `check_length()` – Check that the length of an object is the
    required length;
