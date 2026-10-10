

# The Micro-Task Market for Lemons

Douglas J. Ahler, Carolyn E. Roush, and Gaurav Sood

Amazon's Mechanical Turk makes survey recruitment cheap and fast, but researchers
have limited information about the quality of the responses they buy. Workers
can earn approval and payment without answering carefully or sincerely. We
examine how often response-quality screens flag participants and whether
excluding flagged responses changes experimental results.

Three surveys conducted in August 2018, June 2020, and July 2020 contain
2,000, 1,505, and 409 records. Suspicious IP addresses account for
20.3%, 12.1%, 9.8% of records, respectively.
The share reporting at least two uncommon traits or experiences is
6.2%, 30.0%, 21.0%.
Combining these screens flags
24.6%, 37.9%, 27.6%.
The decline in IP flags therefore does not imply a corresponding improvement
in every measure of response quality.

Screening also changes experimental estimates. In the June survey, participants
evaluated the same economic changes under randomly assigned partisan cues.
The full-sample effects are 23% smaller for
unemployment and 34% smaller for inflation than
the effects among unflagged respondents. The respective 95% bootstrap intervals
are 4% to
39% and
9% to
57%.
These comparisons show how screening affects the estimates; the screens do not
establish which individual responses are truthful.

![Experimental effects by response screen](figs/experiment_effects.png)

The [paper](https://doi.org/10.1017/psrm.2021.57) appeared in *Political Science
Research and Methods*. The original replication archive is on
[Harvard Dataverse](https://doi.org/10.7910/DVN/KF1LAK).
This repository builds the analyses and [manuscript](ms/main.pdf) in R.

## Run the analysis

With R 4.6, XeLaTeX, `latexmk`, and Poppler installed:

```sh
make restore
make check
```

Or run the complete build and checks in Docker:

```sh
make ci-docker
```

Both commands generate the figures in `figs/`, numerical results and tables in
`tabs/`, and the paper at `ms/main.pdf`. `make check` also runs lint and tests.
For the paper alone, use `make paper`. Both bootstrap analyses use 1,999 draws
and fixed seeds; set `TURK_CORES=4` to use four local processes.

## Data and code

The bundled inputs include anonymized records for the three surveys and
aggregate counts for the comparison surveys. The public build uses these files
without private data or calls to IP services. The [data documentation](data/README.md)
explains the variables and sources.

`R/00_replicate.R` runs the analysis. The implementation follows this order:

| File | Purpose |
|---|---|
| `01_anonymize.R` | Prepare public inputs from the deposited survey files |
| `02_ip.R` | IP screens and comparisons across surveys |
| `03_survey.R` | Survey measures, response patterns, and timing |
| `04_models.R` | Experimental estimates and bootstrap intervals |
| `05_diagnostics.R` | Outcome summaries and inference checks |
| `06_exhibits.R` | Tables and figures |

Anonymization is only needed when rebuilding the bundled inputs:

```sh
Rscript R/01_anonymize.R data/original/replication_public.zip
```

Keep original survey files in `data/original/`, which Git ignores.
`README.Rmd` generates this README using the analysis results.
