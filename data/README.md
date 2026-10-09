# Data sources and definitions

The three public study files contain 2,000 responses collected in August 2018,
1,505 in June 2020, and 409 in July 2020. Their source is version 1.0 of the
[Harvard Dataverse replication deposit](https://doi.org/10.7910/DVN/KF1LAK).
The deposited ZIP matches the local August 2021 ZIP for these files. Each row
is an archived analysis record. In Study 1 the records represent paid
assignments: two assignments by different workers share one survey completion
code and therefore link to the same survey response. An IP address can occur
in several rows; `ip_index` is
an anonymized grouping index that is meaningful only within a study.

`raw/study1.csv`, `raw/study2.csv`, and `raw/study3.csv` retain the deposited
analysis variables. Empty recipient identifiers and unused IP-service
metadata are omitted. Hostnames and URLs in the deposit contain respondent
IP addresses. Omitted metadata also include precise coordinates, postal
codes, network infrastructure information, and other service outputs unused
by the analysis. Retained metadata are the MaxMind city and country names,
IPVoid blacklist status, APIVoid detection count, and the all-missing VPN
placeholder selected by the original code. The retained files have 189, 76,
and 54 columns. Three ten-digit date responses lacking the survey year are
replaced with `[numeric response redacted]`: one in Study 2 and two in Study
3. These remain invalid responses under both the original and updated date
screens. No rows are removed. All other retained values are unchanged.
The source byte strings are read as Latin-1 and written as UTF-8;
analyses use the original column names and standard `read.csv` missing-value
parsing. The string `NA` is missing; blank numeric cells become missing, while
blank character cells remain empty strings.

[The source manifest](../docs/source-manifest.csv) records archive members,
SHA-256 hashes before and after processing, sample sizes, and omitted columns.
[The data dictionary](../docs/data-dictionary.csv) records every retained
column, available original Stata labels, derived-variable definitions, and
observed missingness. Source labels retain the original questionnaire wording
or metadata field name; a source field name alone is not a validated measure
description.

To regenerate the public files from the deposited ZIP, run this command from
the repository root:

```sh
Rscript scripts/prepare_public_inputs.R /path/to/replication_public.zip
```

The normal analysis uses only the bundled public files and makes no IP-service
requests. `blacklisted` uses IPVoid in 2018 and APIVoid in 2020. The original
2020 construction treats unavailable blacklist results as zero; the analysis
preserves that rule for replication and separately reports the range obtained
by classifying otherwise unflagged unavailable results as positive. The
`funny_ip` variable is the union of missing, blacklisted, duplicated, and
foreign IP indicators. These indicators characterize responses and do not
establish respondent intent. Repeated addresses can also reflect shared
networks. Location tables count responses with observed metadata.

The [survey-unit sensitivity](../tabs/ip_unit_sensitivity.csv) retains the
deposited sample in the default analysis and separately counts each fully
matching observed survey record once, excluding the assignment-specific
`worktimeinseconds` field from the comparison. Missing-IP records are excluded
from duplicate detection because they have no observed survey response in
Study 1. IP flags are reconstructed after changing the sample. The repeated
record otherwise has no positive IP or rare-response flags; linking it to two
assignments mechanically creates two IP-duplication flags. The sensitivity
also reports rare-response and combined-screen bounds when unanswered rare
items are counted as negative or positive. These bounds describe uncertainty
from item nonresponse; they do not establish the prevalence of bad actors.

Third-party surveys used for the paper's cross-study comparison are absent
from the public deposit. [The restricted-data audit](../docs/restricted-ip-audit.csv)
contains aggregate calculations from the locally supplied files, with
[source hashes](../docs/restricted-source-manifest.csv). Those calculations
cannot be independently reproduced from this repository alone. With authorized
access to the source data directory, reproduce them using:

```sh
Rscript scripts/audit_restricted.R /path/to/restricted/data /path/to/audit/output
```

The audit compares the archived composite flags with flags reconstructed
using duplicates within each named survey group. The two Graham 2019
experiments share one archived group label; the audit also reports each
experiment separately. The group-level correction isolates the composite
flag implementation error. Splitting the experiments changes the duplicate
definition and is a separate comparison. Aggregate percentages use all
survey responses as the denominator, with unresolved composite flags
reported explicitly in `corrected_unknown`.
