# Data

`raw/study1.csv`, `raw/study2.csv`, and `raw/study3.csv` contain 2,000,
1,505, and 409 records from the August 2018, June 2020, and July 2020 surveys.
They come from version 1.0 of the
[Dataverse replication deposit](https://doi.org/10.7910/DVN/KF1LAK).
`ip_index` groups repeated addresses within a survey without revealing the
address. Study 1 records are paid assignments; the other studies contain survey
responses. Missing survey answers remain missing unless a measure's definition
specifies otherwise.

The [data dictionary](../docs/data-dictionary.csv) lists retained columns and
source labels. The [source manifest](../docs/source-manifest.csv) records input
and output hashes and the fields removed during anonymization.
[Instrument items](../docs/instrument-items.csv) give the sleep and sincerity
wording and response options from the supplied questionnaires.

## Anonymization

`R/01_anonymize.R` extracts the three study files from the deposited ZIP. It
removes unused recipient, network, and fine-location metadata, including
hostnames and URLs that contain IP addresses. It replaces three phone-shaped
entries in the date field with `[numeric response redacted]`. It preserves all
rows and other retained values, and writes UTF-8 CSVs.

Place source files in `data/original/`, which Git ignores. From the repository
root, run:

```sh
Rscript R/01_anonymize.R data/original/replication_public.zip
```

The public build reads the bundled files. It does not need the original ZIP.

## Measures

The IP screen combines missing addresses, blacklist indicators, repeated
addresses within a survey, and locations outside the United States. The
analysis uses the IP-service metadata collected for the study. In 2020, an
unavailable blacklist result does not trigger the blacklist flag; sensitivity
results also report the counts if those unavailable results were positive.
Country and city fields describe service-reported locations.

The rare-trait screen flags at least two affirmative items. Its component
recodes treat missing answers as negative; the accompanying sensitivity results
allow unanswered items to be affirmative. Timing screens exclude missing
durations. Self-reported insincerity uses some/most/always in 2018 and
most/always in 2020; a separate some-or-more measure uses the same threshold
across all three surveys.

## Comparison surveys

`raw/comparison_surveys.csv` supplies aggregate counts for eight surveys:

| Column | Meaning |
|---|---|
| `survey` | Survey name |
| `platform` | Recruitment platform |
| `year` | Survey year |
| `n` | Number of records |
| `flagged` | Records flagged by at least one IP indicator |
| `unknown` | Records whose combined flag remains unresolved |

Repeated addresses are counted within each survey. The June and August Graham
2019 experiments are separate surveys. The pipeline calculates percentages
as `100 * flagged / n` and combines them with the three study results to
produce the platform table. These aggregate counts were calculated from
restricted comparison files identified in [comparison sources](../docs/comparison-sources.csv).
The individual records from those surveys are not distributed here.
