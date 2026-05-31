# Skeena Watershed Fish Passage Restoration Planning 2024

> Reproducible, web-first restoration-planning report for fish-passage barriers across the Skeena watershed, prepared for the Skeena Environmental Restoration Network (SERN BC).

**Read the report:** <https://newgraphenvironment.github.io/fish_passage_skeena_2024_reporting>
&middot; **Source:** [`NewGraphEnvironment/fish_passage_skeena_2024_reporting`](https://github.com/NewGraphEnvironment/fish_passage_skeena_2024_reporting)
&middot; **Version history:** [`NEWS.md`](NEWS.md)

## What this is

A bookdown report assembling crossing-by-crossing assessments, fish-presence and habitat-confirmation data, and prioritization analyses across the Skeena watershed. Each prioritized barrier crossing has a dedicated appendix with photos, field measurements, and recommended remediation; the executive summary rolls them up for partners and decision-makers. Source data and methods are open — anyone can rebuild the report from `scripts/run.R`.

## Sections

- **Executive Summary** ([`0050-executive-summary.Rmd`](0050-executive-summary.Rmd))
- **Introduction** ([`0100-intro.Rmd`](0100-intro.Rmd))
- **Background** ([`0200-background.Rmd`](0200-background.Rmd)) — Skeena ecological context, prior assessment work.
- **Methods** ([`0300-methods.Rmd`](0300-methods.Rmd))
- **Results** ([`0400-results.Rmd`](0400-results.Rmd)) — barrier prioritization, per-watershed-group findings.
- **Recommendations** ([`0500-recommendations.Rmd`](0500-recommendations.Rmd))
- **Appendices** ([`0600-appendix.Rmd`](0600-appendix.Rmd) + per-site `0800-appendix-*.Rmd` files) — site-specific data for each prioritized crossing.

## Build

```r
source("scripts/run.R")
```

`scripts/run.R` sources `scripts/staticimports.R` (which inlines helper functions via the [staticimports](https://github.com/wch/staticimports) package) and then runs `bookdown::render_book()`. Bypassing `run.R` gives "undefined function" errors at render time.

## Open-source packages used

| Package | Role |
|---|---|
| [`fresh`](https://github.com/NewGraphEnvironment/fresh) | FWA stream-network primitives + habitat classification driving accessible / spawning / rearing layers. |
| [`link`](https://github.com/NewGraphEnvironment/link) | Cross-system crossing matching + barrier-override resolution from observation evidence. |
| [`ngr`](https://github.com/NewGraphEnvironment/ngr) | Reporting utilities — table formatting, S3 helpers, STAC, GitHub-issue scraping. |
| [`fpr`](https://github.com/NewGraphEnvironment/fpr) | Fish-passage-specific reporting functions (PSCIS tables, crossing details). |
| [`gq`](https://github.com/NewGraphEnvironment/gq) | Cartographic style registry across the report's maps. |
| [`cd`](https://github.com/NewGraphEnvironment/cd) | Climate-departure analysis for the climate context appendix. |
| [`cred`](https://github.com/NewGraphEnvironment/cred) | Post-draft citation verification across references. |

External: [`bcfishpass`](https://github.com/smnorris/bcfishpass), [`fwapg`](https://github.com/smnorris/fwapg).

## License

MIT (see [`LICENSE`](LICENSE)). Indigenous traditional knowledge shared with this work remains the cultural and intellectual property of the contributing Nations; sharing or republishing those contributions requires explicit consent.
