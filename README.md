# Skeena Watershed Fish Passage Restoration Planning 2024

> Reproducible, web-first restoration-planning report for fish-passage barriers across the Skeena watershed, prepared for the Society for Ecosystem Restoration in Northern BC (SERN).

**Read the report:** <https://newgraphenvironment.github.io/fish_passage_skeena_2024_reporting>
&middot; **Source:** [`NewGraphEnvironment/fish_passage_skeena_2024_reporting`](https://github.com/NewGraphEnvironment/fish_passage_skeena_2024_reporting)
&middot; **Version history:** [`NEWS.md`](NEWS.md)

## What this is

A bookdown report assembling crossing-by-crossing assessments, fish-presence and habitat-confirmation data, and prioritization analyses across the Skeena watershed. Each prioritized barrier crossing has a dedicated appendix with photos, field measurements, and recommended remediation; the executive summary rolls them up for partners and decision-makers. Source data and methods are open — anyone can rebuild the report from `scripts/run.R`.

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

External: [`bcfishpass`](https://github.com/smnorris/bcfishpass), [`fwapg`](https://github.com/smnorris/fwapg).

## License

MIT (see [`LICENSE`](LICENSE)).
