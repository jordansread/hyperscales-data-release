# hyperscales-data-release
Process-guided deep learning predictions of lake water temperature

## Organization

Though we don't strictly adhere to these divisions, here's the general idea for the output folders:

* `out` contains git-committable yml, csv, or ind files that describe the existence of data files
* `out_data` contains data files that are directly posted to SB as-is
* `out_xml` contains the rendered XML files that are used to populate SB metadata

## Building 00 through 05

Jordan completes these steps.

## Building `06_habitat`

All habitat files are built using the [`lake-temperature-out` pipeline](https://github.com/USGS-R/lake-temperature-out) on Yeti. The targets in `6_habitat.yml` copy over the corresponding data files from `lake-temperature-out` and the `remake.yml` target for `6_habitat` pushes the files to ScienceBase. In order to build `6_habitat` targets, the files used from `lake-temperature-out` need to have been built on Yeti already. Then, this part of the data release pipeline can also be built on Yeti (find it in `iidd/data-sci/lake-temp/`). Once on Yeti in this data release directory, use the following steps to build the `6_habitat` targets and post to SB:

```
module load legacy R/3.6.3 tools/nco-4.7.8-gnu tools/netcdf-c-4.3.2-intel gdal/3.1.0 proj/7.0.1
R
library(scipiper)
sbtools::authenticate_sb('cidamanager')
scmake('log/06_habitat_sb.csv')
```
