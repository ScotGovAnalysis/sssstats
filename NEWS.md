# sssstats 0.2.0

* The following new functions have been introduced:

  * `get_datazone_lookup()` for creating the data zone lookup from statistics.gov.scot
  * `get_simd_lookup()` for creating the Scottish Index of Multiple Deprivation lookup (2011 data zone based) from statistics.gov.scot
  * `get_sspl_lookup()` for reading in (a local CSV file of) the Scottish Statistics Postcode Lookup published by National Records of Scotland
  * `add_geography()` for adding geography fields into data extracts used for all Social Security Scotland's official statistics publications.
  
  See function documentation for further information.
  
* Removes a dependency on the `config` R package
