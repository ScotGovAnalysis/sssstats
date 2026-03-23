# sssstats 0.2.2

- adds the 2026/2027 holidays
- moves create_sss_calendar to its own file

# sssstats 0.2.1

-   The existing `add_geography()` function is now split into two new functions:

    -   `format_postcode()` for formatting the postcode field within any data extract to meet the standard UK postcode format whenever possible before linking the postcode field with the Scottish Statistics Postcode lookup (which is the job for `add_geo_columns()` - see below)
    -   `add_geo_columns()` for adding geography fields into data extracts from the Scottish Statistics Postcode lookup, the data zone lookup and (optionally) the Scottish Index of Multiple Deprivation lookup (2011 data zone based).

    See function documentation for further information, especially the `format_postcode()` function to know what it can and cannot be used for.

-   The `add_geography()` function is still preserved - since sssstats 0.2.1, it calls both `format_postcode()` and `add_geo_columns()` functions within.

-   The `simd_lookup` argument for the `add_geography()` function is set as `NULL` as default since the Scottish Index of Multiple Deprivation lookup is not always required.

# sssstats 0.2.0

-   The following new functions have been introduced:

    -   `get_datazone_lookup()` for creating the data zone lookup from statistics.gov.scot
    -   `get_simd_lookup()` for creating the Scottish Index of Multiple Deprivation lookup (2011 data zone based) from statistics.gov.scot
    -   `get_sspl_lookup()` for reading in (a local CSV file of) the Scottish Statistics Postcode Lookup published by National Records of Scotland
    -   `add_geography()` for adding geography fields into data extracts used for all Social Security Scotland's official statistics publications.

    See function documentation for further information.

-   Removes a dependency on the `config` R package
