# fedstatAPIr 1.0.0

* removed all dependencies except httr, jsonlite, xml2, readsdmx, magrittr, utils, methods, data.table for much easier installation
* backend of all functions rewritten in data.table
* new features: it's now possible to load database full dictionary from sdmx by providing new argument `return_type` to `fedstat_parse_sdmx_to_table`, obervation value in data is now parsed from character to numeric type automatically
* fixed some old problems with russian encoding data fields and filters by converting to UTF-8
* removed all functions related to automatic data update checks (`fedstat_check_data_update`, `fedstat_prepare_reference_data_for_check_data_update`), were experemental, too complex and not a main part of a package
* removed function `fedstat_get_data_ids_special_cases_handle`, no longer needed due to change of upstream API
* new function `fedstat_indicator_info` - allows to update database of all indicators by user
* user interface to the package (except deleted functions) has not changed, some non-significant default arguments have been changed or added