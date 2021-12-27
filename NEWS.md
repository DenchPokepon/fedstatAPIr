# fedstatAPIr 0.2.0

* `fedstat_data_ids_filter` by default `filters` arg is set to `list()` (e.g. select all possible filters)
* `fedstat_data_load_with_filters` the interface has been changed a little bit, `...` (dots) 
were added as a *second* argument to pass additional arguments to `httr::GET` and `httr::POST`. 
Also `timeout_seconds` and `retry_max_times` arguments were added to `fedstat_data_load_with_filters`
which are passed to `fedstat_get_data_ids` and `fedstat_post_data_ids_filtered`
* `fedstat_get_data_ids` the interface has been changed a little bit, `...` (dots) were added as a second argument to pass additional arguments to `httr::GET`
* `fedstat_post_data_ids_filtered` the interface has been changed a little bit, `...` (dots) were added as a *second* argument to pass additional arguments to `httr::POST`
* An error handler has been added for a new change to the public interface (about dots) for faster user adaptation
* Title has been changed slightly (added Rosstat keyword)
* New *experimental* functions (public: `fedstat_check_data_update`, `fedstat_prepare_reference_data_for_check_data_update`, internal: `fedstat_check_data_update_`)
for checking indicator data for updates on fedstat.ru
* Many changes to the code, bug fixes, refactoring, optimization. Removed dependency on the `rsdmx` package because of its memory leaks
* The dataset of all available indicators on fedstat.ru was added to the package (`fedstat_indicators_names_database`)
