# fedstatAPIr (development version)

* `fedstat_data_ids_filter` by default `filters` arg is set to `list()` (e.g. select all possible filters)
* `fedstat_data_load_with_filters` the interface has been changed a little bit, `...` (dots) were added as a *second* argument to pass additional arguments to `httr::GET` and `httr::POST`
* `fedstat_get_data_ids` the interface has been changed a little bit, `...` (dots) were added as a second argument to pass additional arguments to `httr::GET`
* `fedstat_post_data_ids_filtered` the interface has been changed a little bit, `...` (dots) were added as a *second* argument to pass additional arguments to `httr::POST`
*  Slightly changed description
