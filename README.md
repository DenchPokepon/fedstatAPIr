
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fedstatAPIr

<!-- badges: start -->
<!-- badges: end -->

fedstatAPIr представляет собой неофициальное API для загрузки данных с
fedstat.ru (система ЕМИСС Росстата) с заданными фильтрами.

Пакет позволяет сэкономить ⏳ и нервы 😡 при работе с бесконечно
падающим ЕМИСС через автоматическое повторение неотвеченных запросов и
сохранение ранее использованных настроек фильтров.

Несколько дней назад (02.04.2025) ЕМИСС начал банить запросы с автоматическими заголовками запросов httr (RCurl), выдает ошибку 403. Для лечения проблемы можно передавать свои заголовки `httr::add_headers(.headers = c('user-agent' = 'Ваш user-agent'))` в любую функцию отправки запроса из fedstatAPIr (передается в аргумент dots - `...` в функциях `fedstat_data_load_with_filters`, `fedstat_get_data_ids`, `fedstat_post_data_ids_filtered`). Действия ЕМИСС по бану таких запросов, вероятно, связаны с высокой нагрузкой на их сервер. Пользователям пакета крайне рекомендуется разбивать большие запросы на несколько маленьких и распределять их во времени. В ином случае в будущем ЕМИСС может ограничить возможности простого лечения таких банов и помешать рабоспособности пакета.

## Поделитесь чеканной монетой :)
Проект уже существует длительное время и, по-видимому, получился довольно стабильным ⚖️ и легковесным 🪽.
Если пакет оказал влияние на Вашу деятельность, просим оказать [поддержку](https://pay.cloudtips.ru/p/db468def).
Если будет набрана необходимая сумма, также сделаем порт на Python и API для витрины данных Росстата (showdata).
Спасибо за использование пакета!

## Установка

На данный момент пакет доступен только на R, Вы можете загрузить
fedstatAPIr из [CRAN](https://CRAN.R-project.org), используя следующую
команду в R:

``` r
install.packages("fedstatAPIr")
```

Версия для разработки может быть загружена следующей командой:

``` r
# install.packages("devtools")
devtools::install_github("DenchPokepon/fedstatAPIr")
```

## Использование

Загрузим данные по инфляции к предыдущему месяцу и к соотв. месяцу
прошлого года за январь 2023 года по всем товарам и услугам только по
России

Возьмем номер индикатора из URL веб страницы с данными
<https://fedstat.ru/indicator/31074>

``` r
library(fedstatAPIr)
data <- fedstat_data_load_with_filters(
  indicator_id = "31074",
  filters = list(
    "Классификатор объектов административно-территориального деления (ОКАТО)" = "Российская Федерация",
    "Виды показателя" = c(
      "К соответствующему периоду предыдущего года",
      "К предыдущему месяцу"
    ),
    "Период" = "Январь",
    "Год" = "2023",
    "Виды товаров и услуг" = "*"
  )
)
```

| EI      | ObsValue | PERIOD | Time | s_POK                                       | s_grtov                            | s_OKATO              | s_OKATO_code | s_POK_code | s_grtov_code |
|:--------|---------:|:-------|:-----|:--------------------------------------------|:-----------------------------------|:---------------------|:-------------|:-----------|:-------------|
| процент |   100.84 | январь | 2023 | К предыдущему месяцу                        | Все товары и услуги                | Российская Федерация | 643          | 44         | 1            |
| процент |   111.77 | январь | 2023 | К соответствующему периоду предыдущего года | Все товары и услуги                | Российская Федерация | 643          | 9          | 1            |
| процент |   100.78 | январь | 2023 | К предыдущему месяцу                        | Все товары                         | Российская Федерация | 643          | 44         | 2            |
| процент |   111.15 | январь | 2023 | К соответствующему периоду предыдущего года | Все товары                         | Российская Федерация | 643          | 9          | 2            |
| процент |   100.30 | январь | 2023 | К предыдущему месяцу                        | Базовый индекс потребительских цен | Российская Федерация | 643          | 44         | 3            |
| процент |   113.72 | январь | 2023 | К соответствующему периоду предыдущего года | Базовый индекс потребительских цен | Российская Федерация | 643          | 9          | 3            |

Необходимо учитывать, что ЕМИСС часто лагает, поэтому не стоит загружать
больше 500 тысяч строк за один запрос. В случае большой выгрузки
желательно разбить запросы на подзапросы, в ином случае ЕМИСС может
игнорировать такие большие запросы

## Спецификация фильтров

Фильтры задаются в форме JSON:

``` json_example
{
 "Название поля фильтра 1": ["Значение фильтра 1", "Значение фильтра 2"],
 "Название поля фильтра 2": ["Значение фильтра 1", "Значение фильтра 2"],
 ...
}
```

Где `Название поля фильтра 1` может быть, например `Год`, а
`Значение фильтра 1` для этого поля `2023`.

Поля и значения фильтров берутся прямо из названий фильтров на
fedstat.ru, в `filters` они должны быть такими же. Однако разные
регистры фильтров и лишние пробелы не сломают фильтрацию.

Доступны следующие специальные значения фильтров:

1.  Звездочка/asterix (\*) выбирает все данные по этому полю фильтра.

Можно не указывать какой-то фильтр вообще, тогда по умолчанию будет
использоваться \*.

## Доступные индикаторы

Библиотека стремится поддерживать загрузку всех доступных на fedstat
индикаторов, на данный момент поддерживается абсолютное большинство, но
в некоторых случаях специальных индикаторов могут быть непредвиденные
баги.

Внутри пакета доступна база данных всех индикаторов, по которым
представлены данные на fedstat.ru. Она содержится в переменной
`fedstat_indicators_names_database`.

Все доступные фильтры каждого индикатора можно посмотреть через вызов
функции

``` r
data_ids <- fedstat_get_data_ids("31074")
```

Результат выглядит следующим образом

| filter_field_id | filter_field_title | filter_value_id | filter_value_title                             | filter_field_object_ids |
|:----------------|:-------------------|:----------------|:-----------------------------------------------|:------------------------|
| 0               | Показатель         | 31074           | Индексы потребительских цен на товары и услуги | filterObjectIds         |
| 3               | Год                | 2002            | 2002                                           | columnObjectIds         |
| 3               | Год                | 2003            | 2003                                           | columnObjectIds         |
| 3               | Год                | 2004            | 2004                                           | columnObjectIds         |
| 3               | Год                | 2005            | 2005                                           | columnObjectIds         |
| 3               | Год                | 2006            | 2006                                           | columnObjectIds         |

Также можно посмотреть полный внутренний словарь (соотношения кодов и
человеческих названий сущностей) через задание аргумента
`return_type = dictionary`

``` r
library(fedstatAPIr)
data <- fedstat_data_load_with_filters(
  indicator_id = "31074",
  filters = list(
    "Классификатор объектов административно-территориального деления (ОКАТО)" = "Российская Федерация",
    "Виды показателя" = c(
      "К соответствующему периоду предыдущего года",
      "К предыдущему месяцу"
    ),
    "Период" = "Январь",
    "Год" = "2023",
    "Виды товаров и услуг" = "*"
  ),
  return_type = "dictionary"
)
```

## Продвинутое использование

Функция `fedstat_data_load_with_filters` является оберткой из отдельных
функций, которые отвечают за:

1.  Загрузку id фильтров через GET запрос (`fedstat_get_data_ids`);
2.  Фильтрацию данных на основе `filters` с учетом специальных значений
    и стандартных фильтров (`fedstat_data_ids_filter`);
3.  Отправку POST запроса с телом из фильтров
    (`fedstat_post_data_ids_filtered`);
4.  Парсинг полученного SDMX файла в таблицу
    (`fedstat_parse_sdmx_to_table`).

По каждой функции доступна подробная документация, которую можно вызвать
в R через функцию `help()`. Например
`help("fedstat_data_load_with_filters")` или
`?fedstat_data_load_with_filters`.

Загрузим те же данные, используя каждую функцию отдельно

``` r
data_ids <- fedstat_get_data_ids("31074") # Можеть занимать много времени из-за лагов
# fedstat, для скорости лучше всего кэшировать или даже записывать в базу данных,
# более подробно по этому моменту в документации

data_ids_filtered <- data_ids %>%
  fedstat_data_ids_filter(
    filters = list(
      "Классификатор объектов административно-территориального деления (ОКАТО)" = "Российская Федерация",
      "Виды показателя" = c(
        "К соответствующему периоду предыдущего года",
        "К предыдущему месяцу"
      ),
      "Период" = "Январь",
      "Год" = "2023",
      "Виды товаров и услуг" = "*"
    )
  )

data_sdmx_parsed <- data_ids_filtered %>%
  fedstat_post_data_ids_filtered(data_format = "sdmx") %>%
  fedstat_parse_sdmx_to_table()

data_xls_binary <- data_ids_filtered %>%
  fedstat_post_data_ids_filtered(data_format = "excel") # также можем загружать excel

writeBin(data_xls_binary, "data.xls") # также можем записать в xls файл.
# Его формат зависит от колонки filter_field_object_ids в data_ids, можно изменять самостоятельно
```

## Помощь

Вопросы можно писать на почту <deniskrylovvit@gmail.com>

------------------------------------------------------------------------

## Разработка

Буду рад Вашему вкладу в проект. Перед коммитом изменений необходимо
пройти R CRAN check –as-cran.
