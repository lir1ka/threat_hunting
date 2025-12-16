# pr 06 vakhrameev
vakhrameevaleksandr@yandex.ru

## Цель работы

1.  Закрепить навыки исследования данных журнала Windows Active
    Directory
2.  Изучить структуру журнала системы Windows Active Directory
3.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
4.  Закрепить знания основных функций обработки данных экосистемы
    `tidyverse` языка R

## Исходные данные

1.  Оепрационная система Darwin Mac.lan 25.0.0 Darwin Kernel Version
    25.0.0
2.  IDE Positron
3.  R version 4.5.2
4.  Логи Windows Active Directory из SIEM.

## Задание

Используя программный пакет `dplyr` языка программирования R провести
анализ журналов и ответить на вопросы.

## Ход работы

### Подготовка данных

1.  Импортируйте данные в R. Это можно выполнить с помощью
    jsonlite::stream_in(file()) . Датасет находится по адресу
    https://storage.yandexcloud.net/iamcth-data/dataset.tar.gz.
2.  Привести датасеты в вид “аккуратных данных”, преобразовать типы
    столбцов в соответствии с типом данных
3.  Просмотрите общую структуру данных с помощью функции `glimpse()`

### Анализ данных

1.  Раскройте датафрейм избавившись от вложенных датафреймов. Для
    обнаружения таких можно использовать функцию dplyr::glimpse() , а
    для раскрытия вложенности – tidyr::unnest(). Обратите внимание, что
    при раскрытии теряются внешние названия колонок – это можно
    предотвратить если использовать параметр tidyr::unnest(…, names_sep
    = ) .
2.  Минимизируйте количество колонок в датафрейме – уберите колоки с
    единственным значением параметра.
3.  Какое количество хостов представлено в данном датасете?
4.  Подготовьте датафрейм с расшифровкой Windows Event_ID, приведите
    типы данных к типу их значений.
5.  Есть ли в логе события с высоким и средним уровнем значимости?
    Сколько их?

### Шаг 1. Подготовка данных

``` r
dataset_url <- "https://storage.yandexcloud.net/iamcth-data/dataset.tar.gz"
temp_dir <- tempdir()
tar_path <- file.path(temp_dir, "dataset.tar.gz")

download.file(
  url = dataset_url,
  destfile = tar_path,
  mode = "wb"
)

untar(tarfile = tar_path, exdir = temp_dir)

json_path <- file.path(
  temp_dir,
  "caldera_attack_evals_round1_day1_2019-10-20201108.json"
)

ad_data_raw <- jsonlite::stream_in(
  file(json_path, open = "r"),
  verbose = FALSE
)

ad_data_raw <- ad_data_raw %>%
  mutate(across(where(is.character), ~ if_else(.x == "", NA_character_, .x))) %>%
  mutate(across(where(is.logical), as.integer)) %>%
  type_convert()

glimpse(ad_data_raw)
```

    Rows: 101,904
    Columns: 9
    $ `@timestamp` <dttm> 2019-10-20 20:11:06, 2019-10-20 20:11:07, 2019-10-20 20:…
    $ `@metadata`  <df[,4]> <data.frame[26 x 4]>
    $ event        <df[,4]> <data.frame[26 x 4]>
    $ log          <df[,1]> <data.frame[26 x 1]>
    $ message      <chr> "A token right was adjusted.\n\nSubject:\n\tSecurity I…
    $ winlog       <df[,16]> <data.frame[26 x 16]>
    $ ecs          <df[,1]> <data.frame[26 x 1]>
    $ host         <df[,1]> <data.frame[26 x 1]>
    $ agent        <df[,5]> <data.frame[26 x 5]>

### Шаг 2. Анализ

#### Раскройте датафрейм избавившись от вложенных датафреймов. Для обнаружения таких можно использовать функцию dplyr::glimpse() , а для раскрытия вложенности – tidyr::unnest(). Обратите внимание, что при раскрытии теряются внешние названия колонок – это можно предотвратить если использовать параметр tidyr::unnest(…, names_sep = ).

Последовательно преобразуем вложенные структуры данных в плоский формат
для упрощения последующего анализа. Итеративно применяем функцию
разворачивания к каждому столбцу, содержащему вложенные объекты,
сохраняя исходные имена через разделитель.

``` r
is_nested_structure <- function(x) {
  is.data.frame(x) || (is.list(x) && !is.null(x))
}

unnest_nested_columns <- function(df) {
  while (TRUE) {
    nested_cols <- names(df)[vapply(df, is_nested_structure, logical(1))]
    if (length(nested_cols) == 0) break
    
    for (col in nested_cols) {
      df <- tidyr::unnest(
        df,
        cols = dplyr::all_of(col),
        names_sep = paste0(col, "_"),
        keep_empty = TRUE
      )
    }
  }
  return(df)
}

ad_data <- unnest_nested_columns(ad_data_raw)

ad_data %>%
  select(
    `@timestamp`,
    winlogwinlog_computer_name,
    eventevent_code,
    eventevent_action,
    starts_with("winlogwinlog_event_data_")
  ) %>%
  glimpse()
```

    Rows: 101,904
    Columns: 4
    $ `@timestamp`               <dttm> 2019-10-20 20:11:06, 2019-10-20 20:11:07, …
    $ winlogwinlog_computer_name <chr> "HR001.shire.com", "HFDC01.shire.com", "IT0…
    $ eventevent_code            <int> 4703, 4673, 10, 10, 10, 10, 11, 10, 10, 10,…
    $ eventevent_action          <chr> "Token Right Adjusted Events", "Sensitive P…

``` r
ncol(ad_data)
```

    [1] 300

#### Минимизируйте количество колонок в датафрейме – уберите колоки с единственным значением параметра.

Исключаем из датафрейма столбцы, содержащие только одно уникальное
значение, так как они не несут информационной нагрузки для анализа.

``` r
ad_data <- ad_data %>%
  select(where(~ n_distinct(., na.rm = TRUE) > 1))

ncol(ad_data)
```

    [1] 181

#### Какое количество хостов представлено в данном датасете?

Определяем общее количество уникальных хостов в наборе данных путем
подсчета различных значений в столбце с именем компьютера.

``` r
host_count <- ad_data %>%
  select(winlogwinlog_computer_name) %>%
  distinct() %>%
  count()

host_count
```

    # A tibble: 1 × 1
          n
      <int>
    1     5

#### Подготовьте датафрейм с расшифровкой Windows Event_ID, приведите типы данных к типу их значений.

Формируем справочную таблицу с идентификаторами событий Windows и их
описаниями, автоматически определяя корректные типы данных для каждого
столбца.

``` r
win_events <- ad_data %>%
  select(eventevent_code, eventevent_action) %>%
  distinct() %>%
  rename(
    event_id = eventevent_code,
    task = eventevent_action
  ) %>%
  type_convert()

win_events
```

    # A tibble: 119 × 2
       event_id task                                       
          <int> <chr>                                      
     1     4703 Token Right Adjusted Events                
     2     4673 Sensitive Privilege Use                    
     3       10 Process accessed (rule: ProcessAccess)     
     4       11 File created (rule: FileCreate)            
     5        7 Image loaded (rule: ImageLoad)             
     6     4689 Process Termination                        
     7        5 Process terminated (rule: ProcessTerminate)
     8     5158 Filtering Platform Connection              
     9     5156 Filtering Platform Connection              
    10     4672 Special Logon                              
    # ℹ 109 more rows

#### Есть ли в логе события с высоким и средним уровнем значимости? Сколько их?

Создаем справочную таблицу соответствия идентификаторов событий Windows
их уровням критичности, включающую события с высоким и средним
приоритетом.

``` r
event_severity <- tribble(
  ~event_id, ~severity,
  4618L, "High", 4649L, "High", 4719L, "High", 4765L, "High",
  4766L, "High", 4794L, "High", 4897L, "High", 4964L, "High",
  5124L, "High", 1102L, "Medium to High", 4621L, "Medium",
  4675L, "Medium", 4692L, "Medium", 4693L, "Medium", 4706L, "Medium",
  4713L, "Medium", 4714L, "Medium", 4715L, "Medium", 4716L, "Medium",
  4724L, "Medium", 4727L, "Medium", 4735L, "Medium", 4737L, "Medium",
  4739L, "Medium", 4754L, "Medium", 4755L, "Medium", 4764L, "Medium",
  4780L, "Medium", 4816L, "Medium", 4865L, "Medium", 4866L, "Medium",
  4867L, "Medium", 4868L, "Medium", 4870L, "Medium", 4882L, "Medium",
  4885L, "Medium", 4890L, "Medium", 4892L, "Medium", 4896L, "Medium",
  4906L, "Medium", 4907L, "Medium", 4908L, "Medium", 4912L, "Medium",
  4960L, "Medium", 4961L, "Medium", 4962L, "Medium", 4963L, "Medium",
  4965L, "Medium", 4976L, "Medium", 4977L, "Medium", 4978L, "Medium",
  4983L, "Medium", 4984L, "Medium", 5027L, "Medium", 5028L, "Medium",
  5029L, "Medium", 5030L, "Medium", 5035L, "Medium", 5037L, "Medium",
  5038L, "Medium", 5120L, "Medium", 5121L, "Medium", 5122L, "Medium",
  5123L, "Medium", 5376L, "Medium", 5377L, "Medium", 5453L, "Medium",
  5480L, "Medium", 5483L, "Medium", 5484L, "Medium", 5485L, "Medium",
  5827L, "Medium", 5828L, "Medium", 6145L, "Medium", 6273L, "Medium",
  6274L, "Medium", 6275L, "Medium", 6276L, "Medium", 6277L, "Medium",
  6278L, "Medium", 6279L, "Medium", 6280L, "Medium", 24586L, "Medium",
  24592L, "Medium", 24593L, "Medium", 24594L, "Medium"
) %>%
  arrange(event_id)

event_severity
```

    # A tibble: 86 × 2
       event_id severity      
          <int> <chr>         
     1     1102 Medium to High
     2     4618 High          
     3     4621 Medium        
     4     4649 High          
     5     4675 Medium        
     6     4692 Medium        
     7     4693 Medium        
     8     4706 Medium        
     9     4713 Medium        
    10     4714 Medium        
    # ℹ 76 more rows

Выполняем левое соединение таблицы событий из логов со справочником
критичности. Для событий, отсутствующих в справочнике, устанавливаем
уровень значимости по умолчанию как “Low”.

``` r
win_events_with_severity <- win_events %>%
  left_join(event_severity, by = "event_id") %>%
  mutate(severity = coalesce(severity, "Low"))

win_events_with_severity
```

    # A tibble: 119 × 3
       event_id task                                        severity
          <int> <chr>                                       <chr>   
     1     4703 Token Right Adjusted Events                 Low     
     2     4673 Sensitive Privilege Use                     Low     
     3       10 Process accessed (rule: ProcessAccess)      Low     
     4       11 File created (rule: FileCreate)             Low     
     5        7 Image loaded (rule: ImageLoad)              Low     
     6     4689 Process Termination                         Low     
     7        5 Process terminated (rule: ProcessTerminate) Low     
     8     5158 Filtering Platform Connection               Low     
     9     5156 Filtering Platform Connection               Low     
    10     4672 Special Logon                               Low     
    # ℹ 109 more rows

Извлекаем подмножество событий, имеющих уровень критичности отличный от
низкого, для дальнейшего анализа потенциально значимых инцидентов.

``` r
not_low_severity <- win_events_with_severity %>%
  filter(severity != "Low")

not_low_severity %>%
  count()
```

    # A tibble: 1 × 1
          n
      <int>
    1     0

В результате анализа установлено, что в исследуемом наборе логов
отсутствуют события с уровнем критичности Medium или High.

## Вывод

В ходе практической работы я улучшил навыки работы с языком
программирования R и освоил методы исследования признаков вредоносной
активности в домене Windows.
