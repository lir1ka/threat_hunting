# pr 04 vakhrameev
vakhrameevaleksandr@yandex.ru

## Цель работы

1.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания основных функций обработки данных экосистемы
    `tidyverse` языка R
3.  Закрепить навыки исследования метаданных DNS трафика

## Исходные данные

1.  Оепрационная система Darwin Mac.lan 25.0.0 Darwin Kernel Version
    25.0.0
2.  IDE Positron
3.  R version 4.5.2
4.  Информация о dns траффике во внутренний сети Доброй Организаци

## Задание

Используя программный пакет `dplyr`, освоить анализ DNS логов с помощью
языка программирования R.

## Ход работы

### Подготовка данных

1.  Импортируйте данные DNS –
    https://storage.yandexcloud.net/dataset.ctfsec/dns.zip
2.  Добавьте пропущенные данные о структуре данных (назначении столбцов)
3.  Преобразуйте данные в столбцах в нужный формат
4.  Просмотрите общую структуру данных с помощью функции glimpse()
5.  Оформить отчет в соответствии с шаблоном

### Анализ данных

1.  Сколько участников информационного обмена в сети Доброй Организации?
2.  Какое соотношение участников обмена внутри сети и участников
    обращений к внешним ресурсам?
3.  Найдите топ-10 участников сети, проявляющих наибольшую сетевую
    активность.
4.  Найдите топ-10 доменов, к которым обращаются пользователи сети и
    соответственное количество обращений.
5.  Опеределите базовые статистические характеристики (функция summary()
    ) интервала времени между последовательными обращениями к топ-10
    доменам.
6.  Часто вредоносное программное обеспечение использует DNS канал в
    качестве канала управления, периодически отправляя запросы на
    подконтрольный злоумышленникам DNS сервер. По периодическим запросам
    на один и тот же домен можно выявить скрытый DNS канал. Есть ли
    такие IP адреса в исследуемом датасете?

### Обогащение данных

1.  Определите местоположение (страну, город) и организацию-провайдера
    для топ-10 доменов. Для этого можно использовать сторонние сервисы,
    например http://ip-api.com (API-эндпоинт – http://ip-api.com/json).

### Шаг 1

Подключаем необходимый пакет для работы с данными:

``` r
require(dplyr)
```

    Loading required package: dplyr


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

Загружаем DNS-логи из файла в таблицу данных:

``` r
dns_data <- read.table(
  file = "dns.log",
  sep = "\t",
  header = FALSE,
  fill = TRUE,
  quote = "",
  stringsAsFactors = FALSE
)
```

Присваиваем имена столбцам согласно структуре DNS-логов:

``` r
column_names <- c(
  "ts","uid","id.orig_h","id.orig_p","id.resp_h","id.resp_p",
  "proto","trans_id","query","qclass","qclass_name","qtype",
  "qtype_name","rcode","rcode_name","AA","TC","RD","RA","Z",
  "answers","TTLs","rejected"
)
names(dns_data) <- column_names[1:ncol(dns_data)]
```

Выполняем преобразование типов данных для корректной работы с временными
метками и портами:

``` r
dns_data <- dns_data %>%
  mutate(
    ts = as.POSIXct(as.numeric(ts), origin = "1970-01-01", tz = "UTC"),
    id.orig_p = as.integer(id.orig_p),
    id.resp_p = as.integer(id.resp_p)
  )
```

Просматриваем структуру загруженных данных:

``` r
glimpse(dns_data)
```

    Rows: 427,935
    Columns: 23
    $ ts          <dttm> 2012-03-16 12:30:05, 2012-03-16 12:30:15, 2012-03-16 12:3…
    $ uid         <chr> "CWGtK431H9XuaTN4fi", "C36a282Jljz7BsbGH", "C36a282Jljz7Bs…
    $ id.orig_h   <chr> "192.168.202.100", "192.168.202.76", "192.168.202.76", "19…
    $ id.orig_p   <int> 45658, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 1…
    $ id.resp_h   <chr> "192.168.27.203", "192.168.202.255", "192.168.202.255", "1…
    $ id.resp_p   <int> 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137…
    $ proto       <chr> "udp", "udp", "udp", "udp", "udp", "udp", "udp", "udp", "u…
    $ trans_id    <int> 33008, 57402, 57402, 57402, 57398, 57398, 57398, 62187, 62…
    $ query       <chr> "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\…
    $ qclass      <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"…
    $ qclass_name <chr> "C_INTERNET", "C_INTERNET", "C_INTERNET", "C_INTERNET", "C…
    $ qtype       <chr> "33", "32", "32", "32", "32", "32", "32", "32", "32", "32"…
    $ qtype_name  <chr> "SRV", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB…
    $ rcode       <chr> "0", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"…
    $ rcode_name  <chr> "NOERROR", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-…
    $ AA          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ TC          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ RD          <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRU…
    $ RA          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ Z           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0…
    $ answers     <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"…
    $ TTLs        <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"…
    $ rejected    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…

### Шаг 2

#### Сколько участников информационного обмена в сети Доброй Организации?

Найдем все уникальные IP-адреса в данных DNS (и источников, и
получателей) и считам их количество:

``` r
all_ips <- dns_data %>%
  select(id.orig_h, id.resp_h) %>%
  unlist() %>%
  unique()

total_participants <- length(all_ips)
total_participants
```

    [1] 1359

#### 5. Какое соотношение участников обмена внутри сети и участников обращений к внешним ресурсам?

Ip-адреса можно разделить на серые и белые (публичные), cерые адреса:

-   192.168.0.0/16
-   10.0.0.0/8,
-   172.16.0.0/12

Подключаем пакет для работы с IP-адресами:

``` r
library(ipaddress)
```

Определяем уникальные IP-адреса получателей, классифицируем их на
внутренние и внешние, затем вычисляем долю внутренних адресов:

``` r
unique_resp_ips <- dns_data %>%
  distinct(id.resp_h) %>%
  pull(id.resp_h)

ip_objects <- ip_address(unique_resp_ips)
internal_count <- sum(is_private(ip_objects), na.rm = TRUE)
external_count <- sum(!is_private(ip_objects), na.rm = TRUE)
internal_ratio <- internal_count / (internal_count + external_count)
internal_ratio
```

    [1] 0.9658537

#### Найдите топ-10 участников сети, проявляющих наибольшую сетевую активность.

Объединяем IP-адреса источников и получателей, подсчитываем количество
запросов для каждого участника и определяем топ-10 наиболее активных:

``` r
top10_act <- dns_data %>%
  select(ip = id.orig_h) %>%
  bind_rows(dns_data %>% select(ip = id.resp_h)) %>%
  filter(!is.na(ip), ip != "-") %>%
  count(ip, name = "act") %>%
  arrange(desc(act)) %>%
  head(10)

top10_act
```

                    ip    act
    1    192.168.207.4 266627
    2    10.10.117.210  75943
    3  192.168.202.255  68720
    4   192.168.202.93  26522
    5     172.19.1.100  25481
    6  192.168.202.103  18121
    7   192.168.202.76  16978
    8   192.168.202.97  16176
    9  192.168.202.141  14976
    10 192.168.202.110  14493

#### Найдите топ-10 доменов, к которым обращаются пользователи сети и соответственное количество обращений

Подсчитываем количество обращений к каждому домену, сортируем по
убыванию и выбираем топ-10:

``` r
top_dmn <- dns_data %>%
  count(query, sort = TRUE, name = "hits") %>%
  head(10)

top_dmn
```

                                                                         query
    1                                                teredo.ipv6.microsoft.com
    2                                                         tools.google.com
    3                                                            www.apple.com
    4                                                           time.apple.com
    5                                          safebrowsing.clients.google.com
    6  *\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00
    7                                                                     WPAD
    8                                              44.206.168.192.in-addr.arpa
    9                                                                 HPE8AA67
    10                                                                  ISATAP
        hits
    1  39273
    2  14057
    3  13390
    4  13109
    5  11658
    6  10401
    7   9134
    8   7248
    9   6929
    10  6569

#### Опеределите базовые статистические характеристики (функция summary() ) интервала времени между последовательными обращениями к топ-10 доменам.

Фильтруем данные по топ-10 доменам, сортируем по домену и времени,
вычисляем интервалы между последовательными запросами и получаем
статистику:

``` r
time_int <- dns_data %>%
  filter(query %in% top_dmn$query) %>%
  arrange(query, ts) %>%
  group_by(query) %>%
  mutate(time_diff = ts - lag(ts)) %>%
  ungroup()

summary(time_int$time_diff)
```

    Time differences in secs
         Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
        0.000     0.000     0.750     8.758     1.740 52723.500        10 

#### Поиск возможного скрытого DNS-канала

Для каждого домена вычисляем интервалы между последовательными
запросами, рассчитываем среднее значение и стандартное отклонение, затем
отбираем домены с большим количеством запросов и низким отклонением
интервалов:

``` r
periodic_sus <- dns_data %>%
  filter(!is.na(query), query != "-") %>%
  arrange(query, ts) %>%
  group_by(query) %>%
  mutate(
    interval = as.numeric(ts - lag(ts), units = "secs")
  ) %>%
  summarise(
    mean_int = mean(interval, na.rm = TRUE),
    sd_int = sd(interval, na.rm = TRUE),
    n = sum(!is.na(interval))
  ) %>%
  filter(n > 30, sd_int < 1)

periodic_sus
```

    # A tibble: 6 × 4
      query             mean_int  sd_int     n
      <chr>                <dbl>   <dbl> <int>
    1 hq.h               0.00265 0.0515    377
    2 httphq.hec.net     0.00990 0.100     102
    3 input.mozilla.com  5.00    0.00475    31
    4 lifehacker.com     0.00612 0.0171     67
    5 www.h              0.0156  0.125      64
    6 www.hkparts.net    0.00371 0.0124     35

Наиболее подозрительными выглядят домены `hq.h` и `httphq.hec.net`: по
ним фиксируется большое число запросов с минимальными временными
промежутками. Домен `input.mozilla.com` обращается примерно каждые 5
секунд — вероятнее всего, это отправка телеметрии браузером.

### Шаг 3

#### Определить местоположение и организацию-провайдера для топ-10 доменов

Подключаем необходимые пакеты для работы с HTTP-запросами и
JSON-данными:

``` r
library(httr)
library(jsonlite)
```

Создаем функцию для получения информации о домене через API и применяем
её к топ-10 доменам:

``` r
get_domain_info <- function(domain) {
  api_url <- paste0("http://ip-api.com/json/", domain)
  api_response <- httr::GET(api_url)
  response_data <- jsonlite::fromJSON(rawToChar(api_response$content))
  
  tibble(
    domain = domain,
    country = response_data$country,
    city = response_data$city,
    isp = response_data$isp,
    org = response_data$org,
    as = response_data$as
  )
}

enriched_data <- purrr::map_df(top_dmn$query, get_domain_info)

enriched_data
```

    # A tibble: 10 × 6
       domain                                        country city  isp   org   as   
       <chr>                                         <chr>   <chr> <chr> <chr> <chr>
     1 "teredo.ipv6.microsoft.com"                   <NA>    <NA>  <NA>   <NA> <NA> 
     2 "tools.google.com"                            The Ne… Amst… Goog… "Goo… AS15…
     3 "www.apple.com"                               The Ne… Amst… Akam… ""    AS20…
     4 "time.apple.com"                              Nether… Amst… Appl… "App… AS61…
     5 "safebrowsing.clients.google.com"             United… Moun… Goog… "Goo… AS15…
     6 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\… <NA>    <NA>  <NA>   <NA> <NA> 
     7 "WPAD"                                        <NA>    <NA>  <NA>   <NA> <NA> 
     8 "44.206.168.192.in-addr.arpa"                 <NA>    <NA>  <NA>   <NA> <NA> 
     9 "HPE8AA67"                                    <NA>    <NA>  <NA>   <NA> <NA> 
    10 "ISATAP"                                      <NA>    <NA>  <NA>   <NA> <NA> 

### Шаг 4

Отчёт написан и оформлен

## Оценка результатов

Задача выполнена на языке R с использованием пакетов `dplyr`, `httr`,
`jsonlite`, `ipaddress`. В ходе работы я закрепил навыки применения этих
инструментов для анализа DNS-логов.

## Вывод

В рамках практической работы, используя пакет `dplyr`, я освоил базовые
приёмы анализа DNS-логов средствами языка программирования R.
