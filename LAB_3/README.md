# pr 03 Vakhrameev
vakhrameevaleksandr@yandex.ru

## Цель работы

1.  Развить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания базовых типов данных языка R
3.  Развить практические навыки использования функций обработки данных
    пакета `dplyr` – функции
    `select(), filter(), mutate(), arrange(), group_by()`

## Исходные данные

1.  Оепрационная система Darwin Mac.lan 25.0.0 Darwin Kernel Version
    25.0.0
2.  IDE Positron
3.  R version 4.5.2

## Задание

Используя пакет `dplyr` и его функции ответить на вопросы по исходным
данным.

## Ход работы

1.  Для работы установить пакет `nycflights13`
2.  Проанализировать встроенные в пакет nycflights13 наборы данных с
    помощью языка R и ответить на вопросы:
    -   Сколько встроенных в пакет nycflights13 датафреймов?
    -   Сколько строк в каждом датафрейме?
    -   Сколько столбцов в каждом датафрейме?
    -   Как просмотреть примерный вид датафрейма?
    -   Сколько компаний-перевозчиков (carrier) учитывают эти наборы
        данных (представлено в наборах данных)?
    -   Сколько рейсов принял аэропорт John F Kennedy Intl в мае?
    -   Какой самый северный аэропорт?
    -   Какой аэропорт самый высокогорный (находится выше всех над
        уровнем моря)?
    -   Какие бортовые номера у самых старых самолетов?
    -   Какая средняя температура воздуха была в сентябре в аэропорту
        John FKennedy Intl (в градусах Цельсия).
    -   Самолеты какой авиакомпании совершили больше всего вылетов в
        июне?
    -   Самолеты какой авиакомпании задерживались чаще других в 2013
        году?
3.  Оформить отчет в соответствии с шаблоном

### Шаг №1

Устанавливаем нужный пакет `nycflights13` с помощью команды

    install.packages('nycflights13')

    > install.packages('nycflights13')
    --- Please select a CRAN mirror for use in this session ---
    Secure CRAN mirrors 

     1: 0-Cloud [https]                   2: Australia (Canberra) [https]   
     3: Australia (Melbourne 1) [https]   4: Australia (Melbourne 2) [https]
     5: Austria (Wien) [https]            6: Belgium (Brussels) [https]     
     7: Brazil (PR) [https]               8: Brazil (SP 1) [https]          
     9: Brazil (SP 2) [https]            10: Bulgaria [https]               
    11: Canada (MB) [https]              12: Canada (ON 1) [https]          
    13: Canada (ON 2) [https]            14: Chile (Santiago) [https]       
    15: China (Beijing 1) [https]        16: China (Beijing 2) [https]      
    17: China (Beijing 3) [https]        18: China (Hefei) [https]          
    19: China (Hong Kong) [https]        20: China (Lanzhou) [https]        
    21: China (Nanjing) [https]          22: China (Shanghai 2) [https]     
    23: China (Shenzhen) [https]         24: China (Wuhan) [https]          
    25: Colombia (Cali) [https]          26: Costa Rica [https]             
    27: Cyprus [https]                   28: Czech Republic [https]         
    29: Denmark [https]                  30: East Asia [https]              
    31: Ecuador (Cuenca) [https]         32: Finland (Helsinki) [https]     
    33: France (Lyon 1) [https]          34: France (Lyon 2) [https]        
    35: France (Paris 1) [https]         36: Germany (Erlangen) [https]     
    37: Germany (Göttingen) [https]      38: Germany (Leipzig) [https]      
    39: Germany (Münster) [https]        40: Greece [https]                 
    41: Hungary [https]                  42: Iceland [https]                
    43: India (Bengaluru) [https]        44: India (Bhubaneswar) [https]    
    45: Indonesia (Banda Aceh) [https]   46: Iran (Mashhad) [https]         
    47: Italy (Milano) [https]           48: Italy (Padua) [https]          
    49: Japan (Yonezawa) [https]         50: Korea (Gyeongsan-si) [https]   
    51: Mexico (Mexico City) [https]     52: Mexico (Texcoco) [https]       
    53: Morocco [https]                  54: Netherlands (Dronten) [https]  
    55: New Zealand [https]              56: Norway [https]                 
    57: Saudi Arabia (Riyadh) [https]    58: Spain (A Coruña) [https]       
    59: Spain (Madrid) [https]           60: Sweden (Umeå) [https]          
    61: Switzerland (Zurich 1) [https]   62: Taiwan (Taipei) [https]        
    63: UK (Bristol) [https]             64: UK (London 1) [https]          
    65: USA (IA) [https]                 66: USA (MI) [https]               
    67: USA (OH) [https]                 68: USA (OR) [https]               
    69: USA (PA 1) [https]               70: USA (TN) [https]               
    71: USA (UT) [https]                 72: United Arab Emirates [https]   
    73: Uruguay [https]                  74: (other mirrors)                


    Selection: 1
    trying URL 'https://cloud.r-project.org/bin/macosx/big-sur-arm64/contrib/4.5/nycflights13_1.0.2.tgz'
    Content type 'application/x-gzip' length 4504016 bytes (4.3 MB)
    ==================================================
    downloaded 4.3 MB


    The downloaded binary packages are in
        /var/folders/ny/lc521mg53ms5gf7p02xj_z040000gn/T//RtmpGdJqGs/downloaded_packages

### Шаг №2

Для начала подключим необходимые библиотеки:

``` r
library(nycflights13)
```

``` r
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

Теперь ответим на поставленные вопросы:

#### Сколько встроенных в пакет nycflights13 датафреймов?

``` r
ls("package:nycflights13")
```

    [1] "airlines" "airports" "flights"  "planes"   "weather" 

#### Сколько строк и столбцов в каждом датафрейме?

``` r
list(
  airlines = dim(airlines),
  airports = dim(airports),
  flights  = dim(flights),
  planes   = dim(planes),
  weather  = dim(weather)
)
```

    $airlines
    [1] 16  2

    $airports
    [1] 1458    8

    $flights
    [1] 336776     19

    $planes
    [1] 3322    9

    $weather
    [1] 26115    15

#### Как просмотреть примерный вид датафрейма?

``` r
glimpse(flights)
```

    Rows: 336,776
    Columns: 19
    $ year           <int> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2…
    $ month          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ day            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ dep_time       <int> 517, 533, 542, 544, 554, 554, 555, 557, 557, 558, 558, …
    $ sched_dep_time <int> 515, 529, 540, 545, 600, 558, 600, 600, 600, 600, 600, …
    $ dep_delay      <dbl> 2, 4, 2, -1, -6, -4, -5, -3, -3, -2, -2, -2, -2, -2, -1…
    $ arr_time       <int> 830, 850, 923, 1004, 812, 740, 913, 709, 838, 753, 849,…
    $ sched_arr_time <int> 819, 830, 850, 1022, 837, 728, 854, 723, 846, 745, 851,…
    $ arr_delay      <dbl> 11, 20, 33, -18, -25, 12, 19, -14, -8, 8, -2, -3, 7, -1…
    $ carrier        <chr> "UA", "UA", "AA", "B6", "DL", "UA", "B6", "EV", "B6", "…
    $ flight         <int> 1545, 1714, 1141, 725, 461, 1696, 507, 5708, 79, 301, 4…
    $ tailnum        <chr> "N14228", "N24211", "N619AA", "N804JB", "N668DN", "N394…
    $ origin         <chr> "EWR", "LGA", "JFK", "JFK", "LGA", "EWR", "EWR", "LGA",…
    $ dest           <chr> "IAH", "IAH", "MIA", "BQN", "ATL", "ORD", "FLL", "IAD",…
    $ air_time       <dbl> 227, 227, 160, 183, 116, 150, 158, 53, 140, 138, 149, 1…
    $ distance       <dbl> 1400, 1416, 1089, 1576, 762, 719, 1065, 229, 944, 733, …
    $ hour           <dbl> 5, 5, 5, 5, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 6, 6…
    $ minute         <dbl> 15, 29, 40, 45, 0, 58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 59, 0…
    $ time_hour      <dttm> 2013-01-01 05:00:00, 2013-01-01 05:00:00, 2013-01-01 0…

``` r
glimpse(weather)
```

    Rows: 26,115
    Columns: 15
    $ origin     <chr> "EWR", "EWR", "EWR", "EWR", "EWR", "EWR", "EWR", "EWR", "EW…
    $ year       <int> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013,…
    $ month      <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    $ day        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    $ hour       <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, …
    $ temp       <dbl> 39.02, 39.02, 39.02, 39.92, 39.02, 37.94, 39.02, 39.92, 39.…
    $ dewp       <dbl> 26.06, 26.96, 28.04, 28.04, 28.04, 28.04, 28.04, 28.04, 28.…
    $ humid      <dbl> 59.37, 61.63, 64.43, 62.21, 64.43, 67.21, 64.43, 62.21, 62.…
    $ wind_dir   <dbl> 270, 250, 240, 250, 260, 240, 240, 250, 260, 260, 260, 330,…
    $ wind_speed <dbl> 10.35702, 8.05546, 11.50780, 12.65858, 12.65858, 11.50780, …
    $ wind_gust  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 20.…
    $ precip     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ pressure   <dbl> 1012.0, 1012.3, 1012.5, 1012.2, 1011.9, 1012.4, 1012.2, 101…
    $ visib      <dbl> 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,…
    $ time_hour  <dttm> 2013-01-01 01:00:00, 2013-01-01 02:00:00, 2013-01-01 03:00…

``` r
glimpse(planes)
```

    Rows: 3,322
    Columns: 9
    $ tailnum      <chr> "N10156", "N102UW", "N103US", "N104UW", "N10575", "N105UW…
    $ year         <int> 2004, 1998, 1999, 1999, 2002, 1999, 1999, 1999, 1999, 199…
    $ type         <chr> "Fixed wing multi engine", "Fixed wing multi engine", "Fi…
    $ manufacturer <chr> "EMBRAER", "AIRBUS INDUSTRIE", "AIRBUS INDUSTRIE", "AIRBU…
    $ model        <chr> "EMB-145XR", "A320-214", "A320-214", "A320-214", "EMB-145…
    $ engines      <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
    $ seats        <int> 55, 182, 182, 182, 55, 182, 182, 182, 182, 182, 55, 55, 5…
    $ speed        <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ engine       <chr> "Turbo-fan", "Turbo-fan", "Turbo-fan", "Turbo-fan", "Turb…

``` r
glimpse(airports)
```

    Rows: 1,458
    Columns: 8
    $ faa   <chr> "04G", "06A", "06C", "06N", "09J", "0A9", "0G6", "0G7", "0P2", "…
    $ name  <chr> "Lansdowne Airport", "Moton Field Municipal Airport", "Schaumbur…
    $ lat   <dbl> 41.13047, 32.46057, 41.98934, 41.43191, 31.07447, 36.37122, 41.4…
    $ lon   <dbl> -80.61958, -85.68003, -88.10124, -74.39156, -81.42778, -82.17342…
    $ alt   <dbl> 1044, 264, 801, 523, 11, 1593, 730, 492, 1000, 108, 409, 875, 10…
    $ tz    <dbl> -5, -6, -6, -5, -5, -5, -5, -5, -5, -8, -5, -6, -5, -5, -5, -5, …
    $ dst   <chr> "A", "A", "A", "A", "A", "A", "A", "A", "U", "A", "A", "U", "A",…
    $ tzone <chr> "America/New_York", "America/Chicago", "America/Chicago", "Ameri…

``` r
glimpse(airlines)
```

    Rows: 16
    Columns: 2
    $ carrier <chr> "9E", "AA", "AS", "B6", "DL", "EV", "F9", "FL", "HA", "MQ", "O…
    $ name    <chr> "Endeavor Air Inc.", "American Airlines Inc.", "Alaska Airline…

#### Сколько компаний-перевозчиков (carrier) учитывают эти наборы данных (представлено в наборах данных)?

``` r
airlines %>% select(carrier) %>% n_distinct()
```

    [1] 16

#### Сколько рейсов принял аэропорт John F Kennedy Intl в мае?

``` r
flights %>%
  filter(month == 5, dest == "JFK") %>%
  tally()
```

    # A tibble: 1 × 1
          n
      <int>
    1     0

#### Какой самый северный аэропорт?

``` r
airports %>%
  slice_max(lat, n = 1)
```

    # A tibble: 1 × 8
      faa   name                      lat   lon   alt    tz dst   tzone
      <chr> <chr>                   <dbl> <dbl> <dbl> <dbl> <chr> <chr>
    1 EEN   Dillant Hopkins Airport  72.3  42.9   149    -5 A     <NA> 

#### Какой аэропорт самый высокогорный (находится выше всех над уровнем моря)?

``` r
airports %>%
  slice_max(alt, n = 1)
```

    # A tibble: 1 × 8
      faa   name        lat   lon   alt    tz dst   tzone         
      <chr> <chr>     <dbl> <dbl> <dbl> <dbl> <chr> <chr>         
    1 TEX   Telluride  38.0 -108.  9078    -7 A     America/Denver

#### Какие бортовые номера у самых старых самолетов?

``` r
planes %>%
  filter(!is.na(year)) %>%
  filter(year == min(year, na.rm = TRUE)) %>%
  select(tailnum, year)
```

    # A tibble: 1 × 2
      tailnum  year
      <chr>   <int>
    1 N381AA   1956

#### Какая средняя температура воздуха была в сентябре в аэропорту John F Kennedy Intl (в градусах Цельсия).

``` r
weather %>%
  filter(origin == "JFK", month == 9) %>%
  summarise(mean_temp_c = mean((temp - 32) * 5/9, na.rm = TRUE))
```

    # A tibble: 1 × 1
      mean_temp_c
            <dbl>
    1        19.4

#### Самолеты какой авиакомпании совершили больше всего вылетов в июне?

``` r
flights %>%
  filter(month == 6) %>%
  count(carrier, sort = TRUE) %>%
  left_join(airlines, by = "carrier") %>%
  slice(1) %>%
  pull(name)
```

    [1] "United Air Lines Inc."

#### Самолеты какой авиакомпании задерживались чаще других в 2013 году?

``` r
flights %>%
  mutate(delayed = dep_delay > 0 | arr_delay > 0) %>%
  group_by(carrier) %>%
  summarise(rate = mean(delayed, na.rm = TRUE)) %>%
  left_join(airlines, by = "carrier") %>%
  arrange(desc(rate)) %>%
  slice(1)
```

    # A tibble: 1 × 3
      carrier  rate name                  
      <chr>   <dbl> <chr>                 
    1 F9      0.699 Frontier Airlines Inc.

### Шаг 3

Отчет сделан и подготовлен согласно требованиям. На все вопросы получены
ответы с помощью ЯП R и пакетов `dplyr` и `nycflights13`s.
