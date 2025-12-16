# pr 05 vakhrameev
vakhrameevaleksandr@yandex.ru

## Цель работы

1.  Получить знания о методах исследования радиоэлектронной обстановки.
2.  Составить представление о механизмах работы Wi-Fi сетей на канальном
    и сетевом уровне модели OSI.
3.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
4.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R

## Исходные данные

1.  Оепрационная система Darwin Mac.lan 25.0.0 Darwin Kernel Version
    25.0.0
2.  IDE Positron
3.  R version 4.5.2
4.  Логи инструментов для анализа беспроводных сетей — `tcpdump` и
    `airodump-ng`.

## Задание

Используя программный пакет `dplyr` языка программирования R провести
анализ журналов и ответить на вопросы

### Ход работы

### Подготовка данных

1.  Импортируйте данные
2.  Привести датасеты в вид “аккуратных данных”, преобразовать типы
    столбцов в соответствии с типом данных
3.  Просмотрите общую структуру данных с помощью функции `glimpse()`

### Анализ данных

1.  Определить небезопасные точки доступа (без шифрования – OPN)
2.  Определить производителя для каждого обнаруженного устройства
3.  Выявить устройства, использующие последнюю версию протокола
    шифрования WPA3, и названия точек доступа, реализованных на этих
    устройствах
4.  Отсортировать точки доступа по интервалу времени, в течение которого
    они находились на связи, по убыванию.
5.  Обнаружить топ-10 самых быстрых точек доступа.
6.  Отсортировать точки доступа по частоте отправки запросов (beacons) в
    единицу времени по их убыванию.
7.  Определить производителя для каждого обнаруженного устройства.
8.  Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес.
9.  Кластеризовать запросы от устройств к точкам доступа по их именам.
    Определить время появления устройства в зоне радиовидимости и время
    выхода его из нее.
10. Оценить стабильность уровня сигнала внури кластера во времени.
    Выявить наиболее стабильный кластер.

### Шаг 1. Подготовка данных

Инициализируем рабочую среду, загрузив необходимые библиотеки для
обработки данных:

``` r
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(knitr)
  library(readr)
})
```

Загружаем исходный датасет из облачного хранилища:

``` r
DATA_URL <- "https://storage.yandexcloud.net/dataset.ctfsec/P2_wifi_data.csv"
TEMP_DIR <- tempdir()
DATA_FILE <- file.path(TEMP_DIR, "P2_wifi_data.csv")

if (!file.exists(DATA_FILE)) {
  download.file(url = DATA_URL, destfile = DATA_FILE, mode = "wb")
}
```

Выполняем чтение данных с разделением на два датасета: точки доступа и
клиентские устройства:

``` r
suppressWarnings({
  file_content <- read_lines(DATA_FILE)
  section_boundary <- which(grepl("^Station MAC", file_content, ignore.case = TRUE))
  
  if (length(section_boundary) > 0) {
    wifi_ap_raw <- read_csv(DATA_FILE, n_max = section_boundary[1] - 2, show_col_types = FALSE)
    clients_raw <- read_csv(DATA_FILE, skip = section_boundary[1] - 1, show_col_types = FALSE)
  } else {
    stop("Не удалось определить границу между секциями данных")
  }
})

print("Структура клиентских данных:")
```

    [1] "Структура клиентских данных:"

``` r
head(clients_raw, 2)
```

    # A tibble: 2 × 7
      `Station MAC`  `First time seen`   `Last time seen`    Power `# packets` BSSID
      <chr>          <dttm>              <dttm>              <dbl>       <dbl> <chr>
    1 CA:66:3B:8F:5… 2023-07-28 09:13:03 2023-07-28 10:59:44   -33         858 BE:F…
    2 96:35:2D:3D:8… 2023-07-28 09:13:03 2023-07-28 09:13:03   -65           4 (not…
    # ℹ 1 more variable: `Probed ESSIDs` <chr>

Выполняем нормализацию и типизацию данных для датасета точек доступа:

``` r
normalize_access_points <- function(df) {
  names(df) <- trimws(names(df))
  
  df %>%
    rename(
      bssid      = BSSID,
      first_seen = `First time seen`,
      last_seen  = `Last time seen`,
      channel    = channel,
      speed      = Speed,
      privacy    = Privacy,
      cipher     = Cipher,
      auth       = Authentication,
      power      = Power,
      beacons    = `# beacons`,
      iv_count   = `# IV`,
      lan_ip     = `LAN IP`,
      id_length  = `ID-length`,
      essid      = ESSID,
      key        = Key
    ) %>%
    mutate(across(where(is.character), ~ trimws(.))) %>%
    mutate(
      first_seen = as.POSIXct(first_seen, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      last_seen  = as.POSIXct(last_seen,  format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      channel    = as.numeric(channel),
      speed      = as.numeric(speed),
      power      = as.numeric(power),
      beacons    = as.numeric(beacons),
      iv_count   = as.numeric(iv_count),
      id_length  = as.numeric(id_length)
    )
}

wifi_ap_clean <- normalize_access_points(wifi_ap_raw)
```

    Warning: There were 2 warnings in `mutate()`.
    The first warning was:
    ℹ In argument: `channel = as.numeric(channel)`.
    Caused by warning:
    ! NAs introduced by coercion
    ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

Применяем аналогичную процедуру нормализации для датасета клиентских
устройств:

``` r
normalize_clients <- function(df) {
  names(df) <- trimws(names(df))
  
  df %>%
    rename(
      station_mac   = `Station MAC`,
      first_seen    = `First time seen`,
      last_seen     = `Last time seen`,
      power         = Power,
      packets       = `# packets`,
      bssid         = BSSID,
      probed_essids = `Probed ESSIDs`
    ) %>%
    mutate(across(where(is.character), ~ trimws(.))) %>%
    mutate(
      first_seen  = as.POSIXct(first_seen, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      last_seen   = as.POSIXct(last_seen,  format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      power       = as.numeric(power),
      packets     = as.numeric(packets),
      station_mac = toupper(station_mac),
      bssid = case_when(
        is.na(bssid) ~ NA_character_,
        grepl("not associated", bssid, ignore.case = TRUE) ~ NA_character_,
        TRUE ~ toupper(bssid)
      )
    )
}

wifi_clients_clean <- normalize_clients(clients_raw)
```

Проводим инспекцию структуры нормализованных датасетов:

``` r
cat("Структура датасета точек доступа:\n")
```

    Структура датасета точек доступа:

``` r
glimpse(wifi_ap_clean)
```

    Rows: 169
    Columns: 15
    $ bssid      <chr> "BE:F1:71:D5:17:8B", "6E:C7:EC:16:DA:1A", "9A:75:A8:B9:04:1…
    $ first_seen <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 09:13…
    $ last_seen  <dttm> 2023-07-28 11:50:50, 2023-07-28 11:55:12, 2023-07-28 11:53…
    $ channel    <dbl> 1, 1, 1, 7, 6, 6, 11, 11, 11, 1, 6, 14, 11, 11, 6, 6, 6, 6,…
    $ speed      <dbl> 195, 130, 360, 360, 130, 130, 195, 130, 130, 195, 180, 65, …
    $ privacy    <chr> "WPA2", "WPA2", "WPA2", "WPA2", "WPA2", "OPN", "WPA2", "WPA…
    $ cipher     <chr> "CCMP", "CCMP", "CCMP", "CCMP", "CCMP", NA, "CCMP", "CCMP",…
    $ auth       <chr> "PSK", "PSK", "PSK", "PSK", "PSK", NA, "PSK", "PSK", "PSK",…
    $ power      <dbl> -30, -30, -68, -37, -57, -63, -27, -38, -38, -66, -42, -62,…
    $ beacons    <dbl> 846, 750, 694, 510, 647, 251, 1647, 1251, 704, 617, 1390, 1…
    $ iv_count   <dbl> 504, 116, 26, 21, 6, 3430, 80, 11, 0, 0, 86, 0, 0, 0, 907, …
    $ lan_ip     <chr> "0.  0.  0.  0", "0.  0.  0.  0", "0.  0.  0.  0", "0.  0. …
    $ id_length  <dbl> 12, 4, 2, 14, 25, 13, 12, 13, 24, 12, 10, 0, 24, 24, 12, 0,…
    $ essid      <chr> "C322U13 3965", "Cnet", "KC", "POCO X5 Pro 5G", NA, "MIREA_…
    $ key        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…

``` r
cat("\nСтруктура датасета клиентских устройств:\n")
```


    Структура датасета клиентских устройств:

``` r
glimpse(wifi_clients_clean)
```

    Rows: 12,081
    Columns: 7
    $ station_mac   <chr> "CA:66:3B:8F:56:DD", "96:35:2D:3D:85:E6", "5C:3A:45:9E:1…
    $ first_seen    <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 09…
    $ last_seen     <dttm> 2023-07-28 10:59:44, 2023-07-28 09:13:03, 2023-07-28 11…
    $ power         <dbl> -33, -65, -39, -61, -53, -43, -31, -71, -74, -65, -45, -…
    $ packets       <dbl> 858, 4, 432, 958, 1, 344, 163, 3, 115, 437, 265, 77, 7, …
    $ bssid         <chr> "BE:F1:71:D5:17:8B", NA, "BE:F1:71:D6:10:D7", "BE:F1:71:…
    $ probed_essids <chr> "C322U13 3965", "IT2 Wireless", "C322U21 0566", "C322U13…

### Шаг 2. Анализ данных

#### 1. Определить небезопасные точки доступа (без шифрования – OPN)

Выполняем фильтрацию точек доступа с отсутствующим шифрованием:

``` r
insecure_access_points <- wifi_ap_clean %>%
  filter(privacy == "OPN") %>%
  select(bssid, essid, privacy, channel, power)

insecure_access_points
```

    # A tibble: 42 × 5
       bssid             essid         privacy channel power
       <chr>             <chr>         <chr>     <dbl> <dbl>
     1 E8:28:C1:DC:B2:52 MIREA_HOTSPOT OPN           6   -63
     2 E8:28:C1:DC:B2:50 MIREA_GUESTS  OPN           6   -63
     3 E8:28:C1:DC:B2:51 <NA>          OPN           6   -63
     4 E8:28:C1:DC:FF:F2 <NA>          OPN           6    -1
     5 00:25:00:FF:94:73 <NA>          OPN          44    -1
     6 E8:28:C1:DD:04:52 MIREA_HOTSPOT OPN          11   -67
     7 E8:28:C1:DE:74:31 <NA>          OPN           6   -82
     8 E8:28:C1:DE:74:32 MIREA_HOTSPOT OPN           6   -69
     9 E8:28:C1:DC:C8:32 MIREA_HOTSPOT OPN           1   -69
    10 E8:28:C1:DD:04:50 MIREA_GUESTS  OPN          11   -78
    # ℹ 32 more rows

#### 2. Определить производителя для каждого обнаруженного устройства

Инициализируем справочник производителей на основе OUI-таблицы
Wireshark:

``` r
MANUF_URL  <- "https://raw.githubusercontent.com/observ3r/wobs/master/manuf.txt"
MANUF_FILE <- file.path(TEMP_DIR, "manuf.txt")

if (!file.exists(MANUF_FILE)) {
  download.file(MANUF_URL, MANUF_FILE, mode = "wb", quiet = TRUE)
}

manuf_raw <- read.table(
  MANUF_FILE,
  comment.char      = "#",
  stringsAsFactors  = FALSE,
  fill              = TRUE
)

manuf_tbl <- manuf_raw %>%
  transmute(
    prefix6 = toupper(gsub(":", "", V1)), 
    vendor  = V2
  ) %>%
  filter(nchar(prefix6) == 6, vendor != "") %>%
  distinct()
```

Реализуем функцию для нормализации MAC-адресов:

``` r
normalize_mac <- function(mac_address) {
  toupper(gsub("[^0-9A-F]", "", mac_address))
}
```

Обогащаем датасет точек доступа информацией о производителях:

``` r
wifi_ap_clean <- wifi_ap_clean %>%
  mutate(prefix6 = substr(normalize_mac(bssid), 1, 6)) %>%
  left_join(manuf_tbl, by = "prefix6")
```

Выполняем просмотр результатов идентификации производителей:

``` r
wifi_ap_clean %>%
  select(bssid, essid, vendor) %>%
  filter(!is.na(vendor)) %>%
  head(20)
```

    # A tibble: 20 × 3
       bssid             essid   vendor  
       <chr>             <chr>   <chr>   
     1 1C:7E:E5:8E:B7:DE <NA>    D-LinkIn
     2 00:25:00:FF:94:73 <NA>    Apple   
     3 00:26:99:F2:7A:E2 GIVC    Cisco   
     4 48:5B:39:F9:7A:48 <NA>    AsustekC
     5 00:26:99:F2:7A:E1 IKB     Cisco   
     6 00:26:99:BA:75:80 GIVC    Cisco   
     7 00:23:EB:E3:81:F2 GIVC    Cisco   
     8 00:23:EB:E3:81:F1 IKB     Cisco   
     9 00:26:99:F2:7A:E0 <NA>    Cisco   
    10 00:23:EB:E3:81:FE IKB     Cisco   
    11 00:23:EB:E3:81:FD GIVC    Cisco   
    12 00:26:CB:AA:62:71 IKB     Cisco   
    13 00:03:7A:1A:18:56 <NA>    TaiyoYud
    14 00:09:9A:12:55:04 <NA>    Elmo    
    15 00:23:EB:E3:49:31 <NA>    Cisco   
    16 00:23:EB:E3:44:31 <NA>    Cisco   
    17 00:03:7A:1A:03:56 MT_FREE TaiyoYud
    18 00:26:99:BA:75:8F <NA>    Cisco   
    19 00:03:7F:12:34:56 MT_FREE AtherosC
    20 00:26:99:F1:1A:E1 IKB     Cisco   

#### 3. Выявить устройства, использующие последнюю версию протокола шифрования WPA3, и названия точек доступа, реализованных на этих устройствах

Идентифицируем точки доступа с поддержкой протокола WPA3:

``` r
wpa3_access_points <- wifi_ap_clean %>%
  filter(
    grepl("WPA3", privacy, ignore.case = TRUE) | 
    grepl("SAE", auth, ignore.case = TRUE)
  ) %>%
  select(bssid, essid, privacy, auth, vendor)

wpa3_access_points
```

    # A tibble: 8 × 5
      bssid             essid                privacy   auth    vendor
      <chr>             <chr>                <chr>     <chr>   <chr> 
    1 26:20:53:0C:98:E8 <NA>                 WPA3 WPA2 SAE PSK <NA>  
    2 A2:FE:FF:B8:9B:C9 Christie’s           WPA3 WPA2 SAE PSK <NA>  
    3 96:FF:FC:91:EF:64 <NA>                 WPA3 WPA2 SAE PSK <NA>  
    4 CE:48:E7:86:4E:33 iPhone (Анастасия)   WPA3 WPA2 SAE PSK <NA>  
    5 8E:1F:94:96:DA:FD iPhone (Анастасия)   WPA3 WPA2 SAE PSK <NA>  
    6 BE:FD:EF:18:92:44 Димасик              WPA3 WPA2 SAE PSK <NA>  
    7 3A:DA:00:F9:0C:02 iPhone XS Max 🦊🐱🦊 WPA3 WPA2 SAE PSK <NA>  
    8 76:C5:A0:70:08:96 <NA>                 WPA3 WPA2 SAE PSK <NA>  

#### 4. Отсортировать точки доступа по интервалу времени, в течение которого они находились на связи, по убыванию

Выполняем агрегацию записей в сессии и вычисление длительности
активности. Сессии разделяются при интервале между записями более 45
минут:

``` r
aggregate_access_point_sessions <- function(df, session_gap_minutes = 45) {
  df %>%
    arrange(bssid, first_seen) %>%
    group_by(bssid) %>%
    mutate(
      gap_minutes = as.numeric(difftime(first_seen, lag(last_seen), units = "mins")),
      is_new_session = if_else(is.na(gap_minutes) | gap_minutes > session_gap_minutes, 1L, 0L),
      session_id = cumsum(is_new_session)
    ) %>%
    group_by(bssid, session_id) %>%
    summarise(
      essid      = first(essid),
      vendor     = first(vendor),
      first_seen = min(first_seen, na.rm = TRUE),
      last_seen  = max(last_seen,  na.rm = TRUE),
      beacons    = sum(beacons,    na.rm = TRUE),
      speed      = max(speed,      na.rm = TRUE),
      mean_power = mean(power,     na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    mutate(
      duration_seconds = as.numeric(difftime(last_seen, first_seen, units = "secs")),
      beacon_rate = beacons / duration_seconds
    )
}

wifi_ap_sessions <- aggregate_access_point_sessions(wifi_ap_clean)
```

    Warning: There were 3 warnings in `summarise()`.
    The first warning was:
    ℹ In argument: `first_seen = min(first_seen, na.rm = TRUE)`.
    ℹ In group 169: `bssid = "Station MAC"` `session_id = 1`.
    Caused by warning in `min.default()`:
    ! no non-missing arguments to min; returning Inf
    ℹ Run `dplyr::last_dplyr_warnings()` to see the 2 remaining warnings.

Выполняем сортировку по длительности сессий:

``` r
access_points_by_duration <- wifi_ap_sessions %>%
  arrange(desc(duration_seconds)) %>%
  select(bssid, essid, vendor, session_id, duration_seconds, first_seen, last_seen)

access_points_by_duration
```

    # A tibble: 169 × 7
       bssid            essid vendor session_id duration_seconds first_seen         
       <chr>            <chr> <chr>       <int>            <dbl> <dttm>             
     1 00:25:00:FF:94:… <NA>  Apple           1             9795 2023-07-28 09:13:06
     2 E8:28:C1:DD:04:… MIRE… <NA>            1             9776 2023-07-28 09:13:09
     3 E8:28:C1:DC:B2:… MIRE… <NA>            1             9755 2023-07-28 09:13:03
     4 08:3A:2F:56:35:… <NA>  <NA>            1             9746 2023-07-28 09:13:27
     5 6E:C7:EC:16:DA:… Cnet  <NA>            1             9729 2023-07-28 09:13:03
     6 E8:28:C1:DC:B2:… MIRE… <NA>            1             9726 2023-07-28 09:13:06
     7 48:5B:39:F9:7A:… <NA>  Asust…          1             9725 2023-07-28 09:13:06
     8 E8:28:C1:DC:B2:… <NA>  <NA>            1             9725 2023-07-28 09:13:06
     9 E8:28:C1:DC:FF:… <NA>  <NA>            1             9724 2023-07-28 09:13:06
    10 8E:55:4A:85:5B:… Vlad… <NA>            1             9723 2023-07-28 09:13:06
    # ℹ 159 more rows
    # ℹ 1 more variable: last_seen <dttm>

#### 5. Обнаружить топ-10 самых быстрых точек доступа

Идентифицируем точки доступа с максимальной пропускной способностью:

``` r
top_speed_access_points <- wifi_ap_sessions %>%
  arrange(desc(speed)) %>%
  slice_head(n = 10) %>%
  select(bssid, essid, vendor, session_id, speed, first_seen, last_seen)

top_speed_access_points
```

    # A tibble: 10 × 7
       bssid   essid vendor session_id speed first_seen          last_seen          
       <chr>   <chr> <chr>       <int> <dbl> <dttm>              <dttm>             
     1 26:20:… <NA>  <NA>            1   866 2023-07-28 09:15:45 2023-07-28 09:33:10
     2 8E:1F:… iPho… <NA>            1   866 2023-07-28 10:08:32 2023-07-28 10:15:27
     3 96:FF:… <NA>  <NA>            1   866 2023-07-28 09:52:54 2023-07-28 10:25:02
     4 CE:48:… iPho… <NA>            1   866 2023-07-28 09:59:20 2023-07-28 10:04:15
     5 CA:66:… <NA>  <NA>            1   858 2023-07-28 09:13:03 2023-07-28 10:59:44
     6 02:B3:… HONO… <NA>            1   360 2023-07-28 10:54:47 2023-07-28 10:54:47
     7 14:EB:… Gnez… <NA>            1   360 2023-07-28 09:25:01 2023-07-28 11:53:36
     8 4A:EC:… POCO… <NA>            1   360 2023-07-28 09:13:03 2023-07-28 11:04:01
     9 56:C5:… OneP… <NA>            1   360 2023-07-28 09:17:49 2023-07-28 10:27:22
    10 9A:75:… KC    <NA>            1   360 2023-07-28 09:13:03 2023-07-28 11:53:31

#### 6. Отсортировать точки доступа по частоте отправки запросов (beacons) в единицу времени по их убыванию

Выполняем ранжирование точек доступа по интенсивности отправки
beacon-кадров:

``` r
access_points_by_beacon_rate <- wifi_ap_sessions %>%
  filter(is.finite(beacon_rate)) %>%
  arrange(desc(beacon_rate)) %>%
  select(bssid, essid, vendor, session_id, beacons, duration_seconds, beacon_rate)

access_points_by_beacon_rate
```

    # A tibble: 126 × 7
       bssid            essid vendor session_id beacons duration_seconds beacon_rate
       <chr>            <chr> <chr>       <int>   <dbl>            <dbl>       <dbl>
     1 F2:30:AB:E9:03:… iPho… <NA>            1       6                7       0.857
     2 B2:CF:C0:00:4A:… Миха… <NA>            1       4                5       0.8  
     3 3A:DA:00:F9:0C:… iPho… <NA>            1       5                9       0.556
     4 00:3E:1A:5D:14:… MT_F… <NA>            1       1                2       0.5  
     5 02:BC:15:7E:D5:… MT_F… <NA>            1       1                2       0.5  
     6 76:C5:A0:70:08:… <NA>  <NA>            1       1                2       0.5  
     7 D2:25:91:F6:6C:… Саня  <NA>            1       5               13       0.385
     8 BE:F1:71:D6:10:… C322… <NA>            1    1647             9461       0.174
     9 00:03:7A:1A:03:… MT_F… Taiyo…          1       1                6       0.167
    10 38:1A:52:0D:84:… EBFC… <NA>            1     704             4319       0.163
    # ℹ 116 more rows

#### 7. Определить производителя для каждого обнаруженного устройства

Применяем аналогичную процедуру идентификации производителей для
клиентских устройств:

``` r
wifi_clients_clean <- wifi_clients_clean %>%
  mutate(prefix6 = substr(normalize_mac(station_mac), 1, 6)) %>%
  left_join(manuf_tbl, by = "prefix6")
```

Выполняем просмотр результатов идентификации производителей клиентских
устройств:

``` r
wifi_clients_clean %>%
  select(station_mac, vendor, bssid, power, packets) %>%
  filter(!is.na(vendor)) %>%
  head(20)
```

    # A tibble: 8 × 5
      station_mac       vendor   bssid             power packets
      <chr>             <chr>    <chr>             <dbl>   <dbl>
    1 00:95:69:E7:7F:35 LsdScien <NA>                -69    2245
    2 00:95:69:E7:7C:ED LsdScien <NA>                -55    4096
    3 00:95:69:E7:7D:21 LsdScien <NA>                -33    8171
    4 B8:27:EB:59:FA:0E Raspberr 6E:C7:EC:16:DA:1A    -1     405
    5 00:90:4C:E6:54:54 Epigram  <NA>                -65      16
    6 EC:55:F9:A1:4C:6B HonHaiPr 9A:9F:06:44:24:5B    -1       1
    7 00:04:35:22:4F:75 ComptekI 00:AB:0A:00:10:10   -83      20
    8 00:0C:E7:A8:D6:73 Mediatek <NA>                -67       3

#### 8. Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес

Реализуем алгоритм определения рандомизации MAC-адресов на основе
анализа битов первого октета:

``` r
is_randomized_mac <- function(mac_address) {
  mac_hex <- normalize_mac(mac_address)
  
  if (nchar(mac_hex) < 2) return(NA)
  
  first_octet <- strtoi(substr(mac_hex, 1, 2), base = 16)
  if (is.na(first_octet)) return(NA)
  
  if (bitwAnd(first_octet, 0x01) != 0) return(NA)
  
  bitwAnd(first_octet, 0x02) != 0
}
```

Выполняем идентификацию устройств с нерандомизированными MAC-адресами:

``` r
non_randomized_clients <- wifi_clients_clean %>%
  mutate(is_randomized = vapply(station_mac, is_randomized_mac, logical(1))) %>%
  filter(is_randomized == FALSE) %>%
  select(station_mac, vendor, bssid, first_seen, last_seen, power, packets)

non_randomized_clients
```

    # A tibble: 217 × 7
       station_mac       vendor  bssid first_seen          last_seen           power
       <chr>             <chr>   <chr> <dttm>              <dttm>              <dbl>
     1 5C:3A:45:9E:1A:7B <NA>    BE:F… 2023-07-28 09:13:03 2023-07-28 11:51:54   -39
     2 C0:E4:34:D8:E7:E5 <NA>    BE:F… 2023-07-28 09:13:03 2023-07-28 11:53:16   -61
     3 10:51:07:CB:33:E7 <NA>    <NA>  2023-07-28 09:13:05 2023-07-28 11:56:06   -43
     4 68:54:5A:40:35:9E <NA>    1E:9… 2023-07-28 09:13:06 2023-07-28 11:50:50   -31
     5 74:4C:A1:70:CE:F7 <NA>    E8:2… 2023-07-28 09:13:06 2023-07-28 09:20:01   -71
     6 BC:F1:71:D4:DB:04 <NA>    <NA>  2023-07-28 09:13:07 2023-07-28 10:57:52   -45
     7 4C:44:5B:14:76:E3 <NA>    E8:2… 2023-07-28 09:13:09 2023-07-28 09:47:44    -1
     8 A0:E7:0B:AE:D5:44 <NA>    0A:C… 2023-07-28 09:13:09 2023-07-28 11:34:42   -37
     9 00:95:69:E7:7F:35 LsdSci… <NA>  2023-07-28 09:13:11 2023-07-28 11:56:07   -69
    10 00:95:69:E7:7C:ED LsdSci… <NA>  2023-07-28 09:13:11 2023-07-28 11:56:13   -55
    # ℹ 207 more rows
    # ℹ 1 more variable: packets <dbl>

#### 9. Кластеризовать запросы от устройств к точкам доступа по их именам. Определить время появления устройства в зоне радиовидимости и время выхода его из нее

Выполняем нормализацию идентификаторов сетей (ESSID) для последующей
кластеризации:

``` r
access_point_essid_map <- wifi_ap_clean %>%
  mutate(essid_normalized = str_squish(essid)) %>%
  select(bssid, essid_normalized) %>%
  distinct()
```

Выполняем кластеризацию взаимодействий устройств с точками доступа:

``` r
device_network_clusters <- wifi_clients_clean %>%
  filter(!is.na(bssid)) %>%
  left_join(access_point_essid_map, by = "bssid") %>%
  filter(!is.na(essid_normalized), essid_normalized != "") %>%
  group_by(station_mac, essid_normalized) %>% 
  summarise(
    first_seen      = min(first_seen, na.rm = TRUE),
    last_seen       = max(last_seen,  na.rm = TRUE),
    duration_seconds = as.numeric(difftime(last_seen, first_seen, units = "secs")),
    record_count    = n(),
    .groups         = "drop"
  ) %>%
  arrange(desc(duration_seconds), station_mac, essid_normalized)

device_network_clusters
```

    # A tibble: 99 × 6
       station_mac       essid_normalized first_seen          last_seen          
       <chr>             <chr>            <dttm>              <dttm>             
     1 8C:55:4A:DE:F2:38 Galaxy A30s5208  2023-07-28 09:13:17 2023-07-28 11:56:16
     2 CA:54:C4:8B:B5:3A GIVC             2023-07-28 09:13:06 2023-07-28 11:55:04
     3 F6:4D:98:98:18:C3 GIVC             2023-07-28 09:14:37 2023-07-28 11:55:29
     4 C0:E4:34:D8:E7:E5 C322U13 3965     2023-07-28 09:13:03 2023-07-28 11:53:16
     5 5C:3A:45:9E:1A:7B C322U21 0566     2023-07-28 09:13:03 2023-07-28 11:51:54
     6 28:7F:CF:23:25:53 KC               2023-07-28 09:13:14 2023-07-28 11:51:50
     7 34:E1:2D:3C:C8:2D Cnet             2023-07-28 09:13:29 2023-07-28 11:51:50
     8 88:D8:2E:4F:9B:1A POCO X5 Pro 5G   2023-07-28 09:13:19 2023-07-28 11:51:24
     9 FE:B7:DD:ED:94:91 MIREA_HOTSPOT    2023-07-28 09:13:55 2023-07-28 11:51:47
    10 68:54:5A:40:35:9E Galaxy A71       2023-07-28 09:13:06 2023-07-28 11:50:50
    # ℹ 89 more rows
    # ℹ 2 more variables: duration_seconds <dbl>, record_count <int>

#### 10. Оценить стабильность уровня сигнала внутри кластера во времени. Выявить наиболее стабильный кластер

Выполняем анализ стабильности уровня сигнала для кластеров (устройство,
сеть):

``` r
signal_stability_clusters <- wifi_clients_clean %>%
  filter(!is.na(bssid)) %>%
  left_join(access_point_essid_map, by = "bssid") %>%
  filter(!is.na(essid_normalized), essid_normalized != "") %>%
  group_by(station_mac, essid_normalized)
```

Вычисляем метрики стабильности сигнала и идентифицируем наиболее
стабильный кластер:

``` r
signal_stability_metrics <- signal_stability_clusters %>%
  summarise(
    observation_count = n(),
    time_span_minutes = as.numeric(difftime(max(last_seen), min(first_seen), units = "mins")),
    rssi_std_dev      = sd(power, na.rm = TRUE),
    rssi_mean         = mean(power, na.rm = TRUE),
    .groups           = "drop"
  ) %>%
  arrange(rssi_std_dev)

most_stable_cluster <- slice_head(signal_stability_metrics, n = 1)

cat("Метрики стабильности сигнала:\n")
```

    Метрики стабильности сигнала:

``` r
print(signal_stability_metrics)
```

    # A tibble: 99 × 6
       station_mac essid_normalized observation_count time_span_minutes rssi_std_dev
       <chr>       <chr>                        <int>             <dbl>        <dbl>
     1 00:E9:3A:6… POCO C40                         1            99.9             NA
     2 00:F4:8D:F… Redmi 12                         1            58.4             NA
     3 02:69:A5:2… Galaxy A71                       1            32.3             NA
     4 02:B3:4E:2… Димасик                          1             0               NA
     5 04:8C:9A:0… MIREA_HOTSPOT                    1            88.1             NA
     6 06:15:2E:1… MIREA_HOTSPOT                    1             2.9             NA
     7 06:7A:BA:E… Vladimir                         1            18.1             NA
     8 06:F2:A9:C… MIREA_HOTSPOT                    1           139.              NA
     9 0A:AB:49:3… MIREA_HOTSPOT                    1            67.1             NA
    10 0A:C2:C3:0… MIREA_HOTSPOT                    1             0.133           NA
    # ℹ 89 more rows
    # ℹ 1 more variable: rssi_mean <dbl>

``` r
cat("\nНаиболее стабильный кластер:\n")
```


    Наиболее стабильный кластер:

``` r
print(most_stable_cluster)
```

    # A tibble: 1 × 6
      station_mac  essid_normalized observation_count time_span_minutes rssi_std_dev
      <chr>        <chr>                        <int>             <dbl>        <dbl>
    1 00:E9:3A:67… POCO C40                         1              99.9           NA
    # ℹ 1 more variable: rssi_mean <dbl>

### Шаг 3.

Отчет подготовлен и оформлен.

## Вывод

В ходе практической работы я улучшил навыки использования языка
программирования R, а также освоил анализ данных о состоянии
беспроводных сетей.
