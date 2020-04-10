library(dplyr)
### 1. baixar o CSV de casos disponível em https://covid.saude.gov.br/
### 2. abrir o arquivo e renomear o campo "região" no header para "reg" (o R não consegue lidar direito com o ~)
### 3. substituir o nome do arquivo no argumento file abaixo
raw <- read.table(file = "filepath", header=TRUE, sep=";", stringsAsFactors = FALSE)
ms_data <- raw %>%
    mutate(date = as.Date(data, format = "%d/%m/%Y"),
           state = sigla
           region = reg) %>%
    select(-data, -sigla, -reg, -casosAcumulados, -obitosAcumulados) %>%
    rename(new_cases = casosNovos,
           new_deaths = obitosNovos) %>%
    arrange(date) %>%
    data.frame()

brazil_aggregate <- ms_data %>%
    group_by(date) %>%
    summarize(new_cases = sum(new_cases),
              new_deaths = sum(new_deaths)) %>%
    ungroup() %>%
    mutate(total_cases = cumsum(new_cases),
           total_deaths = cumsum(new_deaths),
           region = as.factor("Brasil")) %>%
    data.frame()

by_state <- ms_data %>%
    group_by(state) %>%
    mutate(total_cases = cumsum(new_cases),
           total_deaths = cumsum(new_deaths)) %>%
    ungroup() %>%
    mutate(region = state) %>%
    select(-state) %>%
    data.frame()

by_region <- ms_data %>%
    group_by(region) %>%
    mutate(total_cases = cumsum(new_cases),
           total_deaths = cumsum(new_deaths)) %>%
    ungroup() %>%
    select(-state) %>%
    data.frame()

brazil_covid_data <- bind_rows(brazil_aggregate, by_state, by_region) %>%
    mutate(region = as.factor(region))
