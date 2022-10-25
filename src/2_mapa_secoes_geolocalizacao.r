#Eleicoes 2022 - Mapa por bairro JF
#Autores:  Marcello (Count e Faxina de Dados TSE)
#           Arthur (precisao da Geolocaliza??o das Urnas)

#Esse Codigo ? sobre a Localicaliza??o de cada Se??o Eleitoral

library(tidyverse)
library(basedosdados)

# Importando da base dos Dados
basedosdados::set_billing_id("tse22-364418")


tse_jf_completa <- basedosdados::read_sql( "SELECT zona, secao, melhor_urbano, melhor_rural, tse_recente, tse_distribuido, escolas_inep, 
                        escolas_municipais, ibge_cnefe_endereco, ibge_cnefe_local, google, google_relaxado, google_centro_geometrico,
                        ibge_povoados, ano
                        FROM `basedosdados.br_tse_eleicoes.local_secao` 
                        WHERE id_municipio = 3136702 AND ano = 2020
                        ")

tse_jf_select <- basedosdados::read_sql( "SELECT zona, secao, melhor_urbano, google_relaxado
                        FROM `basedosdados.br_tse_eleicoes.local_secao` 
                        WHERE id_municipio = 3136702 AND ano = 2020
                        ")




# Separando de acordo com a coluna que funciona ---------------------------

## Como as geometrias são diferentes, temos que converter pra simple feature ANTES de dar join

shp_tse_urbano <- tse_jf_select %>% 
  filter(!is.na(melhor_urbano)) %>% 
  st_as_sf(
    wkt = "melhor_urbano"
  ) %>% 
  rename(geometry = melhor_urbano) %>% 
  select(-google_relaxado)

### conferindo: class(df_tse_urbano)

shp_tse_google <- tse_jf_select %>% 
  # para garantir que só pegamos de fato seções que não tínhamos informação, inserimos tb is.na(melhor_urbano)
  filter(is.na(melhor_urbano) & !is.na(google_relaxado)) %>% 
  st_as_sf(
    wkt = "google_relaxado"
  ) %>% 
  rename(geometry = google_relaxado) %>% 
  select(-melhor_urbano)

## mapeando para explorar a diferença
tm_shape(shp_tse_urbano) +
  tm_dots(col = "blue") +
  tm_shape(shp_tse_google) +
  tm_dots(col = "red")




# Agregando ---------------------------------------------------------------

## Verificar se CRS é o mesmo
st_crs(shp_tse_urbano)
st_crs(shp_tse_google)

## Plot twist: não tem CRS! Visivelmente, é o WGS 84 (epsg = 4326). Configurando:
shp_tse_urbano <- shp_tse_urbano %>% 
  st_set_crs(4326)

shp_tse_google <- shp_tse_google %>% 
  st_set_crs(4326)

# check: st_crs(shp_tse_urbano) == st_crs(shp_tse_google)

## agora é partir pro abraço:
shp_tse_urnas <- shp_tse_urbano %>% 
  rbind(shp_tse_google)

## Verificar se deu certo via tmap:
tm_shape(shp_tse_urnas) +
  tm_dots(col = "red")

## limpando
remove(tse_jf_select, shp_tse_google, shp_tse_urbano)

## salvando
saveRDS(shp_tse_urnas, "data/shp_tse_urnas.RDS")