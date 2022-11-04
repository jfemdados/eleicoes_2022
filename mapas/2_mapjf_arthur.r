# setup -------------------------------------------------------------------
library(tidyverse)
library(geobr)
library(sf)
library(tmap)
tmap_mode("view")


# mapas geobr -------------------------------------------------------------------

## cidade
juiz_de_fora <- geobr::read_municipality(code_muni = 3136702)


## zonas
juiz_de_fora_zonas <- geobr::read_census_tract(code_tract  = 3136702, year = 2000, simplified = F)

#juiz_de_fora_zonas_rurais <- geobr::read_census_tract(code_tract  = 3136702, year = 2000, zone = "rural" )


## mapa base

### ggplot
mapa_base <- ggplot() +
  geom_sf(data = juiz_de_fora_zonas, fill= "steelblue4", color= "#FEBF57", size= .15, show.legend = F) +
  theme_void()

### tmap
juiz_de_fora_zonas 
tm_shape() +
  tm_fill(col = "steelblue4", alpha = 0.75) +
  tm_borders(col = "#FEBF57")


# base ibge -------------------------------------------------------------------
base_ibge <- readRDS("data/base_ibge.rds") %>%
  select(Cod_setor, Cod_municipio:Nome_do_bairro) %>%
  filter(Nome_do_municipio == "JUIZ DE FORA") %>%
  rename(code_tract = Cod_setor)


## juntando geobr e ibge com right_join 
juiz_de_fora_agregado <- base_ibge %>%
  select(code_tract, Cod_bairro, Nome_do_bairro) %>%
  right_join(juiz_de_fora_zonas, by = "code_tract") %>% 
  rename(code_bairro = Cod_bairro, nome_bairro = Nome_do_bairro)

## agregando setores censitários em bairros
juiz_de_fora_bairros <- juiz_de_fora_agregado %>%
  group_by(nome_bairro) %>%
  summarize(geometry = sf::st_union(geom)) %>% 
  st_as_sf() #%>%
#st_simplify(dTolerance = 0.07)


# mapa de bairros -------------------------------------------------------------------

## ggplot
mapa_bairros <- juiz_de_fora_bairros %>% 
  ggplot() +
  geom_sf(fill= "steelblue4", color= "#FEBF57", size= .15) +
  labs(title = "Bairros de Juiz de Fora",
       caption = "Fonte: elaboração própria com base nos dados do IBGE") +
  theme_void()

## tmap
juiz_de_fora_bairros %>%
  tm_shape() +
  tm_fill(col = "steelblue4", alpha = 0.75) +
  tm_borders(col = "#FEBF57")

saveRDS(juiz_de_fora_bairros, file = "data/mapjf_bairros.RDS")