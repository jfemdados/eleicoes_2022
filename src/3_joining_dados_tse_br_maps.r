#Joining

#Joing Mapas e dados do TSE


library(tidyverse)



# Joining -----------------------------------------------------------------


mapa_pontos<- tse_jf_corrigida %>%
                select(zona, secao, col_compl)%>%
                mutate(across(zona:secao, as.double))

as_tibble(mapa_pontos)


vot_sec_2022_pre_j<- votacao_secao_2022_jf_presid_wide %>%
      select(sg_uf:jose_maria_eymael) %>%
      rename("zona" = "nr_zona",
             "secao" = "nr_secao")

as_tibble(vot_sec_2022_pre_j)
        
#join_mapas_tse <- right_join( vot_sec_2022_pre_j, mapa_pontos)

join_tse_mapas <- left_join( vot_sec_2022_pre_j, mapa_pontos)

#inner_tse_mapas <- inner_join(vot_sec_2022_pre_j, mapa_pontos)

#Anti Join Pra conferir

anti_tse_mapas <- anti_join(vot_sec_2022_pre_j, mapa_pontos)
as_tibble(anti_tse_mapas)

anti_mapas_tse <- anti_join( mapa_pontos, vot_sec_2022_pre_j)
as_tibble(anti_mapas_tse)



# CONSTRUINDO MAPAS -------------------------------------------------------

#Aproveitando codigo Arthur
library(geobr)
library(tmap)
library(sf)

tmap_mode("view")


# mapas geobr -------------------------------------------------------------------

## cidade
#juiz_de_fora <- geobr::read_municipality(code_muni = 3136702)


## zonas censitarias
juiz_de_fora_zonas <- geobr::read_census_tract(code_tract  = 3136702, year = 2000, simplified = F)

#juiz_de_fora_zonas_rurais <- geobr::read_census_tract(code_tract  = 3136702, year = 2000, zone = "rural" )


## mapa base

### ggplot
ggplot() +
  geom_sf(data = juiz_de_fora_zonas, fill= "steelblue4", color= "#FEBF57", size= .15, show.legend = F) +
  theme_void()

### tmap
juiz_de_fora_zonas 
tm_shape() +
  tm_fill(col = "steelblue4", alpha = 0.75) +
  tm_borders(col = "#FEBF57")


# base ibge -------------------------------------------------------------------
base_ibge <- readRDS("mapas/base_ibge.rds") %>%
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
mapa_base<- juiz_de_fora_bairros %>% 
  ggplot() +
  geom_sf(fill= "steelblue4", color= "#FEBF57", size= .15)


## tmap
juiz_de_fora_bairros %>%
  tm_shape() +
  tm_fill(col = "steelblue4", alpha = 0.75) +
  tm_borders(col = "#FEBF57")









# Mapa de Pontos ----------------------------------------------------------

join_shp <- join_tse_mapas %>%
          st_as_sf(sf_column_name = "col_compl")


mapa_pontos %>%
  ggplot(aes(geometry= col_compl)) +
  geom_sf(size= .15) 

escolas <- geobr::read_schools() 

escolas_jf<- escolas%>%
  filter(name_muni == "Juiz de Fora")


mapa_escolas <- escolas_jf %>% 
  ggplot() +
  geom_sf()


mapa_base + mapa_escolas


# test --------------------------------------------------------------------

juiz_de_fora_bairros %>% 
  ggplot() +
  geom_sf(fill= "steelblue4", color= "#FEBF57", size= .15) +
  geom_sf(data= escolas_jf)

