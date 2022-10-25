#Joining

#Joing Mapas e dados do TSE

#source("src/2_mapa_secoes_geolocalizacao.r")

library(tidyverse)




# Join: votos + seções -----------------------------------------------------------------

shp_tse_urnas <- readRDS("data/shp_tse_urnas.RDS")

shp_votos_2022_presid <- df_votos_2022_presid_tally %>% 
  rename(zona = nr_zona,
         secao = nr_secao) %>% 
  right_join(shp_tse_urnas) %>% 
  st_as_sf()

# CONSTRUINDO MAPAS -------------------------------------------------------

# geobr -------------------------------------------------------------------

## cidade
shp_jf <- geobr::read_municipality(code_muni = 3136702)

## setores censitarios
shp_jf_zonas <- geobr::read_census_tract(code_tract  = 3136702, year = 2000, simplified = F)




# mapa de setores censitários ---------------------------------------------------------------

### ggplot
ggplot() +
  geom_sf(data = juiz_de_fora_zonas, fill= "steelblue4", color= "#FEBF57", size= .15, show.legend = F) +
  theme_void()

### tmap
juiz_de_fora_zonas %>% 
tm_shape() +
  tm_fill(col = "steelblue4", alpha = 0.75) +
  tm_borders(col = "#FEBF57")




# base ibge -------------------------------------------------------------------
df_ibge <- readRDS("data/base_ibge.rds") %>%
  select(Cod_setor, Cod_municipio:Nome_do_bairro) %>%
  filter(Nome_do_municipio == "JUIZ DE FORA") %>%
  rename(code_tract = Cod_setor)

## juntando geobr e ibge com right_join 
shp_jf_agregado <- df_ibge %>%
  select(code_tract, Cod_bairro, Nome_do_bairro) %>%
  right_join(shp_jf_zonas, by = "code_tract") %>% 
  rename(code_bairro = Cod_bairro, nome_bairro = Nome_do_bairro)

## agregando setores censitarios em bairros
shp_jf_bairros <- shp_jf_agregado %>%
  group_by(nome_bairro) %>%
  summarize(geometry = sf::st_union(geom)) %>% 
  st_as_sf()




# mapa de bairros -------------------------------------------------------------------

## ggplot
mapa_base <- shp_jf_bairros %>% 
  ggplot() +
  geom_sf(fill= "steelblue4", color= "#FEBF57", size= .15)


## tmap
shp_jf_bairros %>%
  tm_shape() +
  tm_fill(col = "steelblue4", alpha = 0.75) +
  tm_borders(col = "#FEBF57")




# Mapa de colégios eleitorais ----------------------------------------------------------

## base de dados das escolas
shp_escolas <- geobr::read_schools() %>%
  filter(name_muni == "Juiz de Fora")

mapa_escolas <- mapa_base + 
  geom_sf(
    data = shp_escolas,
    size = .15
  )




# Spatial join: tse + bairros ----------------------------------------------------

## verificando CRS
st_crs(shp_jf_bairros) # SIRGAS 2000, padrão do Brasil e mapas {geobr}
st_crs(shp_votos_2022_presid) # WGS 84, padrão GPS. 

## arrumando CRS
shp_votos_2022_presid <- shp_votos_2022_presid %>% 
  st_transform(crs = st_crs(shp_jf_bairros))

foo <- shp_jf_bairros %>% 
  # não foi necessário usar a linha abaixo, mas é bom ter por perto.
  #filter(!is.na(nome_bairro) & nome_bairro != "JUIZ DE FORA (demais setores)") %>% 
  st_join(shp_votos_2022_presid, join = st_contains)




# Summary -----------------------------------------------------------------

foo %>% 
  group_by(nome_bairro, nm_votavel) %>% 
  tally(n)

foo_wide <- foo %>%
  #Pivotando para que 1 linha = 1 seção
  tidyr::pivot_wider(names_from = "nm_votavel",
                     values_from = "n") %>%
  janitor::clean_names() %>%
  #botar os maiores na frente
  dplyr::relocate(.before =  geometry,
                  luiz_inacio_lula_da_silva,
                  jair_messias_bolsonaro,
                  simone_nassar_tebet,
                  ciro_ferreira_gomes,
                  voto_branco,
                  voto_nulo)




# mapeando ----------------------------------------------------------------

foo_wide2 <- foo_wide %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  select(nome_bairro, luiz_inacio_lula_da_silva, jair_messias_bolsonaro) %>%
  mutate(dif_votos = luiz_inacio_lula_da_silva - jair_messias_bolsonaro)

ggplot(foo_wide2) +
  geom_sf(
    aes(fill = factor(dif_votos))
  )


# limpando ----------------------------------------------------------------

remove(df_ibge, shp_jf_agregado, shp_jf_zonas, shp_tse_urnas)
