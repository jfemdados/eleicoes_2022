#Eleicoes 2022 - Mapa por bairro JF
#Autores:  Marcello e Arthur

library(tidyverse)
library(basedosdados)


# Importing da Base dos Dados ---------------------------------------------


# Defina o seu projeto no Google Cloud
basedosdados::set_billing_id("eleicoes2022jf")


### Resultados Governadores e Presidente por Município 

mg_result_exec_query <- basedosdados::bdplyr("br_tse_eleicoes.resultados_candidato_municipio") %>%
                    dplyr::filter(ano == 2022 & sigla_uf == "MG"
                                  & str_detect(cargo,"presidente|governador")
                                  )
              
              
mg_result_exec_data <- basedosdados::bd_collect(mg_result_exec_query)


### Detalhes dos Candidatos - Nomes
mg_candidatos_query <- basedosdados::bdplyr("br_tse_eleicoes.candidatos") %>%
  dplyr::filter(ano == 2022 
                & str_detect(cargo,"presidente|governador")
                  ) %>% 
  dplyr::select(ano, tipo_eleicao,
            id_candidato_bd, sequencial, nome_urna, cargo)

mg_candidatos_data <- basedosdados::bd_collect(mg_candidatos_query)




# Pós BD- Joining e Tidying -----------------------------------------------


mg_result_nome_join<- inner_join(mg_result_exec_data,
                                mg_candidatos_data 
                                #, by = "id_candidato_bd" #tirei pq peguei colunas a mais
                                # e assim o Rn deixar o R juntar colunas iguais
                                # n houve prejuízos com inner_join
                                )

# Número de Município bate
mg_result_nome_join %>% 
  filter(nome_urna == "Jair Bolsonaro") %>%
    count(nome_urna)

# Group By com summarize  =( deu errado

mg_result_nome_join %>%
  group_by(id_municipio, cargo, nome_urna) %>%
    summarize(max(votos))

# ERRO: pq quando você agrupa por tudo ele summariza nada kkk

#Colocando pra Wide

# Tive Que fazer dois DFs pq o join estava dando erro

#Presidente

mg_result_pres_wide<- mg_result_nome_join %>%
  #Sem Filtrar o Pivot Buga
  filter(cargo== "presidente") %>%
  #tive que tirar todos as colunas que remetiam à identificação do candidato
  #pq o pivot wider estava dando erro
  select(!c(numero_partido, sigla_partido,  numero_partido,
            numero_candidato, sequencial_candidato, id_candidato_bd,
            sequencial, resultado)
         ) %>%
  # Pivotando para juntar com o mapa do {geobr}
  tidyr::pivot_wider(names_from = nome_urna,
              values_from = votos) %>%
  janitor::clean_names() %>%
  # Jeito Feio de Fazer conta kkk
  mutate(mais_votado= if_else(lula > jair_bolsonaro,
                              true = "Lula",
                              false = "Bolsonaro")) %>%
  dplyr::relocate(.after = cargo, mais_votado, lula, jair_bolsonaro)

#Governador

mg_result_gov_wide<- mg_result_nome_join %>%
  filter(cargo== "governador") %>%
  #tive que tirar todos as colunas que remetiam à identificação do candidato
  #pq o pivot wider estava dando erro
  select(!c(numero_partido, sigla_partido,  numero_partido,
            numero_candidato, sequencial_candidato, id_candidato_bd,
            sequencial, resultado)
  ) %>%
  # Pivotando para juntar com o mapa do {geobr}
  pivot_wider(names_from = nome_urna,
              values_from = votos)  %>%
  janitor::clean_names() %>%
  # Jeito Feio de Fazer conta kkk
  mutate(mais_votado= if_else(zema > kalil,
                              true = "Zema",
                              false = "Kalil")) %>%
  dplyr::relocate(.after = cargo, mais_votado, zema, kalil)


mg_result_exec_wide<- mg_result_nome_join %>%
  filter(cargo== "governador") %>%
  #tive que tirar todos as colunas que remetiam à identificação do candidato
  #pq o pivot wider estava dando erro
  select(!c(numero_partido, sigla_partido,  numero_partido,
            numero_candidato, sequencial_candidato, id_candidato_bd,
            sequencial, resultado)
  ) %>%
  # Pivotando para juntar com o mapa do {geobr}
  pivot_wider(names_from = nome_urna,
              values_from = votos)  %>%
  janitor::clean_names()# %>%
  # Jeito Feio de Fazer conta kkk
  mutate(mais_votado= if_else(zema > kalil,
                              true = "Zema",
                              false = "Kalil")) %>%
  dplyr::relocate(.after = cargo, mais_votado, zema, kalil)




# Visualização com Mapas =D -----------------------------------------------

# Será que vai?


library(geobr)
library(tmap)
library(sf)

tmap_mode("view")


mapa_munic_mg <- geobr::read_municipality(code_muni = "MG")

base_ibge_raw <- read_rds("mapas/base_ibge.rds")


# Juntando Mapas

base_ibge <- base_ibge_raw %>%
  select(Cod_setor, Cod_meso:Nome_do_bairro) %>%
  filter(Nome_do_municipio == "JUIZ DE FORA") %>%
  rename(code_tract = Cod_setor)

# Presidente mais votado


# Join Shape File e Dados

mapa_vot_presid <-  full_join( x = mapa_munic_mg %>%
                                mutate(id_municipio= as.character(code_muni)),
                               y =  mg_result_pres_wide %>%
                                 select(mais_votado, lula, jair_bolsonaro,
                                        id_municipio)
                  ,by =  "id_municipio")




### Plotando o Mapa
mapa_vot_presid %>%
ggplot() +
  geom_sf( aes( fill= mais_votado) ,size= .20,
           color= "grey20", show.legend = TRUE) +
        theme_void() +
  scale_fill_manual( name= "Candidato",
                     #label= c("Menos de 20%", "20% a 40%"),
                     values= c("dodgerblue3","firebrick1")) +
  labs(title = "Eleições 2022: Presidente mais Votados por Município em MG", 
       #caption = "Fonte"
       )
  

# Governador mais votado



mapa_vot_gov <-  full_join( x = mapa_munic_mg %>%
                                 mutate(id_municipio= as.character(code_muni)),
                               y =  mg_result_gov_wide %>%
                                 select(mais_votado, zema, kalil,
                                        id_municipio)
                               ,by =  "id_municipio")




### ggplot
mapa_vot_gov %>%
  ggplot() +
  geom_sf( aes( fill= mais_votado) ,
           size= .15, color= "gray20", show.legend = TRUE) +
  theme_void() +
  scale_fill_manual( name= "Candidato",
                     #label= c("Menos de 20%", "20% a 40%"),
                     values= c("red4", "orange4")) +
  labs(title = "Eleições 2022: Governadores mais Votados por Município em MG", 
       caption = "DEU CERTO FILHA DA PUTA")


# Juntando os dois Mapas
# Não repara que o codigo é na correria

mg_result_exec_mais_vot<- full_join( mg_result_pres_wide %>%
                                           select(mais_votado, lula, jair_bolsonaro,
                                                  id_municipio),
                                         mg_result_gov_wide %>%
                                           select(mais_votado, zema, kalil,
                                                  id_municipio)
                                         ,by =  "id_municipio",
                                         suffix = c("_pres","_gov")) %>%
      mutate(dois_mais = case_when(
        mais_votado_pres == "Lula" & mais_votado_gov == "Zema" ~ "Lula e Zema",
        mais_votado_pres == "Lula" & mais_votado_gov == "Kalil" ~ "Lula e Kalil",
        mais_votado_pres == "Bolsonaro" & mais_votado_gov == "Zema" ~ "Bolsonaro e Zema",
        mais_votado_pres == "Bolsonaro" & mais_votado_gov == "Kalil" ~ "Bolsonaro e Kalil",
        TRUE ~ "Outros"
      )) %>%
  relocate(id_municipio,
           dois_mais)

mapa_vot_gov <-  full_join( x = mapa_munic_mg %>%
                              mutate(id_municipio= as.character(code_muni)),
                            y = mg_result_exec_mais_vot
                            ,by =  "id_municipio")

mapa_vot_gov %>%
  ggplot() +
  geom_sf( aes( fill= dois_mais) ,
           size= .15, color= "gray20", show.legend = TRUE) +
  theme_void() +
  scale_fill_manual( name= "Candidato",
                     #label= c("Menos de 20%", "20% a 40%"),
                     values= c("dodgerblue3","red2", "brown1")
                     ) +
  labs(title = "Eleições 2022: Pres. e Gov. mais Votados",
        subtitle = "Votos por Município em MG")



as_tibble(mapa_vot_gov)%>%
  count(dois_mais, sort = TRUE)
