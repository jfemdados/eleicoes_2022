#Eleicoes 2022 - Mapa por bairro JF
#Autores:  Marcello (Count e Faxina de Dados TSE)
#           Arthur (precisao da Geolocalização das Urnas)

library(tidyverse)
library(basedosdados)


# Importing da Base dos Dados ---------------------------------------------


# Defina o seu projeto no Google Cloud
basedosdados::set_billing_id("eleicoes2022jf")


### Resultados Governadores e Presidente por Município 

mg_resultados_exec_query <- basedosdados::bdplyr("br_tse_eleicoes.resultados_candidato_municipio") %>%
                    dplyr::filter(ano == 2022 & sigla_uf == "MG"
                                  & str_detect(cargo,"presidente|governador")
                                  )
              
              
mg_resultados_exec_data <- basedosdados::bd_collect(mg_resultados_exec_query)


### Detalhes dos Candidatos - Nomes
mg_candidatos_query <- basedosdados::bdplyr("br_tse_eleicoes.candidatos") %>%
  dplyr::filter(ano == 2022 
                & str_detect(cargo,"presidente|governador")
                  ) %>% 
  dplyr::select(ano, tipo_eleicao,
            id_candidato_bd, sequencial, nome_urna, cargo)

mg_candidatos_data <- basedosdados::bd_collect(mg_candidatos_query)



#Join com Nomes =D

mg_result_nome_join<- inner_join(mg_resultados_exec_data,
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


#Colocando pra Wide

mg_result_pres_wide<- mg_result_nome_join %>%
  filter(cargo== "presidente") %>%
  select(!c(numero_partido, sigla_partido,  numero_partido,
            numero_candidato, sequencial_candidato, id_candidato_bd,
            sequencial, resultado)
         ) %>%
  tidyr::pivot_wider(names_from = nome_urna,
              values_from = votos) %>%
  janitor::clean_names() %>%
  mutate(mais_votado= if_else(lula >= jair_bolsonaro,
                              true = "Lula",
                              false = "Bolsonaro")) %>%
  dplyr::relocate(.after = cargo, mais_votado, lula, jair_bolsonaro)


mg_result_gov_wide<- mg_result_nome_join %>%
  filter(cargo== "governador") %>%
  select(!c(numero_partido, sigla_partido,  numero_partido,
            numero_candidato, sequencial_candidato, id_candidato_bd,
            sequencial, resultado)
  ) %>%
  pivot_wider(names_from = nome_urna,
              values_from = votos)  %>%
  janitor::clean_names() %>%
  mutate(mais_votado= if_else(zema >= kalil,
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

# Presidente mais votado

select(mg_result_pres_wide, mais_votado, lula, jair_bolsonaro,
       id_municipio)

mapa_vot_presid <-  full_join( x = mapa_munic_mg %>%
                                mutate(id_municipio= as.character(code_muni)),
                               y =  mg_result_pres_wide %>%
                                 select(mais_votado, lula, jair_bolsonaro,
                                        id_municipio)
                  ,by =  "id_municipio")




### ggplot
mapa_vot_presid %>%
ggplot() +
  geom_sf( aes( fill= mais_votado) ,size= .15,
           color= "grey25", show.legend = TRUE) +
        theme_void() +
  scale_fill_manual( name= "Candidato",
                     #label= c("Menos de 20%", "20% a 40%"),
                     values= c("dodgerblue3","firebrick1")) +
  labs(title = "Eleições 2022: Presidente mais Votados por Município em MG", 
       caption = "DEU CERTO FILHA DA PUTA")
  

# Governador mais votado

select(mg_result_gov_wide, mais_votado, zema, kalil,
       id_municipio)

mapa_vot_gov <-  full_join( x = mapa_munic_mg %>%
                                 mutate(id_municipio= as.character(code_muni)),
                               y =  mg_result_gov_wide %>%
                                 select(mais_votado, zema, kalil,
                                        id_municipio)
                               ,by =  "id_municipio")




### ggplot
mapa_vot_gov %>%
  ggplot() +
  geom_sf( aes( fill= mais_votado) ,size= .15,
           color= "grey25", show.legend = TRUE) +
  theme_void() +
  scale_fill_manual( name= "Candidato",
                     #label= c("Menos de 20%", "20% a 40%"),
                     values= c("red4", "orange4")) +
  labs(title = "Eleições 2022: Governadores mais Votados por Município em MG", 
       caption = "DEU CERTO FILHA DA PUTA")


