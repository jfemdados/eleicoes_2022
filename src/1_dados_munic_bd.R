#Eleicoes 2022 - Mapa por bairro JF
#Autores:  Marcello (Count e Faxina de Dados TSE)
#           Arthur (precisao da Geolocalização das Urnas)

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

# Detalhes dos Candidatos - Nomes

mg_detalhes_pres_query <- basedosdados::bdplyr("br_tse_eleicoes.detalhes_votacao_municipio") %>%
  dplyr::filter(ano == 2022 & sigla_uf == "MG" &  turno == 1
                & str_detect(cargo,"presidente")
                ) 


mg_detalhes_pres_data <- basedosdados::bd_collect(mg_detalhes_pres_query) 




# Pós BD- Joining ----------------------------------------------------------


mg_result_nome_join<- inner_join(mg_result_exec_data,
                                mg_candidatos_data %>%
                                  rename( "sequencial_candidato" = "sequencial")
                                #,
                              #  by = c("ano", "tipo_eleicao", "cargo", "id_candidato_bd"
                               #        , "sequencial_candidato") 
                                )



# Número de Município bate
mg_result_nome_join %>% 
  filter(nome_urna == "Jair Bolsonaro") %>%
    count(nome_urna)


#Dúvidas com summarize ---------------------
# Group By com summarize  =( deu errado

mg_result_nome_join %>%
  filter(cargo== "presidente") %>%
      group_by(id_municipio,
               nome_urna,
              # cargo,
               ) %>%
        summarize(mais_votado = max(votos),
                  .groups = "drop_last")


mg_result_nome_join %>%
  filter(cargo== "presidente") %>%
  group_by(id_municipio,
           #nome_urna,
           #cargo,
  ) %>%
      summarize(mais_votado = max(votos),
                )

mg_result_nome_join %>%
  filter(cargo== "presidente") %>%
  group_by(id_municipio,
           # cargo,
          ) %>%
slice(which.max(votos))

exemplo<- mg_result_nome_join %>%
 filter( str_detect(nome_urna, "Bolsonaro|Lula|Ciro|Simone" ))%>%
  select(id_municipio, cargo ,votos, nome_urna) # %>% view()
  

exemplo %>%
  group_by(id_municipio,
           nome_urna,
           #cargo,
         ) %>%
          summarize(mais_votado = max(votos))

# ERRO: pq quando você agrupa por tudo ele summariza nada kkk




# Pós BD - Tidying --------------------------------------------------------

# Ideia é fazer um pivot_wider, pré tidying pros mapas

# Tive Que fazer dois DFs pq o join estava dando erro

#Presidente

mg_result_pres_wide<- mg_result_nome_join %>%
  #Sem Filtrar o Pivot Buga
  filter(cargo== "presidente") %>%
  #tive que tirar todos as colunas que remetiam à identificação do candidato
  #pq o pivot wider estava dando erro
  select(!c(numero_partido, sigla_partido,  numero_partido,
            numero_candidato, sequencial_candidato, id_candidato_bd,
             resultado)
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
            resultado)
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


mg_result_pres_wide

# Tentando fazer os dois juntos

#No Meu Jeito de fazer Pivot
 # Estou pivotando por nome do candidato na urna,


#mg_result_exec_wide<- mg_result_nome_join %>%
#  #filter(cargo== "governador") %>%
#  #tive que tirar todos as colunas que remetiam à identificação do candidato
#  #pq o pivot wider estava dando erro
#  select(!c(numero_partido, sigla_partido,  numero_partido,
#            numero_candidato, sequencial_candidato, id_candidato_bd,
#            sequencial, resultado)
#  ) %>%
#  # Pivotando para juntar com o mapa do {geobr}
#  pivot_wider(names_from = nome_urna,
#              values_from = votos)  %>%
#  janitor::clean_names()# %>%
#  # Jeito Feio de Fazer conta kkk
#  mutate(mais_votado= if_else(zema > kalil,
#                              true = "Zema",
#                              false = "Kalil")) %>%
#  dplyr::relocate(.after = cargo, mais_votado, zema, kalil)




# Visualização com Mapas =D -----------------------------------------------

# Será que vai?


library(geobr)
library(tmap)
library(sf)

tmap_mode("view")

#Shape File PAI DE TODOS dos Municípios

mapa_munic_mg <- geobr::read_municipality(code_muni = "MG")



# Presidente mais votado --------------------------------------------------

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
  

# Detalhes
mg_result_detal <- full_join(x= mg_result_pres_wide %>%
                               select(mais_votado, lula, jair_bolsonaro,
                                      id_municipio),
                             y = mg_detalhes_pres_data %>%
                               select(id_municipio, c(aptos_totalizadas:votos_nulos))
                             #,by =  "id_municipio"
                             ) %>%
  mutate(prop_lula_vl=lula/votos_validos)

mapa_vot_presid_det <-  full_join( x = mapa_munic_mg %>%
                                 mutate(id_municipio= as.character(code_muni)),
                               y =  mg_result_detal
                               #,by =  "id_municipio"
                               ) 
mapa_vot_presid_det %>%
  ggplot() +
  geom_sf( aes( fill= prop_lula_vl) ,size= .20,
           color= "grey20", show.legend = TRUE) +
  theme_void() +
  scale_fill_brewer( name= "Candidato",
                     #label= c("Menos de 20%", "20% a 40%"),
                     values= c("dodgerblue3","firebrick1")) +
  labs(title = "Eleições 2022: Presidente mais Votados por Município em MG", 
       #caption = "Fonte"
  )



# Governador mais votado --------------------------------------------------





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



# Juntnado Dois Mais Votados em MG ----------------------------------------


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

shp_df_vot_gov <-  full_join( x = mapa_munic_mg %>%
                              mutate(id_municipio= as.character(code_muni)),
                            y = mg_result_exec_mais_vot
                            ,by =  "id_municipio")


plot_vot_gov <- shp_df_vot_gov%>%
              ggplot() +
              geom_sf( aes( fill= dois_mais) ,
                       size= .40, color= "grey20", show.legend = TRUE) +
              theme_void() +
              scale_fill_manual( name= "Candidato",
                                 #label= c("Menos de 20%", "20% a 40%"),
                                 values= c("#2A628F", "#C81D25","#FF5A5F")
                                 ) #+
              #labs(title = "Eleições 2022: Pres. e Gov. mais Votados",
               #     subtitle = "Votos por Município em MG")

ggsave("mapas/exports/votos_por_municp.png",
       plot_vot_gov,
       dev = "png", dpi = 300, bg = "white",
       width = 1080, height = 1080, units = "px")


# Rode Isso para ver o Errp
as_tibble(shp_df_vot_gov) %>%
  count(dois_mais, sort = TRUE)



# Mapa Por Regiões - Cruzando com Regiões do IBGE -------------------------


base_ibge_raw <- read_rds("mapas/base_ibge.rds")

# Base de Setores Censitários de JF

base_ibge_jf <- base_ibge_raw %>%
      select(Cod_setor, Cod_meso:Nome_do_bairro) %>%
      filter(Nome_do_municipio == "JUIZ DE FORA") %>%
      rename(code_tract = Cod_setor) %>%
       janitor::clean_names()



# Base de Municípios de MG (agrupando setores Censitários)
base_ibge_mg <-  base_ibge_raw %>%
        select(Cod_setor:Nome_do_bairro) %>%
        filter(Nome_da_UF== "Minas Gerais") %>%
        janitor::clean_names() %>%
        #Selecionando as Colunas que eu Quero Manter
        group_by( nome_da_meso, cod_meso, nome_da_micro, cod_micro,
                 nome_da_rm, cod_rm, cod_municipio) %>%
         #Agrupando por Município
        count(nome_do_municipio) %>% 
        rename("code_muni"= cod_municipio) %>%
        mutate(code_muni= as.numeric(code_muni)) %>%
        ungroup()

base_ibge_mg %>% 
  group_by( nome_da_rm ) %>%
    count(nome_da_meso)
        
#Fazendo Polígonos pelo Geobr

library(ggplot2)
library(geobr)


mapa_regioes_mg <- mapa_munic_mg %>%
        inner_join(base_ibge_mg, by= "code_muni")# %>% sf::st_simplify(dTolerance = 0.)

mapa_regioes_mg %>%
  #filter(nome_da_meso== "Zona da Mata") %>%
  ggplot() + geom_sf(aes( fill= nome_da_meso ),
                      size= .15, show.legend = TRUE) 


# Mapa de Votos  pela Zona da Mata ------------------------------


mapa_vot_gov_zm <-  left_join( x = mapa_regioes_mg %>%
                                 filter(nome_da_meso== "Zona da Mata") %>%
                              mutate(id_municipio= as.character(code_muni)),
                            y = mg_result_exec_mais_vot
                            ,by =  "id_municipio")


saveRDS(mapa_vot_gov_zm, "scrapper/data_raw/mapa_vot_gov_zm.rds")

mapa_vot_gov_zm %>%
  ggplot() + geom_sf(aes( fill= dois_mais),
                     colour= "gray20", size= .15,  show.legend = TRUE) +
  theme_void() +
  scale_fill_manual( name= "Candidatos mais Votados",
                     #label= c("Menos de 20%", "20% a 40%"),
                     values= c("#003A88","red2", "brown1")
                    ) #+
  #labs(title = "Eleições 2022: Presidentes e Gov mais Votados",
   #    subtitle = paste("Mais Votados por Município na Zona da Mata"))

#scale_fill_brewer()
#scale_fill_manual( name= "Proporção Vacinas:\nAplicadas/Disponibilizadas",
#                   label= c("Menos de 20%", "20% a 40%","40% a 60%","60% a 80%","80% a 100%", "Acima de 100%\nErros nos Dados"),
#                   values= c("red2","red4", "yellow", "limegreen", "green4", "gray30"))
#labs(title = "Eficiência na Vacinação: Os Municípios estão Aplicando as Vacinas que Recebem?", 
#     subtitle= "Nº de Vacinas enviadas aos Municípios dividido Pelas nº de Vacinas Aplicadas")



as_tibble(mapa_vot_gov_zm) %>%
  filter(dois_mais == "Lula e Zema") %>%
  select(nome_do_municipio, dois_mais,
         lula, jair_bolsonaro, zema, kalil) %>%
  arrange(desc(lula))


as_tibble(mapa_vot_gov_zm) %>%
  count(dois_mais, sort = TRUE)
