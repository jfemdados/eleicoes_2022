#Eleicoes 2022 - Mapa por bairro JF
#Autores:  Marcello (Count e Faxina de Dados TSE)

#Objetivo deste Código é Comparar com 2022 x 2018


library(tidyverse)



# Importing (2018) - Base dos Dados  --------------------------------------


library(basedosdados)

# Defina o seu projeto no Google Cloud
basedosdados::set_billing_id("eleicoes2022jf")


### Resultados Presidente por Município 

bd_18_2t_result_pres_query <- basedosdados::bdplyr("br_tse_eleicoes.resultados_candidato_municipio") %>%
  dplyr::filter(ano == 2018 & sigla_uf == "MG" & turno == 2 
                & str_detect(cargo,"presidente")
  )


bd_18_2t_result_pres <- basedosdados::bd_collect(bd_18_2t_result_pres_query)

### Detalhes dos Candidatos - Nomes
bd_18_2t_cand_pres_query <- basedosdados::bdplyr("br_tse_eleicoes.candidatos") %>%
  dplyr::filter(ano %in% c(2018, 2022)
                & cargo == "presidente") %>% 
  dplyr::select(ano, tipo_eleicao,
                id_candidato_bd, sequencial, nome_urna, cargo)

bd_18_2t_cand_pres <- basedosdados::bd_collect(bd_18_2t_cand_pres_query)


# Detalhes dos Candidatos - Número de Aptos, Brancos e Etc.

bd_18_2t_det_pres_query <- basedosdados::bdplyr("br_tse_eleicoes.detalhes_votacao_municipio") %>%
  dplyr::filter(ano == 2018 & sigla_uf == "MG" & turno == 2 
                & str_detect(cargo,"presidente")
  ) 


bd_18_2t_det_pres <- basedosdados::bd_collect(bd_18_2t_det_pres_query) 




# 2018 - Joining as Bases da BD ------------------------------------------



pres_18_result_name_join<- inner_join(bd_18_2t_result_pres,
                                     bd_18_2t_cand_pres %>%
                                   rename( "sequencial_candidato" = "sequencial")
                                 #,
                                 #  by = c("ano", "tipo_eleicao", "cargo", "id_candidato_bd"
                                 #        , "sequencial_candidato") 
                                 ) %>%
                      select( !c(tipo_eleicao, sequencial_candidato))



# Número de Município bate
pres_18_result_name_join %>% 
  filter(nome_urna == "Jair Bolsonaro") %>%
  count(nome_urna)

# Temos uma Linha por Cidade e por Candidato
pres_18_result_name_join %>%
  count(id_municipio_tse, nome_urna)




# Importing (2022) - Dados do Scrapper por Município ----------------------


dados_scrapper <- municipios %>% 
  bind_rows() %>% 
  .[["abr"]] %>% 
  unnest(cand, names_repair="universal") %>%  
  filter(tpabr == "MU") |> 
  left_join(muni_tse, by=c("cdabr" = "tse")) %>% 
  mutate(votos = as.numeric(vap)) %>% 
  rename(ncand = n) %>%
  select(uf, cdabr, ibge7, ncand, votos, pvap, nm)


pres_22_2t <- tibble(ano= 2022,
                     turno= 2,
                     dados_scrapper) %>%
            rename("id_municipio_tse" = cdabr,
                   "numero_candidato" = ncand,
                   "sigla_uf" = uf,
                   "id_municipio" = ibge7 ) %>%
  #Joguei o Número do Bolsonaro pra 17 pra não dar problema, mesmo sendo 22
  mutate(numero_candidato = str_replace(numero_candidato,"22","17")) 



# Joinning dados 2018 - 2022  ----------------------------------------------


pres2t_comp_18_22 <- full_join( pres_18_result_name_join %>%
                        #tive que tirar todos as colunas que remetiam à identificação do candidato
                        #pq o pivot wider estava dando erro
                        select(!c(sigla_partido,
                                  numero_partido,
                                  id_candidato_bd,
                                  resultado,
                                  nome_urna
                                  )),
                        pres_22_2t 
                       #, by= c("id_municipio_tse")
                       ) %>%
  tidyr::fill(cargo, .direction = "down") %>%
  group_by(id_municipio_tse) %>%
  tidyr::fill(nm, .direction = "up")

# Wide para Ano

pres2t_comp_18_22_wide<- pres2t_comp_18_22 %>%
  ungroup() %>%
  select(!c(nm,pvap)) %>%
  group_by(id_municipio, ano) %>%
  mutate(prop = proportions(votos)*100 ) %>%
  pivot_wider(names_from = c(numero_candidato,ano),
              values_from = c(votos, prop))

#iria fazer Wide para  fazer mais coisas, mas ia ficar imenso...
# Abandonei e fui fazer um mapa por ano...

#pres2t_comp_18_22_wide_2<-

  pres2t_comp_18_22_wide  %>%
  summarise(vt_13_1822 = votos_13_2022 - votos_13_2018,
         vt_17_1822 = votos_17_2022 - votos_17_2018,
         p_13_1822 = prop_13_2022 - prop_13_2018,
         p_17_1822 = prop_17_2022 - prop_17_2018,
         dif_p_1317_22 = prop_13_2022 - prop_17_2022,
         dif_p_1317_18 = prop_13_2018 - prop_17_2018) #%>%view()

  

# Visualização em Mapas ----------------------------------------------------

library(geobr)
library(ggplot2)
library(sf)

#Shapefile Inicial -  TODOS dos Municípios em MG

shp_munic_mg <- geobr::read_municipality(code_muni = "MG")

# Detalhamento Classificação dos Municípios pelo Ibge
base_ibge_mg <- readRDS("mapas/base_ibge_mg.rds")


shp_regioes_mg <- inner_join( shp_munic_mg, # ShapeFile dos Municipios de MG
                               base_ibge_mg, # Divisão/Classificação dos Municípios em Regiões
                               by= "code_muni")# %>%


#Filtre por qualquer uma das zonas para ter um mapa regional
shp_regioes_mg %>%
  filter(nome_da_meso== "Zona da Mata") %>%
  ggplot() + geom_sf(aes( fill= nome_da_meso ),
                     size= .15, show.legend = TRUE)


# Mapa só de 2018 -----------------


pres2t_18_wide <- pres2t_comp_18_22 %>%
  ungroup() %>%
  #Filtrando no ano em em 2018
  filter( ano == 2018) %>%
  select(!c(nm,pvap)) %>%
  group_by(id_municipio) %>%
  mutate(prop = proportions(votos)*100) %>% 
  pivot_wider(names_from = c(numero_candidato),
              values_from = c(votos, prop)) %>%
  mutate( dif_vts = votos_13 - votos_17,
          dif_prop = prop_13 - prop_17,
          niveis_prop_13 = cut(prop_13, #Proporção ao PT
                           breaks = c(0,40,50,60,100),
                           labels = c("Bolsonaro: Mais de 60%","Bolsonaro: Entre 40%-50%",
                                      "Haddad: Entre 40%-50%", "Haddad: Mais de 60%" )
        )
  ) 


shpdf_mg_2018_2t <- inner_join(shp_regioes_mg #%>%
                                     #filter(nome_da_meso== "Zona da Mata") %>% #acho que vou fazer no mapa
                                   , pres2t_18_wide %>%
                                 mutate(code_muni = as.numeric(id_municipio))
                               )





shpdf_mg_2018_2t %>%
  #filter(nome_da_meso== "Zona da Mata") %>% 
  ggplot() + geom_sf(aes( fill= niveis_prop_13),
                     colour= "gray20", size= .15,  show.legend = TRUE) +
  theme_void() +  labs( title =  "Eleições Presidenciais 2º Turno - Ano 2018" ) +
  scale_fill_manual( name= "Percentual de Votos em 2018",
                  # label=  c("Bolsonaro: Mais de 60%","Bolsonaro: Entre 40%-50%",
#"Haddad: Entre 40%-50%", "Haddad: Mais de 60%" ),
                values= c(  "#003B87", "#478FED", "#FF5e5e", "red2")
                    )





# Mapa 2022 ------------------


pres2t_22_wide <- pres2t_comp_18_22 %>%
  ungroup() %>%
  #Filtrando no ano em em 2018
  filter( ano == 2022) %>%
  select(!c(nm,pvap)) %>%
  group_by(id_municipio) %>%
  mutate(prop = proportions(votos)*100) %>% 
  pivot_wider(names_from = c(numero_candidato),
              values_from = c(votos, prop)) %>%
  mutate( dif_vts = votos_13 - votos_17,
          dif_prop = prop_13 - prop_17,
          niveis_prop_13 = cut(prop_13, #Proporção ao PT
                           breaks = c(0,40,50,60,100),
                           labels = c("Bolsonaro: Mais de 60%","Bolsonaro: Entre 40%-50%",
                                      "Lula: Entre 40%-50%", "Lula: Mais de 60%" )
                                )
          ) 


shpdf_mg_2022_2t <- inner_join(shp_regioes_mg,
                                  pres2t_22_wide%>%
                                 mutate(code_muni = as.numeric(id_municipio))
                               ) #%>%
                #select(!c(cod_meso:n, code_state, abbrev_state, sigla_uf:cargo)) # %>% view()




shpdf_mg_2022_2t %>%
    #filter(nome_da_meso== "Zona da Mata") %>% 
  ggplot() + geom_sf(aes( fill= niveis_prop_13),
                     colour= "gray20", size= .15,  show.legend = TRUE) +
  theme_void() +
  scale_fill_manual( name= "Percentual de Votos em 2022",
                     #label=  c("Bolsonaro: Mais de 60%","Bolsonaro: Entre 40%-50%",
                      #         "Lula: Entre 40%-50%", "Lula: Mais de 60%" ),
                     values= c(  "#003B87", "#478FED", "#FF5e5e", "red2")
                      ) +  labs( title =  "Eleições Presidenciais 2º Turno - Ano 2018" ,
                                         caption = "Fonte: TSE")

