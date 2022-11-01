#Eleicoes 2022 - Mapa por bairro JF
#Autores:  Marcello (Count e Faxina de Dados TSE)


library(tidyverse)
library(basedosdados)

#Comparação com 2022

# Importing da Base dos Dados ---------------------------------------------

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




# Pós BD- Joining ----------------------------------------------------------



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


pres_18_result_name_join %>% 
  #filter(nome_urna == "Jair Bolsonaro") %>%
  count(id_municipio_tse)

#%>%
#  mutate(chapa=case_when(
#    numero_candidato == "13" ~ "pt",
#    numero_candidato == "22" ~ "bozo",
#  )


#Dados do scrp

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
  mutate(numero_candidato = str_replace(numero_candidato,"22","17")) 


pres_18_result_name_join %>% 
  select(id_municipio_tse, id_municipio) 

  
pres_22_2t %>% 
  #filter(nome_urna == "Jair Bolsonaro") %>%
  count(id_municipio_tse)


pres_18_result_name_join %>% 
  #filter(nome_urna == "Jair Bolsonaro") %>%
  count(id_municipio_tse)


# Joiinning daados 2018 - 2022

dados2t_18_22_full <- full_join( pres_18_result_name_join %>%
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


dados2t_18_22_wide<- dados2t_18_22_full %>%
  ungroup() %>%
  select(!c(nm,pvap)) %>%
  group_by(id_municipio, ano) %>%
  mutate(prop = proportions(votos)*100) %>% 
  pivot_wider(names_from = c(numero_candidato,ano),
              values_from = c(votos, prop))

#dados2t_18_22_wide_2<-

  dados2t_18_22_wide  %>%
  summarise(vt_13_1822 = votos_13_2022 - votos_13_2018,
         vt_17_1822 = votos_17_2022 - votos_17_2018,
         p_13_1822 = prop_13_2022 - prop_13_2018,
         p_17_1822 = prop_17_2022 - prop_17_2018,
         dif_p_1317_22 = prop_13_2022 - prop_17_2022,
         dif_p_1317_18 = prop_13_2018 - prop_17_2018) %>%view()


dados2t_18_22_full %>%
  ungroup() %>%
  group_by(nm, id_municipio, ano, numero_candidato) %>%
  summarise(voto = cur_group_rows()
            diff = diff)
  n()

  

# Visualização -------------------------------------------------------------

library(geobr)
library(ggplot2)
library(sf)


  shp_vot_gov_ZMAGORA <- readRDS("scrapper/data_raw/mapa_vot_gov_zm.rds") %>%
    select(!c(dois_mais: kalil)) %>%
    select( code_muni, name_muni, nome_da_meso, nome_do_municipio,
            nome_do_municipio, id_municipio, geom)

  dados2t_18_wide <- dados2t_18_22_full %>%
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
            dif_niveis = cut(prop_13, #Proporção do PT
                             breaks = c(0,40,50,60,100),
                             labels = c("Bolsonaro: Mais de 60%","Bolsonaro: Entre 40%-50%",
                                        "Haddad: Entre 40%-50%", "Haddad: Mais de 60%" )
          )
    ) 


mapa_zona_mata_2018_2t<-inner_join(shp_vot_gov_ZMAGORA,
                          dados2t_18_wide)





mapa_zona_mata_2018_2t %>%
  ggplot() + geom_sf(aes( fill= dif_niveis),
                     colour= "gray20", size= .15,  show.legend = TRUE) +
  theme_void() +
  scale_fill_manual( name= "Votos em 2018",
                   label=  c("Bolsonaro: Mais de 60%","Bolsonaro: Entre 40%-50%",
"Haddad: Entre 40%-50%", "Haddad: Mais de 60%" ),
                   values= c(  "#003B87", "#478FED", "#FF5e5e", "red2")
                   )  





# Mapa 2022 ------------------


dados2t_22_wide <- dados2t_18_22_full %>%
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
          dif_niveis = cut(prop_13, #Proporção do PT
                           breaks = c(0,40,50,60,100),
                           labels = c("Bolsonaro: Mais de 60%","Bolsonaro: Entre 40%-50%",
                                      "Lula: Entre 40%-50%", "Lula: Mais de 60%" )
          )
  ) 


mapa_zona_mata_2022_2t<-inner_join(shp_vot_gov_ZMAGORA,
                                   dados2t_22_wide)





mapa_zona_mata_2022_2t %>%
  ggplot() + geom_sf(aes( fill= dif_niveis),
                     colour= "gray20", size= .15,  show.legend = TRUE) +
  theme_void() +
  scale_fill_manual( name= "Votos em 2022",
                     label=  c("Bolsonaro: Mais de 60%","Bolsonaro: Entre 40%-50%",
                               "Lula: Entre 40%-50%", "Lula: Mais de 60%" ),
                     values= c(  "#003B87", "#478FED", "#FF5e5e", "red2")
  )  

