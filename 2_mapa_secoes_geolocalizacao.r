#Eleicoes 2022 - Mapa por bairro JF
#Autores:  Marcello (Count e Faxina de Dados TSE)
#           Arthur (precisao da Geolocalização das Urnas)

#Esse Codigo é sobre a Localicalização de cada Seção Eleitoral

library(tidyverse)
library(basedosdados)

# Importando da base dos Dados
basedosdados::set_billing_id("eleicoes2022jf")


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


# Escolhida melhor_urbano como coluna com menos pontos faltantes


tse_jf_corrigida<- tse_jf_select %>%
      mutate(col_compl= if_else(condition = is.na(melhor_urbano),
                                      false = melhor_urbano,
                                     true= google_relaxado),
             col_texto=  if_else(str_detect(col_compl,"POINT"),
                                 "deu bom",
                                 "faltando",
                                 missing = "n sei")
             )


teste<- tse_jf_completa %>%
  filter(is.na(melhor_urbano))

is.na(tse_jf_completa$ melhor_urbano)%>%
  table()

is.na(tse_jf_corrigida$col_compl)%>%
  table()
