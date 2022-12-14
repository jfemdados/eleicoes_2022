#Eleicoes 2022 - Mapa por bairro JF
#Autores:  Marcello (Count e Faxina de Dados TSE)
#           Arthur (precisao da Geolocaliza??o das Urnas)

library(tidyverse)

# Esse Script pega apenas os votos para Presidente.
# Os votos para Governador e cargos do Legislativo est?o em "eleicoes_2022_tse_dados_abertos_mg.R".


# Importing ---------------------------------------------------------------

# Esse DF est? disponi?vel para download no site oficial do TSE
# Escolha "BR - Vota??o por se??o eleitoral - 2022" dentro de https://dadosabertos.tse.jus.br/dataset/resultados-2022"

votacao_secao_2022_br_raw <- readr::read_delim("tse_dados_abertos/data_raw/votacao_secao_2022_BR.csv", 
                                                   delim = ";",
                                                   escape_double = FALSE,
                                                   trim_ws = TRUE,
                                                   locale = locale(encoding = "latin1")
                                               #,col_select = !c("DT_GERACAO","HH_GERACAO","TP_ABRANGENCIA",
                                                #                   )
                                                )



votacao_secao_2022_jf_presid<- votacao_secao_2022_br_raw %>%
  janitor::clean_names() %>%
  filter(str_detect( nm_municipio, "JUIZ DE FORA")) 




# Checando a Base ---------------------------------------------------------

#C?meros pra ver se a base est? correta 

# Comparando Bases

as_tibble(votacao_secao_2022_br_raw)

as_tibble(votacao_secao_2022_jf_presid)

#comparando bases- Counts

votacao_secao_2022_jf_presid %>%
  count(ds_cargo)


votacao_secao_2022_br_raw %>%
  count(TP_ABRANGENCIA)


#Counts

votacao_secao_2022_jf_presid %>%
  filter(ds_cargo == "PRESIDENTE") %>% 
    dplyr::group_by(nm_votavel) %>%
    dplyr::tally(qt_votos, sort = TRUE) %>%
    mutate(votos_prop= proportions(n))

# Resultados bateram com votos totais
#(percentual votos ao total, n?o votos v?lidos )


votacao_secao_2022_jf_presid %>% 
  filter(ds_cargo == "PRESIDENTE") %>% 
  count(sq_candidato, nm_votavel)
#colunas com infos Iguais


votacao_secao_2022_jf_presid %>% 
  filter(ds_cargo == "PRESIDENTE") %>%
  count(nr_secao)

# A partir daqui podemos ver a necessidade de faxina

# Aparecem Se??es Duplicadas mesmo agrupando
# porque temos uma linha para cada candidato em cada se??o


votacao_secao_2022_jf_presid %>%
 # group_by(nr_zona) %>%
  count(nr_zona, nr_secao)


# Tidying -----------------------------------------------------------------

votacao_secao_2022_jf_presid_wide <- votacao_secao_2022_jf_presid %>%
  # selecionando colunas que eu quero pra diminuir info
  dplyr::select(ano_eleicao, dt_eleicao, nm_tipo_eleicao, nr_turno, ds_eleicao,
             sg_uf, cd_municipio, nm_municipio, nr_zona, nr_secao , nr_local_votacao,
             #nr_votavel, sq_candidato, #info dos candidatos comprometem pivot
             ds_cargo, nm_votavel, qt_votos ) %>%
  #Pivotando para que 1 linha = 1 se??o
  tidyr::pivot_wider(names_from = nm_votavel,
                     values_from = qt_votos) %>%
  janitor::clean_names() %>%
  #botar os maiores na frente
  dplyr::relocate(.after = ds_cargo,
                  luiz_inacio_lula_da_silva,
                  jair_messias_bolsonaro,
                  simone_nassar_tebet,
                  ciro_ferreira_gomes,
                  voto_branco,
                  voto_nulo)




# Checando se a base est? boa ----------------------------------------------------------------



#N?o temos urnas sem valores...
is.na(votacao_secao_2022_jf_presid_wide $ jair_messias_bolsonaro)%>% 
  table()
   
#Se quiser checar na m?o...
votacao_secao_2022_jf_presid_wide %>%
  select( nr_zona, nr_secao, nr_local_votacao,
          jair_messias_bolsonaro, luiz_inacio_lula_da_silva) #%>% view()


#Mass Mass temos um problema 

#Mas temos 5 se??es que tem mesmo local de vota??o
#podem estar em zonas diferentes mas mesmo lugar

votacao_secao_2022_jf_presid_wide %>%
  count( nr_secao,
         nr_local_votacao, sort= TRUE) 
#Temos mesmo n? de Se??es em Diferentes Zonas
  votacao_secao_2022_jf_presid_wide %>%
    count( nr_secao,
           sort= TRUE) %>% view

# N?o temos duplicidade Dividindo por Zona, se??o e Local de Vota??o 

   votacao_secao_2022_jf_presid_wide %>%
    count( nr_zona, nr_secao,
           nr_local_votacao,
           sort= TRUE) 



# Exporting ---------------------------------------------------------------
votacao_secao_2022_br_raw

saveRDS(votacao_secao_2022_br_raw, "tse_dados_abertos/data_raw/votacao_secao_2022_BR.rds")
  
t<- readRDS("tse_dados_abertos/data_raw/votacao_secao_2022_BR.rds")


