# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(jsonlite)

# Configuracao dos municipios ---------------------------------------------

urlm = "https://resultados.tse.jus.br/oficial/ele2022/544/config/mun-e000544-cm.json"

muni_tse <- fromJSON(urlm,
                     simplifyDataFrame = TRUE) %>%
  .[['abr']]  %>%
  unnest('mu', names_repair="universal") %>%
  select(-c, -z) %>%
  set_names("uf", "estado", "tse", "ibge7", "nm") %>%
    dplyr::filter(uf == "MG")

#votos_aptos_municipio_zm <- read_csv("scrapper/data_raw/votos_aptos_municipio_zm.csv")

#votos_aptos_municipio_zm$id_municipio_tse
# Por municipio -----------------------------------------------------------

#-- codigo do pleito
#pleito = 544 # 1t !! (usar esse para testar) !!
 pleito = 545 # 2t !! (usar esse no dia 30/10) !!
cargo = "0001" # presidente
arquivo = "-v"

#-- url das paginas que vamos usar
url_muni = paste0("https://resultados.tse.jus.br/oficial/ele2022/", # url padrao
                  pleito, # codigo do turno
                  "/dados/", # dados consolidados
                  str_to_lower(muni_tse$uf), # indentificacao do estado
                  "/", str_to_lower(muni_tse$uf), # identificacao do estado
                  muni_tse$tse, # codigo do tse do municipio
                  paste0("-c", cargo), # cargo
                  paste0("-e000", pleito), # pleito
                  arquivo, # tipo de arquivo
                  ".json")

# para evitar quedas do servidor do tse
rate <- rate_backoff(pause_base = 0.1, pause_min = 0.005, max_times = 100)

# funcao para rodar o fromJSON e ir avisando em que passo está
fs <- function(x,y) {
  x<-fromJSON(x, simplifyDataFrame = T)
  print(y)
  return(x)}

# rodar insistentemente e nao parar com erros
insistent <- insistently(fs,
                         rate, 
                         quiet = FALSE)

# map com a funcao criada. argumento y no imap é o iterando (1, 2, 3...)
municipios <- imap(url_muni, insistent)

# juntar todos os dados
dados <- municipios %>% 
  bind_rows() %>% 
  .[["abr"]] %>% 
  unnest(cand, names_repair="universal") %>%  
  filter(tpabr == "MU") |> 
  left_join(muni_tse, by=c("cdabr" = "tse")) %>% 
  mutate(votos = as.numeric(vap)) %>% 
  rename(ncand = n) %>%
  select(cdabr, ncand, votos)

#dado2s <- municipios %>% 
#  bind_rows() %>% 
#  .[["abr"]] %>% 
#  unnest(cand, names_repair="universal") %>%  
#  filter(tpabr == "MU") |> 
#  left_join(muni_tse, by=c("cdabr" = "tse")) %>% 
#  mutate(votos = as.numeric(vap),
#         #perc_tse = as.numeric(pvap)
#         ) %>% 
#  rename(ncand = n) #%>%
  #select(cdabr, uf, ncand, votos, pvap, nm)

dados%>% arrange(desc(votos))

dados_mg_wide <- dados %>%
  group_by(cdabr) %>% 
  mutate(prop = proportions(votos), # %>% (NaN, NA),
        # prop2= votos/ sum(votos, na.rm = TRUE)
        ) %>%
  pivot_wider(values_from = c(votos, prop),
              names_from = ncand) %>%
  mutate( ano = 2022,
    mais_votado = case_when(
    votos_13 == 0 & votos_13 == 0 ~ "Sem Urnas Apuradas",
    prop_13 > prop_22 ~ "Lula",
    prop_22 > prop_13 ~ "Bolsonaro",
    TRUE ~ "Erro"),
    dif_vts = votos_13 - votos_22,
    dif_prop = prop_13 - prop_22
    ) 


#saveRDS(dados, paste0("scrapper/data_raw/dados_pres_",
#                      Sys.time()%>% str_replace_all(" |:","_"),".rds")
#                    )


votos_aptos_municipio_zm <- read_csv("scrapper/data_raw/votos_aptos_municipio_zm.csv")

mapa_vot_gov_ZMAGORA <- readRDS("scrapper/data_raw/mapa_vot_gov_zm.rds") %>%
  select(!c(dois_mais: kalil))


votos_aptos_municipio_zm %>%
  mutate(cdabr= id_municipio_tse %>%
           as.character())

cod_ibge<- full_join(dados_mg_wide,
                     votos_aptos_municipio_zm %>%
                       mutate(cdabr= id_municipio_tse %>% as.character())
                     #,by=
                       ) 


# Visu --------------------------------------------------------------------


mapa_vot_2t_zm <-  left_join( x = cod_ibge,
                               y = dados_mg_wide)
                               #,by =  "id_municipio")


                              
mapa_vot_2t_zm_2 <- inner_join(x = mapa_vot_gov_ZMAGORA,
                           y = cod_ibge %>%
                             mutate(id_municipio = as.character(id_municipio))
)
                             #filter(nome_da_meso== "Zona da Mata") %>%
                          

mapa_vot_2t_zm_2 %>%
  ggplot() + geom_sf(aes( fill= mais_votado),
                     colour= "gray20", size= .15,  show.legend = TRUE) +
  theme_void() +
  scale_fill_manual( name= "2º Turno: Mais Votados",
  #                   #label= c("Menos de 20%", "20% a 40%"),
                     values= c("#003B87", "#FF5e5e", "grey70")
  ) #+


mapa_vot_2t_zm_2 %>% select(code_muni, name_muni, c(cdabr:geom)) %>% view()


#




