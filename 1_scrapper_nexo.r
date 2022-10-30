# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(jsonlite)


# Configurar --------------------------------------------------------------

#-- codigo pleito
pleito <- 544 # 1t !! (usar esse para teste) !!
# pleito = 545 # 2t !! (usar esse no dia 30/10) !!

#-- url
url <- paste0("https://resultados.tse.jus.br/oficial/ele2022/", pleito,
             "/dados-simplificados/br/br-c0001-e000", pleito, "-r.json")

pres_br <-  fromJSON(url, simplifyDataFrame = TRUE) 

#-- votacao
votos <- pres_br %>%
  .[["cand"]] %>% 
  tbl_df() %>% 
  mutate(voto=as.numeric(vap),
         pct=voto/sum(voto, na.rm=T)) %>% 
  select(n, nm, pvap, voto) %>% 
  mutate(pvap=paste0(pvap, "%"))
# total comparecimento
votos_totais <- pres_br %>% .[["c"]] %>% as.numeric()
# validos
votos_validos <- pres_br %>% .[["vvc"]] %>% as.numeric()
# brancos nulos
votos_nulos <- pres_br %>%  .[["tvn"]] %>% as.numeric()
# validos
votos_brancos <- pres_br %>% .[["vb"]] %>%  as.numeric()
#abs
abstencoes <- pres_br %>% .[["a"]] %>%  as.numeric()
#apurado
eleitorado_apurado <- pres_br %>% .[["pea"]] %>% paste0(., "%")

#-- votos
votos
votos_totais
votos_validos 
votos_nulos
votos_brancos
abstencoes
eleitorado_apurado
