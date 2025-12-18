
#Banco ReDem

library(haven)

INCT_ReDem <- read_sav("INCT_ReDem.sav")

###Polarizados

#Lulistas
INCT_ReDem$lulista <- (ifelse(INCT_ReDem$P60 > 10,NA,INCT_ReDem$P60)) - 5

#Bolsonaristas
INCT_ReDem$bolsonarista <- (ifelse(INCT_ReDem$P61 > 10,NA,INCT_ReDem$P61)) - 5

###Autoritarios e Democratas

#Minorias

#Self

INCT_ReDem$minorias <- ifelse(INCT_ReDem$P15 > 10,NA,INCT_ReDem$P15)
INCT_ReDem$eleicoes <- 10 - ifelse(INCT_ReDem$P46 > 10,NA,INCT_ReDem$P46)
INCT_ReDem$judiciario <- ifelse(INCT_ReDem$P49 > 10,NA,INCT_ReDem$P49)

#Projecao

#Lula
INCT_ReDem$minorias_lula <- ifelse(INCT_ReDem$P131 > 10,NA,INCT_ReDem$P131)
INCT_ReDem$eleicoes_lula <- 10 - ifelse(INCT_ReDem$P132 > 10,NA,INCT_ReDem$P132)
INCT_ReDem$judiciario_lula <- ifelse(INCT_ReDem$P133 > 10,NA,INCT_ReDem$P133)

#Bolsonaro
INCT_ReDem$minorias_bolsonaro <- ifelse(INCT_ReDem$P134 > 10,NA,INCT_ReDem$P134)
INCT_ReDem$eleicoes_bolsonaro <- 10 - ifelse(INCT_ReDem$P135 > 10,NA,INCT_ReDem$P135)
INCT_ReDem$judiciario_bolsonaro <- ifelse(INCT_ReDem$P136 > 10,NA,INCT_ReDem$P136)

#Graficos descritivos projecao

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

plot_compare_dist <- function(data, var_lula, var_bolsonaro, xlab = "", title = "") {
  
  df_long <- data %>%
    transmute(
      Lula = .data[[var_lula]],
      Bolsonaro = .data[[var_bolsonaro]]
    ) %>%
    pivot_longer(cols = c("Lula", "Bolsonaro"),
                 names_to = "lider",
                 values_to = "valor") %>%
    filter(!is.na(valor))
  
  stats <- df_long %>%
    group_by(lider) %>%
    summarise(
      media = mean(valor, na.rm = TRUE),
      dp    = sd(valor, na.rm = TRUE),
      .groups = "drop"
    )
  
  stats <- stats %>%
    mutate(
      dir = ifelse(lider == "Lula", 1, -1),   
      hjust_txt = ifelse(lider == "Lula", 0, 1)
    )
  
  max_count <- ggplot_build(
    ggplot(df_long, aes(x = valor)) +
      geom_histogram(binwidth = 1)
  )$data[[1]]$count |> max(na.rm = TRUE)
  
  y_text <- 0.95 * max_count
  
  
  cores <- c(
    "Lula" = "darkred",       
    "Bolsonaro" = "darkgreen"  
  )
  
  ggplot(df_long, aes(x = valor, fill = lider, color = lider)) +
    
    #Histogramas
    geom_histogram(
      binwidth = 1,
      position = "identity",
      alpha = 0.25,          
      linewidth = 0.2
    ) +
    
    #Média
    geom_vline(
      data = stats,
      aes(xintercept = media, color = lider),
      linetype = "solid",
      linewidth = 0.9,
      show.legend = FALSE
    ) +
    
    # -1 DP
    geom_vline(
      data = stats,
      aes(xintercept = media - dp, color = lider),
      linetype = "dotted",
      linewidth = 0.85,
      show.legend = FALSE
    ) +
    
    # +1 DP
    geom_vline(
      data = stats,
      aes(xintercept = media + dp, color = lider),
      linetype = "dotted",
      linewidth = 0.85,
      show.legend = FALSE
    ) +
    
    #Textos
    geom_text(
      data = stats,
      aes(
        x = media + dir * 0.4,
        y = y_text,
        label = "Média",
        color = lider,
        hjust = hjust_txt
      ),
      size = 3.5,
      vjust = 1.5,
      show.legend = FALSE
    ) +
    geom_text(
      data = stats,
      aes(
        x = media + dp + dir * 0.4,
        y = y_text,
        label = "+1 DP",
        color = lider,
        hjust = hjust_txt
      ),
      size = 3.5,
      vjust = 1.5,
      show.legend = FALSE
    ) +
    geom_text(
      data = stats,
      aes(
        x = media - dp + dir * 0.4,
        y = y_text,
        label = "-1 DP",
        color = lider,
        hjust = hjust_txt
      ),
      size = 3.5,
      vjust = 1.5,
      show.legend = FALSE
    ) +
    
    labs(
      x = xlab,
      y = "Frequência",
      title = title
    ) +
    
    scale_fill_manual(values = cores) +
    scale_color_manual(values = cores) +
    
    theme_bw(base_size = 12) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25),
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8))
    )
}

#Gerando os 3 graficos

p_minorias <- plot_compare_dist(
  INCT_ReDem,
  var_lula = "minorias_lula",
  var_bolsonaro = "minorias_bolsonaro",
  xlab = "Que a maioria sempre se sobreponha a minoria",
  title = ""
)

p_eleicoes <- plot_compare_dist(
  INCT_ReDem,
  var_lula = "eleicoes_lula",
  var_bolsonaro = "eleicoes_bolsonaro",
  xlab = "Que sejam respeitados os resultados das eleições,\nnão importa qual candidato vença (Invertida)",
  title = ""
)

p_judiciario <- plot_compare_dist(
  INCT_ReDem,
  var_lula = "judiciario_lula",
  var_bolsonaro = "judiciario_bolsonaro",
  xlab = "Que o presidente seja capaz de ignorar as decisões judiciais que\nsejam consideradas politicamente tendenciosas",
  title = ""
)

p_minorias
p_eleicoes
p_judiciario

ggsave("minorias.png", p_minorias, width = 12, height = 7, dpi = 300)
ggsave("eleicoes.png", p_eleicoes, width = 12, height = 7, dpi = 300)
ggsave("judiciario.png", p_judiciario, width = 12, height = 7, dpi = 300)

#Proxy de ideologia do regime

INCT_ReDem$ABORTO <- 10 - (ifelse(INCT_ReDem$P130 > 10,NA,INCT_ReDem$P130))

#Percepcao de ameaca grupo

INCT_ReDem$AMEACA <- as.factor(ifelse(INCT_ReDem$P125 > 4,NA,INCT_ReDem$P125))
INCT_ReDem$AMEACA <- relevel(INCT_ReDem$AMEACA, ref = "4")

#Controles

INCT_ReDem$REGIAO <- as.factor(INCT_ReDem$REGIAO)
INCT_ReDem$REGIAO <- relevel(INCT_ReDem$REGIAO, ref = "3")

INCT_ReDem$PORTE

INCT_ReDem$IDADE_EX

INCT_ReDem$SEXO

INCT_ReDem$RACA <- ifelse(INCT_ReDem$RACA == 1,1,0)

INCT_ReDem$ESCOLARIDADE

INCT_ReDem$RENDA <- ifelse(INCT_ReDem$RENDA_1 > 6,NA,INCT_ReDem$RENDA_1)

#Interacoes

#Minorias

#Visao Lula

summary(lm(minorias_lula ~ minorias*(lulista + bolsonarista)+
             REGIAO+PORTE+IDADE_EX+SEXO+RACA+ESCOLARIDADE+
             RENDA+ABORTO+AMEACA,
           data = INCT_ReDem))

#Visao Bolsonaro

summary(lm(minorias_bolsonaro ~ minorias*(lulista + bolsonarista)+
             REGIAO+PORTE+IDADE_EX+SEXO+RACA+ESCOLARIDADE+
             RENDA+ABORTO+AMEACA,
           data = INCT_ReDem))

#Eleicoes

#Visao Lula

summary(lm(eleicoes_lula ~ eleicoes*(lulista + bolsonarista)+
             REGIAO+PORTE+IDADE_EX+SEXO+RACA+ESCOLARIDADE+
             RENDA+ABORTO+AMEACA,
           data = INCT_ReDem))

#Visao Bolsonaro

summary(lm(eleicoes_bolsonaro ~ eleicoes*(lulista + bolsonarista)+
             REGIAO+PORTE+IDADE_EX+SEXO+RACA+ESCOLARIDADE+
             RENDA+ABORTO+AMEACA,
           data = INCT_ReDem))

#Judiciario

#Visao Lula

summary(lm(judiciario_lula ~ judiciario*(lulista + bolsonarista)+
             REGIAO+PORTE+IDADE_EX+SEXO+RACA+ESCOLARIDADE+
             RENDA+ABORTO+AMEACA,
           data = INCT_ReDem))

#Visao Bolsonaro

summary(lm(judiciario_bolsonaro ~ judiciario*(lulista + bolsonarista)+
             REGIAO+PORTE+IDADE_EX+SEXO+RACA+ESCOLARIDADE+
             RENDA+ABORTO+AMEACA,
           data = INCT_ReDem))

#Graficos

library(dplyr)
library(broom)
library(ggplot2)
library(stringr)
library(forcats)
library(patchwork)

#Funcao para extrair os coeficientes, ICs e significancia

tidy_lm_std <- function(model, model_name){
  
  td <- broom::tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
    mutate(
      model = model_name,
      signif = p.value < 0.05
    )
  
  mf <- model.frame(model)
  y  <- model.response(mf)
  X  <- model.matrix(model)
  
  sd_y <- sd(y, na.rm = TRUE)
  
   sd_x <- apply(X, 2, sd, na.rm = TRUE)
  
  #tabela de fatores de padronização por termo/coluna
  scale_df <- tibble::tibble(
    term = names(sd_x),
    scale = as.numeric(sd_x) / sd_y
  )
  
  #junta e calcula estimativas/IC padronizados
  td %>%
    left_join(scale_df, by = "term") %>%
    mutate(
      estimate_std  = estimate  * scale,
      conf.low_std  = conf.low  * scale,
      conf.high_std = conf.high * scale
    )
}

#Modelos

m_min_lula <- lm(minorias_lula ~ minorias*(lulista + bolsonarista)+
                   REGIAO+PORTE+IDADE_EX+SEXO+RACA+ESCOLARIDADE+
                   RENDA+ABORTO+AMEACA,
                 data = INCT_ReDem)

m_min_bol <- lm(minorias_bolsonaro ~ minorias*(lulista + bolsonarista)+
                  REGIAO+PORTE+IDADE_EX+SEXO+RACA+ESCOLARIDADE+
                  RENDA+ABORTO+AMEACA,
                data = INCT_ReDem)

m_ele_lula <- lm(eleicoes_lula ~ eleicoes*(lulista + bolsonarista)+
                   REGIAO+PORTE+IDADE_EX+SEXO+RACA+ESCOLARIDADE+
                   RENDA+ABORTO+AMEACA,
                 data = INCT_ReDem)

m_ele_bol <- lm(eleicoes_bolsonaro ~ eleicoes*(lulista + bolsonarista)+
                  REGIAO+PORTE+IDADE_EX+SEXO+RACA+ESCOLARIDADE+
                  RENDA+ABORTO+AMEACA,
                data = INCT_ReDem)

m_jud_lula <- lm(judiciario_lula ~ judiciario*(lulista + bolsonarista)+
                   REGIAO+PORTE+IDADE_EX+SEXO+RACA+ESCOLARIDADE+
                   RENDA+ABORTO+AMEACA,
                 data = INCT_ReDem)

m_jud_bol <- lm(judiciario_bolsonaro ~ judiciario*(lulista + bolsonarista)+
                  REGIAO+PORTE+IDADE_EX+SEXO+RACA+ESCOLARIDADE+
                  RENDA+ABORTO+AMEACA,
                data = INCT_ReDem)

#Tidy geral

df_all <- bind_rows(
  tidy_lm_std(m_min_lula, "Minorias — Visão Lula"),
  tidy_lm_std(m_min_bol,  "Minorias — Visão Bolsonaro"),
  tidy_lm_std(m_ele_lula, "Eleições — Visão Lula"),
  tidy_lm_std(m_ele_bol,  "Eleições — Visão Bolsonaro"),
  tidy_lm_std(m_jud_lula, "Judiciário — Visão Lula"),
  tidy_lm_std(m_jud_bol,  "Judiciário — Visão Bolsonaro")
) %>%
  filter(term != "(Intercept)")

label_map <- c(
  "minorias" = "Nem-nem autoritário",
  "eleicoes" = "Nem-nem autoritário",
  "judiciario" = "Nem-nem autoritário",
  
  "lulista" = "Lulista polarizado e democrata",
  "bolsonarista" = "Bolsonarista polarizado e democrata",
  
  "minorias:lulista" = "Lulista autoritário",
  "minorias:bolsonarista" = "Bolsonarista autoritário",
  
  "eleicoes:lulista" = "Lulista autoritário",
  "eleicoes:bolsonarista" = "Bolsonarista autoritário",
  
  "judiciario:lulista" = "Lulista autoritário",
  "judiciario:bolsonarista" = "Bolsonarista autoritário",
  
  "REGIAO1" = "Norte",
  "REGIAO2" = "Nordeste",
  "REGIAO4" = "Sul",
  "REGIAO5" = "Centro-Oeste",
  "PORTE" = "Porte do município",
  "IDADE_EX" = "Idade",
  "SEXO" = "Mulher",
  "RACA" = "Branco",
  "ESCOLARIDADE" = "Escolaridade",
  "RENDA" = "Renda",
  "ABORTO" = "Atitude conservadora sobre o aborto",
  "AMEACA1" = "Bolsonaristas como ameaça",
  "AMEACA2" = "Lulistas como ameaça",
  "AMEACA3" = "Ambos como ameaça"
)

make_term_levels <- function(df, key){
  natural <- df$term %>% unique()
  
  bloco <- c(
    key,
    "lulista", paste0(key, ":lulista"),
    "bolsonarista", paste0(key, ":bolsonarista")
  )
  
  bloco <- bloco[bloco %in% natural]
  
  rest  <- setdiff(natural, bloco)
  
  c(bloco, rest)
}

#Funcao de grafico

plot_forest <- function(df, title, key){
  
  term_levels <- make_term_levels(df, key)
  
  df %>%
    mutate(
      term = factor(term, levels = term_levels),
      term_label = dplyr::recode(as.character(term), !!!label_map, .default = as.character(term)),
      term_label = factor(term_label, levels = dplyr::recode(term_levels, !!!label_map, .default = term_levels))
    ) %>%
    ggplot(aes(
      x = estimate_std,
      y = term_label,
      color = signif
    )) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    geom_errorbarh(
      aes(xmin = conf.low_std, xmax = conf.high_std),
      height = 0,
      linewidth = 0.7
    ) +
    geom_point(size = 2.5) +
    scale_color_manual(
      values = c("TRUE" = "red3", "FALSE" = "black"),
      labels = c("FALSE" = "Não significativo", "TRUE" = "p < 0,05"),
      name = NULL
    ) +
    labs(
      x = "Coeficiente padronizado (1 DP) — IC 95%",
      y = NULL,
      title = title
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

#Arranjo de 1 grafico por questao

only_minorias <- df_all %>% filter(str_detect(model, "^Minorias"))
only_eleicoes <- df_all %>% filter(str_detect(model, "^Eleições"))
only_jud      <- df_all %>% filter(str_detect(model, "^Judiciário"))

#Minorias
p_min_lula <- plot_forest(
  only_minorias %>% filter(str_detect(model, "Lula$")),
  "Visão de Lula autoritário",
  key = "minorias"
)

p_min_bol <- plot_forest(
  only_minorias %>% filter(str_detect(model, "Bolsonaro$")),
  "Visão de Bolsonaro autoritário",
  key = "minorias"
)

fig_minorias <- p_min_lula + p_min_bol + plot_layout(ncol = 2)

#Eleições
p_ele_lula <- plot_forest(
  only_eleicoes %>% filter(str_detect(model, "Lula$")),
  "Visão de Lula autoritário",
  key = "eleicoes"
)

p_ele_bol <- plot_forest(
  only_eleicoes %>% filter(str_detect(model, "Bolsonaro$")),
  "Visão de Bolsonaro autoritário",
  key = "eleicoes"
)

fig_eleicoes <- p_ele_lula + p_ele_bol + plot_layout(ncol = 2)

#Judiciário
p_jud_lula <- plot_forest(
  only_jud %>% filter(str_detect(model, "Lula$")),
  "Visão de Lula autoritário",
  key = "judiciario"
)

p_jud_bol <- plot_forest(
  only_jud %>% filter(str_detect(model, "Bolsonaro$")),
  "Visão de Bolsonaro autoritário",
  key = "judiciario"
)

fig_judiciario <- p_jud_lula + p_jud_bol + plot_layout(ncol = 2)

fig_minorias
fig_eleicoes
fig_judiciario

ggsave("coef_minorias.png", fig_minorias, width = 12, height = 7, dpi = 300)
ggsave("coef_eleicoes.png", fig_eleicoes, width = 12, height = 7, dpi = 300)
ggsave("coef_judiciario.png", fig_judiciario, width = 12, height = 7, dpi = 300)
