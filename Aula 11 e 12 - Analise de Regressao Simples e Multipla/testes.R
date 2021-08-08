#analysis of variance

anova(modelo_tempodist)

f <- rf(100000, df1 = 1, df2 = 8)

hist(f, breaks = 100)

qf(0.05, df1 = 1, df2 = 8, lower.tail = F)

# P-Value -> Pr(>F)
pf(36.30, df1 =1, df2 = 8, lower.tail = F)

# Como p-value do F = 0.0003144 < 0.05, 
# há modelo estatisticamente significante a 95% de confiança

#teste t de Student

t <- rt(100000, df = 8)

hist(t, breaks = 100)

pt(6.025, df=8, lower.tail = F)*2

aggregate(corrupcao$cpi ~ corrupcao$regiao, FUN = mean)

corrupcao_dummies_teste <- dummy_columns(.data = corrupcao,
                                   select_columns = "regiao",
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = F)

corrupcao_dummies_teste %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)


lm(formula = cpi ~ . - pais, data = paises)
