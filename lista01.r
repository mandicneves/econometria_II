require(wooldridge) # Importando biblioteca com dados do livro
require(tidyverse) # Importando biblitoeca para visualização dos dados

# CASO NÃO TENHA INSTALADO ESSAS BIBLIOTECAS É PRECISO FAZER A INSTALAÇÃO
install.packages("wooldridge") # APENAS SE NÃO TIVER INSTALADO
install.packages("tidyverse") # APENAS SE NÃO TIVER INSTALADO


# ===============================================================================


# C9 - capítulo 3

view(charity)

# i)

regressao_c9_i <- lm(gift ~ mailsyear + giftlast + propresp, data = charity)
summary(regressao_c9_i)

# gift = -4,55 + 2,17(mailsyear) + 0,0059(giftlast) + 15,36(propresp)
#        (0,8030)   (0,3320)            (0,0014)       (0,8745)
# n = 4268  R² = 0,0834

regressao_c9_i_simples <- lm(gift ~ mailsyear, data = charity)
summary(regressao_c9_i_simples)

# gif = 2,01 + 2,65(maislyear)
#     (0,7395) (0,3431)
# n = 4268  R² = 0,0138

# Na regressão simples, o R² é de 0,0138, já na regressão com mais variáves é de 0,0834. Portanto, as variáveis giftlast e propresp ajudam a explicar mais o modelo.

# ii)

# Na regressão simples o coeficiente é maior. Isso acontece pois a variável está sendo superestimado

# iii)

# Dado que propresp está em proporção, um aumento de 10% em propres, leva ao aumento de 1,54 em gift, mantido outros fatores fixados.

# iv)

regressao_c9_iv <- lm(gift ~ mailsyear + giftlast + propresp + avggift, data = charity)
summary(regressao_c9_iv)

# gift = -7,33 + 1,20(mailsyear) + ...

# O efeito causado é uma redução no estimador da variável mailsyear

# v)

# gift = -7,33 + 1,20(mailsyear) - 0,26(giftlast)

# O efeito causado é a inversão do sinal do estimador de giftlast, ou seja, a quantidade de presente está negativamente relacionada com o presente mais recente.


# ===============================================================================


# C11 - capítulo 3

view(meapsingle)

# i)

regressao_c11_i_simples <- lm(math4 ~ pctsgle, data = meapsingle)
summary(regressao_c11_i_simples)

# math4 = 96,77 - 0,8329(pctsgle)
#        (1,5968)  (0,0707)
# n = 229 R² = 0,3795

# O coeficiente é negativamente relacionado com o desempenho em matemética, ou seja, crianças em monoparentalidade performam menos em matemática. O efeito parece ser pequeno.

# ii)

regressao_c11_ii <- lm(math4 ~ pctsgle + lmedinc + free, data = meapsingle)
summary(regressao_c11_ii)

# math4 = 51,72 - 0,1996(pctsgle) + 3,5601(lmedinc) - 0,3964(free)
#        (58,4781)  (0,1587)        (5,0417)          (0,0703)
# n = 229 R² = 0,4598

# O coeficiente de pctsgle em módulo ficou menor. Na regressão simples o coeficiente absorvia os efeitos de outras variávies com as quais possuia correlação, superestimando o efeito de pctsgle

# iii)

cor(meapsingle$lmedinc, meapsingle$free)

# Correlação entre as variáveis é de -0,75. O resultado da correlação é esperado, pois se espera que famílias que possuem uma renda maior, tenham menos crianças elegíveis à refeições gratuitas

# iv)

# Sim. Matematicamente, correlações altas entre as variáveis independentes causam uma instabilidade numérica ao ajustar a curva de regressão, o chamado efeito de multicolinearidade. Uma das consequências da multicolinearidade é a elevação dos erros-padrão, como observado no erro-padrão do intercepto

# v)

install.packages("faraway") # Instalando biblioteca para usar função vif(fator de inflação de variânica)
require(faraway)

dados <- data.frame(
  pctsgle = meapsingle$pctsgle,
  free = meapsingle$free,
  lmedinc = meapsingle$lmedinc
)
vif(dados)

# pctsgle e lmedinc possuem os maiores FIVs. FIVs maiores que 1 indicam multicolinearidade entre as variáveis.


# ===============================================================================


# C04 - capítulo 7

# i)

# B4 > 0, quanto melhor a nota no sat, melhor o desempenho na faculdade. Suponho que B6 < 0, ou seja, atletas performam pior. Os sinais de B1 e B5 não são claros.

# ii)

view(gpa2)


regressao_c04_ii <- lm(colgpa ~ hsize + hsizesq + hsperc + sat + female + athlete, data = gpa2)
summary(regressao_c04)

# colgpa = 1.24 - .05685(hsize) + .004675(hsizesq) - .01321(hsperc) + .001646(sat) + .1549(female) + .1693(athlete)
#         (7.949) (1.635)       (2.249)           (5.728)         (6.682)      (1.800)         (4.235)
# n = 4137  R² = 0,2925

# Para atletas o resultado em colgpa é um aumento de 0,169 pontos. O coeficiente é estatísticamente significante.

# iii)

regressao_c04_iii <- lm(colgpa ~ hsize + hsizesq + hsperc + female + athlete, data = gpa2)
summary(regressao_c04_iii)

# Atletas tem uma previsão de nota maior em 0,0054 pontos, que é praticamente zero. Isso acontece porque não controlamos mais a variável sat.

# iv)

df <- gpa2
df$female_athlete <- gpa2$female & gpa2$athlete
df$male_athlete <- !gpa2$female & gpa2$athlete
df$male_non_athlete <- !gpa2$female & !gpa2$athlete
view(df)

regressao_c04_iv <- lm(
  colgpa ~ hsize + hsizesq + hsperc + sat +
    female_athlete + male_athlete + male_non_athlete,
  data = df
)
summary(regressao_c04_iv)

# colgpa = 1.396 - .0568(hsize) + .00467(hsizesq) - .01321(hsperc) + .001646(sat) + .1751(female_athlete) + .0128(male_athlete) - 0.1546(male_non_athlete)
#         (7.949) (1.635)       (2.249)           (5.728)          (6.682)             (8.403)                 (4.874)             (1.831)
# n = 4137  R² = 0,2925

# A nota de mulheres atletas é de 0,175 mais pontos.


# ===============================================================================


# C12 - capítulo 7

view(beauty)

# i)


women_above <- beauty[which(beauty$abvavg & beauty$female), ]
women_below <- beauty[which(beauty$belavg & beauty$female), ]
men_above <- beauty[which(beauty$abvavg & !beauty$female), ]
men_below <- beauty[which(beauty$belavg & !beauty$female), ]

count(women_above)/count(women_below)
count(men_above)/count(men_below)

view(women_above)
# Existem 144 mulheres acima da média e 239 homens acima da média. Existem 59 mulheres abaixo da média e 96 homens abaixo da média. Portanto, existem mais pessoas acima da média do que abaixo da média.

# ii)

fraction_women <- count(women_above) / count(beauty)
fraction_men <- count(men_above) / count(beauty)

# iii)

view(beauty)

df <- beauty
df$men_above <- beauty$abvavg & !beauty$female
df$women_above <- beauty$abvavg & beauty$female
view(df)

regressao_c12_iii <- lm(lwage ~ women_above, data = df)
summary(regressao_c12_iii)
