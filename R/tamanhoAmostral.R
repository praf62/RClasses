#FÓRMULAS GENÉRICAS PARA CALCULAR O TAMANHO AMOSTRAL, A MARGEM DE ERRO
# E O NÍVEL DE CONFIANÇA, PARA A MÉDIA EM GERAL OU PROPORÇÃO,
# COM POPULAÇÃO FINITA OU INFINITA, COM ABORDAGEM CONSERVADORA OU NÃO
# (PARA O CASO DA PROPORÇÃO), COM VARIÂNCIA CONHECIDA OU NÃO...

#PARA CALCULAR O TAMANHO DA AMOSTRA (n): MÉDIA GERAL, PROPORÇÃO
sampleSize = function(alpha=NULL, N=NULL, e=NULL, sd=NULL, sdSampleSize=NULL, isToProportion=FALSE){
  #x=BD01$TempoFalha#SUBSTITUIR PELO CONJUNTO E DADOS DE INTERESSE
  #alpha=.05#O NÍVEL DE CONFIANÇA É (1-alpha)
  #e=??#MARGEM DE ERRO
  m=sdSampleSize#TAMANHO DA AMOSTRA DA QUAL PROVEIO sd
  quant_m = NULL; text = NULL
  if(sdSampleSize==N | isToProportion){#sd is the parameter value
    quant_m = round(qnorm(1-alpha/2), 2)
  } else {#sd is an  estimate of the sd parameter
    quant_m = round(qt((1-alpha/2), df=(m-1)), 3)#QUANTIL DA T-STUDENT
    text = paste(", df=", (m-1), sep="")
  }
  # n0 = round((quant_m*sd/e)^2, 3)
  n0 = (quant_m*sd/e)^2
  n = ifelse(is.finite(N), (n0*N)/(n0+N-1), n0)
  # n=round(n,3)#TAMANHO DA AMOSTRA
  n=round(n)#TAMANHO DA AMOSTRA
  print(paste("quant_m=", quant_m, ", n=", n, text, sep=""))
  return(n)
}
errorMargin = function(alpha=NULL, N=NULL, n=NULL, sd=NULL, sdSampleSize=NULL, isToProportion=FALSE){
  #x=BD01$TempoFalha#SUBSTITUIR PELO CONJUNTO E DADOS DE INTERESSE
  #alpha=.05#O NÍVEL DE CONFIANÇA É (1-alpha)
  #e=??#MARGEM DE ERRO
  m=sdSampleSize#TAMANHO DA AMOSTRA DA QUAL PROVEIO sd
  quant_m = NULL; text=NULL
  if(sdSampleSize==N | isToProportion){#sd is the parameter value
    quant_m = round(qnorm(1-alpha/2), 2)
  } else {#sd is an  estimate of the parameter
    quant_m = round(qt((1-alpha/2), df=(m-1)), 3)#QUANTIL DA T-STUDENT
    text = paste(", df=", (m-1), sep="")
  }
  correctionFactor = ifelse(is.finite(N), sqrt((N-n)/(N-1)), 1)
  correctionFactor = round(correctionFactor, 3)
  e = round(quant_m*(sd/sqrt(n))*correctionFactor, 3)
  print(paste("quant_m=", quant_m, ", e=", e, text, sep=""))
  return(e)
}
confidenceLevel = function(e=NULL, N=NULL, n=NULL, sd=NULL, sdSampleSize=NULL, isToProportion=FALSE){
  #x=BD01$TempoFalha#SUBSTITUIR PELO CONJUNTO E DADOS DE INTERESSE
  #alpha=.05#O NÍVEL DE CONFIANÇA É (1-alpha)
  #e=??#MARGEM DE ERRO
  m=sdSampleSize#TAMANHO DA AMOSTRA DA QUAL PROVEIO sd
  correctionFactor = ifelse(is.finite(N), sqrt((N-n)/(N-1)), 1)
  correctionFactor = round(correctionFactor, 3)
  quant_m = round(e/((sd/sqrt(n))*correctionFactor), 3)
  v_1_alpha_2 = NULL; text=NULL
  if(sdSampleSize==N | isToProportion){#sd is the parameter value
    quant_m = round(quant_m, 2)
    v_1_alpha_2 = round(pnorm(q = quant_m), 3)
  } else {#sd is an  estimate of the parameter
    v_1_alpha_2 = round(pt(q = quant_m, df=(m-1)), 3)#QUANTIL DA T-STUDENT
    text = paste(", df=", (m-1), sep="")
  }
  confLevel = 1 - 2*(1-v_1_alpha_2)
  print(paste("quant_m=", quant_m, ", confLevel=", confLevel, text, sep=""))
  return(confLevel)
}
# QUANDO A VARIÂNCIA FOR DESCONHECIDA E A APOPULAÇÃO FOR FINITA
tamanhoAmostral_media_sd_amostra_N_finito = function(x, alpha, N, e){
  #x=BD01$TempoFalha#SUBSTITUIR PELO CONJUNTO E DADOS DE INTERESSE
  #alpha=.05#O NÍVEL DE CONFIANÇA É (1-alpha)
  #e=??#MARGEM DE ERRO
  m=length(x)#TAMANHO DA AMOSTRA-PILOTO
  t_m = qt((1-alpha/2), df=(m-1))#QUANTIL DA T-STUDENT
  s=sd(x)#DESVIO-PADRÃO AMOSTRAL
  #N=??#TAMAMNHO DA POPULAÇÃO
  n0 = (t_m*s/e)^2
  n=(n0*N)/(n0+N-1)#TAMANHO DA AMOSTRA
  return(n)
}

#MÉDIA EM GERAL - PARA CALCULAR A MARGEM DE ERRO (e)
# QUANDO A VARIÂNCIA FOR DESCONHECIDA E A APOPULAÇÃO FOR FINITA
margemDeErro_media_sd_amostra_N_finito = function(x, alpha, N){
  n=length(x)#TAMANHO DA AMOSTRA-PILOTO
  #alpha=.05#O NÍVEL DE CONFIANÇA É (1-alpha)
  t_n = qt((1-alpha/2), df=(n-1))#QUANTIL DA T-STUDENT
  s=sd(x)#DESVIO-PADRÃO AMOSTRAL
  e=t_n*(s/sqrt(n))*(sqrt((N-n)/(N-1)))#MARGEM DE ERRO
  return(e)
}

#tamanhoAmostral_sd_amostra_N_finito(BD01$TempoFalha, alpha=.05, N=1000, e=1)
#margemDeErro_sd_amostra_N_finito(BD01$TempoFalha[1:153], alpha=.05, N=254)#Ilustrado Aline
#margemDeErro_sd_amostra_N_finito(BD01$TempoFalha[1:193], alpha=.05, N=561)#Ilustrado Adriana

#PROPORÇÃO - PARA CALCULAR O TAMANHO DA AMOSTRA (n)
# ABORDAGEM CONSERVADORA (sigma=.5) E QUANDO A APOPULAÇÃO FOR FINITA
tamanhoAmostral_proporcao_N_finito_conservador = function(alpha, N, e){
  #x=BD01$TempoFalha#SUBSTITUIR PELO CONJUNTO E DADOS DE INTERESSE
  #alpha=.05#O NÍVEL DE CONFIANÇA É (1-alpha)
  #e=??#MARGEM DE ERRO
  z = qnorm((1-alpha/2))#QUANTIL DA T-STUDENT
  n0 = (z*.5/e)^2
  n=(n0*N)/(n0+N-1)#TAMANHO DA AMOSTRA
  return(n)
}
#tamanhoAmostral_proporcao_N_finito_conservador(.15, 58, .04)
#PROPORÇÃO - PARA CALCULAR O TAMANHO DA AMOSTRA (n)
# ABORDAGEM OUSADA (sigma=raiz(p_obs*(1-p_o))) E QUANDO A APOPULAÇÃO FOR FINITA
tamanhoAmostral_proporcao_N_finito_ousado = function(n, p_obs, alpha, N, e){
  #alpha=.05#O NÍVEL DE CONFIANÇA É (1-alpha)
  #n=length(x)
  t_m = qt(p = (1-alpha/2), df=(n-1))#QUANTIL DA T-STUDENT
  #p_obs = mean(x)
  sigma = sqrt(p_obs*(1-p_obs))
  n0 = (t_m*sigma/e)^2
  n=(n0*N)/(n0+N-1)#TAMANHO DA AMOSTRA
  return(n)
}

#MÉDIA EM GERAL - PARA CALCULAR A MARGEM DE ERRO (e)
# QUANDO A AMOSTRA FOR DESCONHECIDA E A APOPULAÇÃO FOR FINITA
margemDeErro_proporcao_N_finito_conservador = function(n, alpha, N){
  z = qnorm((1-alpha/2))#QUANTIL DA T-STUDENT
  e=z*(.5/sqrt(n))*(sqrt((N-n)/(N-1)))#MARGEM DE ERRO
  return(e)
}

margemDeErro_proporcao_N_finito_ousado = function(x, alpha, N){
  n=length(x)#TAMANHO DA AMOSTRA-PILOTO
  p_obs = mean(x)
  t_n = qt((1-alpha/2), df=(n-1))#QUANTIL DA T-STUDENT
  sigma = sqrt(p_obs*(1-p_obs))
  e=t_n*(sigma/sqrt(n))*(sqrt((N-n)/(N-1)))#MARGEM DE ERRO
  return(e)
}

#tamanhoAmostral_proporcao_N_finito_conservador(alpha = .05, N = 254, e=.05)
#tamanhoAmostral_proporcao_N_finito_ousado(n = 153, alpha=.05, p_obs = (121/153), N = 254, e=.05)

#Exemplo do volume em garrafas de 1 litro
sampleSize(alpha = 0.05, N = 12, e = .2, sd = .08, sdSampleSize = 60, isToProportion = FALSE)

