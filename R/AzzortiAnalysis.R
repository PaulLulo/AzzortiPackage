#' @title Create diagrams and tables about services, inventory, and the best combination of sales predictions.
#'
#' @description
#'
#' @param df_base
#' @param campana
#' @param lugar
#' @param ruta
#' @param linea
#' @param momento
#'
#' @examples
#'
#' @export


gen_q_besty<- function(df_base, campana) {
  names(df_base)[5]<- "Q_REAL"

  n_prod_total <- nrow(df_base)
  df_dlt <- df_base[(complete.cases(df_base)==F | df_base$Q_REAL==0), ]
  df_base <- df_base[(complete.cases(df_base)==F | df_base$Q_REAL==0)==F, ]
  n_prod_compl <- nrow(df_base)

  n_models <- ncol(df_base)-7


  # DivisÃ³n del cociente
  df_base_1<- data.frame(df_base,
                         COCIENTE= abs(apply(df_base[,8:(7+n_models)],2,function(x){(df_base$Q_REAL/x)})-1))

  # Vemos el mÃ¡s cercano
  df_base_2<- data.frame(df_base_1,
                         COCIENTE.MIN= apply(df_base_1[,(8+n_models):(7+2*n_models)],1,min)
  )

  names(df_base_2)

  df_base_3<- data.frame(df_base_2,
                         Q_BESTY= apply(df_base_2[,c(8:ncol(df_base_2))],1, function(x){ifelse(x[ncol(df_base_2)-7]==x[n_models+1],x[1],
                                                                                               ifelse(x[ncol(df_base_2)-7]==x[n_models+2],x[2],x[3]))})
  )


  X_RESULTADO <- list()
  X_RESULTADO[["DF_TOT"]] <- df_base
  X_RESULTADO[["DF_DTL"]] <- df_dlt
  X_RESULTADO[["DF_COC"]] <- df_base_1
  X_RESULTADO[["DF_COC_CER"]] <- df_base_2
  X_RESULTADO[["DF_BESTY"]] <- df_base_3$Q_BESTY
  print(paste("Productos totales ingresados:", nrow(df_base), sep=" "))
  print(paste("Productos incompletos eliminados:", nrow(df_dlt), sep=" "))

  names(df_base)[5]<- "REAL"
  df_base$Q_BESTY <- df_base_3$Q_BESTY
  return(df_base)
}

#DF_TOTAL<- gen_q_besty(DF_TOTAL,201901)


# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# ================================= FunciÃ³n para generar la tabla ==========================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#

gen_table<- function(df_base) {
  #df_base<- BASE.TEST.NORM_2

  campanasas<- unique(df_base$CODI_CAMP)
  i=1
  df_base_3<- df_base[df_base$CODI_CAMP== campanasas[i], ]

  df_base_4<- data.frame(df_base_3,
                         CONTADOR= apply(df_base_3[,c(8:ncol(df_base_3))],1, function(x){
                           ifelse(x[ncol(df_base_3)-7]==x[1],paste(colnames(df_base_3[8]),sep="_"),
                                  ifelse(x[ncol(df_base_3)-7]==x[2],paste(colnames(df_base_3[9]),sep="_"),paste(colnames(df_base_3[10]),sep="_")))}))
  contadores<- data.frame(unique(df_base_4$CONTADOR))
  contadores<- contadores[order(contadores$unique.df_base_4.CONTADOR., decreasing = F), ]
  tabla<- data.frame(round(prop.table(table(df_base_4$CONTADOR))*100,0))
  base<- data.frame(CAMP= "CAMP",
                    contadores[1],
                    contadores[2],
                    contadores[3])
  base1<- t(base)

  colnames(base)<-base1
  base_acumula<- base[-c(1:nrow(base)), ]


  for (i in 1: length(campanasas)) {
    df_base_3<- df_base[df_base$CODI_CAMP== campanasas[i], ]

    df_base_4<- data.frame(df_base_3,
                           CONTADOR= apply(df_base_3[,c(8:ncol(df_base_3))],1, function(x){
                             ifelse(x[ncol(df_base_3)-7]==x[1],paste(colnames(df_base_3[8]),sep="_"),
                                    ifelse(x[ncol(df_base_3)-7]==x[2],paste(colnames(df_base_3[9]),sep="_"),paste(colnames(df_base_3[10]),sep="_")))}))
    contadores<- data.frame(unique(df_base_4$CONTADOR))
    contadores<- contadores[order(contadores$unique.df_base_4.CONTADOR., decreasing = F), ]
    tabla<- data.frame(round(prop.table(table(df_base_4$CONTADOR))*100,0))
    base<- data.frame(CAMP= "CAMP",
                      contadores[1],
                      contadores[2],
                      contadores[3])
    base1<- t(base)

    colnames(base)<-base1
    base[1]<- campanasas[i]
    base[2]<- tabla[1,2]
    base[3]<- tabla[2,2]
    base[4]<- tabla[3,2]

    base_acumula<- rbind(base_acumula, base)

  }



  return(base_acumula)
}



# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# =========================== FunciÃ³n para calcular la tabla de sobrantes y faltantes ======================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#

#tabla1<- gen_tabla(DF_TOTAL)
#df_base_o<- DF_TOTAL

niv_flt_sob<- function(df_base_o, lugar, ruta="") {
  names(df_base_o)[5]<- "Q_REAL"
  n_models= ncol(df_base_o)-6

  df_base_o_1sm= data.frame(df_base_o,
                            Q_FATL_SM= apply(df_base_o[ ,c(6,8: ncol(df_base_o))],2, function(x) {ifelse(x<df_base_o$Q_REAL,x- df_base_o$Q_REAL,0)}))


  df_base_o_2sm= data.frame(df_base_o,
                            Q_SOB_SM= apply(df_base_o_1sm[ ,c(6,8: (6+n_models))],2, function(x) {ifelse(x>df_base_o_1sm$Q_REAL,x-df_base_o_1sm$Q_REAL,0)}))

  df_base_o_pre= data.frame(df_base_o,
                            Q_MAX= apply(df_base_o[ ,c(6,8: ncol(df_base_o))],2, function(x) {ifelse(x<df_base_o$Q_MKT,df_base_o$Q_MKT,x)}))


  df_base_o_1cm= data.frame(df_base_o,
                            Q_FATL_CM= apply(df_base_o_pre[ ,c(12:16)],2, function(x) {ifelse(x<df_base_o_pre$Q_REAL,x-df_base_o_pre$Q_REAL,0)}))


  df_base_o_2cm= data.frame(df_base_o,
                            Q_SOB_CM= apply(df_base_o_pre[ ,c(12:16)],2, function(x) {ifelse(x>df_base_o_pre$Q_REAL,x-df_base_o_pre$Q_REAL,0)}))

  names(df_base_o)[5]<- "REAL"
  names(df_base_o_1sm)[5]<- "REAL"
  names(df_base_o_1cm)[5]<- "REAL"
  names(df_base_o_2sm)[5]<- "REAL"
  names(df_base_o_2cm)[5]<- "REAL"

  Y_RESULTADO<- list()

  Y_RESULTADO[["DF_TOTAL"]]<- data.frame(df_base_o,
                                         df_base_o_1sm[12:16],
                                         df_base_o_2sm[12:16],
                                         df_base_o_pre[12:16],
                                         df_base_o_1cm[12:16],
                                         df_base_o_2cm[12:16]
  )

  Y_RESULTADO[["DF_NIV_SVC_SM"]]<- data.frame(df_base_o,
                                              df_base_o_1sm[12:16]
  )

  Y_RESULTADO[["DF_NIV_SVC_CM"]]<- data.frame(df_base_o,
                                              df_base_o_1cm[12:16]
  )

  Y_RESULTADO[["DF_NIV_INV_SM"]]<- data.frame(df_base_o,
                                              df_base_o_2sm[12:16]
  )

  Y_RESULTADO[["DF_NIV_INV_CM"]]<- data.frame(df_base_o,
                                              df_base_o_2cm[12:16]
  )


  return(Y_RESULTADO)

}


#df_base<- df_base_o$DF_NIV_SVC_SM

# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# =========================== FunciÃ³n para calcular el nivel de servicio  ==================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#

#df_base_o_1sm<- niv_flt_sob(DF_TOTAL)
#df_base_o_2cm<- df_base_o_1sm$DF_NIV_INV_CM
#df_base<- df_base_o_1sm
niv_svc<- function(df_base, campana, lugar, ruta=""){

  df_base<- df_base[order(df_base$CODI_CAMP,decreasing = F), ]

  niveles<- data.frame(niv1= paste("NIV_SVC",substr(colnames(df_base[12]),11,30),sep="_"),
                       niv2= paste("NIV_SVC",substr(colnames(df_base[13]),11,30),sep="_"),
                       niv3= paste("NIV_SVC",substr(colnames(df_base[14]),11,30),sep="_"),
                       niv4= paste("NIV_SVC",substr(colnames(df_base[15]),11,30),sep="_"),
                       niv5= paste("NIV_SVC",substr(colnames(df_base[16]),11,30),sep="_")
  )


  niveles_1<- t(niveles)
  colnames(niveles)<- niveles_1

  acumu_F<- data.frame(CODI_CAMP= " ",
                       niveles[1:5]
  )


  acumu_F1<- acumu_F[-c(1:nrow(acumu_F)),]

  campanitas<- unique(df_base$CODI_CAMP)

  for (w in 1: length(campanitas)) {
    #w=1
    base_w<-df_base[df_base$CODI_CAMP==campanitas[w],]

    acumu_s<- acumu_F
    names(acumu_s)

    acumu_s[1]<- campanitas[w]
    acumu_s[2]<- round((1+ sum(base_w[12])/sum(base_w[5]))*100,2)
    acumu_s[3]<- round((1+ sum(base_w[13])/sum(base_w[5]))*100,2)
    acumu_s[4]<- round((1+ sum(base_w[14])/sum(base_w[5]))*100,2)
    acumu_s[5]<- round((1+ sum(base_w[15])/sum(base_w[5]))*100,2)
    acumu_s[6]<- round((1+ sum(base_w[16])/sum(base_w[5]))*100,2)

    acumu_F1<- rbind(acumu_F1,acumu_s)
  }

  NIV_SVC_RESULT<- acumu_F1
  return(NIV_SVC_RESULT)
}


#df_base<- niv_svc(df_base_o_1sm)

# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# ============================ FunciÃ³n para plotear el nivel de servicio  ==================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
#df_base<-NIV_SVC_RESULT
# lugar= "PerÃº"
plot_niv_svc<- function(df_base, lugar, linea ,momento,ruta=""){

  DFA<- df_base
  library(reshape)
  df_niv_svc <- melt(DFA, id.vars = "CODI_CAMP")

  library(ggplot2)

  DF1<- data.frame(
    TIPO= unique(df_niv_svc$variable),
    colores = c("chartreuse", "chartreuse3", "green3", "darkgreen","firebrick3")
  )

  colores = c("chartreuse", "chartreuse3", "green3", "darkgreen","firebrick3")
  df_niv_svc$TIPO= DF1$colores[match(df_niv_svc$variable, DF1$TIPO)]
  df_niv_svc$variable<- as.character(df_niv_svc$variable)

  df_niv_svc$variable<- substr(df_niv_svc$variable,9, length(df_niv_svc$variable))
  library(dplyr)
  df_niv_svc$variable[grepl("BES", df_niv_svc$variable)] <- "Q_BESTY"
  df_niv_svc$variable<- as.factor(df_niv_svc$variable)
  colnames(df_niv_svc)[colnames(df_niv_svc)== "variable"]<- "MODELO"
  modelo<- unique(df_niv_svc$MODELO)
  #df_niv_svc$value<- round(df_niv_svc$value,0)

  plot_DFAP <- ggplot(df_niv_svc,
                      aes(x = CODI_CAMP,
                          y = value,
                          group = TIPO,
                          #colour= MODELO,
                          label = paste0(format(value,
                                                digits = 1),"%"),
                          fill = MODELO))+
    geom_line(aes(color=MODELO), size=1)+
    scale_linetype_manual(values=modelo)+
    scale_color_manual(values=colores)+
    scale_size_manual(values=c(1, 1.5,2,2.5,3))+
    scale_y_continuous(labels = scales::percent_format(scale = 1,
                                                       accuracy = 1)) +
    #scale_x_discrete(drop=F) +
    theme(plot.title = element_text(color = "#636363",
                                    size = 18,
                                    face = "bold.italic"
    ),
    axis.title.x = element_text(size = 14,
                                face = "bold",
                                colour = "#252525"),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14,
                                face = "bold",
                                colour = "#252525"),
    text = element_text(face = "bold"),
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = "bottom")+
    ggtitle(paste("NIVEL DE SERVICIO (MDS) \n",
                  lugar, " - ",
                  linea, " - ",
                  momento,"\n",
                  substr(min(DFA$CODI_CAMP), 1,4),
                  sep = ""))+
    xlab("CampaÃ±a") +
    ylab("Nivel de servicio") +
    geom_text(#position = position_dodge(width = 0.5),
      color = "black",
      size = 4,
      fontface = "bold",
      vjust = -0.25) +
    guides(fill=guide_legend(
      keywidth=0.6,
      keyheight=0.8,
      default.unit="inch"))



  print(plot_DFAP)


}

#base1<- niv_flt_sob(DF_TOTAL)
#base2<- base1$DF_NIV_SVC_CM
#base3<- niv_svc(base2)
#df_base<- base3
# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# ============================ FunciÃ³n para plotear el nivel de servicio con marketing =====================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#

plot_niv_svc_cm<- function(df_base, lugar, linea ,momento,ruta=""){
  DFA<- df_base
  library(reshape)
  df_niv_svc <- melt(DFA, id.vars = "CODI_CAMP")

  library(ggplot2)

  DF1<- data.frame(
    TIPO= unique(df_niv_svc$variable),
    colores = c("chartreuse", "chartreuse3", "green3", "darkgreen","firebrick3")
  )

  colores = c("chartreuse", "chartreuse3", "green3", "darkgreen","firebrick3")
  df_niv_svc$TIPO= DF1$colores[match(df_niv_svc$variable, DF1$TIPO)]
  df_niv_svc$variable<- as.character(df_niv_svc$variable)

  df_niv_svc$variable<- substr(df_niv_svc$variable,15, length(df_niv_svc$variable))
  library(dplyr)
  df_niv_svc$variable[grepl("BE", df_niv_svc$variable)] <- "Q_BESTY"
  df_niv_svc$variable[grepl("RG", df_niv_svc$variable)] <- "Q_RG3"
  df_niv_svc$variable<- as.factor(df_niv_svc$variable)
  colnames(df_niv_svc)[colnames(df_niv_svc)== "variable"]<- "MODELO"
  modelo<- unique(df_niv_svc$MODELO)
  #df_niv_svc$value<- round(df_niv_svc$value,0)


  plot_DFAP <- ggplot(df_niv_svc,
                      aes(x = CODI_CAMP,
                          y = value,
                          group = TIPO,
                          #colour= MODELO,
                          label = paste0(format(value,
                                                digits = 1),"%"),
                          fill = MODELO))+
    geom_line(aes(color=MODELO), size=1)+
    scale_linetype_manual(values=modelo)+
    scale_color_manual(values=colores)+
    scale_size_manual(values=c(1, 1.5,2,2.5,3))+
    scale_y_continuous(labels = scales::percent_format(scale = 1,
                                                       accuracy = 1)) +
    #scale_x_discrete(drop=F) +
    theme(plot.title = element_text(color = "#636363",
                                    size = 18,
                                    face = "bold.italic"
    ),
    axis.title.x = element_text(size = 14,
                                face = "bold",
                                colour = "#252525"),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14,
                                face = "bold",
                                colour = "#252525"),
    text = element_text(face = "bold"),
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = "bottom")+
    ggtitle (paste("NIVEL DE SERVICIO (MDS - MKT) \n",
                   lugar, " - ",
                   linea, " - ",
                   momento,"\n",
                   substr(min(DFA$CODI_CAMP), 1,4),
                   sep = ""))+
    xlab("CampaÃ±a") +
    ylab("Nivel de servicio") +
    geom_text(#position = position_dodge(width = 0.5),
      color = "black",
      size = 4,
      fontface = "bold",
      vjust = -0.25) +
    guides(fill=guide_legend(
      keywidth=0.6,
      keyheight=0.8,
      default.unit="inch"))



  print(plot_DFAP)


}



# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# ============================ FunciÃ³n para calcular el nivel de inventario  ===============================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#

niv_inv<- function(df_base, lugar, ruta=""){

  #df_base<- base1_2
  df_base<- df_base[order(df_base$CODI_CAMP,decreasing = F), ]

  niveles2<- data.frame(niv1= paste("NIV_INV",substr(colnames(df_base[12]),10,30),sep="_"),
                        niv2= paste("NIV_INV",substr(colnames(df_base[13]),10,30),sep="_"),
                        niv3= paste("NIV_INV",substr(colnames(df_base[14]),10,30),sep="_"),
                        niv4= paste("NIV_INV",substr(colnames(df_base[15]),10,30),sep="_"),
                        niv5= paste("NIV_INV",substr(colnames(df_base[16]),10,30),sep="_")
  )


  niveles_2<- t(niveles2)
  colnames(niveles2)<- niveles_2

  acumu_F<- data.frame(CODI_CAMP= " ",
                       niveles2[1:5]
  )


  acumu_F1<- acumu_F[-c(1:nrow(acumu_F)),]

  campanitas<- unique(df_base$CODI_CAMP)

  for (w in 1: length(campanitas)) {
    #w=1
    base_w<-df_base[df_base$CODI_CAMP==campanitas[w],]

    acumu_s<- acumu_F
    names(acumu_s)

    acumu_s[1]<- campanitas[w]
    acumu_s[2]<- round((sum(base_w[12])/sum(base_w[5]))*100,2)
    acumu_s[3]<- round((sum(base_w[13])/sum(base_w[5]))*100,2)
    acumu_s[4]<- round((sum(base_w[14])/sum(base_w[5]))*100,2)
    acumu_s[5]<- round((sum(base_w[15])/sum(base_w[5]))*100,2)
    acumu_s[6]<- round((sum(base_w[16])/sum(base_w[5]))*100,2)

    acumu_F1<- rbind(acumu_F1,acumu_s)
  }
  NIV_INV_RESULT<- acumu_F1
  return(NIV_INV_RESULT)
}

# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# ============================ FunciÃ³n para plotear el nivel de inventario =================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#

plot_niv_inv<- function(df_base, lugar, linea ,momento,ruta=""){

  DFA<- df_base
  library(reshape)
  df_niv_svc <- melt(DFA, id.vars = "CODI_CAMP")

  library(ggplot2)

  DF1<- data.frame(
    TIPO= unique(df_niv_svc$variable),
    colores = c("chartreuse", "chartreuse3", "green3", "darkgreen","firebrick3")
  )

  colores = c("chartreuse", "chartreuse3", "green3", "darkgreen","firebrick3")
  df_niv_svc$TIPO= DF1$colores[match(df_niv_svc$variable, DF1$TIPO)]
  df_niv_svc$variable<- as.character(df_niv_svc$variable)

  df_niv_svc$variable<- substr(df_niv_svc$variable,9, length(df_niv_svc$variable))
  library(dplyr)
  df_niv_svc$variable[grepl("BES", df_niv_svc$variable)] <- "Q_BESTY"
  df_niv_svc$variable<- as.factor(df_niv_svc$variable)
  colnames(df_niv_svc)[colnames(df_niv_svc)== "variable"]<- "MODELO"
  modelo<- unique(df_niv_svc$MODELO)
  #df_niv_svc$value<- round(df_niv_svc$value,0)

  plot_DFAP <- ggplot(df_niv_svc,
                      aes(x = CODI_CAMP,
                          y = value,
                          group = TIPO,
                          #colour= MODELO,
                          label = paste0(format(value,
                                                digits = 1),"%"),
                          fill = MODELO))+
    geom_line(aes(color=MODELO), size=1)+
    scale_linetype_manual(values=modelo)+
    scale_color_manual(values=colores)+
    scale_size_manual(values=c(1, 1.5,2,2.5,3))+
    scale_y_continuous(labels = scales::percent_format(scale = 1,
                                                       accuracy = 1)) +
    #scale_x_discrete(drop=F) +
    theme(plot.title = element_text(color = "#636363",
                                    size = 18,
                                    face = "bold.italic"
    ),
    axis.title.x = element_text(size = 14,
                                face = "bold",
                                colour = "#252525"),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14,
                                face = "bold",
                                colour = "#252525"),
    text = element_text(face = "bold"),
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = "bottom")+
    ggtitle(paste("NIVEL DE INVENTARIO (MDS) \n",
                  lugar, " - ",
                  linea, " - ",
                  momento,"\n",
                  substr(min(DFA$CODI_CAMP), 1,4),
                  sep = ""))+
    xlab("CampaÃ±a") +
    ylab("Nivel de inventario") +
    geom_text(#position = position_dodge(width = 0.5),
      color = "black",
      size = 4,
      fontface = "bold",
      vjust = -0.25) +
    guides(fill=guide_legend(
      keywidth=0.6,
      keyheight=0.8,
      default.unit="inch"))

  print(plot_DFAP)


}
# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# ============================ FunciÃ³n para plotear el nivel de inventario con marketing ===================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#


plot_niv_inv_cm<- function(df_base, lugar, linea ,momento,ruta=""){

  DFA<- df_base
  library(reshape)
  df_niv_svc <- melt(DFA, id.vars = "CODI_CAMP")

  library(ggplot2)

  DF1<- data.frame(
    TIPO= unique(df_niv_svc$variable),
    colores = c("chartreuse", "chartreuse3", "green3", "darkgreen","firebrick3")
  )

  colores = c("chartreuse", "chartreuse3", "green3", "darkgreen","firebrick3")
  df_niv_svc$TIPO= DF1$colores[match(df_niv_svc$variable, DF1$TIPO)]
  df_niv_svc$variable<- as.character(df_niv_svc$variable)

  df_niv_svc$variable<- substr(df_niv_svc$variable,15, length(df_niv_svc$variable))
  library(dplyr)
  df_niv_svc$variable[grepl("BES", df_niv_svc$variable)] <- "Q_BESTY"
  df_niv_svc$variable<- as.factor(df_niv_svc$variable)
  colnames(df_niv_svc)[colnames(df_niv_svc)== "variable"]<- "MODELO"
  modelo<- unique(df_niv_svc$MODELO)
  #df_niv_svc$value<- round(df_niv_svc$value,0)

  plot_DFAP <- ggplot(df_niv_svc,
                      aes(x = CODI_CAMP,
                          y = value,
                          group = TIPO,
                          #colour= MODELO,
                          label = paste0(format(value,
                                                digits = 1),"%"),
                          fill = MODELO))+
    geom_line(aes(color=MODELO), size=1)+
    scale_linetype_manual(values=modelo)+
    scale_color_manual(values=colores)+
    scale_size_manual(values=c(1, 1.5,2,2.5,3))+
    scale_y_continuous(labels = scales::percent_format(scale = 1,
                                                       accuracy = 1)) +
    #scale_x_discrete(drop=F) +
    theme(plot.title = element_text(color = "#636363",
                                    size = 18,
                                    face = "bold.italic"
    ),
    axis.title.x = element_text(size = 14,
                                face = "bold",
                                colour = "#252525"),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14,
                                face = "bold",
                                colour = "#252525"),
    text = element_text(face = "bold"),
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom")+
    ggtitle(paste("NIVEL DE INVENTARIO (MDS - MKT) \n",
                  lugar, " - ",
                  linea, " - ",
                  momento,"\n",
                  substr(min(DFA$CODI_CAMP), 1,4),
                  sep = ""))+
    xlab("CampaÃ±a") +
    ylab("Nivel de inventario") +
    geom_text(#position = position_dodge(width = 0.5),
      color = "black",
      size = 4,
      fontface = "bold",
      vjust = -0.25) +
    guides(fill=guide_legend(nrow=2, byrow=T))



  print(plot_DFAP)


}


# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# =========================================== FunciÃ³n para consolidar ======================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#
# ==========================================================================================================================#

Niv_Svc_Analysis<- function(df_base, lugar , linea ,momento){
  #df_base<- DF_TOTAL
  # lugar= "Per"
  df_camp <- df_base

  consol_niv_flt_sob <- niv_flt_sob(df_camp, lugar)

  consol_niv_flt_sm<- consol_niv_flt_sob$DF_NIV_SVC_SM
  consol_niv_flt_cm<- consol_niv_flt_sob$DF_NIV_SVC_CM

  niv_svc_sm<- niv_svc(consol_niv_flt_sm)
  niv_svc_cm<- niv_svc(consol_niv_flt_cm)

  gfc_niv_svc_sm<- plot_niv_svc(niv_svc_sm,lugar, linea ,momento)
  gfc_niv_svc_cm<- plot_niv_svc_cm(niv_svc_cm,lugar, linea ,momento)

  Y_RESULT<- list()
  Y_RESULT[["NIV_SRV_MDS"]]<- niv_svc_sm
  Y_RESULT[["NIV_SRV_MDS_MKT"]]<- niv_svc_cm
  print("NIVEL DE SERVICIO (MDS)")
  print(niv_svc_sm)
  print("NIVEL DE SERVICIO (MDS - MKT)")
  print(niv_svc_cm)
  return(Y_RESULT)
}


Niv_Inv_Analysis<- function(df_base, lugar , linea ,momento){
  #df_base_o
  # lugar= "Per"
  df_camp <- df_base

  consol_niv_flt_sob <- niv_flt_sob(df_camp, lugar)

  consol_niv_sob_sm<- consol_niv_flt_sob$DF_NIV_INV_SM
  consol_niv_sob_cm<- consol_niv_flt_sob$DF_NIV_INV_CM

  niv_inv_sm<- niv_inv(consol_niv_sob_sm)
  niv_inv_cm<- niv_inv(consol_niv_sob_cm)

  gfc_niv_inv_sm<- plot_niv_inv(niv_inv_sm,lugar, linea ,momento)
  gfc_niv_inv_cm<- plot_niv_inv_cm(niv_inv_cm,lugar, linea ,momento)

  Y_RESULT<- list()
  Y_RESULT[["NIV_INV_MDS"]]<- niv_inv_sm
  Y_RESULT[["NIV_INV_MDS_MKT"]]<- niv_inv_cm
  print("NIVEL DE INVENTARIO (MDS)")
  print(niv_inv_sm)
  print("NIVEL DE INVENTARIO (MDS - MKT)")
  print(niv_inv_cm)
  return(Y_RESULT)
}
