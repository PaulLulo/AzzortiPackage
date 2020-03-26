\name{Niv_Svc_Analysis}
\alias{Niv_Svc_Analysis}
\title{Create plots about service level.}
\usage{
Niv_Svc_Analysis(df_base,lugar,linea,momento)
}
\arguments{
\item{df_base}{The data must be constituted as follows:}
\itemize{
      \item 1) CODI_CAMP: Sales campaign (ej. 201718..201909)
      \item 2) NOMB_LINE: Line of the SKU (ej. "JO JOYERIA")
      \item 3) CODI_VENT: Sales code SKU (ej. 123456)
      \item 4) DESCRIPCION: Name of the SKU.
      \item 5) REAL: Real Demand SKU.
      \item 7) Q_RG3: Traditional forecast for the SKU.
      \item 8) Q_MKT: Marketing area estimate.
      \item 9...) Comparative Models (ej. "Q_20", "Q_30")
      \item #) Must be the "Besty Model" (Q_BESTY)
      }

\item{lugar}{Three letters summarizing the name of the place (ej. "PER")}
\item{linea}{Three letters summarizing the name of the line (ej. "ROE")}
\item{momento}{Six letters summarizing the names of the current moment and prediction moment (ej. "ADVNAC")}
}
\examples{

#Headers
DF_TOTAL <- data.frame(CODI_CAMP = "",
                      NOMB_LINE = "",
                      CODI_VENT = "",
                      DESCRIPCION = "",
                      REAL = "",
                      Q_RG3 = "",
                      Q_MKT = "",
                      Q_30 = "",
                      Q_50 = "",
                      Q_70 = ""
)

DF_TOTAL<- DF_TOTAL[-c(1:nrow(DF_TOTAL)),]
campanas<- c(201901, 201902, 201903, 201904)

acumula<- DF_TOTAL

for(i in 1: length(campanas)) {
  #i=1
  acumula <- data.frame(CODI_CAMP = campanas[i],
                        NOMB_LINE = "RE ROPA EXTERIOR",
                        CODI_VENT = ceiling(runif(100,100000,999999)),
                        DESCRIPCION = paste0("P",c(1:100)),
                        REAL = 1000+(1:100)*10,
                        Q_RG3 = abs(1000+(1:100)*10 + rnorm(100,300,800)),
                        Q_MKT = abs(1000+(1:100)*10 + rnorm(100,100,700)),
                        Q_30 = abs(1000+(1:100)*10 + rnorm(100,400,700)),
                        Q_50 = abs(1000+(1:100)*10 + rnorm(100,450,900)),
                        Q_70 = abs(1000+(1:100)*10 + rnorm(100,700,900))
  )


  DF_TOTAL<- rbind(DF_TOTAL,acumula)
}

#
DF_TOTAL<-gen_q_besty(DF_TOTAL)
#
NIVEL_SERVICIO<- Niv_Svc_Analysis(DF_TOTAL,lugar= "PER", linea= "ROE", momento="ADVNAC")


}
