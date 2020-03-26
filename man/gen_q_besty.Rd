\name{gen_q_besty}
\alias{gen_q_besty}
\title{The best combination of a set of predictive models.}
\usage{
gen_q_besty(df_base, campana)
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
      }
\item{campana}{Selected campaign}
}

\examples{

## simulated data
DF_TOTAL <- data.frame(CODI_CAMP = 202004,
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

#BESTY
DF_TOTAL<- gen_q_besty(DF_TOTAL)

}
\keyword{besty}
