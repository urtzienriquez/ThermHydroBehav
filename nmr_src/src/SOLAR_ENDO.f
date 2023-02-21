C     NICHEMAPR: SOFTWARE FOR BIOPHYSICAL MECHANISTIC NICHE MODELLING

C     COPYRIGHT (C) 2020 MICHAEL R. KEARNEY AND WARREN P. PORTER

C     THIS PROGRAM IS FREE SOFTWARE: YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C     IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C     THE FREE SOFTWARE FOUNDATION, EITHER VERSION 3 OF THE LICENSE, OR (AT
C      YOUR OPTION) ANY LATER VERSION.

C     THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C     WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C     MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C     GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C     YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C     ALONG WITH THIS PROGRAM. IF NOT, SEE HTTP://WWW.GNU.ORG/LICENSES/.

C     THIS SUBROUTINE COMPUTES SOLAR INCIDENT ON AN ANIMAL
C     INPUT DATA THE DORSAL AND VENTRAL ABSORPTIVITIES, SILHOUETTE AREA AND TOTAL AREA RESPECTIVELY
C     THE CONFIGURATION FACTORS WILL DETERMINE WHERE DIFFUSE SOLAR IS COMING FROM.
C     THE SOLAR CONFIGURATION FACTOR IS SEPARATE FROM THE USER INPUT CONFIG FACTOR
C     FOR IR EXCHANGE. FOR SOLAR, CONFIGURATION FACTOR SHOULD BE THE REMAINDER OF 
C     WHATEVER IS NOT FACING THE SKY SINCE
C     THERE IS NOT THE SAME EXCHANGE BETWEEN LEGS AND THE BODY YOU HAVE WITH IR.

      SUBROUTINE SOLAR_ENDO(AREATOTL,ABSAND,ABSANV,ABSSB,ASIL,PCTDIF,
     &QNORM,ASHADE,QSOLR,FASKY,FAVEG,RESULTS)
     
      IMPLICIT NONE

      DOUBLE PRECISION ABSAND,ABSANV,ABSSB,AREATOTL,ASHADE,ASIL,FASKY
      DOUBLE PRECISION FAVEG,PCTDIF,QDORSL,QNORM,QSDIFF,QSDIR
      DOUBLE PRECISION QSOLAR,QSOLR,QSRSB,QSSKY,QVENTR,RESULTS
     
      DIMENSION RESULTS(7)

C     COMPUTING SOLAR ENERGY ABSORBED (W):
C     NOTE THAT SOLAR ON THE TOP & BOTTOM HALVES IS WHAT IS SENT OUT TO FUN, NOT THE TOTAL

C     DIRECT BEAM
      QSDIR=ABSAND*(ASIL)*(1.00-PCTDIF)*QNORM*(100.-ASHADE)/100.

C     DIFFUSE COMPONENTS (SKY AND SUBSTRATE)

     
      QSSKY=ABSAND*FASKY*(AREATOTL)*PCTDIF*QSOLR*(100.-ASHADE)/100.
      
C     QSRSB=ABSANV*FAGRD*AREATOTL*(1.0-ABSSB)*QSOLR*(100.-ASHADE)/100.
      QSRSB=ABSANV*(1.-FASKY-FAVEG)*(AREATOTL)*
     & (1.0-ABSSB)*QSOLR*(100.-ASHADE)/100.
     
      QSDIFF = QSSKY + QSRSB

C     TOTAL ENERGY ABSORBED ON TOP & BOTTOM PARTS OF ANIMAL
      QDORSL = QSDIR + QSSKY
      QVENTR = QSRSB
      QSOLAR = QSDIR + QSDIFF

      RESULTS=(/QSOLAR,QSDIR,QSSKY,QSRSB,QSDIFF,QDORSL,QVENTR/)
      RETURN
      END