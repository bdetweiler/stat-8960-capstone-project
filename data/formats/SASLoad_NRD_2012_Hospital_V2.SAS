/*****************************************************************************
 * SASload_NRD_2012_Hospital_V2.SAS
 * Created on 09/28/2017.
 * This program will load the 2012 NRD csv Hospital_V2 File into SAS.
 *****************************************************************************/

/* Create SAS informats for missing values */
PROC FORMAT;
  INVALUE N2PF
    '-9' = .
    '-8' = .A
    '-6' = .C
    '-5' = .N
    OTHER = (|2.|)
  ;
  INVALUE N3PF
    '-99' = .
    '-88' = .A
    '-66' = .C
    OTHER = (|3.|)
  ;
  INVALUE N4PF
    '-999' = .
    '-888' = .A
    '-666' = .C
    OTHER = (|4.|)
  ;
  INVALUE N4P1F
    '-9.9' = .
    '-8.8' = .A
    '-6.6' = .C
    OTHER = (|4.1|)
  ;
  INVALUE N5PF
    '-9999' = .
    '-8888' = .A
    '-6666' = .C
    OTHER = (|5.|)
  ;
  INVALUE N5P2F
    '-9.99' = .
    '-8.88' = .A
    '-6.66' = .C
    OTHER = (|5.2|)
  ;
  INVALUE N6PF
    '-99999' = .
    '-88888' = .A
    '-66666' = .C
    OTHER = (|6.|)
  ;
  INVALUE N6P2F
    '-99.99' = .
    '-88.88' = .A
    '-66.66' = .C
    OTHER = (|6.2|)
  ;
  INVALUE N7P2F
    '-999.99' = .
    '-888.88' = .A
    '-666.66' = .C
    OTHER = (|7.2|)
  ;
  INVALUE N8PF
    '-9999999' = .
    '-8888888' = .A
    '-6666666' = .C
    OTHER = (|8.|)
  ;
  INVALUE N8P2F
    '-9999.99' = .
    '-8888.88' = .A
    '-6666.66' = .C
    OTHER = (|8.2|)
  ;
  INVALUE N8P4F
    '-99.9999' = .
    '-88.8888' = .A
    '-66.6666' = .C
    OTHER = (|8.4|)
  ;
  INVALUE N10PF
    '-999999999' = .
    '-888888888' = .A
    '-666666666' = .C
    OTHER = (|10.|)
  ;
  INVALUE N10P4F
    '-9999.9999' = .
    '-8888.8888' = .A
    '-6666.6666' = .C
    OTHER = (|10.4|)
  ;
  INVALUE N10P5F
    '-999.99999' = .
    '-888.88888' = .A
    '-666.66666' = .C
    OTHER = (|10.5|)
  ;
  INVALUE DATE10F
    '-999999999' = .
    '-888888888' = .A
    '-666666666' = .C
    OTHER = (|MMDDYY10.|)
  ;
  INVALUE N11P7F
    '-99.9999999' = .
    '-88.8888888' = .A
    '-66.6666666' = .C
    OTHER = (|11.7|)
  ;
  INVALUE N12P2F
    '-99999999.99' = .
    '-88888888.88' = .A
    '-66666666.66' = .C
    OTHER = (|12.2|)
  ;
  INVALUE N12P5F
    '-99999.99999' = .
    '-88888.88888' = .A
    '-66666.66666' = .C
    OTHER = (|12.5|)
  ;
  INVALUE N13PF
    '-999999999999' = .
    '-888888888888' = .A
    '-666666666666' = .C
    OTHER = (|13.|)
  ;
  INVALUE N15P2F
    '-99999999999.99' = .
    '-88888888888.88' = .A
    '-66666666666.66' = .C
    OTHER = (|15.2|)
  ;
RUN;

/* Data Step to load the file */
DATA NRD_2012_Hospital_V2; 
INFILE 'NRD_2012_Hospital_V2.csv' dsd dlm=',' LRECL = 66;

/* Define data element attributes */
ATTRIB 
  HOSP_BEDSIZE               LENGTH=3
  LABEL="Bed size of hospital"

  H_CONTRL                   LENGTH=3
  LABEL="Control/ownership of hospital"

  HOSP_NRD                   LENGTH=4
  LABEL="NRD hospital identifier"

  HOSP_URCAT4                LENGTH=3
  LABEL="Hospital urban-rural designation"

  HOSP_UR_TEACH              LENGTH=3
  LABEL="Teaching status of urban hospitals"

  NRD_STRATUM                LENGTH=3
  LABEL="NRD stratum used for weighting"

  N_DISC_U                   LENGTH=5
  LABEL="Number of universe discharges in NRD_STRATUM"

  N_HOSP_U                   LENGTH=3
  LABEL="Number of universe hospitals in NRD_STRATUM"

  S_DISC_U                   LENGTH=4
  LABEL="Number of sample discharges in NRD_STRATUM"

  S_HOSP_U                   LENGTH=4
  LABEL="Number of universe hospitals in NRD_STRATUM"

  TOTAL_DISC                 LENGTH=4
  LABEL="Total hospital discharges"

  YEAR                       LENGTH=3
  LABEL="Calendar year"
  ;

/* Read data elements from the CSV file */
INPUT 
      HOSP_BEDSIZE             :N2PF.
      H_CONTRL                 :N2PF.
      HOSP_NRD                 :5.
      HOSP_URCAT4              :N2PF.
      HOSP_UR_TEACH            :N2PF.
      NRD_STRATUM              :N5PF.
      N_DISC_U                 :N8PF.
      N_HOSP_U                 :N4PF.
      S_DISC_U                 :N8PF.
      S_HOSP_U                 :N6PF.
      TOTAL_DISC               :N6PF.
      YEAR                     :N4PF.
      ;
RUN;
