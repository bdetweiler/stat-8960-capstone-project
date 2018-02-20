/*****************************************************************************
 * SASload_NRD_2014_Core.SAS
 * This program will load the 2014 NRD csv Core File into SAS.
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
DATA NRD_2014_Core; 
INFILE 'NRD_2014_Core.csv' dsd dlm=',' LRECL = 705;

/* Define data element attributes */
ATTRIB 
  AGE                        LENGTH=3
  LABEL="Age in years at admission"

  AWEEKEND                   LENGTH=3
  LABEL="Admission day is a weekend"

  DIED                       LENGTH=3
  LABEL="Died during hospitalization"

  DISCWT                     LENGTH=8
  LABEL="Weight to discharges in AHA universe"

  DISPUNIFORM                LENGTH=3
  LABEL="Disposition of patient (uniform)"

  DMONTH                     LENGTH=3
  LABEL="Discharge month"

  DQTR                       LENGTH=3
  LABEL="Discharge quarter"

  DRG                        LENGTH=3
  LABEL="DRG in effect on discharge date"

  DRGVER                     LENGTH=3
  LABEL="DRG grouper version used on discharge date"

  DRG_NoPOA                  LENGTH=3
  LABEL="DRG in use on discharge date, calculated without POA"

  DX1                        LENGTH=$5
  LABEL="Diagnosis 1"

  DX2                        LENGTH=$5
  LABEL="Diagnosis 2"

  DX3                        LENGTH=$5
  LABEL="Diagnosis 3"

  DX4                        LENGTH=$5
  LABEL="Diagnosis 4"

  DX5                        LENGTH=$5
  LABEL="Diagnosis 5"

  DX6                        LENGTH=$5
  LABEL="Diagnosis 6"

  DX7                        LENGTH=$5
  LABEL="Diagnosis 7"

  DX8                        LENGTH=$5
  LABEL="Diagnosis 8"

  DX9                        LENGTH=$5
  LABEL="Diagnosis 9"

  DX10                       LENGTH=$5
  LABEL="Diagnosis 10"

  DX11                       LENGTH=$5
  LABEL="Diagnosis 11"

  DX12                       LENGTH=$5
  LABEL="Diagnosis 12"

  DX13                       LENGTH=$5
  LABEL="Diagnosis 13"

  DX14                       LENGTH=$5
  LABEL="Diagnosis 14"

  DX15                       LENGTH=$5
  LABEL="Diagnosis 15"

  DX16                       LENGTH=$5
  LABEL="Diagnosis 16"

  DX17                       LENGTH=$5
  LABEL="Diagnosis 17"

  DX18                       LENGTH=$5
  LABEL="Diagnosis 18"

  DX19                       LENGTH=$5
  LABEL="Diagnosis 19"

  DX20                       LENGTH=$5
  LABEL="Diagnosis 20"

  DX21                       LENGTH=$5
  LABEL="Diagnosis 21"

  DX22                       LENGTH=$5
  LABEL="Diagnosis 22"

  DX23                       LENGTH=$5
  LABEL="Diagnosis 23"

  DX24                       LENGTH=$5
  LABEL="Diagnosis 24"

  DX25                       LENGTH=$5
  LABEL="Diagnosis 25"

  DX26                       LENGTH=$5
  LABEL="Diagnosis 26"

  DX27                       LENGTH=$5
  LABEL="Diagnosis 27"

  DX28                       LENGTH=$5
  LABEL="Diagnosis 28"

  DX29                       LENGTH=$5
  LABEL="Diagnosis 29"

  DX30                       LENGTH=$5
  LABEL="Diagnosis 30"

  DXCCS1                     LENGTH=4
  LABEL="CCS: diagnosis 1"

  DXCCS2                     LENGTH=4
  LABEL="CCS: diagnosis 2"

  DXCCS3                     LENGTH=4
  LABEL="CCS: diagnosis 3"

  DXCCS4                     LENGTH=4
  LABEL="CCS: diagnosis 4"

  DXCCS5                     LENGTH=4
  LABEL="CCS: diagnosis 5"

  DXCCS6                     LENGTH=4
  LABEL="CCS: diagnosis 6"

  DXCCS7                     LENGTH=4
  LABEL="CCS: diagnosis 7"

  DXCCS8                     LENGTH=4
  LABEL="CCS: diagnosis 8"

  DXCCS9                     LENGTH=4
  LABEL="CCS: diagnosis 9"

  DXCCS10                    LENGTH=4
  LABEL="CCS: diagnosis 10"

  DXCCS11                    LENGTH=4
  LABEL="CCS: diagnosis 11"

  DXCCS12                    LENGTH=4
  LABEL="CCS: diagnosis 12"

  DXCCS13                    LENGTH=4
  LABEL="CCS: diagnosis 13"

  DXCCS14                    LENGTH=4
  LABEL="CCS: diagnosis 14"

  DXCCS15                    LENGTH=4
  LABEL="CCS: diagnosis 15"

  DXCCS16                    LENGTH=4
  LABEL="CCS: diagnosis 16"

  DXCCS17                    LENGTH=4
  LABEL="CCS: diagnosis 17"

  DXCCS18                    LENGTH=4
  LABEL="CCS: diagnosis 18"

  DXCCS19                    LENGTH=4
  LABEL="CCS: diagnosis 19"

  DXCCS20                    LENGTH=4
  LABEL="CCS: diagnosis 20"

  DXCCS21                    LENGTH=4
  LABEL="CCS: diagnosis 21"

  DXCCS22                    LENGTH=4
  LABEL="CCS: diagnosis 22"

  DXCCS23                    LENGTH=4
  LABEL="CCS: diagnosis 23"

  DXCCS24                    LENGTH=4
  LABEL="CCS: diagnosis 24"

  DXCCS25                    LENGTH=4
  LABEL="CCS: diagnosis 25"

  DXCCS26                    LENGTH=4
  LABEL="CCS: diagnosis 26"

  DXCCS27                    LENGTH=4
  LABEL="CCS: diagnosis 27"

  DXCCS28                    LENGTH=4
  LABEL="CCS: diagnosis 28"

  DXCCS29                    LENGTH=4
  LABEL="CCS: diagnosis 29"

  DXCCS30                    LENGTH=4
  LABEL="CCS: diagnosis 30"

  ECODE1                     LENGTH=$5
  LABEL="E code 1"

  ECODE2                     LENGTH=$5
  LABEL="E code 2"

  ECODE3                     LENGTH=$5
  LABEL="E code 3"

  ECODE4                     LENGTH=$5
  LABEL="E code 4"

  ELECTIVE                   LENGTH=3
  LABEL="Elective versus non-elective admission"

  E_CCS1                     LENGTH=3
  LABEL="CCS: E Code 1"

  E_CCS2                     LENGTH=3
  LABEL="CCS: E Code 2"

  E_CCS3                     LENGTH=3
  LABEL="CCS: E Code 3"

  E_CCS4                     LENGTH=3
  LABEL="CCS: E Code 4"

  FEMALE                     LENGTH=3
  LABEL="Indicator of sex"

  HCUP_ED                    LENGTH=3
  LABEL="HCUP Emergency Department service indicator"

  HOSP_NRD                   LENGTH=4
  LABEL="NRD hospital identifier"

  KEY_NRD                    LENGTH=8
  LABEL="NRD record identifier"

  LOS                        LENGTH=4
  LABEL="Length of stay (cleaned)"

  MDC                        LENGTH=3
  LABEL="MDC in effect on discharge date"

  MDC_NoPOA                  LENGTH=3
  LABEL="MDC in use on discharge date, calculated without POA"

  NCHRONIC                   LENGTH=3
  LABEL="Number of chronic conditions"

  NDX                        LENGTH=3
  LABEL="Number of diagnoses on this record"

  NECODE                     LENGTH=3
  LABEL="Number of E codes on this record"

  NPR                        LENGTH=3
  LABEL="Number of procedures on this record"

  NRD_DaysToEvent            LENGTH=8
  LABEL="Timing variable used to identify days between admissions"

  NRD_STRATUM                LENGTH=3
  LABEL="NRD stratum used for weighting"

  NRD_VisitLink              LENGTH=$7
  LABEL="NRD visitlink"

  ORPROC                     LENGTH=3
  LABEL="Major operating room procedure indicator"

  PAY1                       LENGTH=3
  LABEL="Primary expected payer (uniform)"

  PL_NCHS                    LENGTH=3
  LABEL="Patient Location: NCHS Urban-Rural Code"

  PR1                        LENGTH=$4
  LABEL="Procedure 1"

  PR2                        LENGTH=$4
  LABEL="Procedure 2"

  PR3                        LENGTH=$4
  LABEL="Procedure 3"

  PR4                        LENGTH=$4
  LABEL="Procedure 4"

  PR5                        LENGTH=$4
  LABEL="Procedure 5"

  PR6                        LENGTH=$4
  LABEL="Procedure 6"

  PR7                        LENGTH=$4
  LABEL="Procedure 7"

  PR8                        LENGTH=$4
  LABEL="Procedure 8"

  PR9                        LENGTH=$4
  LABEL="Procedure 9"

  PR10                       LENGTH=$4
  LABEL="Procedure 10"

  PR11                       LENGTH=$4
  LABEL="Procedure 11"

  PR12                       LENGTH=$4
  LABEL="Procedure 12"

  PR13                       LENGTH=$4
  LABEL="Procedure 13"

  PR14                       LENGTH=$4
  LABEL="Procedure 14"

  PR15                       LENGTH=$4
  LABEL="Procedure 15"

  PRCCS1                     LENGTH=3
  LABEL="CCS: procedure 1"

  PRCCS2                     LENGTH=3
  LABEL="CCS: procedure 2"

  PRCCS3                     LENGTH=3
  LABEL="CCS: procedure 3"

  PRCCS4                     LENGTH=3
  LABEL="CCS: procedure 4"

  PRCCS5                     LENGTH=3
  LABEL="CCS: procedure 5"

  PRCCS6                     LENGTH=3
  LABEL="CCS: procedure 6"

  PRCCS7                     LENGTH=3
  LABEL="CCS: procedure 7"

  PRCCS8                     LENGTH=3
  LABEL="CCS: procedure 8"

  PRCCS9                     LENGTH=3
  LABEL="CCS: procedure 9"

  PRCCS10                    LENGTH=3
  LABEL="CCS: procedure 10"

  PRCCS11                    LENGTH=3
  LABEL="CCS: procedure 11"

  PRCCS12                    LENGTH=3
  LABEL="CCS: procedure 12"

  PRCCS13                    LENGTH=3
  LABEL="CCS: procedure 13"

  PRCCS14                    LENGTH=3
  LABEL="CCS: procedure 14"

  PRCCS15                    LENGTH=3
  LABEL="CCS: procedure 15"

  PRDAY1                     LENGTH=4
  LABEL="Number of days from admission to PR1"

  PRDAY2                     LENGTH=4
  LABEL="Number of days from admission to PR2"

  PRDAY3                     LENGTH=4
  LABEL="Number of days from admission to PR3"

  PRDAY4                     LENGTH=4
  LABEL="Number of days from admission to PR4"

  PRDAY5                     LENGTH=4
  LABEL="Number of days from admission to PR5"

  PRDAY6                     LENGTH=4
  LABEL="Number of days from admission to PR6"

  PRDAY7                     LENGTH=4
  LABEL="Number of days from admission to PR7"

  PRDAY8                     LENGTH=4
  LABEL="Number of days from admission to PR8"

  PRDAY9                     LENGTH=4
  LABEL="Number of days from admission to PR9"

  PRDAY10                    LENGTH=4
  LABEL="Number of days from admission to PR10"

  PRDAY11                    LENGTH=4
  LABEL="Number of days from admission to PR11"

  PRDAY12                    LENGTH=4
  LABEL="Number of days from admission to PR12"

  PRDAY13                    LENGTH=4
  LABEL="Number of days from admission to PR13"

  PRDAY14                    LENGTH=4
  LABEL="Number of days from admission to PR14"

  PRDAY15                    LENGTH=4
  LABEL="Number of days from admission to PR15"

  REHABTRANSFER              LENGTH=3
  LABEL="A combined record involving rehab transfer"

  RESIDENT                   LENGTH=3
  LABEL="Patient State is the same as Hospital State"

  SAMEDAYEVENT               LENGTH=$2
  LABEL="Transfer flag indicating combination of discharges involve same day events"

  SERVICELINE                LENGTH=3
  LABEL="Hospital Service Line"

  TOTCHG                     LENGTH=6
  LABEL="Total charges (cleaned)"

  YEAR                       LENGTH=3
  LABEL="Calendar year"

  ZIPINC_QRTL                LENGTH=3
  LABEL="Median household income national quartile for patient ZIP Code"
  ;

/* Read data elements from the CSV file */
INPUT 
      AGE                      :N3PF.
      AWEEKEND                 :N2PF.
      DIED                     :N2PF.
      DISCWT                   :N11P7F.
      DISPUNIFORM              :N2PF.
      DMONTH                   :N2PF.
      DQTR                     :N2PF.
      DRG                      :N3PF.
      DRGVER                   :N2PF.
      DRG_NoPOA                :N3PF.
      DX1                      :$CHAR5.
      DX2                      :$CHAR5.
      DX3                      :$CHAR5.
      DX4                      :$CHAR5.
      DX5                      :$CHAR5.
      DX6                      :$CHAR5.
      DX7                      :$CHAR5.
      DX8                      :$CHAR5.
      DX9                      :$CHAR5.
      DX10                     :$CHAR5.
      DX11                     :$CHAR5.
      DX12                     :$CHAR5.
      DX13                     :$CHAR5.
      DX14                     :$CHAR5.
      DX15                     :$CHAR5.
      DX16                     :$CHAR5.
      DX17                     :$CHAR5.
      DX18                     :$CHAR5.
      DX19                     :$CHAR5.
      DX20                     :$CHAR5.
      DX21                     :$CHAR5.
      DX22                     :$CHAR5.
      DX23                     :$CHAR5.
      DX24                     :$CHAR5.
      DX25                     :$CHAR5.
      DX26                     :$CHAR5.
      DX27                     :$CHAR5.
      DX28                     :$CHAR5.
      DX29                     :$CHAR5.
      DX30                     :$CHAR5.
      DXCCS1                   :N3PF.
      DXCCS2                   :N3PF.
      DXCCS3                   :N3PF.
      DXCCS4                   :N3PF.
      DXCCS5                   :N3PF.
      DXCCS6                   :N3PF.
      DXCCS7                   :N3PF.
      DXCCS8                   :N3PF.
      DXCCS9                   :N3PF.
      DXCCS10                  :N3PF.
      DXCCS11                  :N3PF.
      DXCCS12                  :N3PF.
      DXCCS13                  :N3PF.
      DXCCS14                  :N3PF.
      DXCCS15                  :N3PF.
      DXCCS16                  :N3PF.
      DXCCS17                  :N3PF.
      DXCCS18                  :N3PF.
      DXCCS19                  :N3PF.
      DXCCS20                  :N3PF.
      DXCCS21                  :N3PF.
      DXCCS22                  :N3PF.
      DXCCS23                  :N3PF.
      DXCCS24                  :N3PF.
      DXCCS25                  :N3PF.
      DXCCS26                  :N3PF.
      DXCCS27                  :N3PF.
      DXCCS28                  :N3PF.
      DXCCS29                  :N3PF.
      DXCCS30                  :N3PF.
      ECODE1                   :$CHAR5.
      ECODE2                   :$CHAR5.
      ECODE3                   :$CHAR5.
      ECODE4                   :$CHAR5.
      ELECTIVE                 :N2PF.
      E_CCS1                   :N4PF.
      E_CCS2                   :N4PF.
      E_CCS3                   :N4PF.
      E_CCS4                   :N4PF.
      FEMALE                   :N2PF.
      HCUP_ED                  :N2PF.
      HOSP_NRD                 :5.
      KEY_NRD                  :15.
      LOS                      :N5PF.
      MDC                      :N2PF.
      MDC_NoPOA                :N2PF.
      NCHRONIC                 :N2PF.
      NDX                      :N3PF.
      NECODE                   :N3PF.
      NPR                      :N3PF.
      NRD_DaysToEvent          :N10PF.
      NRD_STRATUM              :N5PF.
      NRD_VisitLink            :$CHAR7.
      ORPROC                   :N2PF.
      PAY1                     :N2PF.
      PL_NCHS                  :N3PF.
      PR1                      :$CHAR4.
      PR2                      :$CHAR4.
      PR3                      :$CHAR4.
      PR4                      :$CHAR4.
      PR5                      :$CHAR4.
      PR6                      :$CHAR4.
      PR7                      :$CHAR4.
      PR8                      :$CHAR4.
      PR9                      :$CHAR4.
      PR10                     :$CHAR4.
      PR11                     :$CHAR4.
      PR12                     :$CHAR4.
      PR13                     :$CHAR4.
      PR14                     :$CHAR4.
      PR15                     :$CHAR4.
      PRCCS1                   :N3PF.
      PRCCS2                   :N3PF.
      PRCCS3                   :N3PF.
      PRCCS4                   :N3PF.
      PRCCS5                   :N3PF.
      PRCCS6                   :N3PF.
      PRCCS7                   :N3PF.
      PRCCS8                   :N3PF.
      PRCCS9                   :N3PF.
      PRCCS10                  :N3PF.
      PRCCS11                  :N3PF.
      PRCCS12                  :N3PF.
      PRCCS13                  :N3PF.
      PRCCS14                  :N3PF.
      PRCCS15                  :N3PF.
      PRDAY1                   :N3PF.
      PRDAY2                   :N3PF.
      PRDAY3                   :N3PF.
      PRDAY4                   :N3PF.
      PRDAY5                   :N3PF.
      PRDAY6                   :N3PF.
      PRDAY7                   :N3PF.
      PRDAY8                   :N3PF.
      PRDAY9                   :N3PF.
      PRDAY10                  :N3PF.
      PRDAY11                  :N3PF.
      PRDAY12                  :N3PF.
      PRDAY13                  :N3PF.
      PRDAY14                  :N3PF.
      PRDAY15                  :N3PF.
      REHABTRANSFER            :N2PF.
      RESIDENT                 :N2PF.
      SAMEDAYEVENT             :$CHAR2.
      SERVICELINE              :N2PF.
      TOTCHG                   :N10PF.
      YEAR                     :N4PF.
      ZIPINC_QRTL              :N2PF.
      ;
RUN;
