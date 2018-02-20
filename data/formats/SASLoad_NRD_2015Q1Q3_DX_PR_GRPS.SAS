/*****************************************************************************
 * SASload_NRD_2015Q1Q3_DX_PR_GRPS.SAS
 * Created on 10/31/2017.
 * This program will load the NRD_2015Q1Q3_DX_PR_GRPS CSV File into SAS.
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
DATA NRD_2015Q1Q3_DX_PR_GRPS; 
INFILE 'NRD_2015Q1Q3_DX_PR_GRPS.csv' dsd dlm=',' LRECL = 1081;

/* Define data element attributes */
ATTRIB 
  BODYSYSTEM1                LENGTH=3
  LABEL="Body system 1"

  BODYSYSTEM2                LENGTH=3
  LABEL="Body system 2"

  BODYSYSTEM3                LENGTH=3
  LABEL="Body system 3"

  BODYSYSTEM4                LENGTH=3
  LABEL="Body system 4"

  BODYSYSTEM5                LENGTH=3
  LABEL="Body system 5"

  BODYSYSTEM6                LENGTH=3
  LABEL="Body system 6"

  BODYSYSTEM7                LENGTH=3
  LABEL="Body system 7"

  BODYSYSTEM8                LENGTH=3
  LABEL="Body system 8"

  BODYSYSTEM9                LENGTH=3
  LABEL="Body system 9"

  BODYSYSTEM10               LENGTH=3
  LABEL="Body system 10"

  BODYSYSTEM11               LENGTH=3
  LABEL="Body system 11"

  BODYSYSTEM12               LENGTH=3
  LABEL="Body system 12"

  BODYSYSTEM13               LENGTH=3
  LABEL="Body system 13"

  BODYSYSTEM14               LENGTH=3
  LABEL="Body system 14"

  BODYSYSTEM15               LENGTH=3
  LABEL="Body system 15"

  BODYSYSTEM16               LENGTH=3
  LABEL="Body system 16"

  BODYSYSTEM17               LENGTH=3
  LABEL="Body system 17"

  BODYSYSTEM18               LENGTH=3
  LABEL="Body system 18"

  BODYSYSTEM19               LENGTH=3
  LABEL="Body system 19"

  BODYSYSTEM20               LENGTH=3
  LABEL="Body system 20"

  BODYSYSTEM21               LENGTH=3
  LABEL="Body system 21"

  BODYSYSTEM22               LENGTH=3
  LABEL="Body system 22"

  BODYSYSTEM23               LENGTH=3
  LABEL="Body system 23"

  BODYSYSTEM24               LENGTH=3
  LABEL="Body system 24"

  BODYSYSTEM25               LENGTH=3
  LABEL="Body system 25"

  BODYSYSTEM26               LENGTH=3
  LABEL="Body system 26"

  BODYSYSTEM27               LENGTH=3
  LABEL="Body system 27"

  BODYSYSTEM28               LENGTH=3
  LABEL="Body system 28"

  BODYSYSTEM29               LENGTH=3
  LABEL="Body system 29"

  BODYSYSTEM30               LENGTH=3
  LABEL="Body system 30"

  CHRON1                     LENGTH=3
  LABEL="Chronic condition indicator 1"

  CHRON2                     LENGTH=3
  LABEL="Chronic condition indicator 2"

  CHRON3                     LENGTH=3
  LABEL="Chronic condition indicator 3"

  CHRON4                     LENGTH=3
  LABEL="Chronic condition indicator 4"

  CHRON5                     LENGTH=3
  LABEL="Chronic condition indicator 5"

  CHRON6                     LENGTH=3
  LABEL="Chronic condition indicator 6"

  CHRON7                     LENGTH=3
  LABEL="Chronic condition indicator 7"

  CHRON8                     LENGTH=3
  LABEL="Chronic condition indicator 8"

  CHRON9                     LENGTH=3
  LABEL="Chronic condition indicator 9"

  CHRON10                    LENGTH=3
  LABEL="Chronic condition indicator 10"

  CHRON11                    LENGTH=3
  LABEL="Chronic condition indicator 11"

  CHRON12                    LENGTH=3
  LABEL="Chronic condition indicator 12"

  CHRON13                    LENGTH=3
  LABEL="Chronic condition indicator 13"

  CHRON14                    LENGTH=3
  LABEL="Chronic condition indicator 14"

  CHRON15                    LENGTH=3
  LABEL="Chronic condition indicator 15"

  CHRON16                    LENGTH=3
  LABEL="Chronic condition indicator 16"

  CHRON17                    LENGTH=3
  LABEL="Chronic condition indicator 17"

  CHRON18                    LENGTH=3
  LABEL="Chronic condition indicator 18"

  CHRON19                    LENGTH=3
  LABEL="Chronic condition indicator 19"

  CHRON20                    LENGTH=3
  LABEL="Chronic condition indicator 20"

  CHRON21                    LENGTH=3
  LABEL="Chronic condition indicator 21"

  CHRON22                    LENGTH=3
  LABEL="Chronic condition indicator 22"

  CHRON23                    LENGTH=3
  LABEL="Chronic condition indicator 23"

  CHRON24                    LENGTH=3
  LABEL="Chronic condition indicator 24"

  CHRON25                    LENGTH=3
  LABEL="Chronic condition indicator 25"

  CHRON26                    LENGTH=3
  LABEL="Chronic condition indicator 26"

  CHRON27                    LENGTH=3
  LABEL="Chronic condition indicator 27"

  CHRON28                    LENGTH=3
  LABEL="Chronic condition indicator 28"

  CHRON29                    LENGTH=3
  LABEL="Chronic condition indicator 29"

  CHRON30                    LENGTH=3
  LABEL="Chronic condition indicator 30"

  DRG                        LENGTH=3
  LABEL="DRG in effect on discharge date"

  DRGVER                     LENGTH=3
  LABEL="DRG grouper version used on discharge date"

  DRG_NoPOA                  LENGTH=3
  LABEL="DRG in use on discharge date, calculated without POA"

  DX1                        LENGTH=$7
  LABEL="Diagnosis 1"

  DX2                        LENGTH=$7
  LABEL="Diagnosis 2"

  DX3                        LENGTH=$7
  LABEL="Diagnosis 3"

  DX4                        LENGTH=$7
  LABEL="Diagnosis 4"

  DX5                        LENGTH=$7
  LABEL="Diagnosis 5"

  DX6                        LENGTH=$7
  LABEL="Diagnosis 6"

  DX7                        LENGTH=$7
  LABEL="Diagnosis 7"

  DX8                        LENGTH=$7
  LABEL="Diagnosis 8"

  DX9                        LENGTH=$7
  LABEL="Diagnosis 9"

  DX10                       LENGTH=$7
  LABEL="Diagnosis 10"

  DX11                       LENGTH=$7
  LABEL="Diagnosis 11"

  DX12                       LENGTH=$7
  LABEL="Diagnosis 12"

  DX13                       LENGTH=$7
  LABEL="Diagnosis 13"

  DX14                       LENGTH=$7
  LABEL="Diagnosis 14"

  DX15                       LENGTH=$7
  LABEL="Diagnosis 15"

  DX16                       LENGTH=$7
  LABEL="Diagnosis 16"

  DX17                       LENGTH=$7
  LABEL="Diagnosis 17"

  DX18                       LENGTH=$7
  LABEL="Diagnosis 18"

  DX19                       LENGTH=$7
  LABEL="Diagnosis 19"

  DX20                       LENGTH=$7
  LABEL="Diagnosis 20"

  DX21                       LENGTH=$7
  LABEL="Diagnosis 21"

  DX22                       LENGTH=$7
  LABEL="Diagnosis 22"

  DX23                       LENGTH=$7
  LABEL="Diagnosis 23"

  DX24                       LENGTH=$7
  LABEL="Diagnosis 24"

  DX25                       LENGTH=$7
  LABEL="Diagnosis 25"

  DX26                       LENGTH=$7
  LABEL="Diagnosis 26"

  DX27                       LENGTH=$7
  LABEL="Diagnosis 27"

  DX28                       LENGTH=$7
  LABEL="Diagnosis 28"

  DX29                       LENGTH=$7
  LABEL="Diagnosis 29"

  DX30                       LENGTH=$7
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

  DXMCCS1                    LENGTH=$11
  LABEL="Multi-Level CCS:  Diagnosis 1"

  DXVER                      LENGTH=3
  LABEL="Diagnosis Version"

  ECODE1                     LENGTH=$7
  LABEL="E code 1"

  ECODE2                     LENGTH=$7
  LABEL="E code 2"

  ECODE3                     LENGTH=$7
  LABEL="E code 3"

  ECODE4                     LENGTH=$7
  LABEL="E code 4"

  E_CCS1                     LENGTH=3
  LABEL="CCS: E Code 1"

  E_CCS2                     LENGTH=3
  LABEL="CCS: E Code 2"

  E_CCS3                     LENGTH=3
  LABEL="CCS: E Code 3"

  E_CCS4                     LENGTH=3
  LABEL="CCS: E Code 4"

  E_MCCS1                    LENGTH=$11
  LABEL="Multi-Level CCS:  E Code 1"

  HOSP_NRD                   LENGTH=4
  LABEL="NRD hospital identifier"

  KEY_NRD                    LENGTH=8
  LABEL="NRD record identifier"

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

  ORPROC                     LENGTH=3
  LABEL="Major operating room procedure indicator"

  PCLASS1                    LENGTH=3
  LABEL="Procedure class 1"

  PCLASS2                    LENGTH=3
  LABEL="Procedure class 2"

  PCLASS3                    LENGTH=3
  LABEL="Procedure class 3"

  PCLASS4                    LENGTH=3
  LABEL="Procedure class 4"

  PCLASS5                    LENGTH=3
  LABEL="Procedure class 5"

  PCLASS6                    LENGTH=3
  LABEL="Procedure class 6"

  PCLASS7                    LENGTH=3
  LABEL="Procedure class 7"

  PCLASS8                    LENGTH=3
  LABEL="Procedure class 8"

  PCLASS9                    LENGTH=3
  LABEL="Procedure class 9"

  PCLASS10                   LENGTH=3
  LABEL="Procedure class 10"

  PCLASS11                   LENGTH=3
  LABEL="Procedure class 11"

  PCLASS12                   LENGTH=3
  LABEL="Procedure class 12"

  PCLASS13                   LENGTH=3
  LABEL="Procedure class 13"

  PCLASS14                   LENGTH=3
  LABEL="Procedure class 14"

  PCLASS15                   LENGTH=3
  LABEL="Procedure class 15"

  PR1                        LENGTH=$7
  LABEL="Procedure 1"

  PR2                        LENGTH=$7
  LABEL="Procedure 2"

  PR3                        LENGTH=$7
  LABEL="Procedure 3"

  PR4                        LENGTH=$7
  LABEL="Procedure 4"

  PR5                        LENGTH=$7
  LABEL="Procedure 5"

  PR6                        LENGTH=$7
  LABEL="Procedure 6"

  PR7                        LENGTH=$7
  LABEL="Procedure 7"

  PR8                        LENGTH=$7
  LABEL="Procedure 8"

  PR9                        LENGTH=$7
  LABEL="Procedure 9"

  PR10                       LENGTH=$7
  LABEL="Procedure 10"

  PR11                       LENGTH=$7
  LABEL="Procedure 11"

  PR12                       LENGTH=$7
  LABEL="Procedure 12"

  PR13                       LENGTH=$7
  LABEL="Procedure 13"

  PR14                       LENGTH=$7
  LABEL="Procedure 14"

  PR15                       LENGTH=$7
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

  PRMCCS1                    LENGTH=$8
  LABEL="Multi-Level CCS:  Procedure 1"

  PRVER                      LENGTH=3
  LABEL="Procedure Version"

  SERVICELINE                LENGTH=3
  LABEL="Hospital Service Line"
  ;

/* Read data elements from the CSV file */
INPUT 
      BODYSYSTEM1              :N3PF.
      BODYSYSTEM2              :N3PF.
      BODYSYSTEM3              :N3PF.
      BODYSYSTEM4              :N3PF.
      BODYSYSTEM5              :N3PF.
      BODYSYSTEM6              :N3PF.
      BODYSYSTEM7              :N3PF.
      BODYSYSTEM8              :N3PF.
      BODYSYSTEM9              :N3PF.
      BODYSYSTEM10             :N3PF.
      BODYSYSTEM11             :N3PF.
      BODYSYSTEM12             :N3PF.
      BODYSYSTEM13             :N3PF.
      BODYSYSTEM14             :N3PF.
      BODYSYSTEM15             :N3PF.
      BODYSYSTEM16             :N3PF.
      BODYSYSTEM17             :N3PF.
      BODYSYSTEM18             :N3PF.
      BODYSYSTEM19             :N3PF.
      BODYSYSTEM20             :N3PF.
      BODYSYSTEM21             :N3PF.
      BODYSYSTEM22             :N3PF.
      BODYSYSTEM23             :N3PF.
      BODYSYSTEM24             :N3PF.
      BODYSYSTEM25             :N3PF.
      BODYSYSTEM26             :N3PF.
      BODYSYSTEM27             :N3PF.
      BODYSYSTEM28             :N3PF.
      BODYSYSTEM29             :N3PF.
      BODYSYSTEM30             :N3PF.
      CHRON1                   :N3PF.
      CHRON2                   :N3PF.
      CHRON3                   :N3PF.
      CHRON4                   :N3PF.
      CHRON5                   :N3PF.
      CHRON6                   :N3PF.
      CHRON7                   :N3PF.
      CHRON8                   :N3PF.
      CHRON9                   :N3PF.
      CHRON10                  :N3PF.
      CHRON11                  :N3PF.
      CHRON12                  :N3PF.
      CHRON13                  :N3PF.
      CHRON14                  :N3PF.
      CHRON15                  :N3PF.
      CHRON16                  :N3PF.
      CHRON17                  :N3PF.
      CHRON18                  :N3PF.
      CHRON19                  :N3PF.
      CHRON20                  :N3PF.
      CHRON21                  :N3PF.
      CHRON22                  :N3PF.
      CHRON23                  :N3PF.
      CHRON24                  :N3PF.
      CHRON25                  :N3PF.
      CHRON26                  :N3PF.
      CHRON27                  :N3PF.
      CHRON28                  :N3PF.
      CHRON29                  :N3PF.
      CHRON30                  :N3PF.
      DRG                      :N3PF.
      DRGVER                   :N2PF.
      DRG_NoPOA                :N3PF.
      DX1                      :$CHAR7.
      DX2                      :$CHAR7.
      DX3                      :$CHAR7.
      DX4                      :$CHAR7.
      DX5                      :$CHAR7.
      DX6                      :$CHAR7.
      DX7                      :$CHAR7.
      DX8                      :$CHAR7.
      DX9                      :$CHAR7.
      DX10                     :$CHAR7.
      DX11                     :$CHAR7.
      DX12                     :$CHAR7.
      DX13                     :$CHAR7.
      DX14                     :$CHAR7.
      DX15                     :$CHAR7.
      DX16                     :$CHAR7.
      DX17                     :$CHAR7.
      DX18                     :$CHAR7.
      DX19                     :$CHAR7.
      DX20                     :$CHAR7.
      DX21                     :$CHAR7.
      DX22                     :$CHAR7.
      DX23                     :$CHAR7.
      DX24                     :$CHAR7.
      DX25                     :$CHAR7.
      DX26                     :$CHAR7.
      DX27                     :$CHAR7.
      DX28                     :$CHAR7.
      DX29                     :$CHAR7.
      DX30                     :$CHAR7.
      DXCCS1                   :N4PF.
      DXCCS2                   :N4PF.
      DXCCS3                   :N4PF.
      DXCCS4                   :N4PF.
      DXCCS5                   :N4PF.
      DXCCS6                   :N4PF.
      DXCCS7                   :N4PF.
      DXCCS8                   :N4PF.
      DXCCS9                   :N4PF.
      DXCCS10                  :N4PF.
      DXCCS11                  :N4PF.
      DXCCS12                  :N4PF.
      DXCCS13                  :N4PF.
      DXCCS14                  :N4PF.
      DXCCS15                  :N4PF.
      DXCCS16                  :N4PF.
      DXCCS17                  :N4PF.
      DXCCS18                  :N4PF.
      DXCCS19                  :N4PF.
      DXCCS20                  :N4PF.
      DXCCS21                  :N4PF.
      DXCCS22                  :N4PF.
      DXCCS23                  :N4PF.
      DXCCS24                  :N4PF.
      DXCCS25                  :N4PF.
      DXCCS26                  :N4PF.
      DXCCS27                  :N4PF.
      DXCCS28                  :N4PF.
      DXCCS29                  :N4PF.
      DXCCS30                  :N4PF.
      DXMCCS1                  :$CHAR11.
      DXVER                    :N2PF.
      ECODE1                   :$CHAR7.
      ECODE2                   :$CHAR7.
      ECODE3                   :$CHAR7.
      ECODE4                   :$CHAR7.
      E_CCS1                   :N4PF.
      E_CCS2                   :N4PF.
      E_CCS3                   :N4PF.
      E_CCS4                   :N4PF.
      E_MCCS1                  :$CHAR11.
      HOSP_NRD                 :5.
      KEY_NRD                  :15.
      MDC                      :N2PF.
      MDC_NoPOA                :N2PF.
      NCHRONIC                 :N2PF.
      NDX                      :N3PF.
      NECODE                   :N3PF.
      NPR                      :N3PF.
      ORPROC                   :N2PF.
      PCLASS1                  :N3PF.
      PCLASS2                  :N3PF.
      PCLASS3                  :N3PF.
      PCLASS4                  :N3PF.
      PCLASS5                  :N3PF.
      PCLASS6                  :N3PF.
      PCLASS7                  :N3PF.
      PCLASS8                  :N3PF.
      PCLASS9                  :N3PF.
      PCLASS10                 :N3PF.
      PCLASS11                 :N3PF.
      PCLASS12                 :N3PF.
      PCLASS13                 :N3PF.
      PCLASS14                 :N3PF.
      PCLASS15                 :N3PF.
      PR1                      :$CHAR7.
      PR2                      :$CHAR7.
      PR3                      :$CHAR7.
      PR4                      :$CHAR7.
      PR5                      :$CHAR7.
      PR6                      :$CHAR7.
      PR7                      :$CHAR7.
      PR8                      :$CHAR7.
      PR9                      :$CHAR7.
      PR10                     :$CHAR7.
      PR11                     :$CHAR7.
      PR12                     :$CHAR7.
      PR13                     :$CHAR7.
      PR14                     :$CHAR7.
      PR15                     :$CHAR7.
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
      PRMCCS1                  :$CHAR8.
      PRVER                    :N2PF.
      SERVICELINE              :N2PF.
      ;
RUN;
