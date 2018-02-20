/*****************************************************************************
 * SASload_NRD_2014_DX_PR_GRPS.SAS
 * This program will load the 2014 NRD csv DX_PR_GRPS File into SAS.
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
DATA NRD_2014_DX_PR_GRPS; 
INFILE 'NRD_2014_DX_PR_GRPS.csv' dsd dlm=',' LRECL = 355;

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

  DXMCCS1                    LENGTH=$11
  LABEL="Multi-Level CCS:  Diagnosis 1"

  E_MCCS1                    LENGTH=$11
  LABEL="Multi-Level CCS:  E Code 1"

  HOSP_NRD                   LENGTH=4
  LABEL="NRD hospital identifier"

  KEY_NRD                    LENGTH=8
  LABEL="NRD record identifier"

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

  PRMCCS1                    LENGTH=$8
  LABEL="Multi-Level CCS:  Procedure 1"
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
      DXMCCS1                  :$CHAR11.
      E_MCCS1                  :$CHAR11.
      HOSP_NRD                 :5.
      KEY_NRD                  :15.
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
      PRMCCS1                  :$CHAR8.
      ;
RUN;
