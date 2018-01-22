/******************************************************************/
/* Title:       CCS MULTI-LEVEL LOAD SOFTWARE, VERSION 1.2        */
/*                                                                */
/* PROGRAM:     MULTI_CCS_LOAD_PROGRAM.SAS                        */
/*                                                                */
/* Description: This program creates multi-level CCS categories   */
/*              for data using ICD-9-CM diagnosis or procedure    */
/*              codes. The multi-level CCS categories are an      */
/*              expansion of the single-level CCS.  Some multi-   */
/*              level categories aggregate single-level           */
/*              categories into broader groupings.  Some multi-   */
/*              level categories break down single-level          */
/*              categories into their constituent ICD-9-CM codes. */
/*                                                                */
/*              There are two general sections to this program:   */
/*                                                                */
/*              1) The first section creates temporary SAS        */
/*                 informats using the multi CCS ".csv" file.     */
/*                 These  informats are used in step 2 to create  */
/*                 the multi-level CCS variables. To save         */
/*                 space, we used macros to call the same code    */
/*                 repeatedly for the four formats.               */
/*              2) The second section loops through the diagnosis */
/*                 and/or procedure arrays on your SAS dataset    */
/*                 and assigns the multi-level CCS categories.    */
/*                                                                */
/******************************************************************/
* Path & name of Multi-Level DX tool file ;
FILENAME INRAW1  'C:\CCS\CCS_MULTI_DX_TOOL_2015.CSV' LRECL=300;  
FILENAME INRAW2  'C:\CCS\CCS_MULTI_PR_TOOL_2015.CSV' LRECL=300;  
LIBNAME  IN1     'C:\SASDATA\';                 * Location of input discharge data;
LIBNAME  OUT1    'C:\SASDATA\';                 * Location of output data;

TITLE1 'CREATE CCS MULTI-LEVEL TOOL CATEGORIES';
TITLE2 'USE WITH DISCHARGE ADMINISTRATIVE DATA THAT HAS DIAGNOSIS OR PROCECDURE CODES';

/******************************************************************/
/*  Macro Variables that must be set to define the characteristics*/
/*  of your SAS discharge data. Change these values to match the  */
/*  number of diagnoses and procedures on your dataset. Change    */
/*  CORE to match the name of your dataset.                       */
/******************************************************************/
* Maximum number of DXs on any record;        %LET NUMDX=15;
* Maximum number of PRs on any record;        %LET NUMPR=15;
* Input SAS file member name;                 %LET CORE=YOUR_SAS_FILE_HERE;

%Macro MultiCCS;
/******************* SECTION 1: CREATE INFORMATS ******************/
/*  SAS Load the CCS multi-level tool and convert it into a       */
/*  temporary SAS informat that will be used to assign the multi- */
/*  level CCS variables in the next step.                         */
/******************************************************************/
%macro multidxccs(fmt_,var1_,var2_,var3_,var4_,var5_,var6_,var7_,var8_);
DATA CCS_MULTI_DX;
   INFILE INRAW1 DSD DLM=',' END = EOF FIRSTOBS=2;
	INPUT
	   START       : $CHAR5.
	   &var1_      : $CHAR2.
		&var2_      : $CHAR100.
	   &var3_      : $CHAR5.
		&var4_      : $CHAR100.
	   &var5_      : $CHAR7.
		&var6_      : $CHAR100.
	   &var7_      : $CHAR9.
		&var8_      : $CHAR100.
	;
   RETAIN HLO " ";
   FMTNAME = "&fmt_" ;
   TYPE    = "J" ;
   OUTPUT;

   IF EOF THEN DO ;
      START = " " ;
		LABEL = " " ;
      HLO   = "O";
      OUTPUT ;
   END ;
RUN;

PROC FORMAT LIB=WORK CNTLIN = CCS_MULTI_DX;
RUN;
%mend multidxccs;

%if &NUMDX > 0 %then %do;
%multidxccs($L1DCCS,LABEL,L1L,L2,L2L,L3,L3L,L4,L4L);
%multidxccs($L2DCCS,L1,L1L,LABEL,L2L,L3,L3L,L4,L4L);
%multidxccs($L3DCCS,L1,L1L,L2,L2L,LABEL,L3L,L4,L4L);
%multidxccs($L4DCCS,L1,L1L,L2,L2L,L3,L3L,LABEL,L4L);
%end;

%macro multiprccs(fmt_,var1_,var2_,var3_,var4_,var5_,var6_);
DATA CCS_MULTI_PR ;
   INFILE INRAW2 DSD DLM=',' END = EOF FIRSTOBS=2;
	INPUT
	   START       : $CHAR4.
	   &var1_      : $CHAR2.
		&var2_      : $CHAR100.
	   &var3_      : $CHAR5.
		&var4_      : $CHAR100.
	   &var5_      : $CHAR7.
		&var6_      : $CHAR100.
	;
   RETAIN HLO " ";
   FMTNAME = "&fmt_" ;
   TYPE    = "J" ;
   OUTPUT;

   IF EOF THEN DO ;
      START = " " ;
		LABEL = " " ;
      HLO   = "O";
      OUTPUT ;
   END ;
RUN;

PROC FORMAT LIB=WORK CNTLIN = CCS_MULTI_PR ;
RUN;
%mend multiprccs;

%if &NUMPR > 0 %then %do;
%multiprccs($L1PCCS,LABEL,L1L,L2,L2L,L3,L3L);
%multiprccs($L2PCCS,L1,L1L,LABEL,L2L,L3,L3L);
%multiprccs($L3PCCS,L1,L1L,L2,L2L,LABEL,L3L);
%end;


/*************** SECTION 2: CREATE MULTI-LEVEL CCS CATS ***********/
/*  Create multi-level CCS categories for DX/PR using the SAS     */
/*  informats created above & the SAS file you wish to augment.   */
/*  Users can change the names of the output CCS variables if     */
/*  needed here. It is also important to make sure that the       */
/*  correct diagnosis or procedure names from your SAS file are   */
/*  used in the arrays 'DXS' and 'PRS'.                           */
/******************************************************************/  
DATA OUT1.NEW_MULTI_CCS (DROP = i);
  SET IN1.&CORE;

  %if &NUMDX > 0 %then %do;
  ARRAY L1DCCS  (*)   $5 L1DCCS1-L1DCCS&NUMDX;   * Suggested name for Level 1 Multi-Level DX CCS variables;
  ARRAY L2DCCS  (*)   $5 L2DCCS1-L2DCCS&NUMDX;   * Suggested name for Level 2 Multi-Level DX CCS variables;
  ARRAY L3DCCS  (*)   $7 L3DCCS1-L3DCCS&NUMDX;   * Suggested name for Level 3 Multi-Level DX CCS variables;
  ARRAY L4DCCS  (*)   $9 L4DCCS1-L4DCCS&NUMDX;   * Suggested name for Level 4 Multi-Level DX CCS variables;
  ARRAY DXS     (*)   $  DX1-DX&NUMDX;           * Change diagnosis variable names to match your file;
  %end;

  %if &NUMPR > 0 %then %do;
  ARRAY L1PCCS  (*)   $5 L1PCCS1-L1PCCS&NUMPR;   * Suggested name for Level 1 Multi-Level PR CCS variables;
  ARRAY L2PCCS  (*)   $5 L2PCCS1-L2PCCS&NUMPR;   * Suggested name for Level 2 Multi-Level PR CCS variables;
  ARRAY L3PCCS  (*)   $7 L3PCCS1-L3PCCS&NUMPR;   * Suggested name for Level 3 Multi-Level PR CCS variables;
  ARRAY PRS     (*)   $  PR1-PR&NUMPR;           * Change procedure variable names to match your file;
  %end;
 
  /***************************************************/
  /*  Loop through the diagnosis array on your SAS   */
  /*  dataset and create the multi-level diagnosis   */
  /*  CCS variables.                                 */
  /***************************************************/
  %if &NUMDX > 0 %then %do;
  DO I = 1 TO &NUMDX;
	 L1DCCS(I) = INPUT(DXS(I),$L1DCCS.);
	 L2DCCS(I) = INPUT(DXS(I),$L2DCCS.);
	 L3DCCS(I) = INPUT(DXS(I),$L3DCCS.);
	 L4DCCS(I) = INPUT(DXS(I),$L4DCCS.);
  END;  
  %end;

  /***************************************************/
  /*  Loop through the procedure array on your SAS   */
  /*  dataset and create the multi-level procedure   */
  /*  CCS variables.                                 */
  /***************************************************/
  %if &NUMPR > 0 %then %do;
  DO I = 1 TO &NUMPR;
	 L1PCCS(I) = INPUT(PRS(I),$L1PCCS.);
	 L2PCCS(I) = INPUT(PRS(I),$L2PCCS.);
	 L3PCCS(I) = INPUT(PRS(I),$L3PCCS.);
  END;  
  %end;

RUN;

PROC PRINT DATA=OUT1.NEW_MULTI_CCS (OBS=10);
  %if &NUMDX > 0 %then %do;
     VAR  DX1 L1DCCS1 L2DCCS1 L3DCCS1;
  %end;
  %else %if &NUMPR > 0 %then %do;
     VAR L4DCCS1 PR1 L1PCCS1 L2PCCS1 L3PCCS1;
  %end;
  title2 "Partial Print of the Output Multi-Level CCS File";
RUN;
%Mend MultiCCS;
%MultiCCS;


