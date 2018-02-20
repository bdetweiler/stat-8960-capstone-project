CREATE TABLE nrd_hospital (HOSP_NRD      VARCHAR(5),
                           HOSP_BEDSIZE  VARCHAR(3),
                           H_CONTRL      VARCHAR(3),
                           HOSP_URCAT4   VARCHAR(3),
                           HOSP_UR_TEACH VARCHAR(3),
                           NRD_STRATUM   VARCHAR(5),
                           N_DISC_U      VARCHAR(6),
                           N_HOSP_U      VARCHAR(4),
                           S_DISC_U      VARCHAR(6),
                           S_HOSP_U      VARCHAR(6),
                           TOTAL_DISC    VARCHAR(6),
                           NRD_YEAR      INTEGER,
                           PRIMARY KEY   (HOSP_NRD)
                          );
