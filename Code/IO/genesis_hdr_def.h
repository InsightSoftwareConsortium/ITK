#ifndef __GENESIS_HDR_DEF_H__
#define __GENESIS_HDR_DEF_H__
/***************************************************************************
FILE GENESIS_HDR_DEF.H

CONVERTED TO CHARACTER OFFSETS SINCE GENESIS STRUCTURE DEFINITIONS ARE
INCOMPATABLE WITH SUN4

***************************************************************************/
/*
SUITE HEADER
*/
#ifdef __cplusplus
extern "C"
{
#endif
#define SU_ID 0                 /*SUITE ID */
#define SU_UNIQ 4               /*MAKE UNIQUE FLAG */
#define SU_DISKID 6             /*DISK ID */
#define SU_PRODID 7             /*PRODUCT ID */
#define SU_VERSCRE 20           /*GENESIS VERSION OF RECORD */
#define SU_VERSCUR 22           /*GENESIS VERSION OF RECORD */
#define SU_CHECKSUM 24          /*SUITE RECORD CHECKSUM */
#define SU_HDR_LEN 114
/*
EXAM HEADER
*/
#define EX_SUID 0               /*SUITE ID FOR THIS EXAM */
#define EX_UNIQ 4               /*THE MAKE-UNIQUE FLAG */
#define EX_DISKID 6             /*DISK ID FOR THIS EXAM */
#define EX_NO 8                 /*EXAM NUMBER */
#define EX_HOSPNAME 10          /*HOSPITAL NAME */
#define EX_DETECT 44            /*DETECTOR TYPE */
#define EX_NUMCELLS 46          /*NUMBER OF CELLS IN DET */
#define EX_ZEROCELL 50          /*CELL NUMBER AT THETA */
#define EX_CELLSPACE 54         /*CELL SPACING */
#define EX_SRCTODET 58          /*DISTANCE FROM SOURCE TO DETECTOR */
#define EX_SRCTOISO 62          /*DISTANCE FROM SOURCE TO ISO */
#define EX_TUBETYP 66           /*TUBE TYPE */
#define EX_DASTYP 68            /*DAS TYPE */
#define EX_NUM_DCNK 70          /*NUMBER OF DECON KERNALS */
#define EX_DCN_LEN 72           /*NUMBER OF ELEMENTS IN A DECON KERNAL */
#define EX_DCN_DENSITY 74       /*DECON KERNAL DENSITY */
#define EX_DCN_STEPSIZE 76      /*DECON KERNAL STEPSIZE */
#define EX_DCN_SHIFTCNT 78      /*DECON KERNAL SHIFT COUNT */
#define EX_MAGSTRENGTH 80       /*MAGNET STRENGTH (IN GAUSS) */
#define EX_PATID 84             /*PATIENT ID FOR THIS EXAM */
#define EX_PATNAME 97           /*PATIENT NAME */
#define EX_PATAGE 122           /*PATIENT AGE (YEARS, MONTHS OR DAYS) */
#define EX_PATIAN 124           /*PATIENT AGE NOTATION */
#define EX_PATSEX 126           /*PATIENT SEX */
#define EX_PATWEIGHT 128        /*PATIENT WEIGHT */
#define EX_TRAUMA 132           /*TRAUMA FLAG */
#define EX_HIST 134             /*PATIENT HISTORY */
#define EX_REQNUM 195           /*REQUISITION NUMBER */
#define EX_DATETIME 208         /*EXAM DATE/TIME STAMP */
#define EX_REFPHY 212           /*REFERRING PHYSICIAN */
#define EX_DIAGRAD 245          /*DIAGNOSTICIAN/RADIOLOGIST */
#define EX_OP 278 /*OPERATOR*/
#define EX_DESC 282             /*EXAM DESCRIPTION */
#define EX_TYP 305              /*EXAM TYPE */
#define EX_FORMAT 308           /*EXAM FORMAT */
#define EX_FIRSTAXTIME 310      /*START TIME(SECS) OF FIRST AXIAL IN EXAM */
#define EX_SYSID 318            /*CREATOR SUITE AND HOST */
#define EX_LASTMOD 328          /*DATE/TIME OF LAST CHANGE */
#define EX_PROTOCOLFLAG 332     /*NON-ZERO INDICATES PROTOCOL EXAM */
#define EX_ALLOC_KEY 334        /*PROCESS THAT ALLOCATED THIS RECORD */
#define EX_DELTA_CNT 348        /*INDICATES NUMBER OF UPDATES TO HEADER */
#define EX_VERSCRE 352          /*GENESIS VERSION - CREATED */
#define EX_VERSCUR 354          /*GENESIS VERSION - NOW */
#define EX_CHECKSUM 356         /*EXAM RECORD CHECKSUM */
#define EX_COMPLETE 360         /*EXAM COMPLETE FLAG */
#define EX_SERIESCT 364         /*LAST SERIES NUMBER USED */
#define EX_NUMARCH 368          /*NUMBER OF SERIES ARCHIVED */
#define EX_NUMSERIES 372        /*NUMBER OF SERIES EXISTING */
#define EX_SERIES 376           /*SERIES KEYS FOR THIS EXAM */
#define EX_NUMUNSER 384         /*NUMBER OF UNSTORED SERIES */
#define EX_UNSERIES 388         /*UNSTORED SERIES KEYS FOR THIS EXAM */
#define EX_TOARCHCNT 396        /*NUMBER OF UNARCHIVED SERIES */
#define EX_TOARCHIVE 400        /*UNARCHIVED SERIES KEYS FOR THIS EXAM */
#define EX_PROSPCNT 408         /*NUMBER OF PROSPECTIVE/SCOUT SERIES */
#define EX_PROSP 421            /*PROSPECTIVE/SCOUT SERIES KEYS FOR THIS EXAM */
#define EX_MODELNUM 420         /*LAST MODEL NUMBER USED */
#define EX_MODELCNT 424         /*NUMBER OF THREED MODELS */
#define EX_MODELS 428           /*THREED MODEL KEYS FOR EXAM */
#define EX_STAT 436             /*PATIENT STATUS */
#define EX_UNIQUE_SYS_ID 438    /* UNIQUE SYSTEM ID */
#define EX_SERVICE_ID 454       /* UNIQUE SERVICE ID */
#define EX_MOBILE_LOC 470       /* MOBILE LOCATION LOCATION */
#define EX_HDR_LEN 1024
#define SE_SUID 0               /*SUITE ID FOR THIS SERIES */
#define SE_UNIQ 4               /*THE MAKE-UNIQUE FLAG */
#define SE_DISKID 6             /*DISK ID FOR THIS SERIES */
#define SE_EXNO 8               /*EXAM NUMBER */
#define SE_NO 10                /*SERIES NUMBER */
#define SE_DATETIME 12          /*ALLOCATION SERIES DATA/TIME STAMP */
#define SE_ACTUAL_DT 16         /*ACTUAL SERIES DATA/TIME STAMP */
#define SE_DESC 20              /*SERIES DESCRIPTION */
#define SE_PR_SYSID 50          /*PRIMARY RECEIVER SUITE AND HOST */
#define SE_PANSYSID 59          /*ARCHIVER SUITE AND HOST */
#define SE_TYP 68               /*SERIES TYPE */
#define SE_SOURCE 70            /*SERIES FROM WHICH PRESCRIBED */
#define SE_PLANE 72             /*MOST-LIKE PLANE (FOR L/S) */
#define SE_SCAN_TYPE 74         /*SCOUT OR AXIAL (FOR CT) */
#define SE_POSITION 76          /*PATIENT POSITION */
#define SE_ENTRY 80             /*PATIENT ENTRY */
#define SE_ANREF 84             /*ANATOMICAL REFERENCE */
#define SE_LMHOR 88             /*HORIZONTAL LANDMARK */
#define SE_PRTCL 92             /*SCAN PROTOCOL NAME */
#define SE_CONTRAST 118         /*NON-ZERO IF > 0 IMAGE USED CONTRAST(L/S) */
#define SE_START_RAS 120        /*RAS LETTER FOR FIRST SCAN LOCATION (L/S) */
#define SE_START_LOC 122        /*FIRST SCAN LOCATION (L/S) */
#define SE_END_RAS 126          /*RAS LETTER FOR LAST SCAN LOCATION (L/S) */
#define SE_END_LOC 128          /*LAST SCAN LOCATION (L/S) */
#define SE_PSEQ 132             /*LAST PULSE SEQUENCE USED (L/S) */
#define SE_SORTORDER 134        /*IMAGE SORT ORDER (L/S) */
#define SE_LNDMRKCNT 136        /*LANDMARK COUNTER */
#define SE_NACQ 140             /*NUMBER OF ACQUISITIONS */
#define SE_XBASEST 142          /*STARTING NUMBER FOR BASELINES */
#define SE_XBASEEND 144         /*ENDING NUMBER FOR BASELINES */
#define SE_XENHST 146           /*STARTING NUMBER FOR ENHANCED SCANS */
#define SE_XENHEND 148          /*ENDING NUMBER FOR ENHANCED SCANS */
#define SE_LASTMOD 150          /*DATE/TIME OF LAST CHANGE */
#define SE_ALLOC_KEY 154        /*PROCESS THAT ALLOCATED THIS RECORD */
#define SE_DELTA_CNT 168        /*INDICATES NUMBER OF UPDATES TO HEADER */
#define SE_VERSCRE 172          /*GENESIS VERSION - CREATED */
#define SE_VERSCUR 174          /*GENESIS VERSION - NOW */
#define SE_PDS_A 176            /*PIXELDATA SIZE - AS STORED */
#define SE_PDS_C 180            /*PIXELDATA SIZE - COMPRESSED */
#define SE_PDS_U 184            /*PIXELDATA SIZE - UNCOMPRESSED */
#define SE_CHECKSUM 188         /*SERIES RECORD CHECKSUM */
#define SE_COMPLETE 192         /*SERIES COMPLETE FLAG */
#define SE_NUMARCH 196          /*NUMBER OF IMAGES ARCHIVED */
#define SE_IMAGECT 200          /*LAST IMAGE NUMBER USED */
#define SE_NUMIMAGES 204        /*NUMBER OF IMAGES EXISTING */
#define SE_IMAGES 208           /*IMAGE KEYS FOR THIS SERIES */
#define SE_NUMUNIMG 216         /*NUMBER OF UNSTORED IMAGES */
#define SE_UNIMAGES 220         /*UNSTORED IMAGE KEYS FOR THIS SERIES */
#define SE_TOARCHCNT 228        /*NUMBER OF UNARCHIVED IMAGES */
#define SE_TOARCHIVE 232        /*UNARCHIVED IMAGE KEYS FOR THIS SERIES */
#define SE_ECHO1_ALPHA 240      /*ECHO 1 ALPHA VALUE */
#define SE_ECHO1_BETA 244       /*ECHO 1 BETA VALUE */
#define SE_ECHO1_WINDOW 248     /*ECHO 1 WINDOW VALUE */
#define SE_ECHO1_LEVEL 250      /*ECHO 1 LEVEL VALUE */
#define SE_ECHO2_ALPHA 252      /*ECHO 2 ALPHA VALUE */
#define SE_ECHO2_BETA 256       /*ECHO 2 BETA VALUE */
#define SE_ECHO2_WINDOW 260     /*ECHO 2 WINDOW VALUE */
#define SE_ECHO2_LEVEL 262      /*ECHO 2 LEVEL VALUE */
#define SE_ECHO3_ALPHA 264      /*ECHO 3 ALPHA VALUE */
#define SE_ECHO3_BETA 268       /*ECHO 3 BETA VALUE */
#define SE_ECHO3_WINDOW 272     /*ECHO 3 WINDOW VALUE */
#define SE_ECHO3_LEVEL 274      /*ECHO 3 LEVEL VALUE */
#define SE_ECHO4_ALPHA 276      /*ECHO 4 ALPHA VALUE */
#define SE_ECHO4_BETA 280       /*ECHO 4 BETA VALUE */
#define SE_ECHO4_WINDOW 284     /*ECHO 4 WINDOW VALUE */
#define SE_ECHO4_LEVEL 286      /*ECHO 4 LEVEL VALUE */
#define SE_ECHO5_ALPHA 288      /*ECHO 5 ALPHA VALUE */
#define SE_ECHO5_BETA 292       /*ECHO 5 BETA VALUE */
#define SE_ECHO5_WINDOW 296     /*ECHO 5 WINDOW VALUE */
#define SE_ECHO5_LEVEL 298      /*ECHO 5 LEVEL VALUE */
#define SE_ECHO6_ALPHA 300      /*ECHO 6 ALPHA VALUE */
#define SE_ECHO6_BETA 304       /*ECHO 6 BETA VALUE */
#define SE_ECHO6_WINDOW 308     /*ECHO 6 WINDOW VALUE */
#define SE_ECHO6_LEVEL 310      /*ECHO 6 LEVEL VALUE */
#define SE_ECHO7_ALPHA 312      /*ECHO 7 ALPHA VALUE */
#define ECHO7_BETA 316          /*ECHO 7 BETA VALUE */
#define ECHO7_WINDOW 320        /*ECHO 7 WINDOW VALUE */
#define ECHO7_LEVEL  322        /*ECHO 7 LEVEL VALUE */
#define ECHO8_ALPHA 324         /*ECHO 8 ALPHA VALUE */
#define ECHO8_BETA 328          /*ECHO 8 BETA VALUE */
#define ECHO8_WINDOW 332        /*ECHO 8 WINDOW VALUE */
#define ECHO8_LEVEL 334         /*ECHO 8 LEVEL VALUE */
#define SE_HDR_LEN 1020
/*
image header
*/
#define IM_SUID 0               /*SUITE ID FOR THIS IMAGE */
#define IM_UNIQ 4               /*THE MAKE-UNIQUE FLAG */
#define IM_DISKID 6             /*DISK ID FOR THIS IMAGE */
#define IM_EXNO 8               /*EXAM NUMBER FOR THIS IMAGE */
#define IM_SENO 10              /*SERIES NUMBER FOR THIS IMAGE */
#define IM_NO 12                /*IMAGE NUMBER */
#define IM_DATETIME 14          /*ALLOCATION IMAGE DATE/TIME STAMP */
#define IM_ACTUAL_DT 18         /*ACTUAL IMAGE DATE/TIME STAMP */
#define IM_SCTIME 22            /*DURATION OF SCAN (SECS) */
#define IM_SLTHICK 26           /*SLICE THICKNESS (MM) */
#define IM_IMATRIX_X 30         /*IMAGE MATRIX SIZE - X */
#define IM_IMATRIX_Y 32         /*IMAGE MATRIX SIZE - Y */
#define IM_DFOV 34              /*DISPLAY FIELD OF VIEW - X (MM) */
#define IM_DFOV_RECT 38         /*DISPLAY FIELD OF VIEW - Y (IF DIFFERENT) */
#define IM_DIM_X 42             /*IMAGE DIMENSION - X */
#define IM_DIM_Y 46             /*IMAGE DIMENSION - Y */
#define IM_PIXSIZE_X 50         /*IMAGE PIXEL SIZE - X */
#define IM_PIXSIZE_Y 54         /*IMAGE PIXEL SIZE - Y */
#define IM_PDID 58              /*PIXEL DATA ID */
#define IM_CONTRASTIV 72        /*IV CONTRAST AGENT */
#define IM_CONTRASTORAL 79      /*ORAL CONTRAST AGENT */
#define IM_CONTMODE 106         /*IMAGE CONTRAST MODE */
#define IM_SERRX 108            /*SERIES FROM WHICH PRESCRIBED */
#define IM_IMGRX 110            /*IMAGE FROM WHICH PRESCRIBED */
#define IM_SCREENFORMAT 112     /*SCREEN FORMAT(8/16 BIT) */
#define IM_PLANE 114            /*PLANE TYPE */
#define IM_SCANSPACING 116      /*SPACING BETWEEN SCANS (MM?) */
#define IM_COMPRESS 120         /*IMAGE COMPRESSION TYPE FOR ALLOCATION */
#define IM_SCOUTTYPE 122        /*SCOUT TYPE (AP OR LATERAL) */
#define IM_LOC_RAS 124          /*RAS LETTER OF IMAGE LOCATION */
#define IM_LOC 126              /*IMAGE LOCATION */
#define IM_CTR_R 130            /*CENTER R COORD OF PLANE IMAGE */
#define IM_CTR_A 134            /*CENTER A COORD OF PLANE IMAGE */
#define IM_CTR_S 138            /*CENTER S COORD OF PLANE IMAGE */
#define IM_NORM_R 142           /*NORMAL R COORD */
#define IM_NORM_A 146           /*NORMAL A COORD */
#define IM_NORM_S 150           /*NORMAL S COORD */
#define IM_TLHC_R 154           /*R COORD OF TOP LEFT HAND CORNER */
#define IM_TLHC_A 158           /*A COORD OF TOP LEFT HAND CORNER */
#define IM_TLHC_S 162           /*S COORD OF TOP LEFT HAND CORNER */
#define IM_TRHC_R 166           /*R COORD OF TOP RIGHT HAND CORNER */
#define IM_TRHC_A 170           /*A COORD OF TOP RIGHT HAND CORNER */
#define IM_TRHC_S 174           /*S COORD OF TOP RIGHT HAND CORNER */
#define IM_BRHC_R 178           /*R COORD OF BOTTOM RIGHT HAND CORNER */
#define IM_BRHC_A 182           /*A COORD OF BOTTOM RIGHT HAND CORNER */
#define IM_BRHC_S 186           /*S COORD OF BOTTOM RIGHT HAND CORNER */
#define IM_FORIMGREV 190        /*FOREIGN IMAGE REVISION */
#define IM_TR 194               /*PULSE REPETITION TIME(USEC) */
#define IM_TI 198               /*PULSE INVERSION TIME(USEC) */
#define IM_TE 202               /*PULSE ECHO TIME(USEC) */
#define IM_TE2 206              /*SECOND ECHO ECHO (USEC) */
#define IM_NUMECHO 210          /*NUMBER OF ECHOES */
#define IM_ECHONUM 212          /*ECHO NUMBER */
#define IM_TBLDLTA 214          /*TABLE DELTA */
#define IM_NEX 218              /*NUMBER OF EXCITATIONS */
#define IM_CONTIG 222           /*CONTINUOUS SLICES FLAG */
#define IM_HRTRATE 224          /*CARDIAC HEART RATE (BPM) */
#define IM_TDEL 226             /*DELAY TIME AFTER TRIGGER (USEC) */
#define IM_SARAVG 230           /*AVERAGE SAR */
#define IM_SARPEAK 234          /*PEAK SAR */
#define IM_MONSAR 238           /*MONITOR SAR FLAG */
#define IM_TRGWINDOW 240        /*TRIGGER WINDOW (% OF R-R INTERVAL) */
#define IM_REPTIME 242          /*CARDIAC REPETITION TIME */
#define IM_IMGPCYC 246          /*IMAGES PER CARDIAC CYCLE */
#define IM_XMTGAIN 248          /*ACTUAL TRANSMIT GAIN (.1 DB) */
#define IM_RCVGAIN1 250         /*ACTUAL RECEIVE GAIN ANALOG (.1 DB) */
#define IM_RCVGAIN2 252         /*ACTUAL RECEIVE GAIN DIGITAL (.1 DB) */
#define IM_MR_FLIP 254          /*FLIP ANGLE FOR GRASS SCANS (DEG.) */
#define IM_MINDAT 256           /*MINIMUM DELAY AFTER TRIGGER (USEC) */
#define IM_CPHASE 260           /*TOTAL CARDIAC PHASE PRESCRIBED */
#define IM_SWAPPF 262           /*SWAP PHASE/FREQUENCY AXIS */
#define IM_PAUSEINT 264         /*PAUSE INTERVAL (SLICES) */
#define IM_PAUSETIME 266        /*PAUSE TIME */
#define IM_OBPLANE 270          /*OBLIQUE PLANE */
#define IM_SLOCFOV 274          /*SLICE OFFSETS ON FREQ AXIS */
#define IM_XMTFREQ 278          /*CENTER FREQUENCY (0.1 HZ) */
#define IM_AUTOXMTFREQ 282      /*AUTO CENTER FREQUENCY (0.1 HZ) */
#define IM_AUTOXMTGAIN 286      /*AUTO TRANSMIT GAIN (0.1 DB) */
#define IM_PRESCAN_R1 288       /*PRESCAN R1 - ANALOG */
#define IM_PRESCAN_R2 290       /*PRESCAN R2 - DIGITAL */
#define IM_USER_BITMAP 292      /*BITMAP DEFINING USER CVS */
#define IM_CENFREQ 296          /*CENTER FREQUENCY METHOD */
#define IM_IMODE 298            /*IMAGING MODE */
#define IM_IOPT 300             /*IMAGING OPTIONS */
#define IM_PSEQ 304             /*PULSE SEQUENCE */
#define IM_PSEQMODE 306         /*PULSE SEQUENCE MODE */
#define IM_PSDNAME 308          /*PULSE SEQUENCE NAME */
#define IM_PSD_DATETIME 342     /*PSD CREATION DATE AND TIME */
#define IM_PSD_INAME 346        /*PSD NAME FROM INSIDE PSD */
#define IM_CTYP 360             /*COIL TYPE */
#define IM_CNAME 362            /*COIL NAME */
#define IM_SURFCTYP 380         /*SURFACE COIL TYPE */
#define IM_SURFCEXT 382         /*EXTREMITY COIL FLAG */
#define IM_RAWRUNNUM 384        /*RAWDATA RUN NUMBER */
#define IM_CAL_FLDSTR 388       /*CALIBRATED FIELD STRENGTH (X10 UGAUSS) */
#define IM_SUPP_TECH 392        /*SAT FAT/WATER/NONE */
#define IM_VBW 394              /*VARIABLE BANDWIDTH (HZ) */
#define IM_SLQUANT 398          /*NUMBER OF SLICES IN THIS SCAN GROUP */
#define IM_GPRE 400             /*GRAPHICALLY PRESCRIBED */
#define IM_INTR_DEL 402         /*INTERIMAGE/INTERLOC DELAY (USEC) */
#define IM_USER0 406            /*USER VARIABLE 0 */
#define IM_USER1 410            /*USER VARIABLE 1 */
#define IM_USER2 414            /*USER VARIABLE 2 */
#define IM_USER3 418            /*USER VARIABLE 3 */
#define IM_USER4 422            /*USER VARIABLE 4 */
#define IM_USER5 426            /*USER VARIABLE 5 */
#define IM_USER6 430            /*USER VARIABLE 6 */
#define IM_USER7 434            /*USER VARIABLE 7 */
#define IM_USER8 438            /*USER VARIABLE 8 */
#define IM_USER9 442            /*USER VARIABLE 9 */
#define IM_USER10 446           /*USER VARIABLE 10 */
#define IM_USER11 450           /*USER VARIABLE 11 */
#define IM_USER12 454           /*USER VARIABLE 12 */
#define IM_USER13 458           /*USER VARIABLE 13 */
#define IM_USER14 462           /*USER VARIABLE 14 */
#define IM_USER15 466           /*USER VARIABLE 15 */
#define IM_USER16 470           /*USER VARIABLE 16 */
#define IM_USER17 474           /*USER VARIABLE 17 */
#define IM_USER18 478           /*USER VARIABLE 18 */
#define IM_USER19 482           /*USER VARIABLE 19 */
#define IM_USER20 486           /*USER VARIABLE 20 */
#define IM_USER21 490           /*USER VARIABLE 21 */
#define IM_USER22 494           /*USER VARIABLE 22 */
#define IM_USER23 498           /*USER VARIABLE 23 */
#define IM_USER24 502           /*USER VARIABLE 24 */
#define IM_ALLOC_KEY 506 /**/
#define IM_LASTMOD 520          /*DATE/TIME OF LAST CHANGE */
#define IM_VERSCRE 524          /*GENESIS VERSION - CREATED */
#define IM_VERSCUR 526          /*GENESIS VERSION - NOW */
#define IM_PDS_A 528            /*PIXELDATA SIZE - AS STORED */
#define IM_PDS_C 532            /*PIXELDATA SIZE - COMPRESSED */
#define IM_PDS_U 536            /*PIXELDATA SIZE - UNCOMPRESSED */
#define IM_CHECKSUM 540         /*ACQRECON RECORD CHECKSUM */
#define IM_ARCHIVED 544         /*IMAGE ARCHIVE FLAG */
#define IM_COMPLETE 548         /*IMAGE COMPLETE FLAG */
#define IM_SATBITS 552          /*BITMAP OF SAT SELECTIONS */
#define IM_SCIC 554             /*SURFACE COIL INTENSITY CORRECTION FLAG */
#define IM_SATXLOC1 556         /*R-SIDE SAT PULSE LOC REL TO LNDMRK */
#define IM_SATXLOC2 558         /*L-SIDE SAT PULSE LOC REL TO LNDMRK */
#define IM_SATYLOC1 560         /*A-SIDE SAT PULSE LOC REL TO LNDMRK */
#define IM_SATYLOC2 562         /*P-SIDE SAT PULSE LOC REL TO LNDMRK */
#define IM_SATZLOC1 566         /*S-SIDE SAT PULSE LOC REL TO LNDMRK */
#define IM_SATZLOC2 568         /*I-SIDE SAT PULSE LOC REL TO LNDMRK */
#define IM_SATXTHICK 570        /*THICKNESS OF X-AXIS SAT PULSE */
#define IM_SATYTHICK 572        /*THICKNESS OF Y-AXIS SAT PULSE */
#define IM_SATZTHICK 574        /*THICKNESS OF Z-AXIS SAT PULSE */
#define IM_FLAX 576             /*PHASE CONTRAST FLOW AXIS */
#define IM_VENC 578             /*PHASE CONTRAST VELOCITY ENCODING */
#define IM_THK_DISCLMR 580      /*SLICE THICKNESS */
#define IM_PS_FLAG 582          /*AUTO/MANUAL PRESCAN FLAG */
#define IM_PS_STATUS 584        /*BITMAP OF CHANGED VALUES */
#define IM_IMAGE_TYPE 586       /*MAGNITUDE, PHASE, IMAGINARY, OR REAL */
#define IM_VAS_COLLAPSE 588     /*COLLAPSE IMAGE */
#define IM_HDR_LEN 1022
/*
these values are relative to the start of the suite information
the suite information starts at some variable location as specified
in the pixel data header at the start of the image file
*/
#define SU_HDR_START 0
#define EX_HDR_START SU_HDR_LEN
#define SE_HDR_START EX_HDR_START+EX_HDR_LEN
#define IM_HDR_START SE_HDR_START+SE_HDR_LEN
#define IMG_HDR_START 0
#ifdef __cplusplus
}
#endif
#endif
