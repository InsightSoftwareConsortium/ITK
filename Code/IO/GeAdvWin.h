#ifndef __GEADVWIN_H__
#define __GEADVWIN_H__
/*
    GE Advantage Windows image file format

    Offsets from start of image header for GE Advantage Windows
    formatted images. Documented below are some important
    header varibles. More are to come as time permits.

    Format of the file is as follows:
    -define variable offset         * Comment - type *
    -define variable length

    The Comment provides a brief description about the header
    variable and the type is one of the following based on a
    32 bit system.
        1. String
        2. Unsigned Short Integer
        3. Short Integer
        4. Integer
        5. Float

    Hopefully these will be beneficial to those concerned.

    Wrtten: Vincent A. Magnotta
    Date:    3/4/96

Note:    GE image file contains two header portions:
        1) Fixed header portion
        2) Variable header portion.
    The total size of the header is given by 3228 + size of variable header.
    The size of the variable header is given at (VARIABLE_HDR_LENGTH).

Modifications:

Date        Initials        Comment
----        --------        -------
8/20/96        VAM            GE Advantage Windows image file consists
                    of two header portions: fixed image header
                    and a variable image header. Added these
                    two definitions into the header file.
*/
#define    SU_ID            0   /* Site id - String  */
#define SU_ID_LEN        4
#define SU_PRODID        7      /* Product ID - String */
#define SU_PRODID_LEN        13
#define EX_SUID            116  /* Exam study ID - String */
#define EX_SUID_LEN        4
#define EX_NO            124    /* Exam Number - Unsigned short Int */
#define EX_NO_LEN        2
#define EX_HOSPNAME        126  /* Hospital Name - String */
#define EX_HOSPNAME_LEN        33
#define EX_MAGSTRENGTH        200       /* Magnet Strength - Integer */
#define EX_MAGSTRENGTH_LEN    4
#define EX_PATID        204     /* Patient ID - String */
#define EX_PATID_LEN        13
#define EX_PATNAME        217   /* Patient Name - String */
#define EX_PATNAME_LEN        25
#define EX_PATAGE        242    /* Patient Age - Short Int */
#define EX_PATAGE_LEN        2
#define EX_PATIAN        244    /* Patient Age Notation - SHort Int */
#define EX_PATIAN_LEN        2
#define EX_PATSEX        246    /* Patient Sex - Short Integer */
#define EX_PATSEX_LEN        2
#define EX_PATWEIGHT        248 /* Patient Weight - Integer */
#define EX_PATWEIGHT_LEN    4
#define EX_HIST            254  /* Patient History - String */
#define EX_HIST_LEN        61
#define EX_DATETIME        328  /* Exam date/time stamp - Integer */
#define EX_DATETIME_LEN        4
#define EX_REFPHY        332    /* Referring Physician - String */
#define EX_REFPHY_LEN        33
#define EX_DIAGRAD        365   /* Diagnostician/Radiologist - String */
#define EX_DIAGRAD_LEN        33
#define EX_OP            398    /* Operator - String */
#define EX_OP_LEN        4
#define EX_DESC            402  /* Exam Description - String */
#define EX_DESC_LEN        23
#define EX_TYP            425   /* Exam Type - String */
#define EX_TYP_LEN        3
#define EX_FORMAT        428    /* Exam Format - Short Integer */
#define EX_FORMAT_LEN        2
#define EX_SYSID        444     /* Creator Suite and Host - String */
#define EX_SYSID_LEN         9

/*** Series Header Variables ***/
#define SE_SUID            1156 /* Suite Id for Series  - String */
#define SE_SUID_LEN        4
#define SE_UNIQ            1160 /* The Make-Unique Flag - Short Integer */
#define SE_UNIQ_LEN         2
#define SE_EXNO            1164 /* Exam Number - Unsigned Short Integer */
#define SE_EXNO_LEN        2
#define SE_NO            1166   /* Series Number - Short Integer */
#define SE_NO_LEN        2
#define SE_DATETIME        1168 /* Date/Time stamp - Integer */
#define SE_DATETIME_LEN        4
#define SE_DESC            1176 /* Series description - String */
#define SE_DESC_LEN        30
#define SE_TYP            1224  /* Series Type - Short Integer */
#define SE_TYP_LEN        2
#define SE_PLANE        1228    /* Most Like Plane  - Short Integer */
#define SE_PLANE_LEN        2
#define SE_POSITION        1232 /* Patient Position - Integer */
#define SE_POSITION_LEN        4
#define SE_ENTRY        1236    /* Patient Entry - Integer */
#define SE_ENTRY_LEN        4
#define SE_ANREF        1240    /* Anatomical reference - String */
#define SE_ANREF_LEN        3
#define SE_CONTRAST        1274 /* Non-zero if contrast - Short Int */
#define SE_CONTRAST_LEN        2
#define SE_START_RAS        1276        /* RAS letter for first scan location - STring */
#define SE_START_RAS_LEN    1
#define SE_START_LOC        1280        /* Start location position - Float */
#define SE_START_LOC_LEN    4
#define SE_END_RAS        1284  /* RAS letter for last scan location - String */
#define SE_END_RAS_LEN        1
#define SE_END_LOC        1288  /* End location position - Float */
#define SE_END_LOC_LEN        4
#define SE_NUMIMAGES        1368        /* Number of Images Existing - Integer */
#define SE_NUMIMAGES_LEN     4

/*** Image Header Variables ***/
#define IM_SUID            2184 /* Suite ID for this image - String */
#define IM_SUID_LEN        4
#define IM_UNIQ            2188 /* The make unique flag - Short Integer */
#define IM_UNIQ_LEN        2
#define IM_EXNO            2192 /* Exam number for this image - Unsigned short */
#define IM_EXNO_LEN        2
#define IM_SENO            2194 /* Series number for image - short integer */
#define IM_SENO_LEN        2
#define IM_NO            2196   /* Image number - short integer */
#define IM_NO_LEN        2
#define IM_DATETIME        2200 /* Image allocation date/time stamp - integer */
#define IM_DATETIME_LEN        4
#define IM_ACTUAL_DT        2204        /* Actual image date/time stamp - Date */
#define IM_ACTUAL_DT_LEN    4
#define IM_SCTIME        2208   /* Duration of scan (secs) - float */
#define IM_SCTIME_LEN        4
#define IM_SLTHICK        2212  /* Slice thickness (mm) - float */
#define IM_SLTHICK_LEN        4
#define IM_IMATRIX_X        2216        /* Image matrix size X - short integer */
#define IM_IMATRIX_X_LEN    2
#define IM_IMATRIX_Y        2218        /* Image matrix size Y - short integer */
#define IM_IMATRIX_Y_LEN    2
#define IM_DFOV            2220 /* Display field of view X (mm) - float */
#define IM_DFOV_LEN        4
#define IM_DFOV_RECT        2224        /* Display field of view Y (mm) if different - float */
#define IM_DFOV_RECT_LEN    4
#define IM_DIM_X        2228    /* Image dimension X - float */
#define IM_DIM_X_LEN        4
#define IM_DIM_Y        2232    /* Image dimension Y - float */
#define IM_DIM_Y_LEN        4
#define IM_PIXSIZE_X        2236        /* Image pixel size X - float */
#define IM_PIXSIZE_X_LEN    4
#define IM_PIXSIZE_Y        2240        /* Image pixel size Y - float */
#define IM_PIXSIZE_Y_LEN    4
#define IM_CONTMODE        2292 /* Image contrast mode - short integer */
#define IM_CONTMODE_LEN        2
#define IM_PLANE        2300    /* Plane type - short integer */
#define IM_PLANE_LEN        2
#define IM_SCANSPACING        2304      /* Spacing between scans (mm) - float */
#define IM_SCANSPACING_LEN    4
#define IM_LOC_RAS        2312  /* RAS letter of image location - string */
#define IM_LOC_RAS_LEN        1
#define IM_LOC            2316  /* Image location - float */
#define IM_LOC_LEN        4
#define IM_ULHC_R              2344     /* R coordinate of upper left corner - float */
#define IM_ULHC_R_LEN        4
#define IM_ULHC_A              2348     /* A coordinate of upper left corner - float */
#define IM_ULHC_A_LEN        4
#define IM_ULHC_S              2352     /* S coordinate of upper left corner - float */
#define IM_ULHC_S_LEN        4
#define IM_URHC_R               2356    /* R coordinate of upper right corner - float */
#define IM_URHC_R_LEN        4
#define IM_URHC_A               2360    /* A coordinate of upper right corner - float */
#define IM_URHC_A_LEN        4
#define IM_URHC_S               2364    /* S coordinate of upper right corner - float */
#define IM_URHC_S_LEN        4
#define IM_BRHC_R               2368    /* R coordinate of bottom right corner - float */
#define IM_BRHC_R_LEN        4
#define IM_BRHC_A               2372    /* A coordinate of bottom right corner - float */
#define IM_BRHC_A_LEN        4
#define IM_BRHC_S               2376    /* S coordinate of bottom right corner - float */
#define IM_BRHC_S_LEN        4
#define IM_TR            2384   /* Pulse repetition time (usec) - integer */
#define IM_TR_LEN        4
#define IM_TI            2388   /* Pulse inversion time (usec) - integer */
#define IM_TI_LEN        4
#define IM_TE            2392   /* Pulse echo time (usec) - integer */
#define IM_TE_LEN        4
#define IM_NUMECHO        2400  /* Number of echoes - short integer */
#define IM_NUMECHO_LEN        2
#define IM_ECHONUM        2402  /* Echo number - short integer */
#define IM_ECHONUM_LEN        2
#define IM_NEX            2408  /* Number of averages - float */
#define IM_NEX_LEN        4
#define IM_CONTIG        2412   /* Continuos slices flag - short integer */
#define IM_CONTIG_LEN        2
#define IM_HRTRATE        2414  /* Cardiac Heart rate (bpm) - short integer */
#define IM_HRTRATE_LEN        2
#define IM_TDEL            2416 /* Delay after trigger (usec) - integer */
#define IM_TDEL_LEN        4
#define IM_XMTGAIN        2438  /* Actual transmit gain (.1 dB) - short integer */
#define IM_XMTGAIN_LEN        2
#define IM_MR_FLIP        2444  /* Flip angle for GRASS scans (dgr) - short integer */
#define IM_MR_FLIP_LEN        2
#define IM_CPHASE        2452   /* Total cardiac phases prescribed - short integer */
#define IM_CPHASE_LEN        2
#define IM_SWAPPF        2454   /* Swap phase/frequency - short integer */
#define IM_SWAPPF_LEN         2
#define IM_OBPLANE        2464  /* Oblique plane - integer */
#define IM_OBPLANE_LEN        4
#define IM_XMTFREQ        2472  /* Transmit frequency - integer */
#define IM_XMTFREQ_LEN        4
#define IM_PRESCAN_R1        2482       /* Prescan R1 value - short integer */
#define IM_PRESCAN_R1_LEN    2
#define IM_PRESCAN_R2        2484       /* Prescan R2 value - short integer */
#define IM_PRESCAN_R2_LEN    2
#define IM_IMODE        2494    /* Imaging mode - short integer */
#define IM_IMODE_LEN        2
#define IM_IOPT            2496 /* Imaging options - integer */
#define IM_IOPT_LEN        4
#define IM_PSEQ            2500 /* Imaging pulse sequence - short integer */
#define IM_PSEQ_LEN        2
#define IM_PSDNAME        2504  /* Pulse sequence name - string */
#define IM_PSDNAME_LEN        33
#define IM_CTYP            2558 /* Coil type - short integer */
#define IM_CTYP_LEN        2
#define IM_CNAME        2560    /* Coil name - string */
#define IM_CNAME_LEN        17
#define IM_SUPP_TECH        2592        /* SAT type FAT/WATER/NONE - short integer */
#define IM_SUPP_TECH_LEN    2
#define IM_VBW            2596  /* Variable bandwidth (Hz) - float */
#define IM_VBW_LEN        4
#define IM_SLQUANT        2600  /* Number of slices in scan group - short integer */
#define IM_SLQUANT_LEN        2
#define IM_USER0        2608    /* User variable 0 - float */
#define IM_USER0_LEN        4
#define IM_USER1        2612    /* User variable 1 - float */
#define IM_USER1_LEN        4
#define IM_USER2        2616    /* User variable 2 - float */
#define IM_USER2_LEN        4
#define IM_USER3        2620    /* User variable 3 - float */
#define IM_USER3_LEN        4
#define IM_USER4        2624    /* User variable 4 - float */
#define IM_USER4_LEN        4
#define IM_USER5        2628    /* User variable 5 - float */
#define IM_USER5_LEN        4
#define IM_USER6        2632    /* User variable 6 - float */
#define IM_USER6_LEN        4
#define IM_USER7        2636    /* User variable 7 - float */
#define IM_USER7_LEN        4
#define IM_USER8        2640    /* User variable 8 - float */
#define IM_USER8_LEN        4
#define IM_USER9        2644    /* User variable 9 - float */
#define IM_USER9_LEN        4
#define IM_USER10        2648   /* User variable 10 - float */
#define IM_USER10_LEN        4
#define IM_USER11        2652   /* User variable 11 - float */
#define IM_USER11_LEN        4
#define IM_USER12        2656   /* User variable 12 - float */
#define IM_USER12_LEN        4
#define IM_USER13        2660   /* User variable 13 - float */
#define IM_USER13_LEN        4
#define IM_USER14        2664   /* User variable 14 - float */
#define IM_USER14_LEN        4
#define IM_USER15        2668   /* User variable 15 - float */
#define IM_USER15_LEN        4
#define IM_USER16        2672   /* User variable 16 - float */
#define IM_USER16_LEN        4
#define IM_USER17        2676   /* User variable 17 - float */
#define IM_USER17_LEN        4
#define IM_USER18        2680   /* User variable 18 - float */
#define IM_USER18_LEN        4
#define IM_USER19        2684   /* User variable 19 - float */
#define IM_USER19_LEN        4
#define IM_USER20        2688   /* User variable 20 - float */
#define IM_USER20_LEN        4
#define IM_USER21        2692   /* User variable 21 - float */
#define IM_USER21_LEN        4
#define IM_USER22        2696   /* User variable 22 - float */
#define IM_USER22_LEN        4
#define IM_USER23        2700   /* User variable 23 - float */
#define IM_USER23_LEN        4
#define IM_USER24        2704   /* User variable 24 - float */
#define IM_USER24_LEN        4
#define IM_SATBITS        2756  /* Bitmap of sat selections - short integer */
#define IM_SATBITS_LEN        2
#define IM_SCIC            2758 /* Surface coil intensity correction flag - short integer */
#define IM_SCIC_LEN        2
#define IM_FLAX            2778 /* Phase contrast flow analysis - short integer */
#define IM_FLAX_LEN        2
#define IM_VENC            2780 /* Phase contrast flow encoding - short integer */
#define IM_VENC_LEN        2
#define IM_THK_DISCLMR        2782      /* Slice thickness - short integer */
#define IM_THK_DISCLMR_LEN    2
#define IM_VAS_COLLAPSE        2790     /* Collapse image - short integer */
#define IM_VAS_COLLAPSE_LEN    2
#define IM_X_AXIS_ROT        2816       /* X-axis rotation - float */
#define IM_X_AXIS_ROT_LEN    4
#define IM_Y_AXIS_ROT        2820       /* Y-axis rotation - float */
#define IM_Y_AXIS_ROT_LEN    4
#define IM_Z_AXIS_ROT        2824       /* Z-axis rotation - float */
#define IM_Z_AXIS_ROT_LEN    4
#define IM_ECHO_TRN        2844 /* Length of echo train - short integer */
#define IM_ECHO_TRN_LEN        2
#define IM_FRAC_ECHO        2846        /* Fractional echo - short integer */
#define IM_FRAC_ECHO_LEN    2
#define IM_PREP_PULSE        2848       /* Prep pulses - short integer */
#define IM_PREP_PULSE_LEN    2
#define IM_CPHASENUM        2850        /* Cardiac phase number - short integer */
#define IM_CPHASENUM_LEN    2
#define IM_VAR_ECHO        2852 /* Variable echo - short integer */
#define IM_VAR_ECHO_LEN        2
#define IM_FREQ_DIR        2948 /* Frequency direction - short integer */
#define IM_FREQ_DIR_LEN        2
#define IM_VMODE        2950    /* Vascular mode - short integer */
#define IM_VMODE_LEN        2

#define FIXED_HDR_LENGTH    3228        /* Total Length of the Fixed header */
#define VARIABLE_HDR_LENGTH_LEN 4
#define VARIABLE_HDR_LENGTH    3232     /* Size of variable header - integer */
#endif
