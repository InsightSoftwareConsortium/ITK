/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkGE5ImageIO.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/**
 * \file   Much of the code for this file reader/writer was taken from
 *         the University of Iowa Imaging Group library with the
 *         permission of the authors, Milan Sonka, Joseph Reinhardt,
 *         Ryan Long, Hans Johnson, Gary Christensen, and others.
 *         The specification for this file format is taken from the
 *         web site http://www.mayo.edu/bir/PDF/ANALYZE75.pdf.
 * \author Kent Williams
 *         The University of Iowa 2003
 * \brief This file was written as a modification to the itkMetaImageIO
 *        as a new method for reading in files from the GE4 scanner.
 */

#ifndef __itkGE5ImageIO_h
#define __itkGE5ImageIO_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include <fstream>
#include "itkImageIOBase.h"
#include "itkIPLCommonImageIO.h"
#include "itkGEImageHeader.h"
//#include "idbm_hdr_def.h"

namespace itk
{
/**
   * \author Hans J. Johnson
   * \brief Class that defines how to read GE5 file format.
   * */
class ITK_EXPORT GE5ImageIO : public IPLCommonImageIO
{
public:
  /** Standard class typedefs. */
  typedef GE5ImageIO            Self;
  typedef IPLCommonImageIO  Superclass;
  typedef SmartPointer<Self>  Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GE5ImageIO, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this ImageIO implementation.
       * \author Hans J Johnson
       * \param FileNameToRead The name of the file to test for reading.
       * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
       * \return Returns true if this ImageIO can read the file specified.
       */
  virtual bool CanReadFile(const char* FileNameToRead) ;

  /** Set the spacing and dimension information for the set filename. */
  // Implemented in superclass
  //      virtual void ReadImageInformation();

  /** Get the type of the pixel.  */
  // Implemented in superclass
  //      virtual const std::type_info& GetPixelType() const;

  /** Reads the data from disk into the memory buffer provided. */
  // Implemented in superclass
  //      virtual void Read(void* buffer);

  /** Compute the size (in bytes) of the components of a pixel. For
       * example, and RGB pixel of unsigned char would have a
       * component size of 1 byte. */
  // Implemented in superclass
  //      virtual unsigned int GetComponentSize() const;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine if the file can be written with this ImageIO implementation.
       * \param FileNameToWrite The name of the file to test for writing.
       * \author Hans J. Johnson
       * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
       * \return Returns true if this ImageIO can write the file specified.
       */
  // Implemented in superclass
  //      virtual bool CanWriteFile(const char * FileNameToWrite);

  /** Set the spacing and dimension information for the set filename. */
  // Implemented in superclass
  //      virtual void WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
       * that the IORegions has been set properly. */
  // Implemented in superclass
  //      virtual void Write(const void* buffer);
protected:
  GE5ImageIO();
  ~GE5ImageIO();
  // Implemented in superclass
  //      void PrintSelf(std::ostream& os, Indent indent) const;
  virtual struct GEImageHeader *ReadHeader(const char *FileNameToRead);
private:
  /*
        SUITE HEADER
      */
  enum GE_SU_ENUM {
    SU_ID=0,                  /**<SUITE ID */
    SU_UNIQ=4,                /**<MAKE UNIQUE FLAG */
    SU_DISKID=6,              /**<DISK ID */
    SU_PRODID=7,              /**<PRODUCT ID */
    SU_VERSCRE=20,            /**<GENESIS VERSION OF RECORD */
    SU_VERSCUR=22,            /**<GENESIS VERSION OF RECORD */
    SU_CHECKSUM=24,           /**<SUITE RECORD CHECKSUM */
    SU_HDR_LEN=114
  };
  /*
        EXAM HEADER
      */
  enum GE_EX_ENUM {
    EX_SUID=0,                /**<SUITE ID FOR THIS EXAM */
    EX_UNIQ=4,                /**<THE MAKE-UNIQUE FLAG */
    EX_DISKID=6,              /**<DISK ID FOR THIS EXAM */
    EX_NO=8,                  /**<EXAM NUMBER */
    EX_HOSPNAME=10,           /**<HOSPITAL NAME */
    EX_DETECT=44,             /**<DETECTOR TYPE */
    EX_NUMCELLS=46,           /**<NUMBER OF CELLS IN DET */
    EX_ZEROCELL=50,           /**<CELL NUMBER AT THETA */
    EX_CELLSPACE=54,          /**<CELL SPACING */
    EX_SRCTODET=58,           /**<DISTANCE FROM SOURCE TO DETECTOR */
    EX_SRCTOISO=62,           /**<DISTANCE FROM SOURCE TO ISO */
    EX_TUBETYP=66,            /**<TUBE TYPE */
    EX_DASTYP=68,             /**<DAS TYPE */
    EX_NUM_DCNK=70,           /**<NUMBER OF DECON KERNALS */
    EX_DCN_LEN=72,            /**<NUMBER OF ELEMENTS IN A DECON KERNAL */
    EX_DCN_DENSITY=74,        /**<DECON KERNAL DENSITY */
    EX_DCN_STEPSIZE=76,       /**<DECON KERNAL STEPSIZE */
    EX_DCN_SHIFTCNT=78,       /**<DECON KERNAL SHIFT COUNT */
    EX_MAGSTRENGTH=80,        /**<MAGNET STRENGTH (IN GAUSS) */
    EX_PATID=84,              /**<PATIENT ID FOR THIS EXAM */
    EX_PATNAME=97,            /**<PATIENT NAME */
    EX_PATAGE=122,            /**<PATIENT AGE (YEARS, MONTHS OR DAYS) */
    EX_PATIAN=124,            /**<PATIENT AGE NOTATION */
    EX_PATSEX=126,            /**<PATIENT SEX */
    EX_PATWEIGHT=128,         /**<PATIENT WEIGHT */
    EX_TRAUMA=132,            /**<TRAUMA FLAG */
    EX_HIST=134,              /**<PATIENT HISTORY */
    EX_REQNUM=195,            /**<REQUISITION NUMBER */
    EX_DATETIME=208,          /**<EXAM DATE/TIME STAMP */
    EX_REFPHY=212,            /**<REFERRING PHYSICIAN */
    EX_DIAGRAD=245,           /**<DIAGNOSTICIAN/RADIOLOGIST */
    EX_OP=278,  /**<OPERATOR*/
    EX_DESC=282,              /**<EXAM DESCRIPTION */
    EX_TYP=305,               /**<EXAM TYPE */
    EX_FORMAT=308,            /**<EXAM FORMAT */
    EX_FIRSTAXTIME=310,       /**<START TIME(SECS) OF FIRST AXIAL IN EXAM */
    EX_SYSID=318,             /**<CREATOR SUITE AND HOST */
    EX_LASTMOD=328,           /**<DATE/TIME OF LAST CHANGE */
    EX_PROTOCOLFLAG=332,      /**<NON-ZERO INDICATES PROTOCOL EXAM */
    EX_ALLOC_KEY=334,         /**<PROCESS THAT ALLOCATED THIS RECORD */
    EX_DELTA_CNT=348,         /**<INDICATES NUMBER OF UPDATES TO HEADER */
    EX_VERSCRE=352,           /**<GENESIS VERSION - CREATED */
    EX_VERSCUR=354,           /**<GENESIS VERSION - NOW */
    EX_CHECKSUM=356,          /**<EXAM RECORD CHECKSUM */
    EX_COMPLETE=360,          /**<EXAM COMPLETE FLAG */
    EX_SERIESCT=364,          /**<LAST SERIES NUMBER USED */
    EX_NUMARCH=368,           /**<NUMBER OF SERIES ARCHIVED */
    EX_NUMSERIES=372,         /**<NUMBER OF SERIES EXISTING */
    EX_SERIES=376,            /**<SERIES KEYS FOR THIS EXAM */
    EX_NUMUNSER=384,          /**<NUMBER OF UNSTORED SERIES */
    EX_UNSERIES=388,          /**<UNSTORED SERIES KEYS FOR THIS EXAM */
    EX_TOARCHCNT=396,         /**<NUMBER OF UNARCHIVED SERIES */
    EX_TOARCHIVE=400,         /**<UNARCHIVED SERIES KEYS FOR THIS EXAM */
    EX_PROSPCNT=408,          /**<NUMBER OF PROSPECTIVE/SCOUT SERIES */
    EX_PROSP=421,             /**<PROSPECTIVE/SCOUT SERIES KEYS FOR THIS EXAM */
    EX_MODELNUM=420,          /**<LAST MODEL NUMBER USED */
    EX_MODELCNT=424,          /**<NUMBER OF THREED MODELS */
    EX_MODELS=428,            /**<THREED MODEL KEYS FOR EXAM */
    EX_STAT=436,              /**<PATIENT STATUS */
    EX_UNIQUE_SYS_ID=438,     /**< UNIQUE SYSTEM ID */
    EX_SERVICE_ID=454,        /**< UNIQUE SERVICE ID */
    EX_MOBILE_LOC=470,        /**< MOBILE LOCATION LOCATION */
    EX_HDR_LEN=1024
  };

  enum GE_SE_ENUM {
    SE_SUID=0,                /**<SUITE ID FOR THIS SERIES */
    SE_UNIQ=4,                /**<THE MAKE-UNIQUE FLAG */
    SE_DISKID=6,              /**<DISK ID FOR THIS SERIES */
    SE_EXNO=8,                /**<EXAM NUMBER */
    SE_NO=10,                 /**<SERIES NUMBER */
    SE_DATETIME=12,           /**<ALLOCATION SERIES DATA/TIME STAMP */
    SE_ACTUAL_DT=16,          /**<ACTUAL SERIES DATA/TIME STAMP */
    SE_DESC=20,               /**<SERIES DESCRIPTION */
    SE_PR_SYSID=50,           /**<PRIMARY RECEIVER SUITE AND HOST */
    SE_PANSYSID=59,           /**<ARCHIVER SUITE AND HOST */
    SE_TYP=68,                /**<SERIES TYPE */
    SE_SOURCE=70,             /**<SERIES FROM WHICH PRESCRIBED */
    SE_PLANE=72,              /**<MOST-LIKE PLANE (FOR L/S) */
    SE_SCAN_TYPE=74,          /**<SCOUT OR AXIAL (FOR CT) */
    SE_POSITION=76,           /**<PATIENT POSITION */
    SE_ENTRY=80,              /**<PATIENT ENTRY */
    SE_ANREF=84,              /**<ANATOMICAL REFERENCE */
    SE_LMHOR=88,              /**<HORIZONTAL LANDMARK */
    SE_PRTCL=92,              /**<SCAN PROTOCOL NAME */
    SE_CONTRAST=118,          /**<NON-ZERO IF > 0 IMAGE USED CONTRAST(L/S) */
    SE_START_RAS=120,         /**<RAS LETTER FOR FIRST SCAN LOCATION (L/S) */
    SE_START_LOC=122,         /**<FIRST SCAN LOCATION (L/S) */
    SE_END_RAS=126,           /**<RAS LETTER FOR LAST SCAN LOCATION (L/S) */
    SE_END_LOC=128,           /**<LAST SCAN LOCATION (L/S) */
    SE_PSEQ=132,              /**<LAST PULSE SEQUENCE USED (L/S) */
    SE_SORTORDER=134,         /**<IMAGE SORT ORDER (L/S) */
    SE_LNDMRKCNT=136,         /**<LANDMARK COUNTER */
    SE_NACQ=140,              /**<NUMBER OF ACQUISITIONS */
    SE_XBASEST=142,           /**<STARTING NUMBER FOR BASELINES */
    SE_XBASEEND=144,          /**<ENDING NUMBER FOR BASELINES */
    SE_XENHST=146,            /**<STARTING NUMBER FOR ENHANCED SCANS */
    SE_XENHEND=148,           /**<ENDING NUMBER FOR ENHANCED SCANS */
    SE_LASTMOD=150,           /**<DATE/TIME OF LAST CHANGE */
    SE_ALLOC_KEY=154,         /**<PROCESS THAT ALLOCATED THIS RECORD */
    SE_DELTA_CNT=168,         /**<INDICATES NUMBER OF UPDATES TO HEADER */
    SE_VERSCRE=172,           /**<GENESIS VERSION - CREATED */
    SE_VERSCUR=174,           /**<GENESIS VERSION - NOW */
    SE_PDS_A=176,             /**<PIXELDATA SIZE - AS STORED */
    SE_PDS_C=180,             /**<PIXELDATA SIZE - COMPRESSED */
    SE_PDS_U=184,             /**<PIXELDATA SIZE - UNCOMPRESSED */
    SE_CHECKSUM=188,          /**<SERIES RECORD CHECKSUM */
    SE_COMPLETE=192,          /**<SERIES COMPLETE FLAG */
    SE_NUMARCH=196,           /**<NUMBER OF IMAGES ARCHIVED */
    SE_IMAGECT=200,           /**<LAST IMAGE NUMBER USED */
    SE_NUMIMAGES=204,         /**<NUMBER OF IMAGES EXISTING */
    SE_IMAGES=208,            /**<IMAGE KEYS FOR THIS SERIES */
    SE_NUMUNIMG=216,          /**<NUMBER OF UNSTORED IMAGES */
    SE_UNIMAGES=220,          /**<UNSTORED IMAGE KEYS FOR THIS SERIES */
    SE_TOARCHCNT=228,         /**<NUMBER OF UNARCHIVED IMAGES */
    SE_TOARCHIVE=232,         /**<UNARCHIVED IMAGE KEYS FOR THIS SERIES */
    SE_ECHO1_ALPHA=240,       /**<ECHO 1 ALPHA VALUE */
    SE_ECHO1_BETA=244,        /**<ECHO 1 BETA VALUE */
    SE_ECHO1_WINDOW=248,      /**<ECHO 1 WINDOW VALUE */
    SE_ECHO1_LEVEL=250,       /**<ECHO 1 LEVEL VALUE */
    SE_ECHO2_ALPHA=252,       /**<ECHO 2 ALPHA VALUE */
    SE_ECHO2_BETA=256,        /**<ECHO 2 BETA VALUE */
    SE_ECHO2_WINDOW=260,      /**<ECHO 2 WINDOW VALUE */
    SE_ECHO2_LEVEL=262,       /**<ECHO 2 LEVEL VALUE */
    SE_ECHO3_ALPHA=264,       /**<ECHO 3 ALPHA VALUE */
    SE_ECHO3_BETA=268,        /**<ECHO 3 BETA VALUE */
    SE_ECHO3_WINDOW=272,      /**<ECHO 3 WINDOW VALUE */
    SE_ECHO3_LEVEL=274,       /**<ECHO 3 LEVEL VALUE */
    SE_ECHO4_ALPHA=276,       /**<ECHO 4 ALPHA VALUE */
    SE_ECHO4_BETA=280,        /**<ECHO 4 BETA VALUE */
    SE_ECHO4_WINDOW=284,      /**<ECHO 4 WINDOW VALUE */
    SE_ECHO4_LEVEL=286,       /**<ECHO 4 LEVEL VALUE */
    SE_ECHO5_ALPHA=288,       /**<ECHO 5 ALPHA VALUE */
    SE_ECHO5_BETA=292,        /**<ECHO 5 BETA VALUE */
    SE_ECHO5_WINDOW=296,      /**<ECHO 5 WINDOW VALUE */
    SE_ECHO5_LEVEL=298,       /**<ECHO 5 LEVEL VALUE */
    SE_ECHO6_ALPHA=300,       /**<ECHO 6 ALPHA VALUE */
    SE_ECHO6_BETA=304,        /**<ECHO 6 BETA VALUE */
    SE_ECHO6_WINDOW=308,      /**<ECHO 6 WINDOW VALUE */
    SE_ECHO6_LEVEL=310,       /**<ECHO 6 LEVEL VALUE */
    SE_ECHO7_ALPHA=312,       /**<ECHO 7 ALPHA VALUE */
    ECHO7_BETA=316,           /**<ECHO 7 BETA VALUE */
    ECHO7_WINDOW=320,         /**<ECHO 7 WINDOW VALUE */
    ECHO7_LEVEL=322,         /**<ECHO 7 LEVEL VALUE */
    ECHO8_ALPHA=324,          /**<ECHO 8 ALPHA VALUE */
    ECHO8_BETA=328,           /**<ECHO 8 BETA VALUE */
    ECHO8_WINDOW=332,         /**<ECHO 8 WINDOW VALUE */
    ECHO8_LEVEL=334,          /**<ECHO 8 LEVEL VALUE */
    SE_HDR_LEN=1020
  };
  /*
        image header
      */
  enum GE_IMAGE_HEADER_ENUM {
    IM_SUID=0,                /**<SUITE ID FOR THIS IMAGE */
    IM_UNIQ=4,                /**<THE MAKE-UNIQUE FLAG */
    IM_DISKID=6,              /**<DISK ID FOR THIS IMAGE */
    IM_EXNO=8,                /**<EXAM NUMBER FOR THIS IMAGE */
    IM_SENO=10,               /**<SERIES NUMBER FOR THIS IMAGE */
    IM_NO=12,                 /**<IMAGE NUMBER */
    IM_DATETIME=14,           /**<ALLOCATION IMAGE DATE/TIME STAMP */
    IM_ACTUAL_DT=18,          /**<ACTUAL IMAGE DATE/TIME STAMP */
    IM_SCTIME=22,             /**<DURATION OF SCAN (SECS) */
    IM_SLTHICK=26,            /**<SLICE THICKNESS (MM) */
    IM_IMATRIX_X=30,          /**<IMAGE MATRIX SIZE - X */
    IM_IMATRIX_Y=32,          /**<IMAGE MATRIX SIZE - Y */
    IM_DFOV=34,               /**<DISPLAY FIELD OF VIEW - X (MM) */
    IM_DFOV_RECT=38,          /**<DISPLAY FIELD OF VIEW - Y (IF DIFFERENT) */
    IM_DIM_X=42,              /**<IMAGE DIMENSION - X */
    IM_DIM_Y=46,              /**<IMAGE DIMENSION - Y */
    IM_PIXSIZE_X=50,          /**<IMAGE PIXEL SIZE - X */
    IM_PIXSIZE_Y=54,          /**<IMAGE PIXEL SIZE - Y */
    IM_PDID=58,               /**<PIXEL DATA ID */
    IM_CONTRASTIV=72,         /**<IV CONTRAST AGENT */
    IM_CONTRASTORAL=79,       /**<ORAL CONTRAST AGENT */
    IM_CONTMODE=106,          /**<IMAGE CONTRAST MODE */
    IM_SERRX=108,             /**<SERIES FROM WHICH PRESCRIBED */
    IM_IMGRX=110,             /**<IMAGE FROM WHICH PRESCRIBED */
    IM_SCREENFORMAT=112,      /**<SCREEN FORMAT(8/16 BIT) */
    IM_PLANE=114,             /**<PLANE TYPE */
    IM_SCANSPACING=116,       /**<SPACING BETWEEN SCANS (MM?) */
    IM_COMPRESS=120,          /**<IMAGE COMPRESSION TYPE FOR ALLOCATION */
    IM_SCOUTTYPE=122,         /**<SCOUT TYPE (AP OR LATERAL) */
    IM_LOC_RAS=124,           /**<RAS LETTER OF IMAGE LOCATION */
    IM_LOC=126,               /**<IMAGE LOCATION */
    IM_CTR_R=130,             /**<CENTER R COORD OF PLANE IMAGE */
    IM_CTR_A=134,             /**<CENTER A COORD OF PLANE IMAGE */
    IM_CTR_S=138,             /**<CENTER S COORD OF PLANE IMAGE */
    IM_NORM_R=142,            /**<NORMAL R COORD */
    IM_NORM_A=146,            /**<NORMAL A COORD */
    IM_NORM_S=150,            /**<NORMAL S COORD */
    IM_TLHC_R=154,            /**<R COORD OF TOP LEFT HAND CORNER */
    IM_TLHC_A=158,            /**<A COORD OF TOP LEFT HAND CORNER */
    IM_TLHC_S=162,            /**<S COORD OF TOP LEFT HAND CORNER */
    IM_TRHC_R=166,            /**<R COORD OF TOP RIGHT HAND CORNER */
    IM_TRHC_A=170,            /**<A COORD OF TOP RIGHT HAND CORNER */
    IM_TRHC_S=174,            /**<S COORD OF TOP RIGHT HAND CORNER */
    IM_BRHC_R=178,            /**<R COORD OF BOTTOM RIGHT HAND CORNER */
    IM_BRHC_A=182,            /**<A COORD OF BOTTOM RIGHT HAND CORNER */
    IM_BRHC_S=186,            /**<S COORD OF BOTTOM RIGHT HAND CORNER */
    IM_FORIMGREV=190,         /**<FOREIGN IMAGE REVISION */
    IM_TR=194,                /**<PULSE REPETITION TIME(USEC) */
    IM_TI=198,                /**<PULSE INVERSION TIME(USEC) */
    IM_TE=202,                /**<PULSE ECHO TIME(USEC) */
    IM_TE2=206,               /**<SECOND ECHO ECHO (USEC) */
    IM_NUMECHO=210,           /**<NUMBER OF ECHOES */
    IM_ECHONUM=212,           /**<ECHO NUMBER */
    IM_TBLDLTA=214,           /**<TABLE DELTA */
    IM_NEX=218,               /**<NUMBER OF EXCITATIONS */
    IM_CONTIG=222,            /**<CONTINUOUS SLICES FLAG */
    IM_HRTRATE=224,           /**<CARDIAC HEART RATE (BPM) */
    IM_TDEL=226,              /**<DELAY TIME AFTER TRIGGER (USEC) */
    IM_SARAVG=230,            /**<AVERAGE SAR */
    IM_SARPEAK=234,           /**<PEAK SAR */
    IM_MONSAR=238,            /**<MONITOR SAR FLAG */
    IM_TRGWINDOW=240,         /**<TRIGGER WINDOW (% OF R-R INTERVAL) */
    IM_REPTIME=242,           /**<CARDIAC REPETITION TIME */
    IM_IMGPCYC=246,           /**<IMAGES PER CARDIAC CYCLE */
    IM_XMTGAIN=248,           /**<ACTUAL TRANSMIT GAIN (.1 DB) */
    IM_RCVGAIN1=250,          /**<ACTUAL RECEIVE GAIN ANALOG (.1 DB) */
    IM_RCVGAIN2=252,          /**<ACTUAL RECEIVE GAIN DIGITAL (.1 DB) */
    IM_MR_FLIP=254,           /**<FLIP ANGLE FOR GRASS SCANS (DEG.) */
    IM_MINDAT=256,            /**<MINIMUM DELAY AFTER TRIGGER (USEC) */
    IM_CPHASE=260,            /**<TOTAL CARDIAC PHASE PRESCRIBED */
    IM_SWAPPF=262,            /**<SWAP PHASE/FREQUENCY AXIS */
    IM_PAUSEINT=264,          /**<PAUSE INTERVAL (SLICES) */
    IM_PAUSETIME=266,         /**<PAUSE TIME */
    IM_OBPLANE=270,           /**<OBLIQUE PLANE */
    IM_SLOCFOV=274,           /**<SLICE OFFSETS ON FREQ AXIS */
    IM_XMTFREQ=278,           /**<CENTER FREQUENCY (0.1 HZ) */
    IM_AUTOXMTFREQ=282,       /**<AUTO CENTER FREQUENCY (0.1 HZ) */
    IM_AUTOXMTGAIN=286,       /**<AUTO TRANSMIT GAIN (0.1 DB) */
    IM_PRESCAN_R1=288,        /**<PRESCAN R1 - ANALOG */
    IM_PRESCAN_R2=290,        /**<PRESCAN R2 - DIGITAL */
    IM_USER_BITMAP=292,       /**<BITMAP DEFINING USER CVS */
    IM_CENFREQ=296,           /**<CENTER FREQUENCY METHOD */
    IM_IMODE=298,             /**<IMAGING MODE */
    IM_IOPT=300,              /**<IMAGING OPTIONS */
    IM_PSEQ=304,              /**<PULSE SEQUENCE */
    IM_PSEQMODE=306,          /**<PULSE SEQUENCE MODE */
    IM_PSDNAME=308,           /**<PULSE SEQUENCE NAME */
    IM_PSD_DATETIME=342,      /**<PSD CREATION DATE AND TIME */
    IM_PSD_INAME=346,         /**<PSD NAME FROM INSIDE PSD */
    IM_CTYP=360,              /**<COIL TYPE */
    IM_CNAME=362,             /**<COIL NAME */
    IM_SURFCTYP=380,          /**<SURFACE COIL TYPE */
    IM_SURFCEXT=382,          /**<EXTREMITY COIL FLAG */
    IM_RAWRUNNUM=384,         /**<RAWDATA RUN NUMBER */
    IM_CAL_FLDSTR=388,        /**<CALIBRATED FIELD STRENGTH (X10 UGAUSS) */
    IM_SUPP_TECH=392,         /**<SAT FAT/WATER/NONE */
    IM_VBW=394,               /**<VARIABLE BANDWIDTH (HZ) */
    IM_SLQUANT=398,           /**<NUMBER OF SLICES IN THIS SCAN GROUP */
    IM_GPRE=400,              /**<GRAPHICALLY PRESCRIBED */
    IM_INTR_DEL=402,          /**<INTERIMAGE/INTERLOC DELAY (USEC) */
    IM_USER0=406,             /**<USER VARIABLE 0 */
    IM_USER1=410,             /**<USER VARIABLE 1 */
    IM_USER2=414,             /**<USER VARIABLE 2 */
    IM_USER3=418,             /**<USER VARIABLE 3 */
    IM_USER4=422,             /**<USER VARIABLE 4 */
    IM_USER5=426,             /**<USER VARIABLE 5 */
    IM_USER6=430,             /**<USER VARIABLE 6 */
    IM_USER7=434,             /**<USER VARIABLE 7 */
    IM_USER8=438,             /**<USER VARIABLE 8 */
    IM_USER9=442,             /**<USER VARIABLE 9 */
    IM_USER10=446,            /**<USER VARIABLE 10 */
    IM_USER11=450,            /**<USER VARIABLE 11 */
    IM_USER12=454,            /**<USER VARIABLE 12 */
    IM_USER13=458,            /**<USER VARIABLE 13 */
    IM_USER14=462,            /**<USER VARIABLE 14 */
    IM_USER15=466,            /**<USER VARIABLE 15 */
    IM_USER16=470,            /**<USER VARIABLE 16 */
    IM_USER17=474,            /**<USER VARIABLE 17 */
    IM_USER18=478,            /**<USER VARIABLE 18 */
    IM_USER19=482,            /**<USER VARIABLE 19 */
    IM_USER20=486,            /**<USER VARIABLE 20 */
    IM_USER21=490,            /**<USER VARIABLE 21 */
    IM_USER22=494,            /**<USER VARIABLE 22 */
    IM_USER23=498,            /**<USER VARIABLE 23 */
    IM_USER24=502,            /**<USER VARIABLE 24 */
    IM_ALLOC_KEY=506,  /**<*/
    IM_LASTMOD=520,           /**<DATE/TIME OF LAST CHANGE */
    IM_VERSCRE=524,           /**<GENESIS VERSION - CREATED */
    IM_VERSCUR=526,           /**<GENESIS VERSION - NOW */
    IM_PDS_A=528,             /**<PIXELDATA SIZE - AS STORED */
    IM_PDS_C=532,             /**<PIXELDATA SIZE - COMPRESSED */
    IM_PDS_U=536,             /**<PIXELDATA SIZE - UNCOMPRESSED */
    IM_CHECKSUM=540,          /**<ACQRECON RECORD CHECKSUM */
    IM_ARCHIVED=544,          /**<IMAGE ARCHIVE FLAG */
    IM_COMPLETE=548,          /**<IMAGE COMPLETE FLAG */
    IM_SATBITS=552,           /**<BITMAP OF SAT SELECTIONS */
    IM_SCIC=554,              /**<SURFACE COIL INTENSITY CORRECTION FLAG */
    IM_SATXLOC1=556,          /**<R-SIDE SAT PULSE LOC REL TO LNDMRK */
    IM_SATXLOC2=558,          /**<L-SIDE SAT PULSE LOC REL TO LNDMRK */
    IM_SATYLOC1=560,          /**<A-SIDE SAT PULSE LOC REL TO LNDMRK */
    IM_SATYLOC2=562,          /**<P-SIDE SAT PULSE LOC REL TO LNDMRK */
    IM_SATZLOC1=566,          /**<S-SIDE SAT PULSE LOC REL TO LNDMRK */
    IM_SATZLOC2=568,          /**<I-SIDE SAT PULSE LOC REL TO LNDMRK */
    IM_SATXTHICK=570,         /**<THICKNESS OF X-AXIS SAT PULSE */
    IM_SATYTHICK=572,         /**<THICKNESS OF Y-AXIS SAT PULSE */
    IM_SATZTHICK=574,         /**<THICKNESS OF Z-AXIS SAT PULSE */
    IM_FLAX=576,              /**<PHASE CONTRAST FLOW AXIS */
    IM_VENC=578,              /**<PHASE CONTRAST VELOCITY ENCODING */
    IM_THK_DISCLMR=580,       /**<SLICE THICKNESS */
    IM_PS_FLAG=582,           /**<AUTO/MANUAL PRESCAN FLAG */
    IM_PS_STATUS=584,         /**<BITMAP OF CHANGED VALUES */
    IM_IMAGE_TYPE=586,        /**<MAGNITUDE, PHASE, IMAGINARY, OR REAL */
    IM_VAS_COLLAPSE=588,      /**<COLLAPSE IMAGE */
    IM_HDR_LEN=1022
  };
  /*
        these values are relative to the start of the suite information
        the suite information starts at some variable location as specified
        in the pixel data header at the start of the image file
      */
  enum GE_RELATIVE_STARTS {
    SU_HDR_START=0,
    EX_HDR_START=SU_HDR_LEN,
    SE_HDR_START=EX_HDR_START+EX_HDR_LEN,
    IM_HDR_START=SE_HDR_START+SE_HDR_LEN,
    IMG_HDR_START=0
  };
  int checkGe5xImages (char const * const imageFileTemplate);
  GE5ImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#endif // __itkAnalyzeImageIO_h
