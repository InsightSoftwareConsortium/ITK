/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkGE4ImageIO.h
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

#ifndef __itkGE4ImageIO_h
#define __itkGE4ImageIO_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include <fstream>
#include "itkIPLCommonImageIO.h"
#include "itkImageIOBase.h"
#include "itkGEImageHeader.h"

namespace itk
{
/**
   * \author Hans J. Johnson
   * \brief Class that defines how to read GE4 file format.
   * */
class ITK_EXPORT GE4ImageIO : public IPLCommonImageIO
{
public:
  /** Standard class typedefs. */
  typedef GE4ImageIO            Self;
  typedef IPLCommonImageIO  Superclass;
  typedef SmartPointer<Self>  Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GE4ImageIO, Superclass);

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
  // virtual const std::type_info& GetPixelType() const;

  /** Reads the data from disk into the memory buffer provided. */
  // Implemented in superclass
  //      virtual void Read(void* buffer);

  /** Compute the size (in bytes) of the components of a pixel. For
       * example, and RGB pixel of unsigned char would have a
       * component size of 1 byte. */
  // Implemented in superclass
  // virtual unsigned int GetComponentSize() const;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine if the file can be written with this ImageIO implementation.
       * \param FileNameToWrite The name of the file to test for writing.
       * \author Hans J. Johnson
       * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
       * \return Returns true if this ImageIO can write the file specified.
       */
  // Implemented in superclass
  // virtual bool CanWriteFile(const char * FileNameToWrite);

  /** Set the spacing and dimension information for the set filename. */
  // Implemented in superclass
  // virtual void WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
       * that the IORegions has been set properly. */
  // Implemented in superclass
  // virtual void Write(const void* buffer);
protected:
  GE4ImageIO();
  ~GE4ImageIO();
  // Implemented in Superclass
  // void PrintSelf(std::ostream& os, Indent indent) const;
  virtual struct GEImageHeader *ReadHeader(const char *FileNameToRead);
private:
  GE4ImageIO(const Self&); //purposely not implemented
  float MvtSunf (int numb);
  enum Study_Header_offsets {
    STHDR_ID=       0,      // Study Header Identifier
    STHDR_REV=      7,      // Study Header Revision Number
    STHDR_BLKS=     11,     // Number of Study Header Blocks
    STHDR_CRTRP=    12,     // Study Header Creator (Process
    STHDR_CRTRT=    28,     // Study Header Creator (Task
    STHDR_RAWNM=    29,     // Raw Data Study Number
    STHDR_STNUM=    32,     // Study Number
    STHDR_RAWID=    35,     // Raw Data System ID
    STHDR_SGENID=   37,     // System Generation ID
    STHDR_DATE=     39,     // Date of Study (ascii
    STHDR_IDATE=    44,     // Date of Study (integer
    STHDR_TIME=     47,     // Time of Study (ascii
    STHDR_ITIME=    51,     // Time of Study (integer
    STHDR_PNM=      54,     // Patient Name
    STHDR_PID=      70,     // Patient ID
    STHDR_PIDTMP=   76,     // Patient ID padding for future exp.
    STHDR_AGE=      78,     // Age of patient
    STHDR_SEX=      80,     // Sex of patient
    STHDR_WGHT=     81,     // Weight of the patient in grams
    STHDR_RFR=      83,     // Refered by
    STHDR_DGN=      99,     // Diognostician
    STHDR_OP=       115,    // Operator
    STHDR_DESC=     131,    // Description
    STHDR_HIST=     161,    // History
    STHDR_STIME=    221,    // Creation time in seconds.
    STHDR_HOSP=     223,    // Hospital name
    STHDR_STAT=    239,     // Patient status
    STHDR_RSRV1=    240,    // GE NMR Reserved Area
    STHDR_RSRV2=    256,    // GE NMR Reserved Area
    STHDR_CHECK=    511,    // Study Header Checksum

  };
  enum Series_Header_Offsets {
    SEHDR_ID=       0,      // Series Header Identifier
    SEHDR_REV=      7,      // Series Header Revision Number
    SEHDR_BLKS=     11,     // Number of Series Header Blocks
    SEHDR_CRTRP=    12,     // Series Header Creator (Proc
    SEHDR_CRTRT=    28,     // Series Header Creator (Task
    SEHDR_RAWNM=    29,     // Original Series Number
    SEHDR_SERNUM=   31,     // Series Number
    SEHDR_RAWID=    33,     // Raw Data System ID
    SEHDR_SGENID=   35,     // System Generation ID
    SEHDR_DATE=     37,     // Date of series (ascii
    SEHDR_IDATE=    42,     // Date of series (integer
    SEHDR_TIME=     45,     // Time of Series (ascii
    SEHDR_ITIME=    49,     // Time of Series (integer
    SEHDR_DESC=     52,     // Series Description
    SEHDR_TYPE=     112,    // Series Type
    SEHDR_CTYPE=    113,    // Coil Type
    SEHDR_CNAME=    114,    // Coil Name
    SEHDR_CNTRDESC= 122,    // Contrast Description
    SEHDR_PTYPE=    138,    // Plane Type
    SEHDR_PNAME=    139,    // Plane Name
    SEHDR_IMODE=    147,    // Image Mode
    SEHDR_FSTREN=   148,    // Magnetic Field Strength
    SEHDR_PSEQ=     149,    // Pulse Sequence
    SEHDR_PSSTYPE=  150,    // Pulse sequence subtype
    SEHDR_FOV=      151,    // Field of view
    SEHDR_CENTER=   153,    // Center
    SEHDR_ORIEN=    159,    // Orientation
    SEHDR_POS=      160,    // Position
    SEHDR_ANREF=    161,    // Longitudinal Anotomical Reference
    SEHDR_VANREF=   177,    // Vertical Anotomical Reference
    SEHDR_VERLAN=   193,    // Vertical Landmark
    SEHDR_HORLAN=   195,    // Horizontal Landmark
    SEHDR_TBLLOC=   197,    // Physical Table Location
    SEHDR_SMATRIX=  199,    // Scan Matrix
    SEHDR_IMATRIX=  201,    // Image Matrix
    SEHDR_IALLOC=   202,    // No. of Images Allocated
    SEHDR_GTYP=     203,    // Gating Type
    SEHDR_PSMODE=   204,    // Pulse sequence mode
    SEHDR_DNAME=    205,    // PSD name from NAME = aPPL psd name
    SEHDR_LNDMRK=   211,    // Landmark counter
    SEHDR_PROTO=    213,    // Protocol name for Scan
    SEHDR_SCOIL_TYPE= 223, // Surface coil type
    SEHDR_SUPP_TECH=    224, // Suppression technique
    SEHDR_RSRV1=    225,    // GE NMR Reserved
    SEHDR_RSRV2=    256,    // GE NMR Reserved
    SEHDR_CHECK=    511,    // Checksum for Series Header
  };
  enum Image_Header_Offsets {
    IHDR_ID=        0,      // Image Header Identifier
    IHDR_REV=       7,      // Image Header Revision Number
    IHDR_BLKS=      11,     // Number of Image Header Blocks
    IHDR_CRTRP=     12,     // Image Header Creator (Proc
    IHDR_CRTRT=     28,     // Image Header Creator (Task
    IHDR_DATE=      29,     // Image Creation Date (ascii
    IHDR_IDATE=     34,     // Image Creation Date (integer
    IHDR_TIME=      37,     // Image Creation Time (ascii
    IHDR_ITIME=     41,     // Image Creation Time (integer
    IHDR_IMNUM=     44,     // Image Number
    IHDR_SERNM=     46,     // Series Number of Image
    IHDR_RAWID=     48,     // Raw Data System ID
    IHDR_SGENID=    50,     // System Generation ID
    IHDR_STRTX=     52,     // Start Location X, Right min
    IHDR_ENDX=      54,     // End Location X, Right max
    IHDR_STRTY=     56,     // Start Location Y, Anterior min
    IHDR_ENDY=      58,     // End Location Y, Anterior max
    IHDR_STRTZ=     60,     // Start Location Z, Superior min
    IHDR_ENDZ=      62,     // End Location Z, Superior max
    IHDR_OBLIQUE=   64,     // Reserved for future use.
    IHDR_LOCATN=    73,     // Image Location
    IHDR_TBLPOS=    75,     // Table Position
    IHDR_THICK=     77,     // Thickness
    IHDR_SPACE=     79,     // Spacing
    IHDR_ROUND=     81,     // Round
    IHDR_TR=        82,     // Repititon/Recovery Time
    IHDR_TS=        84,     // Scan Time
    IHDR_TE=        86,     // Echo Delay
    IHDR_TI=        88,     // Inversion Time
    IHDR_TY=        90,     // Reserved for future use.
    IHDR_NECHO=     98,     // Number of echos.
    IHDR_ECHON=     99,     // Echo number.
    IHDR_SLQUANT=   100,    // Number of slices in scan group.
    IHDR_NAVE=      101,    // Number of averages.
    IHDR_RSRCH=     102,    // Research mode used ?
    IHDR_PNAME=     103,    // Name of PSD file.
    IHDR_PSDDT=     119,    // Creation Date of PSD file.
    IHDR_GPRE=      125,    // Graphically Prescribed ?
    IHDR_PSERIES=   126,    // Prescribed Series Numbers
    IHDR_PIMAGES=   131,    // Prescribed Image Numbers
    IHDR_SHAPE=     136,    // Image Shape
    IHDR_X=         137,    // X pixel dimension
    IHDR_Y=         138,    // Y pixel dimension
    IHDR_PIXSIZ=    139,    // Pixel Size
    IHDR_CMPRS=     141,    // Image Compressed ?
    IHDR_BITPIX=    142,    // Bits per Pixel
    IHDR_WINDOW=    143,    // Default Window
    IHDR_LEVEL=     144,    // Default Level
    IHDR_IFBLKS=    145,    // Number of Blocks in File
    IHDR_NEX=       146,    // Number of excitations (Real .
    IHDR_PSAR=      148,    // Value of peak SAR (Real .
    IHDR_ASAR=      150,    // Value of average SAR (Real .
    IHDR_MONITOR=   152,    // SAR monitored ?
    IHDR_CONTIG=    153,    // Contiguous slices ?
    IHDR_HRT_RT=    154,    // Cardiac Heart Rate
    IHDR_DEL_TRG=   155,    // Total Delay Time After Trigger
    IHDR_ARR=       157,    // Arrhythmia Rejection Ratio
    IHDR_RTIME=     158,    // Cardiac Rep Time
    IHDR_IMGS_PCY=  159,    // Images per Cardiac Cycle
    IHDR_ARRS_SCN=  160,    // Number of ARR's during the Scan
    IHDR_XMTATTN=   162,    // Transmit attenuator setting
    IHDR_RCVATTN=   163,    // Recieve attenuator setting
    IHDR_FLDSTR=    164,    // Magnetic Field Strength
    IHDR_IMG_OFF=   166,    // Image offser
    IHDR_INTR_DEL=  167,    // Inter image/inter location delay
    IHDR_IHDR_DNAME=169,    // PSD name from NAME = aPPL psd name
    IHDR_FLPANG=    175,    // Flip angle for GRASS
    IHDR_SC_TYPE=   176,   // Type of correction for surface coils
    IHDR_SC_SER=    178,   // Series no. of corrected/uncor images
    IHDR_SC_IMA=    180,    // Image no. of corrected/uncor images
    IHDR_SC_EXTR=   182,    // Extremety coil? true/false
    IHDR_SC_RSRV=   183,   // Reserved for future surface coil use
    IHDR_PSERIES_2= 193,    // Series no. of second localizer
    IHDR_PIMAGE_2=  195,    // Image no. of second localizer
    IHDR_R_CC=      197,    // R center coordinate on plane image
    IHDR_A_CC=      199,    // A center coordinate on plane image
    IHDR_S_CC=      201,    // S center coordinate on plane image
    IHDR_R_NC=      203,    // R normal coordinate
    IHDR_A_NC=      205,    // A normal coordinate
    IHDR_S_NC=      207,    // S normal coordinate
    IHDR_TLHC_R=    209,    // TLHC R coordinate
    IHDR_TLHC_A=    211,    // TLHC A coordinate
    IHDR_TLHC_S=    213,    // TLHC S coordinate
    IHDR_TRHC_R=    215,    // TRHC R coordinate
    IHDR_TRHC_A=    217,    // TRHC A coordinate
    IHDR_TRHC_S=    219,    // TRHC S coordinate
    IHDR_BLHC_R=    221,    // BLHC R coordinate
    IHDR_BLHC_A=    223,    // BLHC A coordinate
    IHDR_BLHC_S=    225,    // BLHC S coordinate
    IHDR_DISCLMR=   227,    // Image header disclamer
    IHDR_MINDAT=    228,    // Minimum delay after trigger
    IHDR_CPHASE=    229,   // Multiplier of slices to obtain phase
    IHDR_TE2=       230,    // TE2 (VEMP)
    IHDR_SWAP_PF=   232,    // swap phase/frequency axis
    IHDR_PAUSIN=    233,    // Pause interval
    IHDR_PAUSTM=    234,    // Pause time
    IHDR_USET=      236,    // Bitmap defining users CVs
    IHDR_USER0=     237, // User defined variables that are PSD dependent
    IHDR_USER1=     239,
    IHDR_USER2=     241,
    IHDR_USER3=     243,
    IHDR_USER4=     245,
    IHDR_USER5=     247,
    IHDR_USER6=     249,
    IHDR_USER7=     251,
    IHDR_USER8=     253,
    IHDR_USER9=     255,
    IHDR_OBPLANE=   257,    // Oblique pl
    IHDR_CNTRST=    258,    // BOOLEAN - was contrast used?
    IHDR_CSTAGNT=   259,    // Contrast agent
    IHDR_CSTAMT=    264,    // Contrast amount
    IHDR_FILFMT=    266,    // FILE format (3.0 or earlier
    IHDR_AUTOCF=    267,    // Auto center frequency
    IHDR_XMTFREQ=   268,    // Actual transmit freq usen on scan
    IHDR_RCVFREQ=   270,    // Actual receive freq usen on scan
    IHDR_AUTOXMTFERQ=272,   // Recommended automated transmit freq
    IHDR_AUTORCVFREQ=274,   // Recommended automated receive freq
    IHDR_AUTOXMTATTN=276, // Recommended automated transmit attenuation
    IHDR_AUTORCVATTN=278, // Recommended automated receive attenuation
    IHDR_HISTO=      280,   // is histogram present in raw header
    IHDR_PF_SWAPPED= 281,  // Swapped phase/frequency - true/false
    IHDR_R1=        282,    // for prescan
    IHDR_R2=        283,    // for prescan
    IHDR_VBW=       284,    // Variable bandwidth
    IHDR_RSRV2=     285,    // GE NMR Reserved
    IHDR_CHECK=     511,    // Image Header Checksum
  };
  enum DSS_Header_Offsets {
    DSSHDR_ID=      0,      // DSS Header Identifier
    DSSHDR_REV=     7,      // DSS Header Revision Number
    DSSHDR_BLKS=    11,     // Number of DSS Header Blocks
    DSSHDR_CRTRP=   12,     // DSS Header Creator (process
    DSSHDR_CRTRT=   28,     // DSS Header Creator (task
    DSSHDR_DATE=    29,     // Date of Creation (ascii
    DSSHDR_IDATE=   34,     // Date of Creation (integer
    DSSHDR_TIME=    37,     // Time of Creation (ascii
    DSSHDR_ITIME=   41,     // Time of Creation (integer
    DSSHDR_TLM=     44,     // Time the DSSHDR was last modified
    DSSHDR_ASTAT=   46,     // Study Archive Status
    DSSHDR_APEND=   47,     // Study archive pending count.
    DSSHDR_SPEND=   48,     // Substructure archive pending count.
    DSSHDR_LOCK=    49,     // Resource locking queue desc
    DSSHDR_STLST=   57,     // IDBM Formated Study List Entry
    DSSHDR_STIME=   121,    // Creation time in seconds.
    DSSHDR_MSERIES= 123,  // Maximum series number created to date
    DSSHDR_SERIES=  124,    // Bit map for series in study.
    DSSHDR_DEFSER=  126,  // Default series and iii for the study.
    DSSHDR_EOF=     129,  // Points to the next block to allocate.
    DSSHDR_SDIR=    130, // Series number and sgenid referenced by max
    DSSHDR_IMAP=    223, // Image block number map for each series
    DSSHDR_RSRV1=   471,    // GE NMR Reserved Area
  };
  enum DSS_Header_Series_Offsets {
    DSSHDR_SERIES_MIMAGE=   0, // Maximum image number created to date.
    DSSHDR_SERIES_IMAGE=    1, // Number of images in the series.
    DSSHDR_SERIES_RSRV=     2, // Number of images reserved for space.
    DSSHDR_SERIES_PTYPE=    3,// Series plane type, AXIAL, SAG ...
    DSSHDR_SERIES_PSEQ=     4, // Pulse sequence type.
    DSSHDR_SERIES_ANREF=    5, // Anatomical reference, 2 chars.
    DSSHDR_SERIES_VANREF=   6, // Vertical Anatomical reference.
    DSSHDR_SERIES_FOV=      7, // Field of view. in I*2 mm * 10
    DSSHDR_SERIES_DESC=     8, // First 22 chars of series description.
    DSSHDR_SERIES_ASTAT=    19, // Archive status.
    DSSHDR_SERIES_APEND=    20, // Archive pending status.
    DSSHDR_SERIES_IMATRIX=  21, // Image matrix size.
    DSSHDR_SERIES_MINLOC=   22, // Minimum location, I*2 mm * 10.
    DSSHDR_SERIES_MAXLOC=   23, // Maximim location, I*2 mm * 10.
    DSSHDR_SERIES_LOCCHAR=  24, // Characters whcih represent +,- pos.
    DSSHDR_SERIES_TLM=      25, // Time the series was last modified.
    DSSHDR_SERIES_CHECK=    27, // Checksum for the series header.
    DSSHDR_SERIES_DEFIM=    28, // Default image for the series.
    DSSHDR_SERIES_TIMAGE=   29, // Total image count (partial & complete
    DSSHDR_SERIES_GTYPE=    30, // Gating type if cardiac gated
    DSSHDR_SERIES_WPENTRY=  33, // Words per series entry.
  };
  enum DSS_Header_Image_Offsets {
    DSSHDR_IMAGE_LOCATN=    0, // Image location in I*2 mm * 10
    DSSHDR_IMAGE_THICK=     1, // Image thickness in I*2 mm * 10
    DSSHDR_IMAGE_NEXT=      2, // Next image by location.
    DSSHDR_IMAGE_PRIOR=     3, // Prior image by location.
    DSSHDR_IMAGE_EXIST=     4, // None, header or data exists.
    DSSHDR_IMAGE_T1=        5,
    DSSHDR_IMAGE_T2=        7,
    DSSHDR_IMAGE_TRGD=      9, // Trigger Delay
    DSSHDR_IMAGE_ECHON=    11, // Echo number
    DSSHDR_IMAGE_NXTEN=    12, // Next image by echo number
    DSSHDR_IMAGE_PREN=     13, // Prior image by echo number
    DSSHDR_IMAGE_GPRE=     14, // Graphically Prescribed flag
    DSSHDR_IMAGE_RCC=      15, // R center coordinate
    DSSHDR_IMAGE_ACC=      17, // A center coordinate
    DSSHDR_IMAGE_SCC=      19, // S center coordinate
    DSSHDR_IMAGE_RNC=      21, // R normal coordinate
    DSSHDR_IMAGE_ANC=      23, // R normal coordinate
    DSSHDR_IMAGE_SNC=      25, // R normal coordinate
    DSSHDR_IMAGE_OBP=      27, // Oblique plane
    DSSHDR_IMAGE_WPENTRY=   32,
  };
  enum Block_Offsets {
    SYSCON_START =   0*256,
    SITCUS_START=    4*256,
    STHDR_START=    6*256,
    SEHDR_START=    8*256,
    IHDR_START=     10*256,
    RDBM_START=     12*256,
    PSD_START=      16*256,
    PIXMAP_START=   26*256,
    IDATA_START=    28*256,
  };
  enum Sys_Config_Offsets {
    SCON_SYSID=     6,      // System ID String
    SCON_HNAME=     16      // Hospital Name
  };
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#endif // __itkAnalyzeImageIO_h
