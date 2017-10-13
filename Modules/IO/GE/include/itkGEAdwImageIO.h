/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/**
 * \file   itkGEAdwImageIO.h
 *         Much of the code for this file reader/writer was taken from
 *         the University of Iowa Imaging Group library with the
 *         permission of the authors, Milan Sonka, Joseph Reinhardt,
 *         Ryan Long, Hans Johnson, Gary Christensen, and others.
 *         The specification for this file format is taken from the
 *         web site http://analyzedirect.com/support/10.0Documents/Analyze_Resource_01.pdf
 * \author Kent Williams
 *         The University of Iowa 2003
 * \brief This file was written as a modification to the itkMetaImageIO
 *        as a new method for reading in files from the GE4 scanner.
 */

#ifndef itkGEAdwImageIO_h
#define itkGEAdwImageIO_h
#include "ITKIOGEExport.h"


#include "itkIPLCommonImageIO.h"

namespace itk
{
/** \class GEAdwImageIO
 *
 * \author Hans J. Johnson
 * \brief Class that defines how to read GEAdw file format.
 *
 * \ingroup IOFilters
 * \ingroup ITKIOGE
 */
class ITKIOGE_EXPORT GEAdwImageIO:public IPLCommonImageIO
{
public:
  /** Standard class typedefs. */
  typedef GEAdwImageIO         Self;
  typedef IPLCommonImageIO     Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GEAdwImageIO, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this ImageIO implementation.
       * \author Hans J Johnson
       * \param FileNameToRead The name of the file to test for reading.
       * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
       * \return Returns true if this ImageIO can read the file specified.
       */
  virtual bool CanReadFile(const char *FileNameToRead) ITK_OVERRIDE;

  /* * Set the spacing and dimension information for the set filename. */
  // Implemented in superclass
  //      virtual void ReadImageInformation();

  /* * Get the type of the pixel.  */
  // Implemented in superclass
  //      virtual const std::type_info& GetPixelType() const;

  /* * Reads the data from disk into the memory buffer provided. */
  // Implemented in superclass
  //      virtual void Read(void* buffer);

  /* * Compute the size (in bytes) of the components of a pixel. For
       * example, and RGB pixel of unsigned char would have a
       * component size of 1 byte. */
  // Implemented in superclass
  //      virtual unsigned int GetComponentSize() const;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /* * Determine if the file can be written with this ImageIO implementation.
       * \param FileNameToWrite The name of the file to test for writing.
       * \author Hans J. Johnson
       * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
       * \return Returns true if this ImageIO can write the file specified.
       */
  // Implemented in superclass
  //      virtual bool CanWriteFile(const char * FileNameToWrite);

  /* * Set the spacing and dimension information for the set filename. */
  // Implemented in superclass
  //      virtual void WriteImageInformation();

  /* * Writes the data to disk from the memory buffer provided. Make sure
       * that the IORegions has been set properly. */
  // Implemented in superclass
  //      virtual void Write(const void* buffer);

protected:
  GEAdwImageIO();
  ~GEAdwImageIO() ITK_OVERRIDE;
  // Implemented in superclass
  //      void PrintSelf(std::ostream& os, Indent indent) const;
  virtual GEImageHeader * ReadHeader(const char *FileNameToRead) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GEAdwImageIO);

  enum GE_ADW_DEFINES {
    GE_ADW_SU_ID = 0,    /**< Site id - String  */
    GE_ADW_SU_ID_LEN = 4,
    GE_ADW_SU_PRODID = 7,       /**< Product ID - String */
    GE_ADW_SU_PRODID_LEN = 13,
    GE_ADW_EX_SUID = 116,   /**< Exam study ID - String */
    GE_ADW_EX_SUID_LEN = 4,
    GE_ADW_EX_NO = 124,     /**< Exam Number - Unsigned short Int */
    GE_ADW_EX_NO_LEN = 2,
    GE_ADW_EX_HOSPNAME = 126,   /**< Hospital Name - String */
    GE_ADW_EX_HOSPNAME_LEN = 33,
    GE_ADW_EX_MAGSTRENGTH = 200,        /**< Magnet Strength - Integer */
    GE_ADW_EX_MAGSTRENGTH_LEN = 4,
    GE_ADW_EX_PATID = 204,      /**< Patient ID - String */
    GE_ADW_EX_PATID_LEN = 13,
    GE_ADW_EX_PATNAME = 217,    /**< Patient Name - String */
    GE_ADW_EX_PATNAME_LEN = 25,
    GE_ADW_EX_PATAGE = 242,     /**< Patient Age - Short Int */
    GE_ADW_EX_PATAGE_LEN = 2,
    GE_ADW_EX_PATIAN = 244,     /**< Patient Age Notation - SHort Int */
    GE_ADW_EX_PATIAN_LEN = 2,
    GE_ADW_EX_PATSEX = 246,     /**< Patient Sex - Short Integer */
    GE_ADW_EX_PATSEX_LEN = 2,
    GE_ADW_EX_PATWEIGHT = 248,  /**< Patient Weight - Integer */
    GE_ADW_EX_PATWEIGHT_LEN = 4,
    GE_ADW_EX_HIST = 254,   /**< Patient History - String */
    GE_ADW_EX_HIST_LEN = 61,
    GE_ADW_EX_DATETIME = 328,   /**< Exam date/time stamp - Integer */
    GE_ADW_EX_DATETIME_LEN = 4,
    GE_ADW_EX_REFPHY = 332,     /**< Referring Physician - String */
    GE_ADW_EX_REFPHY_LEN = 33,
    GE_ADW_EX_DIAGRAD = 365,    /**< Diagnostician/Radiologist - String */
    GE_ADW_EX_DIAGRAD_LEN = 33,
    GE_ADW_EX_OP = 398,     /**< Operator - String */
    GE_ADW_EX_OP_LEN = 4,
    GE_ADW_EX_DESC = 402,   /**< Exam Description - String */
    GE_ADW_EX_DESC_LEN = 23,
    GE_ADW_EX_TYP = 425,    /**< Exam Type - String */
    GE_ADW_EX_TYP_LEN = 3,
    GE_ADW_EX_FORMAT = 428,     /**< Exam Format - Short Integer */
    GE_ADW_EX_FORMAT_LEN = 2,
    GE_ADW_EX_SYSID = 444,      /**< Creator Suite and Host - String */
    GE_ADW_EX_SYSID_LEN = 9,

    /**
     * Series Header Variables
     */
    GE_ADW_SE_SUID = 1156,  /**< Suite Id for Series  - String */
    GE_ADW_SE_SUID_LEN = 4,
    GE_ADW_SE_UNIQ = 1160,  /**< The Make-Unique Flag - Short Integer */
    GE_ADW_SE_UNIQ_LEN = 2,
    GE_ADW_SE_EXNO = 1164,  /**< Exam Number - Unsigned Short Integer */
    GE_ADW_SE_EXNO_LEN = 2,
    GE_ADW_SE_NO = 1166,    /**< Series Number - Short Integer */
    GE_ADW_SE_NO_LEN = 2,
    GE_ADW_SE_DATETIME = 1168,  /**< Date/Time stamp - Integer */
    GE_ADW_SE_DATETIME_LEN = 4,
    GE_ADW_SE_DESC = 1176,  /**< Series description - String */
    GE_ADW_SE_DESC_LEN = 30,
    GE_ADW_SE_TYP = 1224,   /**< Series Type - Short Integer */
    GE_ADW_SE_TYP_LEN = 2,
    GE_ADW_SE_PLANE = 1228,     /**< Most Like Plane  - Short Integer */
    GE_ADW_SE_PLANE_LEN = 2,
    GE_ADW_SE_POSITION = 1232,  /**< Patient Position - Integer */
    GE_ADW_SE_POSITION_LEN = 4,
    GE_ADW_SE_ENTRY = 1236,     /**< Patient Entry - Integer */
    GE_ADW_SE_ENTRY_LEN = 4,
    GE_ADW_SE_ANREF = 1240,     /**< Anatomical reference - String */
    GE_ADW_SE_ANREF_LEN = 3,
    GE_ADW_SE_CONTRAST = 1274,  /**< Non-zero if contrast - Short Int */
    GE_ADW_SE_CONTRAST_LEN = 2,
    GE_ADW_SE_START_RAS = 1276,         /**< RAS letter for first scan location
                                          - STring */
    GE_ADW_SE_START_RAS_LEN = 1,
    GE_ADW_SE_START_LOC = 1280,         /**< Start location position - Float */
    GE_ADW_SE_START_LOC_LEN = 4,
    GE_ADW_SE_END_RAS = 1284,   /**< RAS letter for last scan location - String
                                  */
    GE_ADW_SE_END_RAS_LEN = 1,
    GE_ADW_SE_END_LOC = 1288,   /**< End location position - Float */
    GE_ADW_SE_END_LOC_LEN = 4,
    GE_ADW_SE_NUMIMAGES = 1368,         /**< Number of Images Existing - Integer
                                          */
    GE_ADW_SE_NUMIMAGES_LEN = 4,

    /**
     * Image Header Variables
     */
    GE_ADW_IM_SUID = 2184,  /**< Suite ID for this image - String */
    GE_ADW_IM_SUID_LEN = 4,
    GE_ADW_IM_UNIQ = 2188,  /**< The make unique flag - Short Integer */
    GE_ADW_IM_UNIQ_LEN = 2,
    GE_ADW_IM_EXNO = 2192,  /**< Exam number for this image - Unsigned short */
    GE_ADW_IM_EXNO_LEN = 2,
    GE_ADW_IM_SENO = 2194,  /**< Series number for image - short integer */
    GE_ADW_IM_SENO_LEN = 2,
    GE_ADW_IM_NO = 2196,    /**< Image number - short integer */
    GE_ADW_IM_NO_LEN = 2,
    GE_ADW_IM_DATETIME = 2200,  /**< Image allocation date/time stamp - integer
                                  */
    GE_ADW_IM_DATETIME_LEN = 4,
    GE_ADW_IM_ACTUAL_DT = 2204,         /**< Actual image date/time stamp - Date
                                          */
    GE_ADW_IM_ACTUAL_DT_LEN = 4,
    GE_ADW_IM_SCTIME = 2208,    /**< Duration of scan (secs) - float */
    GE_ADW_IM_SCTIME_LEN = 4,
    GE_ADW_IM_SLTHICK = 2212,   /**< Slice thickness (mm) - float */
    GE_ADW_IM_SLTHICK_LEN = 4,
    GE_ADW_IM_IMATRIX_X = 2216,         /**< Image matrix size X - short integer
                                          */
    GE_ADW_IM_IMATRIX_X_LEN = 2,
    GE_ADW_IM_IMATRIX_Y = 2218,         /**< Image matrix size Y - short integer
                                          */
    GE_ADW_IM_IMATRIX_Y_LEN = 2,
    GE_ADW_IM_DFOV = 2220,  /**< Display field of view X (mm) - float */
    GE_ADW_IM_DFOV_LEN = 4,
    GE_ADW_IM_DFOV_RECT = 2224,         /**< Display field of view Y (mm) if
                                          different - float */
    GE_ADW_IM_DFOV_RECT_LEN = 4,
    GE_ADW_IM_DIM_X = 2228,     /**< Image dimension X - float */
    GE_ADW_IM_DIM_X_LEN = 4,
    GE_ADW_IM_DIM_Y = 2232,     /**< Image dimension Y - float */
    GE_ADW_IM_DIM_Y_LEN = 4,
    GE_ADW_IM_PIXSIZE_X = 2236,         /**< Image pixel size X - float */
    GE_ADW_IM_PIXSIZE_X_LEN = 4,
    GE_ADW_IM_PIXSIZE_Y = 2240,         /**< Image pixel size Y - float */
    GE_ADW_IM_PIXSIZE_Y_LEN = 4,
    GE_ADW_IM_CONTMODE = 2292,  /**< Image contrast mode - short integer */
    GE_ADW_IM_CONTMODE_LEN = 2,
    GE_ADW_IM_PLANE = 2300,     /**< Plane type - short integer */
    GE_ADW_IM_PLANE_LEN = 2,
    GE_ADW_IM_SCANSPACING = 2304,       /**< Spacing between scans (mm) - float
                                          */
    GE_ADW_IM_SCANSPACING_LEN = 4,
    GE_ADW_IM_LOC_RAS = 2312,   /**< RAS letter of image location - string */
    GE_ADW_IM_LOC_RAS_LEN = 1,
    GE_ADW_IM_LOC = 2316,   /**< Image location - float */
    GE_ADW_IM_LOC_LEN = 4,
    GE_ADW_IM_ULHC_R = 2344,      /**< R coordinate of upper left corner - float
                                    */
    GE_ADW_IM_ULHC_R_LEN = 4,
    GE_ADW_IM_ULHC_A = 2348,      /**< A coordinate of upper left corner - float
                                    */
    GE_ADW_IM_ULHC_A_LEN = 4,
    GE_ADW_IM_ULHC_S = 2352,      /**< S coordinate of upper left corner - float
                                    */
    GE_ADW_IM_ULHC_S_LEN = 4,
    GE_ADW_IM_URHC_R = 2356,     /**< R coordinate of upper right corner - float
                                   */
    GE_ADW_IM_URHC_R_LEN = 4,
    GE_ADW_IM_URHC_A = 2360,     /**< A coordinate of upper right corner - float
                                   */
    GE_ADW_IM_URHC_A_LEN = 4,
    GE_ADW_IM_URHC_S = 2364,     /**< S coordinate of upper right corner - float
                                   */
    GE_ADW_IM_URHC_S_LEN = 4,
    GE_ADW_IM_BRHC_R = 2368,     /**< R coordinate of bottom right corner -
                                   float */
    GE_ADW_IM_BRHC_R_LEN = 4,
    GE_ADW_IM_BRHC_A = 2372,     /**< A coordinate of bottom right corner -
                                   float */
    GE_ADW_IM_BRHC_A_LEN = 4,
    GE_ADW_IM_BRHC_S = 2376,     /**< S coordinate of bottom right corner -
                                   float */
    GE_ADW_IM_BRHC_S_LEN = 4,
    GE_ADW_IM_TR = 2384,    /**< Pulse repetition time (usec) - integer */
    GE_ADW_IM_TR_LEN = 4,
    GE_ADW_IM_TI = 2388,    /**< Pulse inversion time (usec) - integer */
    GE_ADW_IM_TI_LEN = 4,
    GE_ADW_IM_TE = 2392,    /**< Pulse echo time (usec) - integer */
    GE_ADW_IM_TE_LEN = 4,
    GE_ADW_IM_NUMECHO = 2400,   /**< Number of echoes - short integer */
    GE_ADW_IM_NUMECHO_LEN = 2,
    GE_ADW_IM_ECHONUM = 2402,   /**< Echo number - short integer */
    GE_ADW_IM_ECHONUM_LEN = 2,
    GE_ADW_IM_NEX = 2408,   /**< Number of averages - float */
    GE_ADW_IM_NEX_LEN = 4,
    GE_ADW_IM_CONTIG = 2412,    /**< Continuos slices flag - short integer */
    GE_ADW_IM_CONTIG_LEN = 2,
    GE_ADW_IM_HRTRATE = 2414,   /**< Cardiac Heart rate (bpm) - short integer */
    GE_ADW_IM_HRTRATE_LEN = 2,
    GE_ADW_IM_TDEL = 2416,  /**< Delay after trigger (usec) - integer */
    GE_ADW_IM_TDEL_LEN = 4,
    GE_ADW_IM_XMTGAIN = 2438,   /**< Actual transmit gain (.1 dB) - short
                                  integer */
    GE_ADW_IM_XMTGAIN_LEN = 2,
    GE_ADW_IM_MR_FLIP = 2444,   /**< Flip angle for GRASS scans (dgr) - short
                                  integer */
    GE_ADW_IM_MR_FLIP_LEN = 2,
    GE_ADW_IM_CPHASE = 2452,    /**< Total cardiac phases prescribed - short
                                  integer */
    GE_ADW_IM_CPHASE_LEN = 2,
    GE_ADW_IM_SWAPPF = 2454,    /**< Swap phase/frequency - short integer */
    GE_ADW_IM_SWAPPF_LEN = 2,
    GE_ADW_IM_OBPLANE = 2464,   /**< Oblique plane - integer */
    GE_ADW_IM_OBPLANE_LEN = 4,
    GE_ADW_IM_XMTFREQ = 2472,   /**< Transmit frequency - integer */
    GE_ADW_IM_XMTFREQ_LEN = 4,
    GE_ADW_IM_PRESCAN_R1 = 2482,        /**< Prescan R1 value - short integer */
    GE_ADW_IM_PRESCAN_R1_LEN = 2,
    GE_ADW_IM_PRESCAN_R2 = 2484,        /**< Prescan R2 value - short integer */
    GE_ADW_IM_PRESCAN_R2_LEN = 2,
    GE_ADW_IM_IMODE = 2494,     /**< Imaging mode - short integer */
    GE_ADW_IM_IMODE_LEN = 2,
    GE_ADW_IM_IOPT = 2496,  /**< Imaging options - integer */
    GE_ADW_IM_IOPT_LEN = 4,
    GE_ADW_IM_PSEQ = 2500,  /**< Imaging pulse sequence - short integer */
    GE_ADW_IM_PSEQ_LEN = 2,
    GE_ADW_IM_PSDNAME = 2504,   /**< Pulse sequence name - string */
    GE_ADW_IM_PSDNAME_LEN = 33,
    GE_ADW_IM_CTYP = 2558,  /**< Coil type - short integer */
    GE_ADW_IM_CTYP_LEN = 2,
    GE_ADW_IM_CNAME = 2560,     /**< Coil name - string */
    GE_ADW_IM_CNAME_LEN = 17,
    GE_ADW_IM_SUPP_TECH = 2592,         /**< SAT type FAT/WATER/NONE - short
                                          integer */
    GE_ADW_IM_SUPP_TECH_LEN = 2,
    GE_ADW_IM_VBW = 2596,   /**< Variable bandwidth (Hz) - float */
    GE_ADW_IM_VBW_LEN = 4,
    GE_ADW_IM_SLQUANT = 2600,   /**< Number of slices in scan group - short
                                  integer */
    GE_ADW_IM_SLQUANT_LEN = 2,
    GE_ADW_IM_USER0 = 2608,     /**< User variable 0 - float */
    GE_ADW_IM_USER0_LEN = 4,
    GE_ADW_IM_USER1 = 2612,     /**< User variable 1 - float */
    GE_ADW_IM_USER1_LEN = 4,
    GE_ADW_IM_USER2 = 2616,     /**< User variable 2 - float */
    GE_ADW_IM_USER2_LEN = 4,
    GE_ADW_IM_USER3 = 2620,     /**< User variable 3 - float */
    GE_ADW_IM_USER3_LEN = 4,
    GE_ADW_IM_USER4 = 2624,     /**< User variable 4 - float */
    GE_ADW_IM_USER4_LEN = 4,
    GE_ADW_IM_USER5 = 2628,     /**< User variable 5 - float */
    GE_ADW_IM_USER5_LEN = 4,
    GE_ADW_IM_USER6 = 2632,     /**< User variable 6 - float */
    GE_ADW_IM_USER6_LEN = 4,
    GE_ADW_IM_USER7 = 2636,     /**< User variable 7 - float */
    GE_ADW_IM_USER7_LEN = 4,
    GE_ADW_IM_USER8 = 2640,     /**< User variable 8 - float */
    GE_ADW_IM_USER8_LEN = 4,
    GE_ADW_IM_USER9 = 2644,     /**< User variable 9 - float */
    GE_ADW_IM_USER9_LEN = 4,
    GE_ADW_IM_USER10 = 2648,    /**< User variable 10 - float */
    GE_ADW_IM_USER10_LEN = 4,
    GE_ADW_IM_USER11 = 2652,    /**< User variable 11 - float */
    GE_ADW_IM_USER11_LEN = 4,
    GE_ADW_IM_USER12 = 2656,    /**< User variable 12 - float */
    GE_ADW_IM_USER12_LEN = 4,
    GE_ADW_IM_USER13 = 2660,    /**< User variable 13 - float */
    GE_ADW_IM_USER13_LEN = 4,
    GE_ADW_IM_USER14 = 2664,    /**< User variable 14 - float */
    GE_ADW_IM_USER14_LEN = 4,
    GE_ADW_IM_USER15 = 2668,    /**< User variable 15 - float */
    GE_ADW_IM_USER15_LEN = 4,
    GE_ADW_IM_USER16 = 2672,    /**< User variable 16 - float */
    GE_ADW_IM_USER16_LEN = 4,
    GE_ADW_IM_USER17 = 2676,    /**< User variable 17 - float */
    GE_ADW_IM_USER17_LEN = 4,
    GE_ADW_IM_USER18 = 2680,    /**< User variable 18 - float */
    GE_ADW_IM_USER18_LEN = 4,
    GE_ADW_IM_USER19 = 2684,    /**< User variable 19 - float */
    GE_ADW_IM_USER19_LEN = 4,
    GE_ADW_IM_USER20 = 2688,    /**< User variable 20 - float */
    GE_ADW_IM_USER20_LEN = 4,
    GE_ADW_IM_USER21 = 2692,    /**< User variable 21 - float */
    GE_ADW_IM_USER21_LEN = 4,
    GE_ADW_IM_USER22 = 2696,    /**< User variable 22 - float */
    GE_ADW_IM_USER22_LEN = 4,
    GE_ADW_IM_USER23 = 2700,    /**< User variable 23 - float */
    GE_ADW_IM_USER23_LEN = 4,
    GE_ADW_IM_USER24 = 2704,    /**< User variable 24 - float */
    GE_ADW_IM_USER24_LEN = 4,
    GE_ADW_IM_SATBITS = 2756,   /**< Bitmap of sat selections - short integer */
    GE_ADW_IM_SATBITS_LEN = 2,
    GE_ADW_IM_SCIC = 2758,  /**< Surface coil intensity correction flag - short
                              integer */
    GE_ADW_IM_SCIC_LEN = 2,
    GE_ADW_IM_FLAX = 2778,  /**< Phase contrast flow analysis - short integer */
    GE_ADW_IM_FLAX_LEN = 2,
    GE_ADW_IM_VENC = 2780,  /**< Phase contrast flow encoding - short integer */
    GE_ADW_IM_VENC_LEN = 2,
    GE_ADW_IM_THK_DISCLMR = 2782,       /**< Slice thickness - short integer */
    GE_ADW_IM_THK_DISCLMR_LEN = 2,
    GE_ADW_IM_VAS_COLLAPSE = 2790,      /**< Collapse image - short integer */
    GE_ADW_IM_VAS_COLLAPSE_LEN = 2,
    GE_ADW_IM_X_AXIS_ROT = 2816,        /**< X-axis rotation - float */
    GE_ADW_IM_X_AXIS_ROT_LEN = 4,
    GE_ADW_IM_Y_AXIS_ROT = 2820,        /**< Y-axis rotation - float */
    GE_ADW_IM_Y_AXIS_ROT_LEN = 4,
    GE_ADW_IM_Z_AXIS_ROT = 2824,        /**< Z-axis rotation - float */
    GE_ADW_IM_Z_AXIS_ROT_LEN = 4,
    GE_ADW_IM_ECHO_TRN = 2844,  /**< Length of echo train - short integer */
    GE_ADW_IM_ECHO_TRN_LEN = 2,
    GE_ADW_IM_FRAC_ECHO = 2846,         /**< Fractional echo - short integer */
    GE_ADW_IM_FRAC_ECHO_LEN = 2,
    GE_ADW_IM_PREP_PULSE = 2848,        /**< Prep pulses - short integer */
    GE_ADW_IM_PREP_PULSE_LEN = 2,
    GE_ADW_IM_CPHASENUM = 2850,         /**< Cardiac phase number - short
                                          integer */
    GE_ADW_IM_CPHASENUM_LEN = 2,
    GE_ADW_IM_VAR_ECHO = 2852,  /**< Variable echo - short integer */
    GE_ADW_IM_VAR_ECHO_LEN = 2,
    GE_ADW_IM_FREQ_DIR = 2948,  /**< Frequency direction - short integer */
    GE_ADW_IM_FREQ_DIR_LEN = 2,
    GE_ADW_IM_VMODE = 2950,     /**< Vascular mode - short integer */
    GE_ADW_IM_VMODE_LEN = 2,

    GE_ADW_FIXED_HDR_LENGTH = 3228,         /**< Total Length of the Fixed
                                              header */
    GE_ADW_VARIABLE_HDR_LENGTH_LEN = 4,
    GE_ADW_VARIABLE_HDR_LENGTH = 3232      /**< Size of variable header -
                                             integer */
    };
};
} // end namespace itk

#endif // itkAnalyzeImageIO_h
