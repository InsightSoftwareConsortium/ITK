/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkIOCommon.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkIOCommon_h
#define __itkIOCommon_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkIntTypes.h"
#include "itkProcessObject.h"
#include "itkObjectFactory.h"

namespace itk
{

/** \class IOCommon
   * \brief Centralized funtionality for IO classes.
   *
   * This class provides encapsulated functionality to support the IO classes.
   *
   * \ingroup IOFilters
   *
   */
class ITK_EXPORT IOCommon 
{
public:

    //  Coordinate orientation codes have a place-value organization such that 
    //  an ImageDimension-al sequence of subcodes says both which varies fastest
    //  through which varies slowest, but also which end of the frame of reference
    //  is considered zero for each of the coordinates.  For example, 'RIP' means
    //  Right to Left varies fastest, then Inferior to Superior, and Posterior to
    //  Anterior varies the slowest.  
    typedef enum {
        ITK_COORDINATE_Right=2,
        ITK_COORDINATE_Left=3,
        ITK_COORDINATE_Posterior=4,    //back
        ITK_COORDINATE_Anterior=5,     //front
        ITK_COORDINATE_Inferior=8,     //below
        ITK_COORDINATE_Superior=9      //above
            // ITK_COORDINATE_Historical=16,
            // ITK_COORDINATE_Future=17
    } CoordinateTerms;

    typedef enum {
        // These code place values have to be far enough apart to separate the CoordinateTerms above.
        // However, if we added History/Future direction in time, we would need at least 5 bits per.
        ITK_COORDINATE_PrimaryMinor=0,
        ITK_COORDINATE_SecondaryMinor=8,
        ITK_COORDINATE_TertiaryMinor=16
            // Majorness is in terms of rank-minor because a 
            // fourth dimension, time, would be even More major than the PrimaryMajor==TertiaryMinor.
            // ITK_COORDINATE_QuaternaryMinor=24
    } CoordinateMajornessTerms;
    // Adding time IN GENERAL would make these 8 x 6 = 48 triples into 16 x 24 = 384 4-tuples.
    // A general fourth dimension would need a unique pair of letters to add to the code;
    // Maybe use H and F, from History to Future? Maybe use 48 x 2 with time ALWAYS highest 
    // order, or 48 x 2 x 2, with 3-space always highest or lowest order?  Multispectra might be 
    // co-registered and pieced together.... PD-T2 interleaving of slices is handled with choosing
    // which spectrum to load via the prototypical file name. 
    typedef enum {
        ITK_COORDINATE_ORIENTATION_RIP = (ITK_COORDINATE_Right     << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Inferior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Posterior << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_LIP = (ITK_COORDINATE_Left      << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Inferior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Posterior << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_RSP = (ITK_COORDINATE_Right     << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Superior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Posterior << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_LSP = (ITK_COORDINATE_Left      << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Superior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Posterior << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_RIA = (ITK_COORDINATE_Right     << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Inferior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Anterior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_LIA = (ITK_COORDINATE_Left      << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Inferior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Anterior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_RSA = (ITK_COORDINATE_Right     << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Superior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Anterior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_LSA = (ITK_COORDINATE_Left      << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Superior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Anterior  << ITK_COORDINATE_TertiaryMinor),

        ITK_COORDINATE_ORIENTATION_IRP = (ITK_COORDINATE_Inferior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Right     << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Posterior << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_ILP = (ITK_COORDINATE_Inferior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Left      << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Posterior << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_SRP = (ITK_COORDINATE_Superior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Right     << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Posterior << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_SLP = (ITK_COORDINATE_Superior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Left      << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Posterior << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_IRA = (ITK_COORDINATE_Inferior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Right     << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Anterior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_ILA = (ITK_COORDINATE_Inferior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Left      << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Anterior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_SRA = (ITK_COORDINATE_Superior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Right     << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Anterior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_SLA = (ITK_COORDINATE_Superior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Left      << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Anterior  << ITK_COORDINATE_TertiaryMinor),

        ITK_COORDINATE_ORIENTATION_RPI = (ITK_COORDINATE_Right     << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Posterior << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Inferior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_LPI = (ITK_COORDINATE_Left      << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Posterior << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Inferior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_RAI = (ITK_COORDINATE_Right     << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Anterior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Inferior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_LAI = (ITK_COORDINATE_Left      << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Anterior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Inferior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_RPS = (ITK_COORDINATE_Right     << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Posterior << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Superior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_LPS = (ITK_COORDINATE_Left      << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Posterior << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Superior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_RAS = (ITK_COORDINATE_Right     << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Anterior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Superior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_LAS = (ITK_COORDINATE_Left      << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Anterior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Superior  << ITK_COORDINATE_TertiaryMinor),

        ITK_COORDINATE_ORIENTATION_PRI = (ITK_COORDINATE_Posterior << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Right     << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Inferior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_PLI = (ITK_COORDINATE_Posterior << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Left      << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Inferior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_ARI = (ITK_COORDINATE_Anterior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Right     << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Inferior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_ALI = (ITK_COORDINATE_Anterior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Left      << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Inferior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_PRS = (ITK_COORDINATE_Posterior << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Right     << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Superior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_PLS = (ITK_COORDINATE_Posterior << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Left      << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Superior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_ARS = (ITK_COORDINATE_Anterior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Right     << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Superior  << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_ALS = (ITK_COORDINATE_Anterior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Left      << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Superior  << ITK_COORDINATE_TertiaryMinor),

        ITK_COORDINATE_ORIENTATION_IPR = (ITK_COORDINATE_Inferior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Posterior << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Right     << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_SPR = (ITK_COORDINATE_Superior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Posterior << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Right     << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_IAR = (ITK_COORDINATE_Inferior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Anterior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Right     << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_SAR = (ITK_COORDINATE_Superior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Anterior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Right     << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_IPL = (ITK_COORDINATE_Inferior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Posterior << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Left      << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_SPL = (ITK_COORDINATE_Superior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Posterior << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Left      << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_IAL = (ITK_COORDINATE_Inferior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Anterior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Left      << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_SAL = (ITK_COORDINATE_Superior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Anterior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Left      << ITK_COORDINATE_TertiaryMinor),

        ITK_COORDINATE_ORIENTATION_PIR = (ITK_COORDINATE_Posterior << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Inferior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Right     << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_PSR = (ITK_COORDINATE_Posterior << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Superior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Right     << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_AIR = (ITK_COORDINATE_Anterior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Inferior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Right     << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_ASR = (ITK_COORDINATE_Anterior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Superior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Right     << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_PIL = (ITK_COORDINATE_Posterior << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Inferior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Left      << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_PSL = (ITK_COORDINATE_Posterior << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Superior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Left      << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_AIL = (ITK_COORDINATE_Anterior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Inferior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Left      << ITK_COORDINATE_TertiaryMinor),
        ITK_COORDINATE_ORIENTATION_ASL = (ITK_COORDINATE_Anterior  << ITK_COORDINATE_PrimaryMinor) 
            + (ITK_COORDINATE_Superior  << ITK_COORDINATE_SecondaryMinor) 
            + (ITK_COORDINATE_Left      << ITK_COORDINATE_TertiaryMinor)
    } ValidCoordinateOrientationFlags;
// ^^^
// |||
// ||\Sequential indexes are separated by (planes=rows*columns) memory locations
// |\Sequential indexes are separated by rows memory locations (sweep out a plane)
// \Sequential indexes are adjacent memory locations (sweep out a row)

#ifndef __TEMPORARILY_INCLUDED_IN_COMPILAITONS__
  typedef enum {
    ITK_ORIGIN_IRP  =0,        /**< Denotes a zeroCorner (image origin) is Inferior Right Posterior */
    ITK_ORIGIN_IRA  =1,        /**< Denotes a zeroCorner (image origin) is Inferior Right Anterior */
    ITK_ORIGIN_ILP  =2,        /**< Denotes a zeroCorner (image origin) is Inferior Left Posterior */
    ITK_ORIGIN_ILA  =3,        /**< Denotes a zeroCorner (image origin) is Inferior Left Anterior */
    ITK_ORIGIN_SRP  =4,        /**< Denotes a zeroCorner (image origin) is Superior Right Posterior */
    ITK_ORIGIN_SRA  =5,        /**< Denotes a zeroCorner (image origin) is Superior Right Anterior */
    ITK_ORIGIN_SLP  =6,        /**< Denotes a zeroCorner (image origin) is Superior Left Posterior */
    ITK_ORIGIN_SLA  =7         /**< Denotes a zeroCorner (image origin) is Superior Left Anterior */
  } ValidOriginFlags;
#endif
  typedef enum
  {
    ITK_MAXPATHLEN =2048, /**< Maximum length of a filename */
    MAX_FILENAMELIST_SIZE = 512
  } SysConstants;
  typedef enum
  {
    ITK_UCHAR,         // aka uint8_t
    ITK_CHAR,         
    ITK_USHORT,        // aka uint16_t
    ITK_SHORT,        
    ITK_UINT,          // aka uint32_t
    ITK_INT,          
    ITK_ULONG,         // aka uint64_t
    ITK_LONG,         
    ITK_FLOAT,        
    ITK_DOUBLE        
  } AtomicPixelType;   // enumerated constants for the different data types

  /** Convert the enumerated type to a string representation. */
  static std::string AtomicPixelTypeToString(const AtomicPixelType pixelType);

  /** Calculate the size, in bytes, that the atomic pixel type occupies. */
  static unsigned int ComputeSizeOfAtomicPixelType(const AtomicPixelType pixelType);

  /** Given a full filename, extracts just the pathname. */
  static char* ExtractFilePath (const char* fileName);

  /** Given a full filename, extracts just the file extension. */
  static char* ExtractFileExtension (const char* fileName);

  /** Given a full filename, extracts just the filename. */
  static char* ExtractFileName (const char* fileName);

  /** Given a filename determine whether it exists and 
       * return true if it does. */
  static bool FileExists(const char* filename);

};
  

extern const char *const ITK_OnDiskStorageTypeName ;
extern const char *const ITK_ImageFileBaseName ;
extern const char *const ITK_VoxelUnits;
extern const char *const ITK_OnDiskBitPerPixel;
extern const char *const SPM_ROI_SCALE;
extern const char *const ITK_FileNotes;
extern const char *const ITK_CoordinateOrientation;
extern const char *const ITK_Origin;
extern const char *const ITK_FileOriginator;
extern const char *const ITK_OriginationDate;
extern const char *const ITK_PatientID;
extern const char *const ITK_ExperimentDate;
extern const char *const ITK_ExperimentTime;
extern const char *const ITK_InputFilterName;
extern const char *const ROI_NAME;
extern const char *const ROI_X_SIZE;
extern const char *const ROI_X_RESOLUTION;
extern const char *const ROI_Y_SIZE;
extern const char *const ROI_Y_RESOLUTION;
extern const char *const ROI_Z_SIZE;
extern const char *const ROI_Z_RESOLUTION;
extern const char *const ROI_NUM_SEGMENTS;
extern const char *const ROI_PLANE;
extern const char *const ROI_SCAN_ID;
} // end namespace itk

#endif // __itkIOCommon_h
