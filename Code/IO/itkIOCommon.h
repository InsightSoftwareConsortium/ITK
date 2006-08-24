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
#include "itkSpatialOrientation.h"

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
#if defined(DEPRECATED_METADATA_ORIENTATION)
extern const char *const ITK_CoordinateOrientation;
#endif
extern const char *const ITK_Origin;
extern const char *const ITK_Spacing;
extern const char *const ITK_FileOriginator;
extern const char *const ITK_OriginationDate;
extern const char *const ITK_PatientID;
extern const char *const ITK_ExperimentDate;
extern const char *const ITK_ExperimentTime;
extern const char *const ITK_InputFilterName;
extern const char *const ITK_NumberOfDimensions;
extern const char *const ITK_ImageType;
extern const char *const ITK_PatientName;
extern const char *const ITK_ScanID;
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
