/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIOCommon.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkIOCommon_h
#define __itkIOCommon_h

#include "itkIntTypes.h"
#include "itkProcessObject.h"
#include "itkObjectFactory.h"
#include <string>
#ifdef MAXPATHLEN
#undef MAXPATHLEN
#endif

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
  /**
   * \enum ValidOrientationFlags
   * Valid Orientation values for objects
   * - Key  Description           Origin   dims[1]  dims[2]  dims[3]
   * - =================================================================
   * - 0    transverse-unflipped   IRP       R->L     P->A    I->S
   * - 1    coronal-unflipped      IRP       R->L     I->S    P->A
   * - 2    sagittal-unflipped     IRP       P->A     I->S    R->L
   * - 3    transverse-flipped     IRA       R->L     A->P    I->S
   * - 4    coronal-flipped        SRP       R->L     S->I    P->A
   * - 5    sagittal-flipped       ILP       P->A     I->S    L->R
   * - Where the Origin disignators are with respect to the patient
   * - [(I)nferior|(S)uperior] [(L}eft|(R)ight] [(A)nterior|(P)osterior]
   * \note Key's 0-5 correspond to the Analyze v7.5 orientations, and should not be changed.
   */
  typedef enum {
    ITK_ORIENTATION_IRP_TRANSVERSE=0,        /**< Denotes a transverse data orientation Right-->Left, */
    ITK_ORIENTATION_IRP_CORONAL   =1,        /**< Denotes a coronal data orientation */
    ITK_ORIENTATION_IRP_SAGITTAL  =2,        /**< Denotes a sagittal data orientation */
    ITK_ORIENTATION_IRA_TRANSVERSE_FLIPPED=3,/**<  */
    ITK_ORIENTATION_SRP_CORONAL_FLIPPED=4,   /**<  */
    ITK_ORIENTATION_ILP_SAGITTAL_FLIPPED=5   /**<  */
  } ValidOrientationFlags;
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

  /** Cross-platform case-insensitive string comparison
   * (MSVC++ doesn't provide strucmp, so necessary to define our own
   * This function taken from /usr/Image library developed at UNC. */
  static int Strucmp(const char *s1, const char *s2);

  /** Given a full filename, extracts just the pathname. */
  static char* ExtractFilePath (const char* fileName);

  /** Given a full filename, extracts just the file extension. */
  static char* ExtractFileExtension (const char* fileName);

  /** Given a full filename, extracts just the filename. */
  static char* ExtractFileName (const char* fileName);

  /** Given a filename determine whether it exists and 
   * return true if it does. */
  static bool FileExists(const char* filename);

  /** Given a filename determine how large it is
   * \author Kent Williams
   * \param filename The name of the file to check
   * \return Returns the size of the file in bytes
   * \ it will return zero for non-existant files as well;
   * \ so it's important to check existence separately
   */
  static unsigned long FileLength(const char* filename);
  static char *RealPath(const char *path, char *resolved_path);
};
  

 const char *const ITK_OnDiskStorageTypeName = "ITK_OnDiskStorageTypeName";
 const char *const ITK_ImageFileBaseName = "ITK_ImageFileBaseName";
 const char *const ITK_VoxelUnits = "ITK_VoxelUnits";
 const char *const ITK_OnDiskBitPerPixel = "ITK_OnDiskBitPerPixel";
 const char *const SPM_ROI_SCALE = "SPM_ROI_SCALE";
 const char *const ITK_FileNotes = "ITK_FileNotes";
 const char *const ITK_Orientation = "ITK_Orientation";
 const char *const ITK_FileOriginator = "ITK_FileOriginator";
 const char *const ITK_OriginationDate = "ITK_OriginationDate";
 const char *const ITK_PatientID = "ITK_PatientID";
 const char *const ITK_ExperimentDate = "ITK_ExperimentDate";
 const char *const ITK_ExperimentTime = "ITK_ExperimentTime";
 const char *const ANALYZE_ScanNumber = "ANALYZE_ScanNumber";
 const char *const ANALYZE_O_MAX = "ANALYZE_O_MAX";
 const char *const ANALYZE_O_MIN = "ANALYZE_O_MIN";
 const char *const ANALYZE_S_MAX = "ANALYZE_S_MAX";
 const char *const ANALYZE_S_MIN = "ANALYZE_S_MIN";
 const char *const ANALYZE_CAL_MAX = "ANALYZE_CAL_MAX";
 const char *const ANALYZE_CAL_MIN = "ANALYZE_CAL_MIN";
 const char *const ANALYZE_GLMAX = "ANALYZE_GLMAX";
 const char *const ANALYZE_GLMIN = "ANALYZE_GLMIN";
 const char *const ANALYZE_AUX_FILE_NAME = "ANALYZE_AUX_FILE_NAME";
 const char *const ANALYZE_CALIBRATIONUNITS = "ANALYZE_CALIBRATIONUNITS";

} // end namespace itk

#endif // __itkIOCommon_h
