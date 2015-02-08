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
#ifndef itkIOCommon_h
#define itkIOCommon_h
#include "ITKIOImageBaseExport.h"


#include "itkIntTypes.h"
#include "itkProcessObject.h"
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
   * \ingroup ITKIOImageBase
   */
class ITKIOImageBase_EXPORT IOCommon
{
public:
  typedef enum {
    ITK_MAXPATHLEN = 2048, /**< Maximum length of a filename */
    MAX_FILENAMELIST_SIZE = 512
    } SysConstants;
  typedef enum {
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
};

extern ITKIOImageBase_EXPORT const char *const ITK_OnDiskStorageTypeName;
extern ITKIOImageBase_EXPORT const char *const ITK_ImageFileBaseName;
extern ITKIOImageBase_EXPORT const char *const ITK_VoxelUnits;
extern ITKIOImageBase_EXPORT const char *const ITK_OnDiskBitPerPixel;
extern ITKIOImageBase_EXPORT const char *const SPM_ROI_SCALE;
extern ITKIOImageBase_EXPORT const char *const ITK_FileNotes;
extern ITKIOImageBase_EXPORT const char *const ITK_Origin;
extern ITKIOImageBase_EXPORT const char *const ITK_Spacing;
extern ITKIOImageBase_EXPORT const char *const ITK_ZDirection;
extern ITKIOImageBase_EXPORT const char *const ITK_FileOriginator;
extern ITKIOImageBase_EXPORT const char *const ITK_OriginationDate;
extern ITKIOImageBase_EXPORT const char *const ITK_PatientID;
extern ITKIOImageBase_EXPORT const char *const ITK_ExperimentDate;
extern ITKIOImageBase_EXPORT const char *const ITK_ExperimentTime;
extern ITKIOImageBase_EXPORT const char *const ITK_InputFilterName;
extern ITKIOImageBase_EXPORT const char *const ITK_NumberOfDimensions;
extern ITKIOImageBase_EXPORT const char *const ITK_ImageType;
extern ITKIOImageBase_EXPORT const char *const ITK_PatientName;
extern ITKIOImageBase_EXPORT const char *const ITK_ScanID;
extern ITKIOImageBase_EXPORT const char *const ROI_NAME;
extern ITKIOImageBase_EXPORT const char *const ROI_X_SIZE;
extern ITKIOImageBase_EXPORT const char *const ROI_X_RESOLUTION;
extern ITKIOImageBase_EXPORT const char *const ROI_Y_SIZE;
extern ITKIOImageBase_EXPORT const char *const ROI_Y_RESOLUTION;
extern ITKIOImageBase_EXPORT const char *const ROI_Z_SIZE;
extern ITKIOImageBase_EXPORT const char *const ROI_Z_RESOLUTION;
extern ITKIOImageBase_EXPORT const char *const ROI_NUM_SEGMENTS;
extern ITKIOImageBase_EXPORT const char *const ROI_PLANE;
extern ITKIOImageBase_EXPORT const char *const ROI_SCAN_ID;
} // end namespace itk

#endif // itkIOCommon_h
