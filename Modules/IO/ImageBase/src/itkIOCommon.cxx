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
#include "itkIOCommon.h"

namespace itk
{
const char *const ITK_OnDiskStorageTypeName = "ITK_OnDiskStorageTypeName";
const char *const ITK_ImageFileBaseName = "ITK_ImageFileBaseName";
const char *const ITK_VoxelUnits = "ITK_VoxelUnits";
const char *const ITK_OnDiskBitPerPixel = "ITK_OnDiskBitPerPixel";
const char *const SPM_ROI_SCALE = "SPM_ROI_SCALE";
const char *const ITK_FileNotes = "ITK_FileNotes";
const char *const ITK_Origin = "ITK_Origin";
const char *const ITK_Spacing = "ITK_Spacing";
const char *const ITK_ZDirection = "ITK_ZDirection";
const char *const ITK_FileOriginator = "ITK_FileOriginator";
const char *const ITK_OriginationDate = "ITK_OriginationDate";
const char *const ITK_PatientID = "ITK_PatientID";
const char *const ITK_ExperimentDate = "ITK_ExperimentDate";
const char *const ITK_ExperimentTime = "ITK_ExperimentTime";
const char *const ITK_InputFilterName = "ITK_InputFilterName";
const char *const ITK_NumberOfDimensions = "ITK_NumberOfDimensions";
const char *const ITK_PatientName = "ITK_PatientName";
const char *const ITK_ScanID = "ITK_ScanID";
const char *const ITK_ImageType = "ITK_ImageType";
const char *const ROI_NAME = "ROI_NAME";
const char *const ROI_X_SIZE = "ROI_X_SIZE";
const char *const ROI_X_RESOLUTION = "ROI_X_RESOLUTION";
const char *const ROI_Y_SIZE = "ROI_Y_SIZE";
const char *const ROI_Y_RESOLUTION = "ROI_Y_RESOLUTION";
const char *const ROI_Z_SIZE = "ROI_Z_SIZE";
const char *const ROI_Z_RESOLUTION = "ROI_Z_RESOLUTION";
const char *const ROI_NUM_SEGMENTS = "ROI_NUM_SEGMENTS";
const char *const ROI_PLANE = "ROI_PLANE";
const char *const ROI_SCAN_ID = "ROI_SCAN_ID";

std::string IOCommon
::AtomicPixelTypeToString(const AtomicPixelType pixelType)
{
  switch ( pixelType )
    {
    case ITK_UCHAR:
      return "unsigned char";

    case ITK_CHAR:
      return "char";

    case ITK_USHORT:
      return "unsigned short";

    case ITK_SHORT:
      return "short";

    case ITK_UINT:
      return "unsigned int";

    case ITK_INT:
      return "int";

    case ITK_ULONG:
      return "unsigned long";

    case ITK_LONG:
      return "long";

    case ITK_FLOAT:
      return "float";

    case ITK_DOUBLE:
      return "double";

    default:
      return "unknown";
    }
}

unsigned int IOCommon
::ComputeSizeOfAtomicPixelType(const AtomicPixelType pixelType)
{
  switch ( pixelType )
    {
    case ITK_CHAR:
      return static_cast< unsigned int >( sizeof( char ) );

    case ITK_UCHAR:
      return static_cast< unsigned int >( sizeof( unsigned char ) );

    case ITK_SHORT:
      return static_cast< unsigned int >( sizeof( short ) );

    case ITK_USHORT:
      return static_cast< unsigned int >( sizeof( unsigned short ) );

    case ITK_INT:
      return static_cast< unsigned int >( sizeof( int ) );

    case ITK_UINT:
      return static_cast< unsigned int >( sizeof( unsigned int ) );

    case ITK_LONG:
      return static_cast< unsigned int >( sizeof( long ) );

    case ITK_ULONG:
      return static_cast< unsigned int >( sizeof( unsigned long ) );

    case ITK_FLOAT:
      return static_cast< unsigned int >( sizeof( float ) );

    case ITK_DOUBLE:
      return static_cast< unsigned int >( sizeof( double ) );

    default:
      return static_cast< unsigned int >( sizeof( char ) );

    }
}
} // namespace itk
