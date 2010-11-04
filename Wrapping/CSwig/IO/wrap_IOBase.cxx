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
#include "itkRawImageIO.h"
#include "itkPNGImageIO.h"
#include "itkMetaImageIO.h"
#include "itkDicomImageIO.h"
#include "itkPNGImageIOFactory.h"
#include "itkMetaImageIOFactory.h"
#include "itkDicomImageIOFactory.h"
#include "itkDICOMSeriesFileNames.h"
#include "itkNumericSeriesFileNames.h"
#include "itkRegularExpressionSeriesFileNames.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(IOBase);
  namespace wrappers
  {
    ITK_WRAP_OBJECT(ImageIOBase);
    ITK_WRAP_OBJECT(PNGImageIO);
    ITK_WRAP_OBJECT(MetaImageIO);
    ITK_WRAP_OBJECT(DicomImageIO);
    ITK_WRAP_OBJECT(GDCMImageIO);
    ITK_WRAP_OBJECT(PNGImageIOFactory);
    ITK_WRAP_OBJECT(MetaImageIOFactory);
    ITK_WRAP_OBJECT(DicomImageIOFactory);
    ITK_WRAP_OBJECT2(RawImageIO, float, 2, itkRawImageIOF2);
    ITK_WRAP_OBJECT2(RawImageIO, float, 3, itkRawImageIOF3);
    ITK_WRAP_OBJECT(DICOMSeriesFileNames);
    ITK_WRAP_OBJECT(NumericSeriesFileNames);
    ITK_WRAP_OBJECT(RegularExpressionSeriesFileNames);
  }
}

#endif
