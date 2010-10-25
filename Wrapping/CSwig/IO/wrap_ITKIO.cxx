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

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const package = ITK_WRAP_PACKAGE_NAME(ITK_WRAP_PACKAGE);
  const char* const groups[] =
  {
    ITK_WRAP_GROUP(IOBase),
    ITK_WRAP_GROUP(itkImageFileReader_2D),
    ITK_WRAP_GROUP(itkImageFileReader_3D),
#ifdef ITK_TCL_WRAP
    ITK_WRAP_GROUP(itkTkImageViewer2D),
#endif
    ITK_WRAP_GROUP(itkImageFileWriter_2D),
    ITK_WRAP_GROUP(itkImageFileWriter_3D),
    ITK_WRAP_GROUP(itkImageSeriesReader),
    ITK_WRAP_GROUP(itkImageSeriesWriter)
  };
}
#endif
