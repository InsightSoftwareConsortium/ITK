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
#include "itkImageFileReader.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImageFileReader_3D);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(ImageFileReader, image::F3, itkImageFileReaderF3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::VF3, itkImageFileReaderVF3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::D3, itkImageFileReaderD3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::UC3, itkImageFileReaderUC3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::US3, itkImageFileReaderUS3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::UL3, itkImageFileReaderUL3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::UI3, itkImageFileReaderUI3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::SS3, itkImageFileReaderSS3);
    ITK_WRAP_OBJECT1(ImageFileReader, image::SI3, itkImageFileReaderSI3);
  }
}

#endif
