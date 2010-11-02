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
  const char* const group = ITK_WRAP_GROUP(itkImageFileReader_2D);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(ImageFileReader, image::F2, itkImageFileReaderF2);
    ITK_WRAP_OBJECT1(ImageFileReader, image::VF2, itkImageFileReaderVF2);
    ITK_WRAP_OBJECT1(ImageFileReader, image::D2, itkImageFileReaderD2);
    ITK_WRAP_OBJECT1(ImageFileReader, image::UC2, itkImageFileReaderUC2);
    ITK_WRAP_OBJECT1(ImageFileReader, image::US2, itkImageFileReaderUS2);
    ITK_WRAP_OBJECT1(ImageFileReader, image::UL2, itkImageFileReaderUL2);
    ITK_WRAP_OBJECT1(ImageFileReader, image::UI2, itkImageFileReaderUI2);
    ITK_WRAP_OBJECT1(ImageFileReader, image::SS2, itkImageFileReaderSS2);
    ITK_WRAP_OBJECT1(ImageFileReader, image::SI2, itkImageFileReaderSI2);
  }
}

#endif
