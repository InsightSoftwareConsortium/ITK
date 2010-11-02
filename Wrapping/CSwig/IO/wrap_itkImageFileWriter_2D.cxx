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
#include "itkImageFileWriter.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImageFileWriter_2D);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(ImageFileWriter, image::F2, itkImageFileWriterF2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::VF2, itkImageFileWriterVF2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::D2, itkImageFileWriterD2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::UC2, itkImageFileWriterUC2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::US2, itkImageFileWriterUS2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::UL2, itkImageFileWriterUL2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::UI2, itkImageFileWriterUI2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::SS2, itkImageFileWriterSS2);
    ITK_WRAP_OBJECT1(ImageFileWriter, image::SI2, itkImageFileWriterSI2);
  }
}

#endif
