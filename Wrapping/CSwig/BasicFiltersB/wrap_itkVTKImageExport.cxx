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
#include "itkVTKImageExport.h"
#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkVTKImageExport);
  namespace wrappers
  {
    ITK_WRAP_OBJECT(VTKImageExportBase);
    ITK_WRAP_OBJECT1(VTKImageExport, image::F2,
                                     itkVTKImageExportF2);
    ITK_WRAP_OBJECT1(VTKImageExport, image::UC2,
                                     itkVTKImageExportUC2);
    ITK_WRAP_OBJECT1(VTKImageExport, image::US2,
                                     itkVTKImageExportUS2);
    ITK_WRAP_OBJECT1(VTKImageExport, image::UL2,
                                     itkVTKImageExportUL2);

    ITK_WRAP_OBJECT1(VTKImageExport, image::F3,
                                     itkVTKImageExportF3);
    ITK_WRAP_OBJECT1(VTKImageExport, image::UC3,
                                     itkVTKImageExportUC3);
    ITK_WRAP_OBJECT1(VTKImageExport, image::US3,
                                     itkVTKImageExportUS3);
    ITK_WRAP_OBJECT1(VTKImageExport, image::UL3,
                                     itkVTKImageExportUL3);

  }
}

#endif
