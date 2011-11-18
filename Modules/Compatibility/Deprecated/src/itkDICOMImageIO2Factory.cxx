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
#include "itkDICOMImageIO2Factory.h"
#include "itkDICOMImageIO2.h"
#include "itkVersion.h"

namespace itk
{
DICOMImageIO2Factory::DICOMImageIO2Factory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkDICOMImageIO2",
                          "DICOM Image IO",
                          1,
                          CreateObjectFunction< DICOMImageIO2 >::New() );
}

DICOMImageIO2Factory::~DICOMImageIO2Factory()
{}

const char *
DICOMImageIO2Factory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
DICOMImageIO2Factory::GetDescription() const
{
  return "DICOM ImageIO Factory, allows the loading of DICOM images into Insight";
}
} // end namespace itk
