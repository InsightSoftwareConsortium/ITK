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
#include "itkDicomImageIOFactory.h"
#include "itkDicomImageIO.h"
#include "itkVersion.h"

namespace itk
{
DicomImageIOFactory::DicomImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkDicomImageIO",
                          "Dicom Image IO",
                          1,
                          CreateObjectFunction< DicomImageIO >::New() );
}

DicomImageIOFactory::~DicomImageIOFactory()
{}

const char *
DicomImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
DicomImageIOFactory::GetDescription() const
{
  return "Dicom ImageIO Factory, allows the loading of Dicom images into Insight";
}
} // end namespace itk
