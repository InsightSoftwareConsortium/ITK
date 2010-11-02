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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkVTKImageIO2Factory.h"
#include "itkCreateObjectFunction.h"
#include "itkVTKImageIO2.h"
#include "itkVersion.h"

namespace itk
{
VTKImageIO2Factory::VTKImageIO2Factory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkVTKImageIO2",
                          "VTK Image IO",
                          1,
                          CreateObjectFunction< VTKImageIO2 >::New() );
}

VTKImageIO2Factory::~VTKImageIO2Factory()
{}

const char *
VTKImageIO2Factory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
VTKImageIO2Factory::GetDescription(void) const
{
  return "VTK ImageIO2 Factory, allows the loading and streaming of VTK images into ITK";
}
} // end namespace itk
