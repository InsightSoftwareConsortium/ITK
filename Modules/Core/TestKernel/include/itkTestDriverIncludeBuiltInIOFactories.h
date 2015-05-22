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
#ifndef itkTestDriverIncludeBuiltInIOFactories_h
#define itkTestDriverIncludeBuiltInIOFactories_h
#include "itkBioRadImageIOFactory.h"
#include "itkBMPImageIOFactory.h"
#include "itkGDCMImageIOFactory.h"
#include "itkNiftiImageIOFactory.h"
#include "itkGiplImageIOFactory.h"
#include "itkJPEGImageIOFactory.h"
#include "itkLSMImageIOFactory.h"
#include "itkMetaImageIOFactory.h"
#include "itkPNGImageIOFactory.h"
#include "itkNrrdImageIOFactory.h"
#include "itkTIFFImageIOFactory.h"
#include "itkVTKImageIOFactory.h"
#include "itkStimulateImageIOFactory.h"
#include "itkTestDriverInclude.h"
#include "itkObjectFactoryBase.h"

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#endif

void ProcessArgumentsAndRegisterBuiltInFactories(int *ac, ArgumentStringType *av)
{
  itk::ObjectFactoryBase::RegisterFactory( itk::BioRadImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::GDCMImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::MetaImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::PNGImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::VTKImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::GiplImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::LSMImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::NiftiImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::StimulateImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::JPEGImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::TIFFImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::NrrdImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::BMPImageIOFactory::New() );

  ProcessArguments( ac, av );

}
#endif
