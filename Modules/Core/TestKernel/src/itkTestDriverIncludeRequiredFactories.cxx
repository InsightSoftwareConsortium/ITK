/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkTestDriverIncludeRequiredFactories.h"

// ImageIO
#include "itkGDCMImageIOFactory.h"
#include "itkMetaImageIOFactory.h"
#include "itkJPEGImageIOFactory.h"
#include "itkPNGImageIOFactory.h"
#include "itkTIFFImageIOFactory.h"
#include "itkBMPImageIOFactory.h"
#include "itkVTKImageIOFactory.h"
#include "itkNrrdImageIOFactory.h"
#include "itkGiplImageIOFactory.h"
#include "itkNiftiImageIOFactory.h"
#include "itkTestDriverInclude.h"
#include "itkObjectFactoryBase.h"

// MeshIO
#include "itkBYUMeshIOFactory.h"
#include "itkFreeSurferAsciiMeshIOFactory.h"
#include "itkFreeSurferBinaryMeshIOFactory.h"
#include "itkGiftiMeshIOFactory.h"
#include "itkOBJMeshIOFactory.h"
#include "itkOFFMeshIOFactory.h"
#include "itkVTKPolyDataMeshIOFactory.h"

// FFT
#include "itkFFTImageFilterFactory.h"
#include "itkVnlComplexToComplex1DFFTImageFilter.h"
#include "itkVnlComplexToComplexFFTImageFilter.h"
#include "itkVnlForward1DFFTImageFilter.h"
#include "itkVnlForwardFFTImageFilter.h"
#include "itkVnlHalfHermitianToRealInverseFFTImageFilter.h"
#include "itkVnlInverse1DFFTImageFilter.h"
#include "itkVnlInverseFFTImageFilter.h"
#include "itkVnlRealToHalfHermitianForwardFFTImageFilter.h"

#ifdef __EMSCRIPTEN__
#  include <emscripten.h>
#endif

void
RegisterRequiredFactories()
{
  RegisterRequiredIOFactories();

  RegisterRequiredFFTFactories();
}

void
RegisterRequiredIOFactories()
{
  // ImageIO
  itk::ObjectFactoryBase::RegisterFactory(itk::MetaImageIOFactory::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::GDCMImageIOFactory::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::JPEGImageIOFactory::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::VTKImageIOFactory::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::PNGImageIOFactory::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::TIFFImageIOFactory::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::BMPImageIOFactory::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::NrrdImageIOFactory::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::GiplImageIOFactory::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::NiftiImageIOFactory::New());

  // MeshIO
  itk::ObjectFactoryBase::RegisterFactory(itk::BYUMeshIOFactory::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::FreeSurferAsciiMeshIOFactory::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::FreeSurferBinaryMeshIOFactory::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::GiftiMeshIOFactory::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::OBJMeshIOFactory::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::OFFMeshIOFactory::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::VTKPolyDataMeshIOFactory::New());
}

void
RegisterRequiredFFTFactories()
{
  itk::ObjectFactoryBase::RegisterFactory(itk::FFTImageFilterFactory<itk::VnlComplexToComplex1DFFTImageFilter>::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::FFTImageFilterFactory<itk::VnlComplexToComplexFFTImageFilter>::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::FFTImageFilterFactory<itk::VnlForward1DFFTImageFilter>::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::FFTImageFilterFactory<itk::VnlForwardFFTImageFilter>::New());
  itk::ObjectFactoryBase::RegisterFactory(
    itk::FFTImageFilterFactory<itk::VnlHalfHermitianToRealInverseFFTImageFilter>::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::FFTImageFilterFactory<itk::VnlInverse1DFFTImageFilter>::New());
  itk::ObjectFactoryBase::RegisterFactory(itk::FFTImageFilterFactory<itk::VnlInverseFFTImageFilter>::New());
  itk::ObjectFactoryBase::RegisterFactory(
    itk::FFTImageFilterFactory<itk::VnlRealToHalfHermitianForwardFFTImageFilter>::New());
}

void
ProcessArgumentsAndRegisterRequiredFactories(int * argc, ArgumentStringType * argv)
{
  try
  {
    RegisterRequiredFactories();
  }
  catch (const itk::ExceptionObject & error)
  {
    std::cerr << "Error during registration of required factories: " << error << std::endl;
  }
  ProcessArguments(argc, argv);
}
