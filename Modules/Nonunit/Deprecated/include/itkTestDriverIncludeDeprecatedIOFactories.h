#ifndef __itkTestDriverIncludeDeprecatedIOFactories_h
#define __itkTestDriverIncludeDeprecatedIOFactories_h
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
#include "itkAnalyzeImageIOFactory.h"
#include "itkDICOMImageIO2Factory.h"
#include "itkDicomImageIOFactory.h"
#include "itkTestDriverInclude.h"
#include "itkObjectFactoryBase.h"

void ProcessArgumentsAndRegisterDeprecatedIOFactories(int *ac, ArgumentStringType *av)
{
  itk::ObjectFactoryBase::RegisterFactory( itk::BioRadImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::GDCMImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::MetaImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::PNGImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::VTKImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::GiplImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::LSMImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::NiftiImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::JPEGImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::TIFFImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::NrrdImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::BMPImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::AnalyzeImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::DicomImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::DICOMImageIO2Factory::New() );

  ProcessArguments( ac, av );

}
#endif
