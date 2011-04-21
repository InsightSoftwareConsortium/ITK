#ifndef __itkTestDriverIncludeRequiredIOFactories_h
#define __itkTestDriverIncludeRequiredIOFactories_h
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

void RegisterRequiredFactories(){
  itk::ObjectFactoryBase::RegisterFactory( itk::MetaImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::GDCMImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::JPEGImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::VTKImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::PNGImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::TIFFImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::BMPImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::NrrdImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::GiplImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::NiftiImageIOFactory::New() );
}

void ProcessArgumentsAndRegisterRequiredFactories(int *ac, ArgumentStringType *av)
{
  RegisterRequiredFactories();
  ProcessArguments( ac, av );

}
#endif
