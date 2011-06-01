#ifndef __itkTestDriverIncludeAnalyzeIOFactory_h
#define __itkTestDriverIncludeAnalyzeIOFactory_h
#include "itkAnalyzeImageIOFactory.h"
#include "itkNiftiImageIOFactory.h"
#include "itkTestDriverInclude.h"
#include "itkObjectFactoryBase.h"

void ProcessArgumentsAndRegisterAnalyzeIOFactory(int *ac, ArgumentStringType *av)
{
  itk::ObjectFactoryBase::RegisterFactory( itk::AnalyzeImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::NiftiImageIOFactory::New() );

  ProcessArguments( ac, av );

}
#endif
