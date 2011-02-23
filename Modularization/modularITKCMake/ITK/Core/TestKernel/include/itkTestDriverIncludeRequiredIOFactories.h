#include "itkMetaImageIOFactory.h"
#include "itkPNGImageIOFactory.h"
#include "itkTestDriverInclude.h"
#include "itkObjectFactoryBase.h"

void ProcessArgumentsAndRegisterRequiredFactories(int *ac, ArgumentStringType *av)
{
  itk::ObjectFactoryBase::RegisterFactory( itk::MetaImageIOFactory::New() );
  itk::ObjectFactoryBase::RegisterFactory( itk::PNGImageIOFactory::New() );

  ProcessArguments( ac, av );

}
