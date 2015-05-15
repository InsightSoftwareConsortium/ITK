#ifndef __AIDANS_INSTANCE_
#define __AIDANS_INSTANCE_
// Aidan's trick
#include <itkSmartPointer.h>
namespace itk
{
template< typename T >
class Instance:public T::Pointer
{
public:
  Instance():SmartPointer< T >( T::New() ) {}
};
}

#endif
