#include <iostream>
#include "itkScalar.h"
#include "itkVector.h"


// using namespace itk;

int main()
{
  itk::Scalar<double> s;
  itk::Vector<int, 4> v;

  s.SetScalar( 5.1 );
  std::cout << "Scalar value = " << s.GetScalar() << std::endl;

  v = 1,2,3,4;
  std::cout << "Vector value = ";
  for (int i=0; i < v.GetVectorDimension(); i++)
    {
    std::cout << v[i];
    if (i < v.GetVectorDimension() - 1)
      {
      std::cout << ", ";
      }
    }
  std::cout << std::endl;
  
  return 0;
}
