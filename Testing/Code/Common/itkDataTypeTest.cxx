#include <iostream>
#include "itkScalar.h"
#include "itkVector.h"


// using namespace itk;

int main()
{
  itk::Scalar<double> s;
  itk::Vector<int, 4> v;

  int vec[4];
  vec[0] = 1; vec[1] = 3; vec[2] = 2; vec[3] = 4;
  
  s.SetScalar( 5.1 );
  std::cout << "Scalar value = " << s.GetScalar() << std::endl;

  v.SetVector( vec );
  std::cout << "Vector value = ";
  for (int i=0; i < v.GetVectorDimension(); i++)
    {
    std::cout << v.GetVector()[i];
    if (i < v.GetVectorDimension() - 1)
      {
      std::cout << ", ";
      }
    }
  std::cout << std::endl;
  
  return 0;
}
