#include <iostream>
#include "itkVector.h"


// using namespace itk;

int main()
{
  itk::Vector<int, 4> v;

  v = 1,2,3,4;
  std::cout << "Vector value = ";
  for (unsigned int i=0; i < v.GetVectorDimension(); i++)
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
