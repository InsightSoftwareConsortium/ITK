#include "iostream.h"
#include "itkScalar.h"
#include "itkVector.h"


// using namespace itk;

int main()
{
  itk::itkScalar<double> s;
  itk::itkVector<int, 4> v;

  int vec[4];
  vec[0] = 1; vec[1] = 3; vec[2] = 2; vec[3] = 4;
  
  s.SetScalar( 5.1 );
  cout << "Scalar value = " << s.GetScalar() << endl;

  v.SetVector( vec );
  cout << "Vector value = ";
  for (int i=0; i < v.GetVectorDimension(); i++)
    {
    cout << v.GetVector()[i];
    if (i < v.GetVectorDimension() - 1)
      {
      cout << ", ";
      }
    }
  cout << endl;
  
  return 0;
}
