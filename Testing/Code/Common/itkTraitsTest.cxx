#include "itkImage.h"
#include "itkPixelTraits.h"
#include "itkScalar.h"
#include "itkVector.h"

// Important note: many compilers define macros like min() and max(). This
// program will fail if that's the case. (For example, the 
// std::numeric_limits<>::min() collides with the min() macro.)

main ()
{
  // Test some numeric traits
  float min = itk::NumericTraits<float>::min();
  unsigned short max = itk::NumericTraits<unsigned short>::max();

  char adder=10;
  itk::NumericTraits<char>::AccumulateType addItUp = itk::NumericTraits<char>::Zero;
  for (int i=0; i<100; i++)
    {
    addItUp += adder;
    }
  
  // Test some pixel traits
  itk::ScalarTraits<float>::ValueType foo = min; //should be float
  itk::VectorTraits<long>::ValueType fooLong = itk::NumericTraits<long>::max();
  
  itk::Scalar<float> scalar;
  itk::Scalar<float>::ValueType s = scalar.GetScalar();
  itk::Vector<unsigned short> vector;
  const itk::Vector<unsigned short>::VectorType v = vector;
  
  itk::Scalar<float>::ValueType scalarMin = min;
  itk::Vector<long>::ValueType vectorMax = fooLong;
  
  itk::Image<unsigned char,2>::Pointer i2 = itk::Image<unsigned char,2>::New();
  

}

