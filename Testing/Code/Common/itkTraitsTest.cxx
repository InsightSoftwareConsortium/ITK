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
  float min = itkNumericTraits<float>::min();
  unsigned short max = itkNumericTraits<unsigned short>::max();

  char adder=10;
  itkNumericTraits<char>::AccumulateType addItUp = itkNumericTraits<char>::Zero;
  for (int i=0; i<100; i++)
    {
    addItUp += adder;
    }
  
  // Test some pixel traits
  itkScalarTraits<float>::ValueType foo = min; //should be float
  itkVectorTraits<long>::ValueType fooLong = itkNumericTraits<long>::max();
  
  itkScalar<float> scalar;
  itkScalar<float>::ValueType s = scalar.GetScalar();
  itkVector<unsigned short> vector;
  const itkVector<unsigned short>::ValueType *v = vector.GetVector();
  
  itkScalar<float>::ValueType scalarMin = min;
  itkVector<long>::ValueType vectorMax = fooLong;
  
  itkImage<unsigned char,2>::Pointer i2 = itkImage<unsigned char,2>::New();
  

}

