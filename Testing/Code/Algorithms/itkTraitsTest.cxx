#include "itkImage.h"
#include "itkPixelTraits.h"
#include "itkScalar.h"
#include "itkVector.h"

/**
 * Helper function to prevent compiler's unused variable warning.
 */
template <typename T>
void IgnoreUnusedVariable(const T&)
{
}

// Important note: many compilers define macros like min() and max(). This
// program will fail if that's the case. (For example, the 
// std::numeric_limits<>::min() collides with the min() macro.)

int main ()
{
  // Test some numeric traits
  float floatMin = itk::NumericTraits<float>::min();
  float floatMax = itk::NumericTraits<float>::max();
  unsigned short unsignedShortMax = itk::NumericTraits<unsigned short>::max();
  unsigned short unsignedShortMin;

  char adder=10;
  itk::NumericTraits<char>::AccumulateType addItUp = itk::NumericTraits<char>::Zero;
  for (int i=0; i<100; i++)
    {
    addItUp += adder;
    }
  
  std::cout << "for (int = 0; i < 100; i++) { addItUp += adder;} = " << addItUp << std::endl;
  
  unsignedShortMin = unsignedShortMax + 1;
  std::cout << "unsigned short min is " << unsignedShortMin << std::endl;
  // Test some pixel traits
  itk::ScalarTraits<float>::ValueType foo = floatMax; //should be float
  std::cout << "itk::ScalarTraits<float>::ValueType foo = floatMax;" << std::endl;

  itk::VectorTraits<long>::ValueType fooLong = itk::NumericTraits<long>::max();
  

  itk::Scalar<float> scalar;
  scalar.SetScalar(23.0);
  itk::Scalar<float>::ValueType s = scalar.GetScalar();
  std::cout << "itk::Scalar<float>::ValueType s = scalar.GetScalar();" << std::endl;
  std::cout << "s is " << s << std::endl;
  itk::Vector<unsigned short,256> vector;
  const itk::Vector<unsigned short,256>::VectorType v = vector;
  std::cout << "const itk::Vector<unsigned short,256>::VectorType v = vector;" << std::endl;
  vector.Fill(unsignedShortMax);
  std::cout << "vector[255] is " << vector[255] << std::endl;
  itk::Scalar<float>::ValueType scalarMin = floatMin;
  itk::Vector<long>::ValueType vectorMax = fooLong;
  
  itk::Image<unsigned char,2>::Pointer i2 = itk::Image<unsigned char,2>::New();
  
  IgnoreUnusedVariable(vectorMax);
  IgnoreUnusedVariable(scalarMin);
  IgnoreUnusedVariable(foo);
  
  return EXIT_SUCCESS;
}

