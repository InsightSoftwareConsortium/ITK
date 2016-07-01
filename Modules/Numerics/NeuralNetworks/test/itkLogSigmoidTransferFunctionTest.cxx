/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>
#include "itkLogSigmoidTransferFunction.h"
#include "itkTestingMacros.h"


/**
 *  This test exercises the functionality of the itk::LogSigmoidTransferFunction
 *  class
 *
 */

namespace itk {
namespace Statistics {

template<class ScalarType>
class LogSigmoidTransferFunctionTestHelper : public LogSigmoidTransferFunction<ScalarType>
{
public:
  typedef LogSigmoidTransferFunctionTestHelper    Self;
  typedef LogSigmoidTransferFunction<ScalarType>  Superclass;
  typedef SmartPointer<Self>                      Pointer;
  typedef SmartPointer<const Self>                ConstPointer;

  itkTypeMacro( LogSigmoidTransferFunctionTestHelper, LogSigmoidTransferFunction );

  itkNewMacro(Self);

  static int Exercise( const ScalarType& input,
    const double& expectedVal, const double& expectedDerivVal )
  {

    double val;
    double deriv;

    typedef LogSigmoidTransferFunction<ScalarType> LogSigmoidTFType;

    typename LogSigmoidTFType::Pointer logSigmoidTF = LogSigmoidTFType::New();

    val = static_cast<double>(logSigmoidTF->Evaluate(input));
    deriv = static_cast<double>(logSigmoidTF->EvaluateDerivative(input));

    TEST_EXPECT_EQUAL( expectedVal, val );
    TEST_EXPECT_EQUAL( expectedDerivVal, deriv );

    std::cout << "Test succeeded." << std::endl;
    return EXIT_SUCCESS;

   }

protected:

};

} // end namespace Statistics
} // end namespace itk

int itkLogSigmoidTransferFunctionTest( int itkNotUsed(argc), char* itkNotUsed(argv)[] )
{

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  typedef itk::Statistics::LogSigmoidTransferFunction<unsigned char> LogSigmoidTFType;
  LogSigmoidTFType::Pointer logSigmoidTF = LogSigmoidTFType::New();

  EXERCISE_BASIC_OBJECT_METHODS( logSigmoidTF, LogSigmoidTransferFunction,
    TransferFunctionBase);


  typedef int ScalarType;

  // Set log sigmoid transfer function's parameters and expected values
  ScalarType input = 2;
  double expectedVal = 0.8807970779778824440597291413024;
  double expectedDerivVal = 0.10499358540350651734862418476042;

  // Test for all possible ScalarTypes
  itk::Statistics::LogSigmoidTransferFunctionTestHelper<unsigned char>::Exercise(
    input,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::LogSigmoidTransferFunctionTestHelper<char>::Exercise(
    input,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::LogSigmoidTransferFunctionTestHelper<unsigned short>::Exercise(
    input,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::LogSigmoidTransferFunctionTestHelper<short>::Exercise(
    input,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::LogSigmoidTransferFunctionTestHelper<unsigned int>::Exercise(
    input,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::LogSigmoidTransferFunctionTestHelper<int>::Exercise(
    input,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::LogSigmoidTransferFunctionTestHelper<unsigned long>::Exercise(
    input,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::LogSigmoidTransferFunctionTestHelper<long>::Exercise(
    input,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::LogSigmoidTransferFunctionTestHelper<float>::Exercise(
    input,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::LogSigmoidTransferFunctionTestHelper<double>::Exercise(
    input,
    expectedVal,
    expectedDerivVal );

  return EXIT_SUCCESS;

}
