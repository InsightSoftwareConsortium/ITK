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
#include "itkSigmoidTransferFunction.h"
#include "itkTestingMacros.h"


/**
 *  This test exercises the functionality of the itk::SigmoidTransferFunction
 *  class
 *
 */

namespace itk {
namespace Statistics {

template<class ScalarType>
class SigmoidTransferFunctionTestHelper : public SigmoidTransferFunction<ScalarType>
{
public:
  typedef SigmoidTransferFunctionTestHelper   Self;
  typedef SigmoidTransferFunction<ScalarType> Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>            ConstPointer;

  itkTypeMacro( SigmoidTransferFunctionTestHelper, SigmoidTransferFunction );

  itkNewMacro(Self);

  static int Exercise( const ScalarType& input, const ScalarType& alpha,
    const ScalarType& beta, const ScalarType& min, const ScalarType& max,
    const double& expectedVal, const double& expectedDerivVal )
  {

    double val;
    double deriv;

    typedef SigmoidTransferFunction<ScalarType> SigmoidTFType;

    typename SigmoidTFType::Pointer sigmoidTF = SigmoidTFType::New();

    sigmoidTF->SetAlpha(alpha);
    sigmoidTF->SetBeta(beta);

    sigmoidTF->SetOutputMinimum(min);
    sigmoidTF->SetOutputMaximum(max);

    TEST_SET_GET_VALUE( alpha, sigmoidTF->GetAlpha() );
    TEST_SET_GET_VALUE( beta, sigmoidTF->GetBeta() );
    TEST_SET_GET_VALUE( min, sigmoidTF->GetOutputMinimum() );
    TEST_SET_GET_VALUE( max, sigmoidTF->GetOutputMaximum() );

    val = static_cast<double>(sigmoidTF->Evaluate(input));
    deriv = static_cast<double>(sigmoidTF->EvaluateDerivative(input));

    TEST_EXPECT_EQUAL( expectedVal, val );
    TEST_EXPECT_EQUAL( expectedDerivVal, deriv );

    sigmoidTF->Print(std::cout);

    std::cout << "Test succeeded." << std::endl;
    return EXIT_SUCCESS;

   }

protected:

};

}
}

int itkSigmoidTransferFunctionTest( int itkNotUsed(argc), char* itkNotUsed(argv)[] )
{

  typedef int ScalarType;

  // Set sigmoid transfer function's parameters
  ScalarType input = 2;
  ScalarType alpha = 1;
  ScalarType beta = 2;
  ScalarType min = 0;
  ScalarType max = 1;
  double expectedVal = 0.5;
  double expectedDerivVal = 0.25;

  // Test for all possible ScalarTypes
  itk::Statistics::SigmoidTransferFunctionTestHelper<unsigned char>::Exercise(
    input,
    alpha,
    beta,
    min,
    max,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::SigmoidTransferFunctionTestHelper<char>::Exercise(
    input,
    alpha,
    beta,
    min,
    max,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::SigmoidTransferFunctionTestHelper<unsigned short>::Exercise(
    input,
    alpha,
    beta,
    min,
    max,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::SigmoidTransferFunctionTestHelper<short>::Exercise(
    input,
    alpha,
    beta,
    min,
    max,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::SigmoidTransferFunctionTestHelper<unsigned int>::Exercise(
    input,
    alpha,
    beta,
    min,
    max,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::SigmoidTransferFunctionTestHelper<int>::Exercise(
    input,
    alpha,
    beta,
    min,
    max,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::SigmoidTransferFunctionTestHelper<unsigned long>::Exercise(
    input,
    alpha,
    beta,
    min,
    max,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::SigmoidTransferFunctionTestHelper<long>::Exercise(
    input,
    alpha,
    beta,
    min,
    max,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::SigmoidTransferFunctionTestHelper<float>::Exercise(
    input,
    alpha,
    beta,
    min,
    max,
    expectedVal,
    expectedDerivVal );

  itk::Statistics::SigmoidTransferFunctionTestHelper<double>::Exercise(
    input,
    alpha,
    beta,
    min,
    max,
    expectedVal,
    expectedDerivVal );

  return EXIT_SUCCESS;

}
