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

#include "itkTransform.h"

namespace itk
{
namespace itkTransformTestHelpers
{

template <
  typename TScalar,
  unsigned int NInputDimensions,
  unsigned int NOutputDimensions>
class TransformTestHelper :
  public Transform< TScalar, NInputDimensions, NOutputDimensions >
{
public:
  typedef TransformTestHelper                                       Self;
  typedef Transform< TScalar, NInputDimensions, NOutputDimensions > Superclass;
  typedef SmartPointer< Self >                                      Pointer;
  typedef SmartPointer< const Self >                                ConstPointer;

  itkNewMacro( Self );
  itkTypeMacro( TransformTestHelper, Transform );

  typedef typename Superclass::JacobianType              JacobianType;
  typedef typename Superclass::ParametersType            ParametersType;
  typedef typename Superclass::InputPointType            InputPointType;
  typedef typename Superclass::OutputPointType           OutputPointType;
  typedef typename Superclass::InputVectorType           InputVectorType;
  typedef typename Superclass::OutputVectorType          OutputVectorType;
  typedef typename Superclass::InputVectorPixelType      InputVectorPixelType;
  typedef typename Superclass::OutputVectorPixelType     OutputVectorPixelType;
  typedef typename Superclass::InputVnlVectorType        InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType       OutputVnlVectorType;
  typedef typename Superclass::InputCovariantVectorType  InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType OutputCovariantVectorType;

  typedef typename Superclass::InputDiffusionTensor3DType  InputDiffusionTensor3DType;
  typedef typename Superclass::OutputDiffusionTensor3DType OutputDiffusionTensor3DType;

  typedef typename Superclass::InputSymmetricSecondRankTensorType
  InputSymmetricSecondRankTensorType;
  typedef typename Superclass::OutputSymmetricSecondRankTensorType
  OutputSymmetricSecondRankTensorType;

  virtual OutputPointType TransformPoint(const InputPointType  & itkNotUsed(inputPoint) ) const ITK_OVERRIDE
  {
    OutputPointType outPoint;
    outPoint.Fill( 22.0 );
    return outPoint;
  }

  using Superclass::TransformVector;
  virtual OutputVectorType TransformVector(const InputVectorType  & itkNotUsed(inputVector) ) const ITK_OVERRIDE
  {
    OutputVectorType outVector;
    outVector.Fill( 12.2 );
    return outVector;
  }

  virtual OutputVnlVectorType TransformVector(const InputVnlVectorType  & itkNotUsed(inputVector) ) const ITK_OVERRIDE
  {
    OutputVnlVectorType outVector( 15.0 );
    return outVector;
  }

  virtual OutputVectorPixelType TransformVector(const InputVectorPixelType  & itkNotUsed(inputVector) ) const ITK_OVERRIDE
  {
    OutputVectorPixelType outVector;
    outVector.Fill( 88.8 );
    return outVector;
  }

  using Superclass::TransformCovariantVector;
  virtual OutputCovariantVectorType TransformCovariantVector(const InputCovariantVectorType  & itkNotUsed(inputVector) ) const ITK_OVERRIDE
  {
    OutputCovariantVectorType outVector;
    outVector.Fill( 8.9 );
    return outVector;
  }

  virtual OutputVectorPixelType TransformCovariantVector(const InputVectorPixelType  & itkNotUsed(inputVector) ) const ITK_OVERRIDE
  {
    OutputVectorPixelType outVector;
    outVector.Fill( 6.9 );
    return outVector;
  }

  using Superclass::TransformDiffusionTensor3D;
  virtual OutputDiffusionTensor3DType TransformDiffusionTensor3D( const InputDiffusionTensor3DType & itkNotUsed( tensor ) ) const ITK_OVERRIDE
  {
    OutputDiffusionTensor3DType outTensor;
    outTensor.Fill( 2.1 );
    return outTensor;
  }

  virtual OutputVectorPixelType TransformDiffusionTensor3D( const InputVectorPixelType & itkNotUsed( tensor ) ) const ITK_OVERRIDE
  {
    OutputVectorPixelType outTensor;
    outTensor.Fill( 29.1 );
    return outTensor;
  }

  using Superclass::TransformSymmetricSecondRankTensor;
  virtual OutputSymmetricSecondRankTensorType TransformSymmetricSecondRankTensor(
    const InputSymmetricSecondRankTensorType & itkNotUsed( tensor ) ) const ITK_OVERRIDE
  {
    OutputSymmetricSecondRankTensorType outTensor;
    outTensor.Fill( 10.0 );
    return outTensor;
  }

  virtual OutputVectorPixelType TransformSymmetricSecondRankTensor(
    const InputVectorPixelType & itkNotUsed( tensor ) ) const ITK_OVERRIDE
  {
    OutputVectorPixelType outTensor;
    outTensor.Fill( 55.9 );
    return outTensor;
  }

  virtual void SetParameters(const ParametersType &) ITK_OVERRIDE
  {
  }

  virtual void SetFixedParameters(const ParametersType &) ITK_OVERRIDE
  {
  }

  virtual void ComputeJacobianWithRespectToParameters(const InputPointType &,
                                                      JacobianType & jacobian) const ITK_OVERRIDE
  {
    jacobian.SetSize(3, 6);
    jacobian.Fill(1);
  }

  virtual void ComputeJacobianWithRespectToPosition(
    const InputPointType &,
    JacobianType & jacobian ) const ITK_OVERRIDE
  {
    jacobian.SetSize(NOutputDimensions, NInputDimensions);
    jacobian.Fill(1);
  }

};

template <
  typename TScalar,
  unsigned int NInputDimensions,
  unsigned int NOutputDimensions>
class TransformTester
{
public:
  typedef TransformTester                                             Self;

  typedef TransformTestHelper<double, NInputDimensions, NOutputDimensions> TransformType;

  typedef typename TransformType::JacobianType              JacobianType;
  typedef typename TransformType::ParametersType            ParametersType;
  typedef typename TransformType::InputPointType            InputPointType;
  typedef typename TransformType::OutputPointType           OutputPointType;
  typedef typename TransformType::InputVectorType           InputVectorType;
  typedef typename TransformType::OutputVectorType          OutputVectorType;
  typedef typename TransformType::InputVectorPixelType      InputVectorPixelType;
  typedef typename TransformType::OutputVectorPixelType     OutputVectorPixelType;
  typedef typename TransformType::InputVnlVectorType        InputVnlVectorType;
  typedef typename TransformType::OutputVnlVectorType       OutputVnlVectorType;
  typedef typename TransformType::InputCovariantVectorType  InputCovariantVectorType;
  typedef typename TransformType::OutputCovariantVectorType OutputCovariantVectorType;

  typedef typename TransformType::InputDiffusionTensor3DType  InputDiffusionTensor3DType;
  typedef typename TransformType::OutputDiffusionTensor3DType OutputDiffusionTensor3DType;

  typedef typename TransformType::InputSymmetricSecondRankTensorType
  InputSymmetricSecondRankTensorType;
  typedef typename TransformType::OutputSymmetricSecondRankTensorType
  OutputSymmetricSecondRankTensorType;

  bool RunTests( void )
  {
    std::cout << "Testing itkTransform<" << NInputDimensions << "," << NOutputDimensions << ">" << std::endl;
    typename TransformType::Pointer transform = TransformType::New();

    InputPointType pnt;
    pnt.Fill(2.9);

    transform->TransformPoint(pnt);
    std::cout << "TransformPoint()                              OK" << std::endl;

    InputVectorType vec;
    transform->TransformVector(vec);
    transform->TransformVector(vec,pnt);

    InputVectorPixelType vecpix;
    vecpix.SetSize( NInputDimensions );
    vecpix.Fill(1.7);
    transform->TransformVector(vecpix);
    transform->TransformVector(vecpix,pnt);

    InputVnlVectorType vec_vnl;
    transform->TransformVector(vec_vnl);
    transform->TransformVector(vec_vnl,pnt);
    std::cout << "TransformVector()                             OK" << std::endl;

    InputCovariantVectorType covec;
    transform->TransformCovariantVector(covec);
    transform->TransformCovariantVector(vecpix);
    transform->TransformCovariantVector(covec,pnt);
    transform->TransformCovariantVector(vecpix,pnt);
    std::cout << "TransformCovariantVector()                    OK" << std::endl;

    InputDiffusionTensor3DType difften;
    vecpix.SetSize( 6 );
    vecpix.Fill(1.7);
    transform->TransformDiffusionTensor3D(difften);
    transform->TransformDiffusionTensor3D(difften,pnt);
    transform->TransformDiffusionTensor3D(vecpix);
    transform->TransformDiffusionTensor3D(vecpix,pnt);
    std::cout << "TransformDiffusionTensor3D()                  OK" << std::endl;

    InputSymmetricSecondRankTensorType ssrten;
    vecpix.SetSize(NInputDimensions*NInputDimensions);
    vecpix.Fill(0);
    transform->TransformSymmetricSecondRankTensor(ssrten);
    transform->TransformSymmetricSecondRankTensor(ssrten,pnt);
    transform->TransformSymmetricSecondRankTensor(vecpix);
    transform->TransformSymmetricSecondRankTensor(vecpix,pnt);
    std::cout << "TransformSymmetricSecondRankTensor()          OK" << std::endl;

    typename TransformType::ParametersType parameters(6);
    try
      {
      transform->SetParameters(parameters);
      }
    catch( itk::ExceptionObject & e )
      {
      std::cerr << e << std::endl;
      }

    try
      {
      transform->GetParameters();
      }
    catch( itk::ExceptionObject & e )
      {
      std::cerr << e << std::endl;
      }

    typename TransformType::JacobianType jacobian;
    try
      {
      transform->ComputeJacobianWithRespectToParameters(pnt, jacobian);
      }
    catch( itk::ExceptionObject & e )
      {
      std::cerr << e << std::endl;
      }

    try
      {
      transform->ComputeJacobianWithRespectToPosition(pnt, jacobian);
      }
    catch( itk::ExceptionObject & e )
      {
      std::cerr << e << std::endl;
      }

    try
      {
      transform->ComputeInverseJacobianWithRespectToPosition(pnt, jacobian);
      }
    catch( itk::ExceptionObject & e )
      {
      std::cerr << e << std::endl;
      }

    typename TransformType::DerivativeType update( transform->GetNumberOfParameters() );
    update.Fill(1);
    try
      {
    transform->UpdateTransformParameters( update );
      }
    catch( itk::ExceptionObject & e )
      {
      std::cerr << e << std::endl;
      }

    // Exercise some methods
    transform->Print( std::cout );
    std::cout <<  transform->GetNameOfClass() << std::endl;

    return true;
  }

};


}
}

int itkTransformTest(int, char * [] )
{
  itk::itkTransformTestHelpers::TransformTester<double,3,3> tester33;
  tester33.RunTests();
  std::cout << "passed 3 3" << std::endl;

  itk::itkTransformTestHelpers::TransformTester<double,3,2> tester32;
  tester32.RunTests();
  std::cout << "passed 3 2" << std::endl;

  itk::itkTransformTestHelpers::TransformTester<double,2,3> tester23;
  tester23.RunTests();
  std::cout << "passed 2 3" << std::endl;


  std::cout << "[ PASSED ]" << std::endl;
  return EXIT_SUCCESS;

}
