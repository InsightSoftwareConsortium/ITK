/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include <set>

#include "itkTransform.h"

namespace itk
{
namespace itkTransformTestHelpers
{

template <typename TScalar, unsigned int NInputDimensions, unsigned int NOutputDimensions>
class TransformTestHelper : public Transform<TScalar, NInputDimensions, NOutputDimensions>
{
public:
  using Self = TransformTestHelper;
  using Superclass = Transform<TScalar, NInputDimensions, NOutputDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);
  itkTypeMacro(TransformTestHelper, Transform);

  using JacobianType = typename Superclass::JacobianType;
  using JacobianPositionType = typename Superclass::JacobianPositionType;

  using ParametersType = typename Superclass::ParametersType;
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;
  using InputVectorType = typename Superclass::InputVectorType;
  using OutputVectorType = typename Superclass::OutputVectorType;
  using InputVectorPixelType = typename Superclass::InputVectorPixelType;
  using OutputVectorPixelType = typename Superclass::OutputVectorPixelType;
  using InputVnlVectorType = typename Superclass::InputVnlVectorType;
  using OutputVnlVectorType = typename Superclass::OutputVnlVectorType;
  using InputCovariantVectorType = typename Superclass::InputCovariantVectorType;
  using OutputCovariantVectorType = typename Superclass::OutputCovariantVectorType;

  using InputDiffusionTensor3DType = typename Superclass::InputDiffusionTensor3DType;
  using OutputDiffusionTensor3DType = typename Superclass::OutputDiffusionTensor3DType;

  using InputSymmetricSecondRankTensorType = typename Superclass::InputSymmetricSecondRankTensorType;
  using OutputSymmetricSecondRankTensorType = typename Superclass::OutputSymmetricSecondRankTensorType;

  OutputPointType
  TransformPoint(const InputPointType & itkNotUsed(inputPoint)) const override
  {
    OutputPointType outPoint;
    outPoint.Fill(22.0);
    return outPoint;
  }

  using Superclass::TransformVector;
  OutputVectorType
  TransformVector(const InputVectorType & itkNotUsed(inputVector)) const override
  {
    OutputVectorType outVector;
    outVector.Fill(12.2);
    return outVector;
  }

  OutputVnlVectorType
  TransformVector(const InputVnlVectorType & itkNotUsed(inputVector)) const override
  {
    OutputVnlVectorType outVector(15.0);
    return outVector;
  }

  OutputVectorPixelType
  TransformVector(const InputVectorPixelType & itkNotUsed(inputVector)) const override
  {
    OutputVectorPixelType outVector;
    outVector.Fill(88.8);
    return outVector;
  }

  using Superclass::TransformCovariantVector;
  OutputCovariantVectorType
  TransformCovariantVector(const InputCovariantVectorType & itkNotUsed(inputVector)) const override
  {
    OutputCovariantVectorType outVector;
    outVector.Fill(8.9);
    return outVector;
  }

  OutputVectorPixelType
  TransformCovariantVector(const InputVectorPixelType & itkNotUsed(inputVector)) const override
  {
    OutputVectorPixelType outVector;
    outVector.Fill(6.9);
    return outVector;
  }

  using Superclass::TransformDiffusionTensor3D;
  OutputDiffusionTensor3DType
  TransformDiffusionTensor3D(const InputDiffusionTensor3DType & itkNotUsed(tensor)) const override
  {
    OutputDiffusionTensor3DType outTensor;
    outTensor.Fill(2.1);
    return outTensor;
  }

  OutputVectorPixelType
  TransformDiffusionTensor3D(const InputVectorPixelType & itkNotUsed(tensor)) const override
  {
    OutputVectorPixelType outTensor;
    outTensor.Fill(29.1);
    return outTensor;
  }

  using Superclass::TransformSymmetricSecondRankTensor;
  OutputSymmetricSecondRankTensorType
  TransformSymmetricSecondRankTensor(const InputSymmetricSecondRankTensorType & itkNotUsed(tensor)) const override
  {
    OutputSymmetricSecondRankTensorType outTensor;
    outTensor.Fill(10.0);
    return outTensor;
  }

  OutputVectorPixelType
  TransformSymmetricSecondRankTensor(const InputVectorPixelType & itkNotUsed(tensor)) const override
  {
    OutputVectorPixelType outTensor;
    outTensor.Fill(55.9);
    return outTensor;
  }

  void
  SetParameters(const ParametersType &) override
  {}

  void
  SetFixedParameters(const ParametersType &) override
  {}

  void
  ComputeJacobianWithRespectToParameters(const InputPointType &, JacobianType & jacobian) const override
  {
    jacobian.SetSize(3, 6);
    jacobian.Fill(1);
  }

  using Superclass::ComputeJacobianWithRespectToPosition;
  void
  ComputeJacobianWithRespectToPosition(const InputPointType &, JacobianPositionType & jacobian) const override
  {
    jacobian.fill(1.0);
  }
};

template <typename TScalar, unsigned int NInputDimensions, unsigned int NOutputDimensions>
class TransformTester
{
public:
  using Self = TransformTester;

  using TransformType = TransformTestHelper<double, NInputDimensions, NOutputDimensions>;

  using JacobianType = typename TransformType::JacobianType;
  using ParametersType = typename TransformType::ParametersType;
  using InputPointType = typename TransformType::InputPointType;
  using OutputPointType = typename TransformType::OutputPointType;
  using InputVectorType = typename TransformType::InputVectorType;
  using OutputVectorType = typename TransformType::OutputVectorType;
  using InputVectorPixelType = typename TransformType::InputVectorPixelType;
  using OutputVectorPixelType = typename TransformType::OutputVectorPixelType;
  using InputVnlVectorType = typename TransformType::InputVnlVectorType;
  using OutputVnlVectorType = typename TransformType::OutputVnlVectorType;
  using InputCovariantVectorType = typename TransformType::InputCovariantVectorType;
  using OutputCovariantVectorType = typename TransformType::OutputCovariantVectorType;

  using InputDiffusionTensor3DType = typename TransformType::InputDiffusionTensor3DType;
  using OutputDiffusionTensor3DType = typename TransformType::OutputDiffusionTensor3DType;

  using InputSymmetricSecondRankTensorType = typename TransformType::InputSymmetricSecondRankTensorType;
  using OutputSymmetricSecondRankTensorType = typename TransformType::OutputSymmetricSecondRankTensorType;

  bool
  RunTests()
  {
    std::cout << "Testing itkTransform<" << NInputDimensions << "," << NOutputDimensions << ">" << std::endl;
    typename TransformType::Pointer transform = TransformType::New();

    InputPointType pnt;
    pnt.Fill(2.9);

    transform->TransformPoint(pnt);
    std::cout << "TransformPoint()                              OK" << std::endl;

    InputVectorType vec;
    transform->TransformVector(vec);
    transform->TransformVector(vec, pnt);

    InputVectorPixelType vecpix;
    vecpix.SetSize(NInputDimensions);
    vecpix.Fill(1.7);
    transform->TransformVector(vecpix);
    transform->TransformVector(vecpix, pnt);

    InputVnlVectorType vec_vnl;
    transform->TransformVector(vec_vnl);
    transform->TransformVector(vec_vnl, pnt);
    std::cout << "TransformVector()                             OK" << std::endl;

    InputCovariantVectorType covec;
    transform->TransformCovariantVector(covec);
    transform->TransformCovariantVector(vecpix);
    transform->TransformCovariantVector(covec, pnt);
    transform->TransformCovariantVector(vecpix, pnt);
    std::cout << "TransformCovariantVector()                    OK" << std::endl;

    InputDiffusionTensor3DType difften;
    vecpix.SetSize(6);
    vecpix.Fill(1.7);
    transform->TransformDiffusionTensor3D(difften);
    transform->TransformDiffusionTensor3D(difften, pnt);
    transform->TransformDiffusionTensor3D(vecpix);
    transform->TransformDiffusionTensor3D(vecpix, pnt);
    std::cout << "TransformDiffusionTensor3D()                  OK" << std::endl;

    InputSymmetricSecondRankTensorType ssrten;
    vecpix.SetSize(NInputDimensions * NInputDimensions);
    vecpix.Fill(0);
    transform->TransformSymmetricSecondRankTensor(ssrten);
    transform->TransformSymmetricSecondRankTensor(ssrten, pnt);
    transform->TransformSymmetricSecondRankTensor(vecpix);
    transform->TransformSymmetricSecondRankTensor(vecpix, pnt);
    std::cout << "TransformSymmetricSecondRankTensor()          OK" << std::endl;

    typename TransformType::ParametersType parameters(6);
    try
    {
      transform->SetParameters(parameters);
    }
    catch (const itk::ExceptionObject & e)
    {
      std::cerr << e << std::endl;
    }

    try
    {
      transform->GetParameters();
    }
    catch (const itk::ExceptionObject & e)
    {
      std::cerr << e << std::endl;
    }

    typename TransformType::JacobianType jacobian;
    try
    {
      transform->ComputeJacobianWithRespectToParameters(pnt, jacobian);
    }
    catch (const itk::ExceptionObject & e)
    {
      std::cerr << e << std::endl;
    }

    typename TransformType::JacobianPositionType jacobian_position;
    try
    {
      transform->ComputeJacobianWithRespectToPosition(pnt, jacobian_position);
    }
    catch (const itk::ExceptionObject & e)
    {
      std::cerr << e << std::endl;
    }

    typename TransformType::InverseJacobianPositionType inv_jacobian_position;
    try
    {
      transform->ComputeInverseJacobianWithRespectToPosition(pnt, inv_jacobian_position);
    }
    catch (const itk::ExceptionObject & e)
    {
      std::cerr << e << std::endl;
    }

    typename TransformType::DerivativeType update(transform->GetNumberOfParameters());
    update.Fill(1);
    try
    {
      transform->UpdateTransformParameters(update);
    }
    catch (const itk::ExceptionObject & e)
    {
      std::cerr << e << std::endl;
    }

    // Exercise some methods
    transform->Print(std::cout);
    std::cout << transform->GetNameOfClass() << std::endl;

    // Test streaming enumeration for TransformBaseTemplateEnums::TransformCategory elements
    const std::set<itk::TransformBaseTemplateEnums::TransformCategory> allTransformCategory{
      itk::TransformBaseTemplateEnums::TransformCategory::UnknownTransformCategory,
      itk::TransformBaseTemplateEnums::TransformCategory::Linear,
      itk::TransformBaseTemplateEnums::TransformCategory::BSpline,
      itk::TransformBaseTemplateEnums::TransformCategory::Spline,
      itk::TransformBaseTemplateEnums::TransformCategory::DisplacementField,
      itk::TransformBaseTemplateEnums::TransformCategory::VelocityField
    };
    for (const auto & ee : allTransformCategory)
    {
      std::cout << "STREAMED ENUM VALUE TransformBaseTemplateEnums::TransformCategory: " << ee << std::endl;
    }

    return true;
  }
};


} // namespace itkTransformTestHelpers
} // namespace itk

int
itkTransformTest(int, char *[])
{
  itk::itkTransformTestHelpers::TransformTester<double, 3, 3> tester33;
  tester33.RunTests();
  std::cout << "passed 3 3" << std::endl;

  itk::itkTransformTestHelpers::TransformTester<double, 3, 2> tester32;
  tester32.RunTests();
  std::cout << "passed 3 2" << std::endl;

  itk::itkTransformTestHelpers::TransformTester<double, 2, 3> tester23;
  tester23.RunTests();
  std::cout << "passed 2 3" << std::endl;


  std::cout << "[ PASSED ]" << std::endl;
  return EXIT_SUCCESS;
}
