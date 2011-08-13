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
#include "itkMatrix.h"

namespace itk
{
namespace itkTransformTestHelpers
{

template <
    class TScalarType,
    unsigned int NInputDimensions,
    unsigned int NOutputDimensions>
class TransformTestHelper :
          public Transform< TScalarType, NInputDimensions, NOutputDimensions >
{
public:
  typedef TransformTestHelper                                           Self;
  typedef Transform< TScalarType, NInputDimensions, NOutputDimensions > Superclass;
  typedef SmartPointer<Self>                                            Pointer;
  typedef SmartPointer<const Self>                                      ConstPointer;

  itkNewMacro( Self );
  itkTypeMacro( TransformTestHelper, Transform );

  typedef typename Superclass::JacobianType                JacobianType;
  typedef typename Superclass::ParametersType              ParametersType;
  typedef typename Superclass::InputPointType              InputPointType;
  typedef typename Superclass::OutputPointType             OutputPointType;
  typedef typename Superclass::InputVectorType             InputVectorType;
  typedef typename Superclass::OutputVectorType            OutputVectorType;
  typedef typename Superclass::InputVnlVectorType          InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType         OutputVnlVectorType;
  typedef typename Superclass::InputCovariantVectorType    InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType   OutputCovariantVectorType;

  virtual OutputPointType TransformPoint(const InputPointType  & inputPoint ) const
    { return inputPoint; }

  using Superclass::TransformVector;
  virtual OutputVectorType TransformVector(const InputVectorType  & inputVector ) const
    { return inputVector; }

  virtual OutputVnlVectorType TransformVector(const InputVnlVectorType  & inputVector ) const
    { return inputVector; }

  using Superclass::TransformCovariantVector;
  virtual OutputCovariantVectorType TransformCovariantVector(const InputCovariantVectorType  & inputVector ) const
    { return inputVector; }

  virtual const JacobianType & GetJacobian(const InputPointType  & ) const
    { return this->m_Jacobian; }

  virtual void GetJacobianWithRespectToParameters(const InputPointType &,
                                                  JacobianType &j) const
    { j.SetSize(3,6); j.Fill(1); }

  inline virtual void GetJacobianWithRespectToPosition(
                                                  const InputPointType &,
                                                  JacobianType &j ) const
    { j.SetSize(NOutputDimensions, NInputDimensions); j.Fill(1); }

  virtual void SetParameters(const ParametersType &) {}
  virtual void SetFixedParameters(const ParametersType &) {}
};


}
}

int itkTransformTest(int, char* [] )
{

  typedef  itk::itkTransformTestHelpers::TransformTestHelper<double,3,3>      TransformType;
  TransformType::Pointer transform = TransformType::New();

  TransformType::InputPointType pnt;
  transform->TransformPoint(pnt);

  TransformType::InputVectorType  vec;
  transform->TransformVector(vec);

  TransformType::InputVnlVectorType   vec_vnl;
  transform->TransformVector(vec_vnl);

  TransformType::InputCovariantVectorType    covec;
  transform->TransformCovariantVector(covec);

  TransformType::ParametersType parameters(6);
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

  try
    {
    transform->GetJacobian(pnt);
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << e << std::endl;
    }

  TransformType::JacobianType jacobian;
  try
    {
    transform->GetJacobianWithRespectToParameters(pnt, jacobian);
    }
  catch ( itk::ExceptionObject & e )
    {
    std::cerr << e << std::endl;
    }

  TransformType::DerivativeType update( transform->GetNumberOfParameters() );
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

  std::cout << "[ PASSED ]" << std::endl;
  return EXIT_SUCCESS;

}
