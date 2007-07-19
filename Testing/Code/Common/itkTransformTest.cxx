/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

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
  typedef TransformTestHelper Self;
  typedef Transform< TScalarType, NInputDimensions, NOutputDimensions > Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
      
  itkNewMacro( Self );
  itkTypeMacro( TransformTestHelper, Transform );

  typedef typename Superclass::JacobianType       JacobianType;
  typedef typename Superclass::ParametersType     ParametersType;
  typedef typename Superclass::InputPointType     InputPointType;
  typedef typename Superclass::OutputPointType    OutputPointType;
  typedef typename Superclass::InputVectorType    InputVectorType;
  typedef typename Superclass::OutputVectorType   OutputVectorType;
  typedef typename Superclass::InputVnlVectorType   InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType   OutputVnlVectorType;
  typedef typename Superclass::InputCovariantVectorType   InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType   OutputCovariantVectorType;

  virtual OutputPointType TransformPoint(const InputPointType  & inputPoint ) const
    { return inputPoint; }

  virtual OutputVectorType TransformVector(const InputVectorType  & inputVector ) const
    { return inputVector; }

  virtual OutputVnlVectorType TransformVector(const InputVnlVectorType  & inputVector ) const
    { return inputVector; }

  virtual OutputCovariantVectorType TransformCovariantVector(const InputCovariantVectorType  & inputVector ) const
    { return inputVector; }

  virtual const JacobianType & GetJacobian(const InputPointType  & ) const
    { return this->m_Jacobian; }
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

  // Exercise some methods
  transform->Print( std::cout );
  std::cout <<  transform->GetNameOfClass() << std::endl;

  std::cout << "[ PASSED ]" << std::endl;
  return EXIT_SUCCESS;

}
