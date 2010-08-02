/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMembershipFunctionBaseTest.cxx
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
#include "itkMembershipFunctionBase.h"
#include "itkObjectFactory.h"

namespace itk {
namespace Statistics {
namespace MembershipFunctionBaseTest {

template <class TMeasurementVector>
class MyMembershipFunctionBase : public MembershipFunctionBase< TMeasurementVector >
{
public:
  /** Standard class typedef. */
  typedef MyMembershipFunctionBase  Self;

  typedef MembershipFunctionBase< TMeasurementVector > Superclass;

  typedef SmartPointer< Self > Pointer;

  typedef SmartPointer<const Self> ConstPointer;

  /** Standard macros */
  itkTypeMacro(MyMembershipFunctionBase, MembershipFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Evaluate membership score */
  double Evaluate(const TMeasurementVector & ) const
    {
    double score;
    score = 1;
    return score;
    }

  typename MembershipFunctionBase< TMeasurementVector >::Pointer Clone()
    {
    Pointer memberFunction = MyMembershipFunctionBase< TMeasurementVector >::New();
    return memberFunction.GetPointer();
    }
};

}
}
}
int itkMembershipFunctionBaseTest(int, char* [] )
{

  const unsigned int MeasurementVectorSize = 17;

  typedef itk::FixedArray<
    float, MeasurementVectorSize >  MeasurementVectorType;

  typedef itk::Statistics::MembershipFunctionBaseTest::MyMembershipFunctionBase<
    MeasurementVectorType >   MembershipFunctionBaseType;

  MembershipFunctionBaseType::Pointer function = MembershipFunctionBaseType::New();

  std::cout << function->GetNameOfClass() << std::endl;
  std::cout << function->MembershipFunctionBaseType::Superclass::GetNameOfClass() << std::endl;

  function->Print(std::cout);

  function->SetMeasurementVectorSize( MeasurementVectorSize ); // for code coverage

  if( function->GetMeasurementVectorSize() != MeasurementVectorSize )
    {
    std::cerr << "GetMeasurementVectorSize() Failed !" << std::endl;
    return EXIT_FAILURE;
    }

  //Test if an exception will be thrown if we try to resize the measurement vector
  //size
  try
    {
    function->SetMeasurementVectorSize( MeasurementVectorSize + 1 );
    std::cerr << "Exception should have been thrown since we are trying to resize\
                  non-resizeable measurement vector type " << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Caughted expected exception: " << excp << std::endl;
    }

  return EXIT_SUCCESS;
}
