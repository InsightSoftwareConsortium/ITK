/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomVariateGeneratorBaseTest.cxx
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

#include "itkRandomVariateGeneratorBase.h"
#include "itkObjectFactory.h"

namespace itk {
namespace Statistics {

class VariateGeneratorTestHelper : public RandomVariateGeneratorBase
{
public:
  typedef VariateGeneratorTestHelper Self;
  typedef RandomVariateGeneratorBase Superclass;
  typedef SmartPointer<Self>         Pointer;
  typedef SmartPointer<const Self>   ConstPointer;

  itkTypeMacro(VariateGeneratorTestHelper, RandomVariateGeneratorBase );

  itkNewMacro(Self);

  virtual double GetVariate()
    {
    double theAnswerToTheQuestionOfLifeTheUniverseAndEverything = 42.0;
    return theAnswerToTheQuestionOfLifeTheUniverseAndEverything;
    }

  void RunTests()
    {
    std::cout << "Superclass Name " << this->Superclass::GetNameOfClass() << std::endl;
    std::cout << "This class Name " << this->GetNameOfClass() << std::endl;
    std::cout << "GetVariate() = "  << this->GetVariate() << std::endl;
    }

};

}
}

int itkRandomVariateGeneratorBaseTest( int , char* [] )
{
  typedef itk::Statistics::VariateGeneratorTestHelper GeneratorType;

  GeneratorType::Pointer generator = GeneratorType::New();

  std::cout << generator->GetNameOfClass() << std::endl;

  generator->RunTests();

  generator->Print( std::cout );

  std::cerr << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
