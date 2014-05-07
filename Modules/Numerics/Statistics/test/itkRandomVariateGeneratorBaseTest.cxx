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

  virtual double GetVariate() ITK_OVERRIDE
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
