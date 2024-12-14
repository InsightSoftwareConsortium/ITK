/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkFEMFactoryBase.h"
#include "itkTestingMacros.h"

int
main(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << '\n';
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFileName" << '\n';
    return EXIT_FAILURE;
  }

  // Need to register default FEM object types,
  // and setup spatialReader to recognize FEM types
  // which is all currently done as a HACK in
  // the initialization of the itk::FEMFactoryBase::GetFactory()
  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();


  itk::FEMFactoryBase::RegisterDefaultTypes();
  itk::FEMFactoryBase::Pointer factory = itk::FEMFactoryBase::New();
  itk::ObjectFactoryBase::RegisterFactory(factory);

  itk::LightObject::Pointer newborn = factory->CreateInstance(argv[1]);
  if (newborn)
  {
    newborn->Print(std::cout);
  }
  else
  {
    std::cout << "Do not know how to create object : " << argv[1] << '\n';
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
