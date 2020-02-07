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

#include "itkSpatialObjectReader.h"
#include "itkTestingMacros.h"

int
itkReadVesselTubeSpatialObjectTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " <inputVessel.tre>" << std::endl;
    return EXIT_FAILURE;
  }
  const char * inputVessel = argv[1];

  constexpr unsigned int Dimension = 3;

  using ReaderType = itk::SpatialObjectReader<Dimension>;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inputVessel);

  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & error)
  {
    std::cerr << "Exception caught: " << error << std::endl;
    return EXIT_FAILURE;
  }

  ReaderType::SpatialObjectPointer soScene = reader->GetOutput();
  const unsigned int               numberOfChildren = soScene->GetNumberOfChildren(2);
  std::cout << "Number of children: " << numberOfChildren << std::endl;
  if (numberOfChildren != 2)
  {
    std::cerr << "Unexpected number of children." << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
