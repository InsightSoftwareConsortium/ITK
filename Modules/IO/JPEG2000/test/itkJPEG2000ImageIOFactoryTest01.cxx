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

#include "itkJPEG2000ImageIOFactory.h"


int itkJPEG2000ImageIOFactoryTest01( int /*argc */, char * /*argv*/[] )
{
  //  Register the factory
  itk::JPEG2000ImageIOFactory::RegisterOneFactory();

  itk::JPEG2000ImageIOFactory::Pointer factory =
    itk::JPEG2000ImageIOFactory::New();

  std::cout << "ITK Version = " << factory->GetITKSourceVersion() << std::endl;
  std::cout << "Description = " << factory->GetDescription() << std::endl;

  std::cout << "ClassName = " << factory->GetNameOfClass() << std::endl;

  itk::JPEG2000ImageIOFactory::Pointer factory2 =
    itk::JPEG2000ImageIOFactory::FactoryNew();

  return EXIT_SUCCESS;
}
