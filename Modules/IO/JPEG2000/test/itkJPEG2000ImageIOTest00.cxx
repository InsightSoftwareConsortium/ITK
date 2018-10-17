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

#include "itkJPEG2000ImageIO.h"


int itkJPEG2000ImageIOTest00( int /*argc */, char * /*argv*/[] )
{
  itk::JPEG2000ImageIO::Pointer imageIO = itk::JPEG2000ImageIO::New();

  std::cout << "ClassName = " << imageIO->GetNameOfClass() << std::endl;

  imageIO->Print( std::cout );

  return EXIT_SUCCESS;
}
