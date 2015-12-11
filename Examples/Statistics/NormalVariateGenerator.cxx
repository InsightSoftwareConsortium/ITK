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

// Software Guide : BeginLatex
//
// \index{itk::Statistics::NormalVariateGenerator}
// \index{Statistics!Random number generation!Normal (Gaussian) distribution}
//
// The \subdoxygen{Statistics}{NormalVariateGenerator} generates random
// variables according to the standard normal distribution (mean = 0,
// standard deviation = 1).
//
// To use the class in a project, we must link the \code{itkStatistics}
// library to the project.
//
// To begin the example we include the header file for the class.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkNormalVariateGenerator.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  //
  // The NormalVariateGenerator is a non-templated class. We simply call
  // the \code{New()} method to create an instance. Then, we provide the seed
  // value using the \code{Initialize(seed value)}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::NormalVariateGenerator GeneratorType;
  GeneratorType::Pointer generator = GeneratorType::New();
  generator->Initialize( (int) 2003 );

  for ( unsigned int i = 0; i < 50; ++i )
    {
    std::cout << i << " : \t" << generator->GetVariate() << std::endl;
    }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
