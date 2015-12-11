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
//  Software Guide : BeginLatex
//
//  The following code is an implementation of a small ITK
//  program. It tests including header files and linking with ITK
//  libraries.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include <iostream>

int main()
{
  typedef itk::Image< unsigned short, 3 > ImageType;

  ImageType::Pointer image = ImageType::New();

  std::cout << "ITK Hello World !" << std::endl;

  return EXIT_SUCCESS;
}
// Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  This code instantiates a $3D$ image\footnote{Also known as a
//  \emph{volume}.} whose pixels are represented with type \code{unsigned
//  short}. The image is then constructed and assigned to a
//  \doxygen{SmartPointer}. Although later in the text we will discuss
//  \code{SmartPointer}s in detail, for now think of it as a handle on an
//  instance of an object (see section \ref{sec:SmartPointers} for more
//  information). The \doxygen{Image} class will be described in
//  Section~\ref{sec:ImageSection}.
//
//  Software Guide : EndLatex
