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
// \index{itk::GaussianSpatialObject}
//
// This example shows how to create a \doxygen{GaussianSpatialObject} which
// defines a Gaussian in a N-dimensional space. This object is particularly useful
// to query the value at a point in physical space.
// Let's begin by including the appropriate header file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkGaussianSpatialObject.h"
// Software Guide : EndCodeSnippet

int main(int, char* [])
{
  // Software Guide : BeginLatex
  //
  // The \doxygen{GaussianSpatialObject} is templated
  // over the dimensionality of the object.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::GaussianSpatialObject<3>   GaussianType;
  GaussianType::Pointer myGaussian = GaussianType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The \code{SetMaximum()} function is used to set the maximum
  // value of the Gaussian.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  myGaussian->SetMaximum(2);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The radius of the Gaussian is defined by the \code{SetRadius()} method.
  // By default the radius is set to 1.0.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  myGaussian->SetRadius(3);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The standard \code{ValueAt()} function is used to determine the value
  // of the Gaussian at a particular point in physical space.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  itk::Point<double,3> pt;
  pt[0]=1;
  pt[1]=2;
  pt[2]=1;
  double value;
  myGaussian->ValueAt(pt, value);
  std::cout << "ValueAt(" << pt << ") = " << value << std::endl;
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
