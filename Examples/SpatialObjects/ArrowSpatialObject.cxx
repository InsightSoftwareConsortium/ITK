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
// \index{itk::ArrowSpatialObject}
//
// This example shows how to create an \doxygen{ArrowSpatialObject}.
// Let's begin by including the appropriate header file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkArrowSpatialObject.h"
// Software Guide : EndCodeSnippet

int main( int , char *[] )
{
// Software Guide : BeginLatex
//
// The \doxygen{ArrowSpatialObject}, like many SpatialObjects, is templated
// over the dimensionality of the object.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::ArrowSpatialObject<3>   ArrowType;
  ArrowType::Pointer myArrow = ArrowType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The length of the arrow in the local coordinate frame is done using the
// \code{SetLength()} method. By default the length is set to 1.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  myArrow->SetLength(2);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The direction of the arrow can be set using the \code{SetDirection()} method.
// Calling \code{SetDirection()} modifies the \code{ObjectToParentTransform}
// (not the \code{IndexToObjectTransform}).
// By default the direction is set along the X axis (first direction).
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ArrowType::VectorType direction;
  direction.Fill(0);
  direction[1] = 1.0;
  myArrow->SetDirection(direction);
// Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
