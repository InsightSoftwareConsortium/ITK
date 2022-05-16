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

int
main(int, char *[])
{
  // Software Guide : BeginLatex
  //
  // The \doxygen{ArrowSpatialObject}, like many SpatialObjects, is templated
  // over the dimensionality of the object.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using ArrowType = itk::ArrowSpatialObject<3>;
  auto myArrow = ArrowType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The position of the arrow in the object (local) coordinate frame is
  // defined using the \code{SetPositionInObjectSpace()} method. By default
  // the position is set to the origin of the space.  This is the "tip" of the
  // arrow.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ArrowType::PointType pos;
  pos.Fill(1);
  myArrow->SetPositionInObjectSpace(pos);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // Software Guide : BeginLatex
  //
  // The length of the arrow in the local coordinate frame is done using the
  // \code{SetLengthInObjectSpace()} method. By default the length is set
  // to 1. This is the euclidean distance spanned by the arrow's tail from its
  // tip (position).
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  myArrow->SetLengthInObjectSpace(2);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The direction of the arrow can be set using the
  // \code{SetDirectionInObjectSpace()} method.
  // This is the direction the tail of the arrow extends from the position.
  // By default the direction is set along the X axis (first direction).
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ArrowType::VectorType direction;
  direction.Fill(0);
  direction[1] = 1.0;
  myArrow->SetDirectionInObjectSpace(direction);
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
