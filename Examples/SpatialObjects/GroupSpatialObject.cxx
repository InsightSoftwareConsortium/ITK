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
// \index{itk::GroupSpatialObject}
//
// A \doxygen{GroupSpatialObject} does not have
// any data associated with it. It can be used to group objects
// or to add transforms to a current object.
// In this example we show how to use a GroupSpatialObject.
//
// Let's begin by including the appropriate header file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkGroupSpatialObject.h"
// Software Guide : EndCodeSnippet
#include "itkEllipseSpatialObject.h"

int main( int , char *[] )
{
// Software Guide : BeginLatex
//
// The \doxygen{GroupSpatialObject} is templated
// over the dimensionality of the object.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::GroupSpatialObject<3>   GroupType;
  GroupType::Pointer myGroup = GroupType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next, we create an \doxygen{EllipseSpatialObject} and add it to
// the group.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::EllipseSpatialObject<3>   EllipseType;
  EllipseType::Pointer myEllipse = EllipseType::New();
  myEllipse->SetRadius(2);

  myGroup->AddSpatialObject(myEllipse);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We then translate the group by 10mm in each direction.
// Therefore the ellipse is translated in physical space at the same time.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  GroupType::VectorType offset;
  offset.Fill(10);
  myGroup->GetObjectToParentTransform()->SetOffset(offset);
  myGroup->ComputeObjectToWorldTransform();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We can then query if a point is inside the group using the
// \code{IsInside()} function. We need to specify in this case that
// we want to consider all the hierarchy, therefore we set the depth to 2.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  GroupType::PointType point;
  point.Fill(10);
  std::cout << "Is my point " << point << " inside?: "
    <<  myGroup->IsInside(point,2) << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Like any other SpatialObjects we can remove the ellipse from the group
// using the \code{RemoveSpatialObject()} method.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  myGroup->RemoveSpatialObject(myEllipse);
// Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
