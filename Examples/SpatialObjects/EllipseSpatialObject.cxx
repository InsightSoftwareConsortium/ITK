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
// \index{itk::EllipseSpatialObject}
//
// \doxygen{EllipseSpatialObject} defines an n-Dimensional ellipse. Like
// other spatial objects this class derives from
// \doxygen{SpatialObject}. Let's start by including the appropriate header
// file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkEllipseSpatialObject.h"
// Software Guide : EndCodeSnippet

int main( int , char *[] )
{
// Software Guide : BeginLatex
//
// Like most of the SpatialObjects, the \doxygen{EllipseSpatialObject} is templated
// over the dimension of the space. In this example we create a 3-dimensional ellipse.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::EllipseSpatialObject<3>   EllipseType;
  EllipseType::Pointer myEllipse = EllipseType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Then we set a radius for each dimension. By default the radius is set to 1.
//
// Software Guide : EndLatex
// Software Guide : BeginCodeSnippet
  EllipseType::ArrayType radius;
  for (unsigned int i = 0; i<3; ++i)
    {
    radius[i] = i;
    }

  myEllipse->SetRadius(radius);
// Software Guide : EndCodeSnippet
// Software Guide : BeginLatex
//
// Or if we have the same radius in each dimension we can do
//
// Software Guide : EndLatex
// Software Guide : BeginCodeSnippet
  myEllipse->SetRadius(2.0);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We can then display the current radius by using the \code{GetRadius()}
// function:
//
// Software Guide : EndLatex
// Software Guide : BeginCodeSnippet
  EllipseType::ArrayType myCurrentRadius = myEllipse->GetRadius();
  std::cout << "Current radius is " << myCurrentRadius << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Like other SpatialObjects, we can query the object if a point is inside
// the object by using the IsInside(itk::Point) function. This function expects
// the point to be in world coordinates.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  itk::Point<double,3> insidePoint;
  insidePoint.Fill(1.0);
  if (myEllipse->IsInside(insidePoint))
    {
    std::cout << "The point " << insidePoint;
    std::cout << " is really inside the ellipse" << std::endl;
    }

  itk::Point<double,3> outsidePoint;
  outsidePoint.Fill(3.0);
  if (!myEllipse->IsInside(outsidePoint))
    {
    std::cout << "The point " << outsidePoint;
    std::cout << " is really outside the ellipse" << std::endl;
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// All spatial objects can be queried for a value at a point.  The
// \code{IsEvaluableAt()} function returns a boolean to know if the object is
// evaluable at a particular point.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
   if (myEllipse->IsEvaluableAt(insidePoint))
    {
    std::cout << "The point " << insidePoint;
    std::cout << " is evaluable at the point " << insidePoint << std::endl;
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
//  If the object is evaluable at that point, the \code{ValueAt()} function
//  returns the current value at that position.  Most of the objects returns
//  a boolean value which is set to true when the point is inside the object
//  and false when it is outside. However, for some objects, it is more
//  interesting to return a value representing, for instance, the distance
//  from the center of the object or the distance from from the boundary.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  double value;
  myEllipse->ValueAt(insidePoint,value);
  std::cout << "The value inside the ellipse is: " << value << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Like other spatial objects, we can also query the bounding box of the
// object by using \code{GetBoundingBox()}. The resulting bounding box is
// expressed in the local frame.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  myEllipse->ComputeBoundingBox();
  EllipseType::BoundingBoxType * boundingBox = myEllipse->GetBoundingBox();
  std::cout << "Bounding Box: " << boundingBox->GetBounds() << std::endl;
// Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
