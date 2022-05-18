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
// \index{itk::SurfaceSpatialObject}
//
// \doxygen{SurfaceSpatialObject} defines a surface in n-dimensional space.
// A \code{SurfaceSpatialObject} is defined by a list of points which lie on
// the surface. Each point has a position and a normal. The example begins by
// including the appropriate header file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkSurfaceSpatialObject.h"
// Software Guide : EndCodeSnippet

int
main(int, char *[])
{
  // Software Guide : BeginLatex
  //
  // \code{SurfaceSpatialObject} is templated over the dimension of the space.
  // A \code{SurfaceSpatialObject} contains a list of
  // \code{SurfaceSpatialObjectPoint}s.  A
  // \code{SurfaceSpatialObjectPoint} has a position, a normal and a color.
  //
  // First we define some type definitions
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using SurfaceType = itk::SurfaceSpatialObject<3>;
  using SurfacePointer = SurfaceType::Pointer;

  using SurfacePointType = SurfaceType::SurfacePointType;
  using CovariantVectorType = SurfaceType::CovariantVectorType;
  using PointType = SurfaceType::PointType;

  SurfacePointer surface = SurfaceType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We create a point list and we set the position of each point in the local
  // coordinate system using the \code{SetPositionInObjectSpace()} method. We
  // also set the color of each point to red.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  SurfaceType::SurfacePointListType list;

  for (unsigned int i = 0; i < 3; ++i)
  {
    SurfacePointType p;
    PointType        pnt;
    pnt[0] = i;
    pnt[1] = i + 1;
    pnt[2] = i + 2;
    p.SetPositionInObjectSpace(pnt);
    p.SetColor(1, 0, 0, 1);
    CovariantVectorType normal;
    for (unsigned int j = 0; j < 3; ++j)
    {
      normal[j] = j;
    }
    p.SetNormalInObjectSpace(normal);
    list.push_back(p);
  }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Next, we create the surface and set his name using \code{SetName()}. We
  // also set its Identification number with \code{SetId()} and we add the
  // list of points previously created.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  surface->GetProperty().SetName("Surface1");
  surface->SetId(1);
  surface->SetPoints(list);
  surface->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The \code{GetPoints()} method returns a reference to the internal list of
  // points of the object.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  SurfaceType::SurfacePointListType pointList = surface->GetPoints();
  std::cout << "Number of points representing the surface: ";
  std::cout << pointList.size() << std::endl;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Then we can access the points using standard STL iterators.
  // \code{GetPositionInObjectSpace()} and \code{GetColor()} functions return
  // respectively
  // the position and the color of the point. \code{GetNormalInObjectSpace()}
  // returns the
  // normal as a \doxygen{CovariantVector}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  SurfaceType::SurfacePointListType::const_iterator it =
    surface->GetPoints().begin();
  while (it != surface->GetPoints().end())
  {
    std::cout << "Position = " << (*it).GetPositionInObjectSpace()
              << std::endl;
    std::cout << "Normal = " << (*it).GetNormalInObjectSpace() << std::endl;
    std::cout << "Color = " << (*it).GetColor() << std::endl;
    std::cout << std::endl;
    it++;
  }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
