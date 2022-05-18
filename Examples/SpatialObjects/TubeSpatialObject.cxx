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
// \index{itk::TubeSpatialObject}
//
// \doxygen{TubeSpatialObject} defines an n-dimensional tube.  A tube is
// defined as a list of centerline points which have a position, a radius,
// some normals and other properties. Let's start by including the
// appropriate header file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkTubeSpatialObject.h"
// Software Guide : EndCodeSnippet

int
main(int, char *[])
{
  // Software Guide : BeginLatex
  //
  // TubeSpatialObject is templated over the dimension of the space.  A
  // TubeSpatialObject contains a list of TubeSpatialObjectPoints.
  //
  // First we define some type definitions and we create the tube.
  //
  // Software Guide : EndLatex

  unsigned int i;

  // Software Guide : BeginCodeSnippet
  using TubeType = itk::TubeSpatialObject<3>;
  using TubePointer = TubeType::Pointer;

  using TubePointType = TubeType::TubePointType;

  using PointType = TubeType::PointType;
  using CovariantVectorType = TubePointType::CovariantVectorType;

  TubePointer tube = TubeType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We create a point list and we set:
  // \begin{enumerate}
  // \item The position of each point in the local coordinate system using the
  // \code{SetPositionInObjectSpace()} method.
  // \item The radius of the tube at this position using
  // \code{SetRadiusInObjectSpace()}.
  // \item The two normals at the tube is set using
  // \code{SetNormal1InObjectSpace()} and
  // \code{SetNormal2InObjectSpace()}.
  // \item The color of the point is set to red in our case.
  // \end{enumerate}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TubeType::TubePointListType list;
  for (i = 0; i < 5; ++i)
  {
    TubePointType p;
    PointType     pnt;
    pnt[0] = i;
    pnt[1] = i + 1;
    pnt[2] = i + 2;
    p.SetPositionInObjectSpace(pnt);
    p.SetRadiusInObjectSpace(1);
    CovariantVectorType normal1;
    CovariantVectorType normal2;
    for (unsigned int j = 0; j < 3; ++j)
    {
      normal1[j] = j;
      normal2[j] = j * 2;
    }

    p.SetNormal1InObjectSpace(normal1);
    p.SetNormal2InObjectSpace(normal2);
    p.SetColor(1, 0, 0, 1);

    list.push_back(p);
  }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Next, we create the tube and set its name using \code{SetName()}. We also
  // set its identification number with \code{SetId()} and, at the end, we add
  // the list of points previously created.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  tube->GetProperty().SetName("Tube1");
  tube->SetId(1);
  tube->SetPoints(list);
  tube->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The \code{GetPoints()} method return a reference to the internal list of
  // points of the object.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TubeType::TubePointListType pointList = tube->GetPoints();
  std::cout << "Number of points representing the tube: ";
  std::cout << pointList.size() << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The \code{ComputeTangentAndNormals()} function computes the normals and
  // the tangent for each point using finite differences.
  //
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  tube->ComputeTangentsAndNormals();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Then we can access the points using STL iterators.
  // \code{GetPositionInObjectSpace()}
  // and \code{GetColor()} functions return respectively the position and the
  // color of the point. \code{GetRadiusInObjectSpace()} returns the radius at
  // that point. \code{GetNormal1InObjectSpace()} and
  // \code{GetNormal2InObjectSpace()} functions return a
  // \doxygen{CovariantVector} and \code{GetTangentInObjectSpace()} returns a
  // \doxygen{Vector}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TubeType::TubePointListType::const_iterator it = tube->GetPoints().begin();
  i = 0;
  while (it != tube->GetPoints().end())
  {
    std::cout << std::endl;
    std::cout << "Point #" << i << std::endl;
    std::cout << "Position: " << (*it).GetPositionInObjectSpace()
              << std::endl;
    std::cout << "Radius: " << (*it).GetRadiusInObjectSpace() << std::endl;
    std::cout << "Tangent: " << (*it).GetTangentInObjectSpace() << std::endl;
    std::cout << "First Normal: " << (*it).GetNormal1InObjectSpace()
              << std::endl;
    std::cout << "Second Normal: " << (*it).GetNormal2InObjectSpace()
              << std::endl;
    std::cout << "Color = " << (*it).GetColor() << std::endl;
    it++;
    i++;
  }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
