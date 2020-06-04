/*=========================================================================
 *
 *  Copyright NumFOCUS
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
// \index{itk::LandmarkSpatialObject}
//
// \doxygen{LandmarkSpatialObject} contains a list of
// \doxygen{SpatialObjectPoint}s which have a position and a color. Let's
// begin this example by including the appropriate header file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkLandmarkSpatialObject.h"
// Software Guide : EndCodeSnippet

int
main(int, char *[])
{
  // Software Guide : BeginLatex
  //
  // \code{LandmarkSpatialObject} is templated over the dimension of the
  // space.
  //
  // Here we create a 3-dimensional landmark.
  //
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  using LandmarkType = itk::LandmarkSpatialObject<3>;
  using LandmarkPointer = LandmarkType::Pointer;
  using LandmarkPointType = LandmarkType::LandmarkPointType;
  using PointType = LandmarkType::PointType;

  LandmarkPointer landmark = LandmarkType::New();
  // Software Guide : EndCodeSnippet
  // Software Guide : BeginLatex
  //
  // Next, we set some properties of the object like its name and its
  // identification number.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  landmark->GetProperty().SetName("Landmark1");
  landmark->SetId(1);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We are now ready to add points into the landmark. We first
  // create a list of SpatialObjectPoint and for each point we set the
  // position and the color.
  //
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  LandmarkType::LandmarkPointListType list;

  for (unsigned int i = 0; i < 5; ++i)
  {
    LandmarkPointType p;
    PointType         pnt;
    pnt[0] = i;
    pnt[1] = i + 1;
    pnt[2] = i + 2;
    p.SetPositionInObjectSpace(pnt);
    p.SetColor(1, 0, 0, 1);
    list.push_back(p);
  }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Then we add  the list to the object using the \code{SetPoints()} method.
  // Calling \code{Update()} afterwards ensures that World Space
  // representations are also updated to match changes in the SpatialObject's
  // parameters.
  //
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  landmark->SetPoints(list);
  landmark->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The current point list can be accessed using the \code{GetPoints()}
  // method. The method returns a reference to the (STL) list.
  //
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  size_t nPoints = landmark->GetPoints().size();
  std::cout << "Number of Points in the landmark: " << nPoints << std::endl;

  LandmarkType::LandmarkPointListType::const_iterator it =
    landmark->GetPoints().begin();
  while (it != landmark->GetPoints().end())
  {
    std::cout << "Position: " << (*it).GetPositionInObjectSpace()
              << std::endl;
    std::cout << "Color: " << (*it).GetColor() << std::endl;
    ++it;
  }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
