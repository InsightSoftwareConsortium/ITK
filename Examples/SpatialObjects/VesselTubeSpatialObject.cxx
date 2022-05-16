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
// \index{itk::VesselTubeSpatialObject}
//
// \doxygen{VesselTubeSpatialObject} derives from \doxygen{TubeSpatialObject}.
// It represents a blood vessel segmented from an image.
// A VesselTubeSpatialObject is described as a list of centerline points which
// have a position, a radius, and normals.
//
// Let's start by including the appropriate header file.
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
  // VesselTubeSpatialObject is templated over the dimension of the space.  A
  // VesselTubeSpatialObject contains a list of VesselTubeSpatialObjectPoints.
  //
  // First we define some type definitions and we create the tube.
  //
  // Software Guide : EndLatex

  unsigned int i;

  // Software Guide : BeginCodeSnippet
  using VesselTubeType = itk::TubeSpatialObject<3>;
  using VesselTubePointType = VesselTubeType::TubePointType;

  using PointType = VesselTubeType::PointType;

  auto vesselTube = VesselTubeType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We create a point list and we set:
  // \begin{enumerate}
  // \item The position of each point in the local coordinate system using the
  // \code{SetPositionInObjectSpace()} method.
  // \item The radius of the tube at this position using
  // \code{SetRadiusInObjectSpace()}. \item The medialness value describing
  // how the point lies in the middle of the vessel using
  // \code{SetMedialness()}. \item The ridgeness value describing how the
  // point lies on the ridge using \code{SetRidgeness()}. \item The branchness
  // value describing if the point is a branch point using
  // \code{SetBranchness()}. \item The three alpha values corresponding to the
  // eigenvalues of the Hessian
  //  using \code{SetAlpha1()},\code{SetAlpha2()} and \code{SetAlpha3()}.
  // \item The mark value using \code{SetMark()}.
  // \item The color of the point is set to red in this example with an
  // opacity of 1. \end{enumerate}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  VesselTubeType::TubePointListType list;
  for (i = 0; i < 5; ++i)
  {
    VesselTubePointType p;
    PointType           pnt;
    pnt[0] = i;
    pnt[1] = i + 1;
    pnt[2] = i + 2;
    p.SetPositionInObjectSpace(pnt);
    p.SetRadiusInObjectSpace(1);
    p.SetAlpha1(i);
    p.SetAlpha2(i + 1);
    p.SetAlpha3(i + 2);
    p.SetMedialness(i);
    p.SetRidgeness(i);
    p.SetBranchness(i);
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
  vesselTube->GetProperty().SetName("VesselTube");
  vesselTube->SetId(1);
  vesselTube->SetPoints(list);
  vesselTube->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The \code{GetPoints()} method return a reference to the internal list of
  // points of the object.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  VesselTubeType::TubePointListType pointList = vesselTube->GetPoints();
  std::cout << "Number of points representing the blood vessel: ";
  std::cout << pointList.size() << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Then we can access the points using STL iterators.
  // \code{GetPositionInObjectSpace()}
  // and \code{GetColor()} functions return respectively the position and the
  // color of the point.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  VesselTubeType::TubePointListType::const_iterator it =
    vesselTube->GetPoints().begin();
  i = 0;
  while (it != vesselTube->GetPoints().end())
  {
    std::cout << std::endl;
    std::cout << "Point #" << i << std::endl;
    std::cout << "Position: " << (*it).GetPositionInObjectSpace()
              << std::endl;
    std::cout << "Radius: " << (*it).GetRadiusInObjectSpace() << std::endl;
    std::cout << "Medialness: " << (*it).GetMedialness() << std::endl;
    std::cout << "Ridgeness: " << (*it).GetRidgeness() << std::endl;
    std::cout << "Branchness: " << (*it).GetBranchness() << std::endl;
    std::cout << "Alpha1: " << (*it).GetAlpha1() << std::endl;
    std::cout << "Alpha2: " << (*it).GetAlpha2() << std::endl;
    std::cout << "Alpha3: " << (*it).GetAlpha3() << std::endl;
    std::cout << "Color = " << (*it).GetColor() << std::endl;
    ++it;
    ++i;
  }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
