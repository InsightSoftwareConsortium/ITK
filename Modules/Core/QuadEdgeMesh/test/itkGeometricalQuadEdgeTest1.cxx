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
#include "itkGeometricalQuadEdge.h"
#include <iostream>

class itkGeometricalQuadEdgeTest1Helper
{
public:
  using PointIdentifier = unsigned int;
  using FaceIdentifier = unsigned int;
  using PointData = float;
  using FaceData = std::string;

  using PrimalQuadEdgeType = itk::GeometricalQuadEdge<PointIdentifier, FaceIdentifier, PointData, FaceData, true>;


  using DualQuadEdgeType = PrimalQuadEdgeType::DualType;


  static PrimalQuadEdgeType *
  MakeQuadEdges()
  {
    auto * e1 = new PrimalQuadEdgeType();
    auto * e2 = new DualQuadEdgeType();
    auto * e3 = new PrimalQuadEdgeType();
    auto * e4 = new DualQuadEdgeType();

    e1->SetRot(e2);
    e2->SetRot(e3);
    e3->SetRot(e4);
    e4->SetRot(e1);

    e1->SetOnext(e1);
    e2->SetOnext(e4);
    e3->SetOnext(e3);
    e4->SetOnext(e4);

    return e1;
  }
};


int
itkGeometricalQuadEdgeTest1(int, char *[])
{

  using HelperType = itkGeometricalQuadEdgeTest1Helper;

  using PrimalQuadEdgeType = HelperType::PrimalQuadEdgeType;
  using DualQuadEdgeType = HelperType::DualQuadEdgeType;


  { // Define a local scope for testing constructors

    DualQuadEdgeType   dummyQuadEdge1; // test constructor
    PrimalQuadEdgeType dummyQuadEdge2; // test constructor

    dummyQuadEdge1.SetRot(&dummyQuadEdge2); // Test SetRot()
  }

  PrimalQuadEdgeType * qe = HelperType::MakeQuadEdges();
  // this will only call the method
  // a more rigorous test woul dbe needed
  qe->SetRight(1);
  qe->GetSym();
  qe->GetOprev();
  qe->GetLprev();
  qe->GetRprev();
  qe->GetDprev();
  qe->GetInvOnext();
  qe->GetInvLnext();
  qe->GetInvRnext();
  qe->GetInvDnext();
  qe->IsInLnextRing(nullptr);

  delete qe->GetRot()->GetRot()->GetRot();
  delete qe->GetRot()->GetRot();
  delete qe->GetRot();
  delete qe;

  return EXIT_SUCCESS;
}
