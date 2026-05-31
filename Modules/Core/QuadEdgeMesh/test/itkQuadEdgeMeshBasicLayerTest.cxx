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

// Exercises the QuadEdge geometric-iterator rings (Onext / Lnext) on a small
// two-triangle mesh built through the geometry-aware public API, and confirms
// the ring iterators throw rather than hang on a non-closing ring
// (InsightSoftwareConsortium/ITK#6372).
#include "itkQuadEdgeMesh.h"
#include "itkGeometricalQuadEdge.h"
#include "itkTestingMacros.h"

#include <array>
#include <iostream>
#include <set>
#include <utility>

namespace
{
using MeshType = itk::QuadEdgeMesh<double, 3>;
using PointType = MeshType::PointType;
using PointIdentifier = MeshType::PointIdentifier;
using QEPrimal = MeshType::QEPrimal;

// Origin ids visited around the left face (Lnext ring) of an edge. On a
// consistent mesh the ring closes, so the loop terminates by edge identity.
std::set<PointIdentifier>
LnextRingOrigins(QEPrimal * start)
{
  std::set<PointIdentifier> origins;
  for (auto it = start->BeginGeomLnext(); it != start->EndGeomLnext(); ++it)
  {
    origins.insert(*it);
  }
  return origins;
}

// Destination ids visited around the origin vertex (Onext ring) of an edge.
std::set<PointIdentifier>
OnextRingDestinations(QEPrimal * start)
{
  std::set<PointIdentifier> destinations;
  for (auto it = start->BeginGeomOnext(); it != start->EndGeomOnext(); ++it)
  {
    destinations.insert(it.Value()->GetDestination());
  }
  return destinations;
}

// A standalone quad-edge (a four-element Rot ring), origin and destination unset.
using BareQuadEdge = itk::GeometricalQuadEdge<int, int, bool, bool>;
using BareDualQuadEdge = BareQuadEdge::DualType;

BareQuadEdge *
MakeBareQuadEdge()
{
  auto * e1 = new BareQuadEdge();
  auto * e2 = new BareDualQuadEdge();
  auto * e3 = new BareQuadEdge();
  auto * e4 = new BareDualQuadEdge();

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

void
DeleteBareQuadEdge(BareQuadEdge * e)
{
  delete e->GetRot()->GetRot()->GetRot();
  delete e->GetRot()->GetRot();
  delete e->GetRot();
  delete e;
}

// Reproduces the internally-inconsistent structure the historical test built
// with the low-level QuadEdge::Splice primitive: only one of its three faces
// closes, so edge 0's Lnext ring never returns to its start edge.
void
MakeNonClosingLnextRing(std::array<BareQuadEdge *, 5> & e)
{
  for (auto & edge : e)
  {
    edge = MakeBareQuadEdge();
  }

  const std::array<int, 5> origin{ 0, 1, 2, 3, 0 };
  const std::array<int, 5> destination{ 1, 2, 3, 0, 2 };
  for (size_t i = 0; i < e.size(); ++i)
  {
    e[i]->SetOrigin(origin[i]);
    e[i]->SetDestination(destination[i]);
  }

  e[0]->Splice(e[4]);
  e[4]->Splice(e[3]->GetSym());
  e[1]->Splice(e[0]->GetSym());
  e[2]->Splice(e[4]->GetSym());
  e[4]->GetSym()->Splice(e[1]->GetSym());
  e[3]->Splice(e[2]->GetSym());
}

// Fully traverse a left-face ring; the iterator guard throws if it never closes.
void
TraverseLnextRing(BareQuadEdge * start, itk::SizeValueType maximumNumberOfSteps)
{
  auto it = start->BeginGeomLnext();
  it.SetMaximumNumberOfSteps(maximumNumberOfSteps);
  for (; it != start->EndGeomLnext(); ++it)
  {
  }
}
} // namespace

int
itkQuadEdgeMeshBasicLayerTest(int, char *[])
{
  // Two triangles sharing the diagonal 0--2, built through the geometry-aware
  // public API, which keeps the primal and dual rings consistent.
  auto mesh = MeshType::New();

  using ValueArrayType = PointType::ValueArrayType;
  const ValueArrayType coordinates[4] = {
    { 0.0, 0.0, 0.0 }, // vertex 0
    { 1.0, 0.0, 0.0 }, // vertex 1
    { 1.0, 1.0, 0.0 }, // vertex 2
    { 0.0, 1.0, 0.0 }  // vertex 3
  };
  for (const auto & coordinate : coordinates)
  {
    PointType point;
    point = coordinate;
    mesh->AddPoint(point);
  }

  ITK_TEST_EXPECT_TRUE(mesh->AddFaceTriangle(0, 1, 2) != nullptr);
  ITK_TEST_EXPECT_TRUE(mesh->AddFaceTriangle(0, 2, 3) != nullptr);

  ITK_TEST_EXPECT_EQUAL(mesh->GetNumberOfPoints(), 4u);
  ITK_TEST_EXPECT_EQUAL(mesh->GetNumberOfEdges(), 5u);
  ITK_TEST_EXPECT_EQUAL(mesh->GetNumberOfFaces(), 2u);

  // Every undirected edge is present in both directions.
  const std::array<std::pair<PointIdentifier, PointIdentifier>, 5> edges{
    { { 0, 1 }, { 1, 2 }, { 2, 0 }, { 2, 3 }, { 3, 0 } }
  };
  for (const auto & [from, to] : edges)
  {
    ITK_TEST_EXPECT_TRUE(mesh->FindEdge(from, to) != nullptr);
    ITK_TEST_EXPECT_TRUE(mesh->FindEdge(to, from) != nullptr);
  }

  // Each triangle's Lnext ring closes and visits exactly its three vertices.
  const std::set<PointIdentifier> face012{ 0, 1, 2 };
  const std::set<PointIdentifier> face023{ 0, 2, 3 };
  ITK_TEST_EXPECT_TRUE(LnextRingOrigins(mesh->FindEdge(0, 1)) == face012);
  ITK_TEST_EXPECT_TRUE(LnextRingOrigins(mesh->FindEdge(0, 2)) == face023);

  // The Onext ring of vertex 0 closes and reaches all three of its neighbors.
  const std::set<PointIdentifier> neighborsOfVertex0{ 1, 2, 3 };
  ITK_TEST_EXPECT_TRUE(OnextRingDestinations(mesh->FindEdge(0, 1)) == neighborsOfVertex0);

  // A non-closing ring must raise an exception instead of looping forever.
  std::array<BareQuadEdge *, 5> malformed{};
  MakeNonClosingLnextRing(malformed);
  ITK_TRY_EXPECT_EXCEPTION(TraverseLnextRing(malformed[0], 1000));
  for (auto * edge : malformed)
  {
    DeleteBareQuadEdge(edge);
  }

  std::cout << "Test finished.\n";
  return EXIT_SUCCESS;
}
