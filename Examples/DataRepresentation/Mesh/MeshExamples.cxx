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
// this file defines the ImageExamples for the test driver
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h"

void RegisterTests()
{
  REGISTER_TEST(AutomaticMeshTest);
  REGISTER_TEST(ImageToPointSetTest);
  REGISTER_TEST(Mesh1Test);
  REGISTER_TEST(Mesh2Test);
  REGISTER_TEST(Mesh3Test);
  REGISTER_TEST(MeshCellsIterationTest);
  REGISTER_TEST(MeshCellVisitorTest);
  REGISTER_TEST(MeshCellVisitor2Test);
  REGISTER_TEST(MeshKComplexTest);
  REGISTER_TEST(MeshPolyLineTest);
  REGISTER_TEST(MeshTraitsTest);
  REGISTER_TEST(PointSet1Test);
  REGISTER_TEST(PointSet2Test);
  REGISTER_TEST(PointSet3Test);
  REGISTER_TEST(PointSetWithCovariantVectorsTest);
  REGISTER_TEST(PointSetWithVectorsTest);
  REGISTER_TEST(RGBPointSetTest);
}

#undef main
#define main AutomaticMeshTest
#include "AutomaticMesh.cxx"

#undef main
#define main ImageToPointSetTest
#include "ImageToPointSet.cxx"

#undef main
#define main Mesh1Test
#include "Mesh1.cxx"

#undef main
#define main Mesh2Test
#include "Mesh2.cxx"

#undef main
#define main Mesh3Test
#include "Mesh3.cxx"

#undef main
#define main MeshCellsIterationTest
#include "MeshCellsIteration.cxx"

#undef main
#define main MeshCellVisitorTest
#include "MeshCellVisitor.cxx"

#undef main
#define main MeshCellVisitor2Test
#include "MeshCellVisitor2.cxx"

#undef main
#define main MeshKComplexTest
#include "MeshKComplex.cxx"

#undef main
#define main MeshPolyLineTest
#include "MeshPolyLine.cxx"

#undef main
#define main MeshTraitsTest
#include "MeshTraits.cxx"

#undef main
#define main PointSet1Test
#include "PointSet1.cxx"

#undef main
#define main PointSet2Test
#include "PointSet2.cxx"

#undef main
#define main PointSet3Test
#include "PointSet3.cxx"

#undef main
#define main PointSetWithCovariantVectorsTest
#include "PointSetWithCovariantVectors.cxx"

#undef main
#define main PointSetWithVectorsTest
#include "PointSetWithVectors.cxx"

#undef main
#define main RGBPointSetTest
#include "RGBPointSet.cxx"
