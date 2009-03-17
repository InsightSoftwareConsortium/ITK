/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    MeshExamples.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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
