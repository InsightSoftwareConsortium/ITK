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

#include <iostream>
#include "itkPolygonGroupSpatialObjectXMLFile.h"
#include "itksys/SystemTools.hxx"

static float strandPoints[11][2] = { { 1, 1 },       { 1, 2 },      { 1.25, 2 },  { 1.25, 1.25 },
                                     { 1.75, 1.25 }, { 1.75, 1.5 }, { 1.5, 1.5 }, { 1.5, 2 },
                                     { 2, 2 },       { 2, 1 },      { 1, 1 } };
using Polygon3DType = itk::PolygonSpatialObject<3>;
using PolygonGroup3DType = itk::GroupSpatialObject<3>;
using PolygonGroup3DPointer = PolygonGroup3DType::Pointer;

int
buildPolygonGroup(PolygonGroup3DPointer & PolygonGroup)
{
  try
  {
    for (float z = 0.0; z <= 10.0; z += 1.0)
    {
      itk::PolygonSpatialObject<3>::Pointer strand = itk::PolygonSpatialObject<3>::New();

      strand->SetThicknessInObjectSpace(1.0);
      //
      // add all points to this strand.
      for (auto & strandPoint : strandPoints)
      {
        double pos[3];
        pos[0] = strandPoint[0];
        pos[1] = strandPoint[1];
        pos[2] = z;
        itk::SpatialObjectPoint<3> curpoint;
        curpoint.SetPositionInObjectSpace(pos);
        strand->AddPoint(curpoint);
      }
      strand->Update();

      PolygonGroup->AddChild(strand);
      PolygonGroup->Update();
    }
  }
  catch (itk::ExceptionObject &)
  {
    std::cerr << "Error creating PolygonGroup" << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

int
testPolygonGroupEquivalence(PolygonGroup3DPointer & p1, PolygonGroup3DPointer & p2)
{
  //
  // Write out polygondata
  PolygonGroup3DType::ChildrenListType * children1 = p1->GetChildren();
  auto                                   it1 = children1->begin();
  auto                                   end1 = children1->end();

  PolygonGroup3DType::ChildrenListType * children2 = p2->GetChildren();
  auto                                   it2 = children2->begin();
  auto                                   end2 = children2->end();

  while (it1 != end1)
  {
    if (it2 == end2) // premature end of list
    {
      delete children1;
      delete children2;
      return EXIT_FAILURE;
    }
    auto * curstrand1 = dynamic_cast<Polygon3DType *>((*it1).GetPointer());
    auto * curstrand2 = dynamic_cast<Polygon3DType *>((*it2).GetPointer());

    Polygon3DType::PolygonPointListType & points1 = curstrand1->GetPoints();
    Polygon3DType::PolygonPointListType & points2 = curstrand2->GetPoints();

    auto pointIt1 = points1.begin();
    auto pointItEnd1 = points1.end();

    auto pointIt2 = points2.begin();
    auto pointItEnd2 = points2.end();


    while (pointIt1 != pointItEnd1)
    {
      if (pointIt2 == pointItEnd2)
      {
        delete children1;
        delete children2;
        return EXIT_FAILURE;
      }
      Polygon3DType::PointType curpoint1 = (*pointIt1).GetPositionInWorldSpace();
      Polygon3DType::PointType curpoint2 = (*pointIt2).GetPositionInWorldSpace();
      if (curpoint1 != curpoint2)
      {
        // Just a silly test to make sure that the positions returned are valid
        std::cerr << "Error: both points should have the same value: " << curpoint1 << " and " << curpoint2
                  << std::endl;
        // This should never happen in this test.
        return EXIT_FAILURE;
      }
      pointIt1++;
      pointIt2++;
    }
    if (pointIt2 != pointItEnd2)
    {
      delete children1;
      delete children2;
      return EXIT_FAILURE;
    }
    it1++;
    it2++;
  }

  if (it2 != end2)
  {
    delete children1;
    delete children2;
    return EXIT_FAILURE;
  }

  delete children1;
  delete children2;
  return EXIT_SUCCESS;
}
int
itkPolygonGroupSpatialObjectXMLFileTest(int ac, char * av[])
{
  if (ac < 2)
  {
    std::cerr << "Usage: " << av[0] << " XMLfile\n";
    return EXIT_FAILURE;
  }

  PolygonGroup3DPointer PolygonGroup = PolygonGroup3DType::New();

  PolygonGroup3DPointer PGroupFromFile;
  if (buildPolygonGroup(PolygonGroup) != 0 || PolygonGroup.IsNull())
  {
    std::cerr << "Error building polygon group" << std::endl;
    return EXIT_FAILURE;
  }

  std::string xmlfilename(av[1]);
  xmlfilename = xmlfilename + "/PolygonGroupSpatialObjectXMLFileTest.xml";
  try
  {
    itk::PolygonGroupSpatialObjectXMLFileWriter::Pointer pw = itk::PolygonGroupSpatialObjectXMLFileWriter::New();

    pw->SetFilename(xmlfilename.c_str());
    pw->SetObject(&(*PolygonGroup));
    pw->WriteFile();
  }
  catch (itk::ExceptionObject &)
  {
    std::cerr << "Error Creating file" << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    itk::PolygonGroupSpatialObjectXMLFileReader::Pointer p = itk::PolygonGroupSpatialObjectXMLFileReader::New();
    p->SetFilename(xmlfilename.c_str());
    p->GenerateOutputInformation();
    PGroupFromFile = p->GetOutputObject();
    if (PGroupFromFile.IsNull())
    {
      std::cerr << "Error retrieving object pointer" << std::endl;
      return EXIT_FAILURE;
    }
  }
  catch (itk::ExceptionObject &)
  {
    std::cerr << "Error Reading file" << std::endl;
    return EXIT_FAILURE;
  }
  itksys::SystemTools::RemoveFile(xmlfilename.c_str());

  return testPolygonGroupEquivalence(PolygonGroup, PGroupFromFile);
}
