/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkPolygonGroupSpatialObjectXMLFileTest.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkPolygonGroupSpatialObject.h"
#include "itkPolygonSpatialObject.h"
#include <iostream>
#include "itkPolygonGroupSpatialObjectXMLFile.h"
#include <itksys/SystemTools.hxx>

static float points[11][2] = 
  {
    {1,1},{1,2},{1.25,2},{1.25,1.25},{1.75,1.25},
    {1.75,1.5},{1.5,1.5},{1.5,2},{2,2},{2,1},{1,1}
  };
typedef itk::PolygonSpatialObject<3> Polygon3DType;
typedef itk::PolygonGroupSpatialObject<3> PolygonGroup3DType;
typedef PolygonGroup3DType::Pointer PolygonGroup3DPointer;

int
buildPolygonGroup(PolygonGroup3DPointer &PolygonGroup)
{
  try
    {
    for(float z = 0.0; z <= 10.0; z += 1.0)
      {      
      itk::PolygonSpatialObject<3>::Pointer strand
        = itk::PolygonSpatialObject<3>::New();

      if(!PolygonGroup->AddStrand(strand))
        {
        std::cerr << "Error adding point" << std::endl;
        return -1;
        }
      strand->SetThickness(1.0);
      //
      // add all points to this strand.
      for(int i = 0; i < 11; i++)
        {
        double pos[3];
        pos[0] = points[i][0];
        pos[1] = points[i][1];
        pos[2] = z;
        itk::PolygonSpatialObject<3>::PointType curpoint(pos);
        if(!strand->AddPoint(curpoint)) 
          {
          std::cerr << "Error adding point" << std::endl;
          return -1;
          }
        }
      }
    }
  catch(itk::ExceptionObject &)
    {
    std::cerr << "Error creating PolygonGroup" << std::endl;
    return -1;
    }
  return 0;
}

int testPolygonGroupEquivalence(PolygonGroup3DPointer &p1,
                                PolygonGroup3DPointer &p2)
{
  //
  // Write out polygondata
  PolygonGroup3DType::ChildrenListType *children1 =
    p1->GetChildren(0,NULL);
  PolygonGroup3DType::ChildrenListType::iterator it1 =
    children1->begin();
  PolygonGroup3DType::ChildrenListType::iterator end1 =
    children1->end();

  PolygonGroup3DType::ChildrenListType *children2 =
    p2->GetChildren(0,NULL);
  PolygonGroup3DType::ChildrenListType::iterator it2 =
    children2->begin();
  PolygonGroup3DType::ChildrenListType::iterator end2 =
    children2->end();

  while(it1 != end1)
    {
    if(it2 == end2) // premature end of list
      return -1;
    Polygon3DType *curstrand1 =
      dynamic_cast<Polygon3DType *>((*it1));
    Polygon3DType *curstrand2 =
      dynamic_cast<Polygon3DType *>((*it2));

    Polygon3DType::PointListType &points1 =
      curstrand1->GetPoints();
    Polygon3DType::PointListType &points2 =
      curstrand2->GetPoints();

    Polygon3DType::PointListType::iterator pointIt1 
      = points1.begin();
    Polygon3DType::PointListType::iterator pointItEnd1
      = points1.end();

    Polygon3DType::PointListType::iterator pointIt2 
      = points2.begin();
    Polygon3DType::PointListType::iterator pointItEnd2
      = points2.end();

    while(pointIt1 != pointItEnd1) 
      {
      if(pointIt2 == pointItEnd2)
        return -1;
      Polygon3DType::PointType curpoint1 = 
        (*pointIt1).GetPosition();
      Polygon3DType::PointType curpoint2 = 
        (*pointIt2).GetPosition();
      pointIt1++;
      pointIt2++;
      }
    if(pointIt2 != pointItEnd2)
      return -1;
    it1++;
    it2++;
    }
  if(it2 != end2)
    return -1;
  return 0;
}
int itkPolygonGroupSpatialObjectXMLFileTest(int ac, char *av[])
{
    return EXIT_FAILURE;
  if(ac < 2)
    {
    std::cerr << "Usage: " << av[0] << " XMLfile\n";
    return EXIT_FAILURE;
    }

  PolygonGroup3DPointer PolygonGroup = PolygonGroup3DType::New();
  PolygonGroup3DPointer PGroupFromFile;
  if(buildPolygonGroup(PolygonGroup) != 0 || PolygonGroup.IsNull())
    {
    return -1;
    }

  std::string xmlfilename(av[1]);
  xmlfilename = xmlfilename + "/PolygonGroupSpatialObjectXMLFileTest.xml";

  try
    {
    itk::PolygonGroupSpatialObjectXMLFileWriter::Pointer pw =
      itk::PolygonGroupSpatialObjectXMLFileWriter::New();
  
    pw->SetFilename(xmlfilename.c_str());
    pw->SetObject(&(*PolygonGroup));
    pw->WriteFile();
    }
  catch(itk::ExceptionObject &)
    {
    std::cerr << "Error Creating file" << std::endl;
    return -1;
    }

  try
    {
    itk::PolygonGroupSpatialObjectXMLFileReader::Pointer p = 
      itk::PolygonGroupSpatialObjectXMLFileReader::New();
    p->SetFilename(xmlfilename.c_str());
    p->GenerateOutputInformation();
    PGroupFromFile = p->GetOutputObject();
    if(PGroupFromFile.IsNull())
      {
      std::cerr << "Error retrieving object pointer" << std::endl;
      return -1;
      }
    }
  catch(itk::ExceptionObject &)
    {
    std::cerr << "Error Reading file" << std::endl;
    return -1;
    }
  itksys::SystemTools::RemoveFile(xmlfilename.c_str());
  return testPolygonGroupEquivalence(PolygonGroup,PGroupFromFile);
}
