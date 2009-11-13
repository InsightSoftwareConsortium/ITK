/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    testMetaSurface.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <stdio.h>
#include <ctype.h>
#include <cstdlib>
#include <metaSurface.h>
#include <itksys/SystemTools.hxx>

int testMetaSurface(int argc, char * argv[])
{
  if (argc > 1)
    {
    itksys::SystemTools::ChangeDirectory(argv[1]);
    }

  std::cout << "Creating test file ...";
  MetaSurface* surface = new MetaSurface(3);
  surface->ID(0);
  SurfacePnt* pnt;

  unsigned int i;
  for(i=0;i<10;i++)
  {
    pnt = new SurfacePnt(3);
    pnt->m_X[0]=(float)0.2;
    pnt->m_X[1]=static_cast<float>(i);
    pnt->m_X[2]=static_cast<float>(i);
    pnt->m_V[0]=(float)0.8;
    pnt->m_V[1]=static_cast<float>(i);
    pnt->m_V[2]=static_cast<float>(i);
    surface->GetPoints().push_back(pnt);
  }
  

  std::cout << "Writing ASCII test file ...";

  surface->Write("mySurface.meta");

  std::cout << "done" << std::endl;
  std::cout << "Reading ASCII test file ...";

  surface->Clear();
  surface->Read("mySurface.meta");
  surface->PrintInfo();

  MetaSurface::PointListType list =  surface->GetPoints();
  MetaSurface::PointListType::const_iterator it = list.begin();
  
  unsigned int d;
  while(it != list.end())
  {
    
    for(d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_X[d] << " ";
    }
    std::cout << std::endl;
    for(d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_V[d] << " ";
    }
    std::cout << std::endl;
    it++;
  }

  delete surface;

  // Testing Binary Data
  MetaSurface* surfaceBin = new MetaSurface(3);

  for(i=0;i<10;i++)
  {
    pnt = new SurfacePnt(3);
    pnt->m_X[0]=(float)0.2;
    pnt->m_X[1]=static_cast<float>(i);
    pnt->m_X[2]=static_cast<float>(i);
    pnt->m_V[0]=(float)0.8;
    pnt->m_V[1]=static_cast<float>(i);
    pnt->m_V[2]=static_cast<float>(i);
    surfaceBin->GetPoints().push_back(pnt);
  }
  

  std::cout << "Writing Binary test file ...";
  surfaceBin->BinaryData(true);
  surfaceBin->ElementType(MET_FLOAT);
  surfaceBin->Write("mySurfaceBin.meta");

  std::cout << "done" << std::endl;
  std::cout << "Reading Binary test file ...";

  surfaceBin->Clear();
  surfaceBin->Read("mySurfaceBin.meta");
  surfaceBin->PrintInfo();
  
  MetaSurface::PointListType list2 =  surfaceBin->GetPoints();
  it = list2.begin();
  
  while(it != list2.end())
  {
    for(d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_X[d] << " ";
    }
    std::cout << std::endl;
    for(d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_V[d] << " ";
    }
    std::cout << std::endl;
    it++;
  }
  
  delete surfaceBin;
  std::cout << "done" << std::endl;
  return EXIT_SUCCESS;
}
