/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshFstreamTest.cxx
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

#include <fstream>
#include <itkMesh.h>

int itkMeshFstreamTest(int, char* [] ) 
{

  
  std::ofstream ofs;
  ofs.open("test.txt");
  ofs << "Testing Mesh & fstream" << std::endl;
  ofs.close();
  
  return 0; 
}
