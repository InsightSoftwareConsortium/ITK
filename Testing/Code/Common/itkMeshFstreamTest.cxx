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

int itkMeshFstreamTest(int argc, char* argv[] ) 
{
  if (argc < 2)
    {
    std::cout << "Usage: " << argv[0] << " logFilename" << std::endl;
    exit(EXIT_FAILURE);
    }
    
  std::ofstream ofs;
  ofs.open(argv[1]);
  ofs << "Testing Mesh & fstream" << std::endl;
  ofs.close();
  
  return 0; 
}
