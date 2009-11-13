/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    testMetaGroup.cxx
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
#include <fstream>
#include <ctype.h>
#include <cstdlib>

#include <metaGroup.h>
#include <itksys/SystemTools.hxx>

int testMetaGroup(int argc, char *argv[])
{
  if (argc > 1)
    {
    itksys::SystemTools::ChangeDirectory(argv[1]);
    }
    
  // Testing metaGroup
  std::cout << "--- Testing metaGroup ---" << std::endl;

  std::cout << "Testing Writing:";
  MetaGroup tGroup;
  tGroup.Write("group.meta");
  std::cout << " [PASSED] " << std::endl;

  std::cout << "Testing Reading:";
  MetaGroup groupLoad("group.meta");
  std::cout << " [PASSED] " << std::endl;

  groupLoad.PrintInfo();

  std::cout << "Testing Copy:";
  MetaGroup groupCopy(&groupLoad);
  std::cout << " [PASSED] " << std::endl;
  
  groupCopy.PrintInfo();

  std::cout << "[DONE]" << std::endl;
  return EXIT_SUCCESS;
}
