/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMBar2DTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMElementBar2D.h"
#include <iostream>

using namespace std;




int itkFEMBar2DTest(int, char**)
{

  string desc="";
  cout<<"Testing FEM element Bar2D ... ";

  // Create a Bar2D element
  itk::fem::Bar2D::Pointer e;
  e=dynamic_cast<itk::fem::Bar2D*> ( &*itk::fem::FEMOF::Create(itk::fem::Bar2D::OFID) );
  if (e==0)
  {
    desc="Element Bar2D could not be created!";
    goto fail;
  }

  // All was good...
  cout<<"PASSED\n";
  return EXIT_SUCCESS;



fail:
  // Something went wrong...
  cout<<"FAILED\n";
  if ( !desc.empty() ) { cout<<"  - "<<desc<<"\n"; }
  return EXIT_FAILURE;

}
