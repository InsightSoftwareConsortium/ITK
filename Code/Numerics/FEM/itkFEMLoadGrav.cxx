/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadGrav.cxx
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

#include "itkFEMLoadGrav.h"

namespace itk {
namespace fem {




/**
 * Read the LoadGravConst object from input stream
 */
void LoadGravConst::Read( std::istream& f, void* info )
{
  int n;

  /** first call the parent's read function */
  LoadGrav::Read(f,info);

  /**
   * Read and set the force vector
   */

  /** first read and set the size of the vector */
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  Fg_value.resize(n);
  /** then the actual values */
  SkipWhiteSpace(f); f>>Fg_value; if(!f) goto out;
  
out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadGravConst::Read()","Error reading FEM load!");
  }

}




/**
 * Write the LoadGravConst to the output stream
 */
void LoadGravConst::Write( std::ostream& f ) const
{
  /** first call the parent's write function */
  LoadGrav::Write(f);

  /** then write the actual data force vector */
  f<<"\t"<<Fg_value.size()<<"\t% Size of the gravity force vector\n";
  f<<"\t"<<Fg_value<<"\t% Gravity force vector\n";

  /** check for errors */
  if (!f)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadGravConst::Write()","Error writing FEM load!");
  }
}

FEM_CLASS_REGISTER(LoadGravConst)




}} // end namespace itk::fem
