/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLightObject.cxx
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

#include "itkFEMLightObject.h"
#include "itkFEMUtility.h"

namespace itk {
namespace fem {




/**
 * Here we just read the global number from the stream.
 * This should be the first function called when reading object data.
 */
void FEMLightObject::Read( std::istream& f, void* info )
{
  int n;

  /** Read and set the global object number */
  SkipWhiteSpace(f); f>>n; if(!f) { goto out; }
  this->GN=n;

out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"FEMLightObject::Read","Error reading FEM object!");
  }

}




/** 
 * Here we just write the class name and GN.
 * This should be the first function called when writing object data, so
 * every derived class should first call the parent's write function.
 * Each derived class should also set the clid to correct value only if
 * clid  is <0.
 * This way the Write function in base (this one) class knows which class is
 * being written and can write the class name properly.
 */
void FEMLightObject::Write( std::ostream& f, int clid ) const
{

  /** check if somebody has defined the clid */
  if (clid<0) {
    /**
     * Nope... This means that either no Write function is defined for
     * the derived class, or somebody was trying to write an abstract
     * class. We should start yelling here...
     */
    return;
  }

  /**  first write the class name */
  f<<'<'<<FEMObjectFactory<Self>::ID2ClassName(clid)<<">\n";

  /** then the global object number */
  f<<"\t"<<GN<<"\t% Global object number\n";

  /** check for errors */
  if (!f)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"FEMLightObject::Write","Error writing FEM object!");
  }

}




}} // end namespace itk::fem
