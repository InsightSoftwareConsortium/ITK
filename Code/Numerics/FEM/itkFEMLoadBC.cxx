/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadBC.cxx
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

#include "itkFEMLoadBC.h"
#include "itkFEMUtility.h"

namespace itk {
namespace fem {




/** Read the LoadBC object from input stream */
void LoadBC::Read( std::istream& f, void* info )
{
  unsigned int n;
  /*
   * Convert the info pointer to a usable objects
   */
  ReadInfoType::ElementArrayPointer elements=static_cast<ReadInfoType*>(info)->m_el;


  /* first call the parent's Read function */
  Superclass::Read(f,info);

  /* read and set pointer to element that we're applying the load to */
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  try
  {
    this->m_element=dynamic_cast<const Element*>( &*elements->Find(n) );
  }
  catch ( FEMExceptionObjectNotFound e )
  {
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"LoadBC::Read()",e.m_baseClassName,e.m_GN);
  }

  /* read the local DOF number within that element */
  SkipWhiteSpace(f); f>>this->m_dof; if(!f) goto out;

  /* read the value to which the DOF is fixed */
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  this->m_value.resize(n);
  SkipWhiteSpace(f); f>>this->m_value; if(!f) goto out;

out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadBC::Read()","Error reading FEM load!");
  }

}


/**
 * Write the LoadBC object to the output stream
 */
void LoadBC::Write( std::ostream& f ) const 
{
  /* first call the parent's write function */
  Superclass::Write(f);

  /*
   * Write the actual Load data
   */
  f<<"\t"<<this->m_element->GN<<"\t% GN of element"<<"\n";
  f<<"\t"<<this->m_dof<<"\t% DOF# in element"<<"\n";

  /* write the value of dof */
  f<<"\t"<<this->m_value.size();
  f<<" "<<this->m_value<<"\t% value of the fixed DOF"<<"\n";

  /* check for errors */
  if (!f)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadBC::Write()","Error writing FEM load!");
  }

}

FEM_CLASS_REGISTER(LoadBC)




}} // end namespace itk::fem
