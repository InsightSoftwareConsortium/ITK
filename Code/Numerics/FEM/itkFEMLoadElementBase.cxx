/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadElementBase.cxx
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

#include "itkFEMLoadElementBase.h"

namespace itk {
namespace fem {




/**
 * Read the LoadElement object from input stream
 */
void LoadElement::Read( std::istream& f, void* info )
{

  int n;
  /**
   * Convert the info pointer to a usable objects
   */
  ReadInfoType::ElementArrayPointer elements=static_cast<ReadInfoType*>(info)->m_el;


  /** first call the parent's read function */
  Superclass::Read(f,info);

  /**
   * read and set pointers to element that we're applying the load to
   */

  /** first we read number of pointers in a list */
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  if (n<=0)
  {
    /** if this is <= 0, the load applies on all elements in a system */
    el.clear();
  }
  else 
  {
    /**
     * otherwise we read all the element numbers.
     * there should be n of them
     */
    for(int i=0;i<n;i++) {
      int m;
      SkipWhiteSpace(f); f>>m; if(!f) goto out;
      Element::ConstPointer e;
      try
      {
        e=elements->Find(m);
      }
      catch ( FEMExceptionObjectNotFound e )
      {
        throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"LoadElementBase::Read()",e.m_baseClassName,e.m_GN);
      }

      el.push_back(e);

    }

  }
  
  
out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadElementBase::Read()","Error reading FEM load!");
  }

}




/**
 * Write the LoadElement to the output stream
 */
void LoadElement::Write( std::ostream& f ) const
{
  /**
   * first call the parent's write function
   */
  Superclass::Write(f);

  /** Write the list of element global numbers */
  if (!el.empty())
    {
    f << "\t" <<static_cast<int>((el.size()));
  f << "\t% # of elements on which the load acts" << std::endl;
    f << "\t";
    for(ElementPointersVectorType::const_iterator i=el.begin(); i!=el.end(); i++) {
      f<<((*i)->GN)<<" ";
    }
  f << "\t% GNs of elements" << std::endl;
  }
  else {
    f << "\t-1\t% Load acts on all elements" << std::endl;
  }

  /** check for errors */
  if (!f)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadElement::Write()","Error writing FEM load!");
  }

}

FEM_CLASS_REGISTER(LoadElement)




}} // end namespace itk::fem
