/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadBCMFC.cxx
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

#include "itkFEMLoadBCMFC.h"

namespace itk {
namespace fem {




/**
 * Fix a DOF to a prescribed value
 */
LoadBCMFC::LoadBCMFC(Element::ConstPointer element, int dof, vnl_vector<Element::Float> val)
{
  lhs.clear();

  /** Set the correct weight */
  lhs.push_back( MFCTerm(element, dof, 1.0) );
  rhs=val;
}




/** Read the LoadBCMFC object from input stream */
void LoadBCMFC::Read( std::istream& f, void* info )
{
  int nlhs, n;
  Node::Float d;
  /**
   * Convert the info pointer to a usable objects
   */
  ReadInfoType::ElementArrayPointer elements=static_cast<ReadInfoType*>(info)->m_el;


  /** first call the parent's Read function */
  Superclass::Read(f,info);

  /** read number of terms in lhs of MFC equation */
  SkipWhiteSpace(f); f>>nlhs; if(!f) goto out;
  
  lhs.clear();
  for(int i=0; i<nlhs; i++) 
  {
    /** read and set pointer to element that we're applying the load to */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    Element::ConstPointer element;
    try
    {
      element=dynamic_cast<const Element*>( &*elements->Find(n));
    }
    catch ( FEMExceptionObjectNotFound e )
    {
      throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"LoadBCMFC::Read()",e.m_baseClassName,e.m_GN);
    }

    /** read the number of dof within that element */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;

    /** read weight */
    SkipWhiteSpace(f); f>>d; if(!f) goto out;

    /** add a new MFCTerm to the lhs */
    lhs.push_back( MFCTerm(element, n, d) );
  }

  /** read the rhs */
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  rhs.resize(n);
  SkipWhiteSpace(f); f>>rhs; if(!f) goto out;

out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadBCMFC::Read()","Error reading FEM load!");
  }

}


/**
 * Write the LoadBCMFC object to the output stream
 */
void LoadBCMFC::Write( std::ostream& f ) const 
{
  /** first call the parent's write function */
  Superclass::Write(f);

  /**
   * Write the actual Load data
   */

  /** write the number of DOFs affected by this MFC */
  f << "\t" << static_cast<int>( lhs.size() ) << "\t% Number of DOFs in this MFC" << std::endl;

  /** write each term */
  f << "\t  %==>\n";
  for(LhsType::const_iterator q=lhs.begin(); q!=lhs.end(); q++) 
  {
    f << "\t  "<<q->m_element->GN<<"\t% GN of element" << std::endl;
    f << "\t  "<<q->dof<<"\t% DOF# in element" << std::endl;
    f << "\t  "<<q->value<<"\t% weight" << std::endl;
    f << "\t  %==>\n";
  }

  /** write the rhs */
  f << "\t" << static_cast<int>( rhs.size() );
  f << " "  << rhs <<"\t% rhs of MFC" << std::endl;

  /** check for errors */
  if (!f)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadBCMFC::Write()","Error writing FEM load!");
  }

}

FEM_CLASS_REGISTER(LoadBCMFC)




}} // end namespace itk::fem
