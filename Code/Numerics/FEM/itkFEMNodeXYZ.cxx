/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMNodeXYZ.cxx
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

#include "itkFEMNodeXYZ.h"
#include "itkFEMUtility.h"

namespace itk {
namespace fem {




/**
 * read the NodeXYZ from input stream
 */
void NodeXYZ::Read(  std::istream& f, void* info )
{
  Float d;

  //std::cout << "TS: Reading node" << std::endl;

  /**
   * first call the parent's read function
   */
  Superclass::Read(f, info);

  /**
   * read and set first coordinate
   */
  SkipWhiteSpace(f); f>>d; if(!f) goto out;
  X=d;

  /**
   * ead and set second coordinate
   */
  SkipWhiteSpace(f); f>>d; if(!f) goto out;
  Y=d;

  /**
   * read and set third coordinate
   */
  SkipWhiteSpace(f); f>>d; if(!f) goto out;
  Z=d;


out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"NodeXYZ::Read()","Error reading FEM node!");
  }

}

/**
 * writes the NodeXYZ to the output stream
 */
void NodeXYZ::Write( std::ostream& f ) const
{
  //std::cout << "TS: Writing node" << std::endl;

  /**
   * first call the parent's write function
   */
  Superclass::Write(f);

  /**
   * then the actual data (node, and properties numbers)
   */
  f<<"\t"<<this->X<<"\t% X"<<"\n";
  f<<"\t"<<this->Y<<"\t% Y"<<"\n";
  f<<"\t"<<this->Z<<"\t% Z"<<"\n";

  /** check for errors */
  if (!f)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"NodeXYZ::Write()","Error writing FEM node!");
  }

}




/**
 * Windows visualization
 */
#ifdef FEM_BUILD_VISUALIZATION
  /**
   * Draws the node on DC.
   * FIXME - implement the function to render in 3-space
   */
  void NodeXYZ::Draw(CDC* pDC, Solution::ConstPointer sol) const 
  {
  }
#endif

FEM_CLASS_REGISTER(NodeXYZ)




}} // end namespace itk::fem
