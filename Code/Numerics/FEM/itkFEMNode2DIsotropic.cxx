/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMNode2DIsotropic.cxx
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

#include "itkFEMNode2DIsotropic.h"
#include "itkFEMUtility.h"

namespace itk {
namespace fem {




/**
 * Read the Node2DIsotropic from input stream
 */
void Node2DIsotropic::Read(  std::istream& f, void* info )
{
  Float d;

  /** first call the parent's read function */
  Superclass::Read(f, info);

  /** read and set first coordinate */
  SkipWhiteSpace(f); f>>d; if(!f) goto out;
  X=d;

  /** read and set second coordinate */
  SkipWhiteSpace(f); f>>d; if(!f) goto out;
  Y=d;

out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"Node2DIsotropic::Read()","Error reading FEM node!");
  }

}




/**
 * Writes the Node2DIsotropic to the output stream
 */
void Node2DIsotropic::Write( std::ostream& f, int ofid ) const {

  /**
   * if not set already, se set the ofid
   */
  if (ofid<0) 
    {
    ofid=OFID;
    }

  /**
   * first call the parent's write function
   */
  Superclass::Write(f,ofid);

  /** Then the actual data (node, and properties numbers) */
  f<<"\t"<<X<<"\t% X"<<"\n";
  f<<"\t"<<Y<<"\t% Y"<<"\n";

  /** check for errors */
  if (!f)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"Node2DIsotropic::Write()","Error writing FEM node!");
  }

}

FEM_CLASS_REGISTER(Node2DIsotropic)




}} // end namespace itk::fem
