/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadEdge.cxx
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

#include "itkFEMLoadEdge.h"

namespace itk {
namespace fem {




/** 
 * Read the Load object from input stream
 */
void LoadEdge::Read( std::istream& f, void* info )
{
  int n,m;

  /** first call the parent's read function */
  Superclass::Read(f,info);

  /** ... edge number */
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  m_Edge=n;
  /** ... # of rows */
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  /** ... # of cols */
  SkipWhiteSpace(f); f>>m; if(!f) goto out;
  m_Force.resize(n,m);
  for(int i=0; i<n; i++)
  {
    SkipWhiteSpace(f);
    for(int j=0; j<m; j++)
    {
      f>>m_Force[i][j];
    }
    SkipWhiteSpace(f);
  }


out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadEdge::Read()","Error reading FEM load!");
  }

}



/**
 * Write the Load object to the output stream
 */
void LoadEdge::Write( std::ostream& f ) const 
{
  /** first call the parent's write function */
  Superclass::Write(f);

  /** Write the actual Load data */

  /** ... edge number */
  f<<"\t"<<m_Edge<<"\t% Edge number"<<"\n";

  /** ... force matrix */
  f<<"\t"<<m_Force.rows()<<"\t% # rows in force matrix"<<"\n";
  f<<"\t"<<m_Force.cols()<<"\t% # cols in force matrix"<<"\n";
  f<<"\t% force matrix\n";
  for(int i=0; i<(int)m_Force.rows(); i++)
  {
    f<<"\t";
    for(int j=0; j<(int)m_Force.cols(); j++)
    {
      f<<m_Force[i][j]<<" ";
    }
    f<<"\n";
  }

  /** check for errors */
  if (!f)
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadBCMFC::Write()","Error writing FEM load!");
  }

}

FEM_CLASS_REGISTER(LoadEdge)




}} // end namespace itk::fem
