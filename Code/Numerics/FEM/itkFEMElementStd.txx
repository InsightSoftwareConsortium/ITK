/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementStd.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElementStd_txx
#define __itkFEMElementStd_txx

#include "itkFEMUtility.h"

namespace itk {
namespace fem {




template<unsigned int VNumberOfPoints, unsigned int VNumberOfDegreesOfFreedomPerNode, unsigned int VNumberOfSpatialDimensions, class TBaseClass>
ElementStd<VNumberOfPoints, VNumberOfDegreesOfFreedomPerNode, VNumberOfSpatialDimensions, TBaseClass>
::ElementStd()
{
  // Set all node ids to 0 (undefined).
  for(int i=0; i<NumberOfNodes; i++)
  {
    this->m_node[i]=0;
  }
}


template<unsigned int VNumberOfPoints, unsigned int VNumberOfDegreesOfFreedomPerNode, unsigned int VNumberOfSpatialDimensions, class TBaseClass>
void
ElementStd<VNumberOfPoints, VNumberOfDegreesOfFreedomPerNode, VNumberOfSpatialDimensions, TBaseClass>
::Read( std::istream& f, void* info )
{
  int n;
  // Convert the info pointer to a usable object
  itk::fem::Node::ArrayType::Pointer nodes=static_cast<ReadInfoType*>(info)->m_node;

  // First call the parent's read function
  Superclass::Read(f,info);

  try
  {
    // Read and set each of the expected global node numbers
    for(unsigned int p=0; p<NumberOfNodes; p++)
    {
      SkipWhiteSpace(f); f>>n; if(!f) goto out;
      m_node[p]=dynamic_cast<const ElementNew::Node*>( &*nodes->Find(n));
    }

  }
  catch ( FEMExceptionObjectNotFound e )
  {
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"ElementStd::Read()",e.m_baseClassName,e.m_GN);
  }


out:

  if( !f )
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"ElementStd::Read()","Error reading FEM element!");
  }

}


template<unsigned int VNumberOfPoints, unsigned int VNumberOfDegreesOfFreedomPerNode, unsigned int VNumberOfSpatialDimensions, class TBaseClass>
void
ElementStd<VNumberOfPoints, VNumberOfDegreesOfFreedomPerNode, VNumberOfSpatialDimensions, TBaseClass>
::Write( std::ostream& f, int ofid ) const
{

  // ElementStd cannot be the most derived class, so
  // if ofid was not set already, we throw an exception.
  if (ofid<0)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"ElementStd::Write()","Error writing FEM element!. Parameter ofid was not set!");
  }

  // First call the parent's write function
  Superclass::Write(f,ofid);

  // ... then write the actual data (node ids)
  // We also add some comments in the output file
  for(unsigned int p=0; p<NumberOfNodes; p++)
  {
    f<<"\t"<<m_node[p]->GN<<"\t% Node #"<<(p+1)<<" ID\n";
  }

  // check for errors
  if (!f)
  { 
    throw FEMExceptionIO(__FILE__,__LINE__,"ElementStd::Write()","Error writing FEM element!");
  }

}




#ifdef _MSC_VER
// Declare a static dummy function to prevent a MSVC 6.0 SP5 from crashing.
// I have no idea why things don't work when this is not declared, but it
// looks like this declaration makes compiler forget about some of the
// troubles it has with templates.
static void Dummy( void );
#endif // #ifdef _MSC_VER

}} // end namespace itk::fem

#endif // #ifndef __itkFEMElementStd_txx
