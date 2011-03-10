/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkFEMElementStd_txx
#define __itkFEMElementStd_txx

#include "itkFEMElementStd.h"

namespace itk {
namespace fem {

template<unsigned int VNumberOfPoints, unsigned int VNumberOfSpatialDimensions, class TBaseClass>
ElementStd<VNumberOfPoints, VNumberOfSpatialDimensions, TBaseClass>
::ElementStd()
{
  // Set all node ids to 0 (undefined).
  for(int i=0; i<NumberOfNodes; i++)
    {
    this->m_node[i]=0;
    }
}


template<unsigned int VNumberOfPoints, unsigned int VNumberOfSpatialDimensions, class TBaseClass>
void
ElementStd<VNumberOfPoints, VNumberOfSpatialDimensions, TBaseClass>
::Read( std::istream& f, void* info )
{
  int n;
  // Convert the info pointer to a usable object
  ReadInfoType::NodeArrayPointer nodes=static_cast<ReadInfoType*>(info)->m_node;

  // First call the parent's read function
  Superclass::Read(f,info);

  try
    {
    // Read and set each of the expected global node numbers
    for(unsigned int p=0; p<NumberOfNodes; p++)
      {
      this->SkipWhiteSpace(f); f>>n; if(!f) goto out;
      m_node[p]=dynamic_cast<const Element::Node*>( &*nodes->Find(n));
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


template<unsigned int VNumberOfPoints, unsigned int VNumberOfSpatialDimensions, class TBaseClass>
void
ElementStd<VNumberOfPoints, VNumberOfSpatialDimensions, TBaseClass>
::Write( std::ostream& f ) const
{
  // First call the parent's write function
  Superclass::Write(f);

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
