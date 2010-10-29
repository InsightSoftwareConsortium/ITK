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
// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMLoadNode.h"

namespace itk {
namespace fem {

/**
 * Read the LoadNode object from input stream
 */
void LoadNode::Read( std::istream& f, void* info )
{
  int n;
  /**
   * Convert the info pointer to a usable objects
   */
  ReadInfoType::ElementArrayPointer elements=static_cast<ReadInfoType*>(info)->m_el;


  /** first call the parent's read function */
  Superclass::Read(f,info);

  /** read and set pointer to node that we're applying the load to */
  this->SkipWhiteSpace(f); f>>n; if(!f) goto out;
  try
    {
    this->m_element=dynamic_cast<const Element*>( &*elements->Find(n));
    }
  catch ( FEMExceptionObjectNotFound e )
    {
    throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"LoadNode::Read()",e.m_baseClassName,e.m_GN);
    }

  /* read and set the point number */
  this->SkipWhiteSpace(f); f>>this->m_pt; if(!f) goto out;


  /** read and set the number of elements inside a force vector */
  this->SkipWhiteSpace(f); f>>n; if(!f) goto out;
  this->F.set_size(n);

  /** read the force vector itself */
  this->SkipWhiteSpace(f); f>>this->F; if(!f) goto out;

out:

  if( !f )
    {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadNode::Read()","Error reading FEM load!");
    }

}

/**
 * Write the LoadNode to the output stream
 */
void LoadNode::Write( std::ostream& f ) const {

  /** first call the parent's write function */
  Superclass::Write(f);

  /** write the actual Load data */
  f<<"\t"<<this->m_element->GN<<"\t% GN of element on which the load acts"<<"\n";
  f<<"\t"<<this->m_pt<<" "<<"\t% Point number within the element\n";
  f<<"\t"<<this->F.size()<<" "<<this->F<<"\t% Force vector (first number is the size of a vector)\n";

  /** check for errors */
  if (!f)
    {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadNode::Write()","Error writing FEM load!");
    }

}

FEM_CLASS_REGISTER(LoadNode)

}} // end namespace itk::fem
