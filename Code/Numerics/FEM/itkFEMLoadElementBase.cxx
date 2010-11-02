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
  this->SkipWhiteSpace(f); f>>n; if(!f) goto out;
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
    for(int i=0;i<n;i++)
      {
      int m;
      this->SkipWhiteSpace(f); f>>m; if(!f) goto out;
      Element::ConstPointer e;
      try
        {
        e=elements->Find(m);
        }
      catch ( FEMExceptionObjectNotFound exc )
        {
        throw FEMExceptionObjectNotFound(__FILE__,__LINE__,"LoadElementBase::Read()",exc.m_baseClassName,exc.m_GN);
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
    for(ElementPointersVectorType::const_iterator i=el.begin(); i != el.end(); i++)
      {
      f<<((*i)->GN)<<" ";
      }
    f << "\t% GNs of elements" << std::endl;
    }
  else
    {
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
