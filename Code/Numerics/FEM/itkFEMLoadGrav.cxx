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

#include "itkFEMLoadGrav.h"

namespace itk {
namespace fem {

/**
 * Read the LoadGravConst object from input stream
 */
void LoadGravConst::Read( std::istream& f, void* info )
{
  int n;

  /** first call the parent's read function */
  LoadGrav::Read(f,info);

  /**
   * Read and set the force vector
   */

  /** first read and set the size of the vector */
  this->SkipWhiteSpace(f); f>>n; if(!f) goto out;
  Fg_value.set_size(n);
  /** then the actual values */
  this->SkipWhiteSpace(f); f>>Fg_value; if(!f) goto out;

out:

  if( !f )
    {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadGravConst::Read()","Error reading FEM load!");
    }

}

/**
 * Write the LoadGravConst to the output stream
 */
void LoadGravConst::Write( std::ostream& f ) const
{
  /** first call the parent's write function */
  LoadGrav::Write(f);

  /** then write the actual data force vector */
  f<<"\t"<<Fg_value.size()<<"\t% Size of the gravity force vector\n";
  f<<"\t"<<Fg_value<<"\t% Gravity force vector\n";

  /** check for errors */
  if (!f)
    {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadGravConst::Write()","Error writing FEM load!");
    }
}

FEM_CLASS_REGISTER(LoadGravConst)

}} // end namespace itk::fem
