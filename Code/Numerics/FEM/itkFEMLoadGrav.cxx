/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadGrav.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMLoadGrav.h"
#include "itkFEMUtility.h"

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
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  Fg_value.resize(n);
  /** then the actual values */
  SkipWhiteSpace(f); f>>Fg_value; if(!f) goto out;
  
out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadGravConst::Read()","Error reading FEM load!");
  }

}




/**
 * Write the LoadGravConst to the output stream
 */
void LoadGravConst::Write( std::ostream& f, int ofid ) const {

  /** if not set already, se set the ofid */
  if (ofid<0) ofid=OFID;

  /** first call the parent's write function */
  LoadGrav::Write(f,ofid);

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
