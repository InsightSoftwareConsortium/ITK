/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLightObject.cxx
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

/** disable debug warnings in MS compiler */
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMLightObject.h"
#include "itkFEMUtility.h"

namespace itk {
namespace fem {




/**
 * Here we just read the global number from the stream.
 * This should be the first function called when reading object data.
 */
void FEMLightObject::Read( std::istream& f, void* info )
{
  int n;

  /** Read and set the global object number */
  SkipWhiteSpace(f); f>>n; if(!f) { goto out; }
  this->GN=n;

out:

  if( !f ) { throw std::runtime_error("Error reading FEM object!"); }

}




/** 
 * Here we just write the class name and GN.
 * This should be the first function called when writing object data, so
 * every derived class should first call the parent's write function.
 * Each derived class should also set the ofid to correct value only if
 * ofid  is <0.
 * This way the Write function in base (this one) class knows which class is
 * being written and can write the class name properly.
 */
void FEMLightObject::Write( std::ostream& f, int ofid ) const
{

  /** check if somebody has defined the ofid */
  if (ofid<0) {
    /**
     * Nope... This means that either no Write function is defined for
     * the derived class, or somebody was trying to write an abstract
     * class. We should start yelling here...
     */
    return;
  }

  /**  first write the class name */
  f<<'<'<<FEMObjectFactory<Self>::ID2ClassName(ofid)<<">\n";

  /** then the global object number */
  f<<"\t"<<GN<<"\t% Global object number\n";

  /** check for errors */
  if (!f) { throw std::runtime_error("Error writing FEM object!"); }

}




}} // end namespace itk::fem
