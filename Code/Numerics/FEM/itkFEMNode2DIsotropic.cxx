/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMNode2DIsotropic.cxx
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
