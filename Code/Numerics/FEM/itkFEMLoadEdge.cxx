/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadEdge.cxx
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

#include "itkFEMLoadEdge.h"
#include "itkFEMUtility.h"

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
void LoadEdge::Write( std::ostream& f, int ofid ) const 
{

  /** if not set already, se set the ofid */
  if (ofid<0) ofid=OFID;

  /** first call the parent's write function */
  Superclass::Write(f,ofid);

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
