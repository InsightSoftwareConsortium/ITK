/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadBCMFC.cxx
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

#include "itkFEMLoadBCMFC.h"
#include "itkFEMUtility.h"

namespace itk {
namespace fem {




/**
 * Fix a DOF to a prescribed value
 */
LoadBCMFC::LoadBCMFC(Node::ConstPointer node, int dof, vnl_vector<Node::Float> val)
{
  lhs.clear();

  /** Set the correct weight */
  lhs.push_back( MFCTerm(node, dof, 1.0) );
  rhs=val;
}




/** Read the LoadBCMFC object from input stream */
void LoadBCMFC::Read( std::istream& f, void* info )
{
  int nlhs, n;
  Node::Float d;
  /**
   * Convert the info pointer to a usable objects
   */
  Node::ArrayType::ConstPointer nodes=static_cast<ReadInfoType*>(info)->m_node;


  /** first call the parent's Read function */
  Superclass::Read(f,info);

  /** read number of terms in lhs of MFC equation */
  SkipWhiteSpace(f); f>>nlhs; if(!f) goto out;
  
  lhs.clear();
  for(int i=0; i<nlhs; i++) 
  {
    /** read and set pointer to node that we're applying the load to */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;
    Node::ConstPointer node;
    if ( !(node=dynamic_cast<const Node*>( &*nodes->Find(n)) ) )
      throw std::runtime_error("Global node number not found!");

    /** read the number of dof within that node */
    SkipWhiteSpace(f); f>>n; if(!f) goto out;

    /** read weight */
    SkipWhiteSpace(f); f>>d; if(!f) goto out;

    /** add a new MFCTerm to the lhs */
    lhs.push_back( MFCTerm(node, n, d) );
  }

  /** read the rhs */
  SkipWhiteSpace(f); f>>n; if(!f) goto out;
  rhs.resize(n);
  SkipWhiteSpace(f); f>>rhs; if(!f) goto out;

out:

  if( !f ) throw std::runtime_error("Error reading load!");

}


/**
 * Write the LoadBCMFC object to the output stream
 */
void LoadBCMFC::Write( std::ostream& f, int ofid ) const 
{

  /** if not set already, se set the ofid */
  if (ofid<0) ofid=OFID;

  /** first call the parent's write function */
  Superclass::Write(f,ofid);

  /**
   * Write the actual Load data
   */

  /** write the number of DOFs affected by this MFC */
  f<<"\t"<<lhs.size()<<"\t% Number of DOFs in this MFC"<<"\n";

  /** write each term */
  f<<"\t  %==>\n";
  for(LhsType::const_iterator q=lhs.begin(); q!=lhs.end(); q++) 
  {
    f<<"\t  "<<q->node->GN<<"\t% GN of node"<<"\n";
    f<<"\t  "<<q->dof<<"\t% DOF# in node"<<"\n";
    f<<"\t  "<<q->value<<"\t% weight"<<"\n";
    f<<"\t  %==>\n";
  }

  /** write the rhs */
  f<<"\t"<<rhs.size();
  f<<" "<<rhs<<"\t% rhs of MFC"<<"\n";

  /** check for errors */
  if (!f) { throw std::runtime_error("Error writing load!"); }

}

FEM_CLASS_REGISTER(LoadBCMFC)




}} // end namespace itk::fem
