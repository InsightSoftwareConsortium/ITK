/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadNode.h
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

#ifndef __itkFEMLoadNode_h
#define __itkFEMLoadNode_h

#include "itkFEMLoadBase.h"
#include "vnl/vnl_vector.h"

namespace itk {
namespace fem {




/**
 * \brief This load is applied on a specific node within the system.
 *
 * Since the load does not depend on the element, we provide the pointer to a node
 * on which the load acts. Force vector F should have node->N() dimensions.
 */
class LoadNode : public Load {
FEM_CLASS(LoadNode,Load)
public:

  typedef Node::Float Float;

  /**
   * Read a LoadNode object from input stream.
   */
  virtual void Read( std::istream& f, void* info );
  /**
   * Write a Load object to the output stream
   */
  virtual void Write( std::ostream& f, int ofid ) const;

  /**
   * Pointer to a node in a system that contains the displacement
   * on which the external force is applied
   */
  Node::ConstPointer node;

  /**
   * Force applied on the node. Dimension of F should equal node->N()
   */
  vnl_vector<Float> F;

  LoadNode() : node(0) {}  // default constructor
  LoadNode( Node::ConstPointer node_, vnl_vector<Float> F_ ) :
    node(node_), F(F_) {}

};

FEM_CLASS_INIT(LoadNode)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadDOF_h
