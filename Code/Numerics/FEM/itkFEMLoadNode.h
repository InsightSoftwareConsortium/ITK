/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadNode.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMLoadNode_h
#define __itkFEMLoadNode_h

#include "itkFEMLoadBase.h"
#include "vnl/vnl_vector.h"

namespace itk {
namespace fem {




/**
 * \class LoadNode
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
