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
 * \brief This load is applied on a specific point within the system.
 *
 * The point is defined as a point within an element object.
 *
 * You must provide a pointer to an element object and a number
 * of point on which on which the load acts. Force vector F should have
 * element->GetNumberOfDegreesOfFreedomPerNode() dimensions.
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
  virtual void Write( std::ostream& f, int clid ) const;

  /**
   * Pointer to an element in a system that contains the DOF
   * on which the external force is applied.
   */
  Element::ConstPointer m_element;

  /**
   * Point within the element on which the force acts.
   */
  unsigned int m_pt;

  /**
   * Force applied on the node. Dimension of F should equal
   * element->GetNumberOfDegreesOfFreedomPerNode().
   */
  vnl_vector<Float> F;

  LoadNode() : m_element(0) {}  // default constructor
  LoadNode( Element::ConstPointer element_, unsigned int pt_, vnl_vector<Float> F_ ) :
    m_element(element_), m_pt(pt_), F(F_) {}

};

FEM_CLASS_INIT(LoadNode)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadDOF_h
