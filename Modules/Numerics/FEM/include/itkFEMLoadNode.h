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
 * \ingroup ITK-FEM
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
  virtual void Write( std::ostream& f ) const;

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

  LoadNode() : m_element(0), m_pt(0) {}  // default constructor
  LoadNode( Element::ConstPointer element_, unsigned int pt_, vnl_vector<Float> F_ ) :
    m_element(element_), m_pt(pt_), F(F_) {}

};

FEM_CLASS_INIT(LoadNode)

}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadDOF_h
