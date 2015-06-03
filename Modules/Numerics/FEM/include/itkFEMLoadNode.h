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

#ifndef itkFEMLoadNode_h
#define itkFEMLoadNode_h

#include "itkFEMLoadBase.h"
#include "ITKFEMExport.h"
#include "vnl/vnl_vector.h"

namespace itk
{
namespace fem
{
/**
 * \class LoadNode
 * \brief This load is applied on a specific point within the system.
 *
 * The point is defined as a point within an element object.
 *
 * You must provide a pointer to an element object and a number
 * of point on which on which the load acts. Force vector F should have
 * element->GetNumberOfDegreesOfFreedomPerNode() dimensions.
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT LoadNode : public Load
{
public:
  /** Standard class typedefs. */
  typedef LoadNode                 Self;
  typedef Load                     Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** New macro for creation of through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoadNode, Load);

  typedef Element::Node::Float Float;

  /**
    * Set the force acting at the node
    */
  void SetForce(const vnl_vector<Float> force);

  /**
  * Get the force acting at the node
  */
  vnl_vector<Float> GetForce() const;

  /**
   * Set the node number on which the load is being applied.
   */
  void SetNode(int num);

  /**
   * Get the node number on which the load is being applied.
   */
  int GetNode() const;

  LoadNode():
    m_Point(0)
  {
  }                             // default constructor
  LoadNode(Element::ConstPointer element_, unsigned int pt_, vnl_vector<Float> F_) :
    m_Point(pt_), m_Force(F_)
  {
    this->m_Element = element_;
  }

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  virtual::itk::LightObject::Pointer CreateAnother(void) const ITK_OVERRIDE;

protected:

  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  /**
   * Point within the element on which the force acts.
   */
  unsigned int m_Point;

  /**
   * Force applied on the node. Dimension of F should equal
   * element->GetNumberOfDegreesOfFreedomPerNode().
   */
  vnl_vector<Float> m_Force;
};

}
}  // end namespace itk::fem

#endif // #ifndef itkFEMLoadDOF_h
