/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

#include <utility>

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
  /** Standard class type aliases. */
  using Self = LoadNode;
  using Superclass = Load;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoadNode, Load);

  using Float = Element::Node::Float;

  /**
   * Set the force acting at the node
   */
  void
  SetForce(const vnl_vector<Float> force);

  /**
   * Get the force acting at the node
   */
  vnl_vector<Float>
  GetForce() const;

  /**
   * Set the node number on which the load is being applied.
   */
  void
  SetNode(int num);

  /**
   * Get the node number on which the load is being applied.
   */
  int
  GetNode() const;

  LoadNode() = default;

  LoadNode(Element::ConstPointer element_, unsigned int pt_, vnl_vector<Float> F_)
    : m_Point(pt_)
    , m_Force(std::move(F_))
  {
    this->m_Element = element_;
  }

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  itk::LightObject::Pointer
  CreateAnother() const override;

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /**
   * Point within the element on which the force acts.
   */
  unsigned int m_Point{ 0 };

  /**
   * Force applied on the node. Dimension of F should equal
   * element->GetNumberOfDegreesOfFreedomPerNode().
   */
  vnl_vector<Float> m_Force;
};
} // end namespace fem
} // end namespace itk

#endif // itkFEMLoadNode_h
