/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkFEMElementStd_h
#define itkFEMElementStd_h

#include "itkFEMElementBase.h"

namespace itk
{
namespace fem
{
/**
 * \class ElementStd
 * \brief Implements standard node management in the element classes.
 *
 * This is a templated helper class that automatically defines some of
 * the virtual functions in elements. It is used to avoid code duplication.
 *
 * If a derived element class has DOFs associated only with points
 * that define the geometry of the element, you can derive from this
 * class to automatically create all the functions required for proper
 * node management.
 *
 * You must specify two or three template parameters:
 *
 *   VNumberOfNodes - Number of nodes that define the element
 *                    (e.g. four for quadrilateral)
 *
 *   VNumberOfSpatialDimensions - Number of dimensions of space in which the
 *                    element is defined. This is also the size of a vector
 *                    returned by GetNodeCoordinates() member function.
 *
 *   TBaseClass - Class from which ElementStd is derived. TBaseClass must
 *                be derived from the Element base class. This enables you
 *                to use this class at any level of element definition.
 *                If not specified, it defaults to the Element class.
 * \ingroup ITKFEM
 */
template <unsigned int VNumberOfNodes, unsigned int VNumberOfSpatialDimensions, typename TBaseClass = Element>
class ITK_TEMPLATE_EXPORT ElementStd : public TBaseClass
{
public:
  /** Standard class type aliases. */
  using Self = ElementStd;
  using Superclass = TBaseClass;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ElementStd, TBaseClass);

  // FIXME: Add concept cheking for TBaseClass, and TPointClass

  // Repeat type alias and enums from parent class

  using Float = typename Superclass::Float;
  using MatrixType = typename Superclass::MatrixType;
  using VectorType = typename Superclass::VectorType;
  using LoadType = typename Superclass::LoadType;
  using LoadPointer = typename Superclass::LoadPointer;
  using NodeIDType = typename Superclass::NodeIDType;
  using DegreeOfFreedomIDType = typename Superclass::DegreeOfFreedomIDType;
  using Node = typename Superclass::Node;
  enum
  {
    InvalidDegreeOfFreedomID = Superclass::InvalidDegreeOfFreedomID
  };

  /** Number of nodes that define the element. */
  enum
  {
    NumberOfNodes = VNumberOfNodes
  };

  /** Number of dimensions of space in which element can exist. */
  enum
  {
    NumberOfSpatialDimensions = VNumberOfSpatialDimensions
  };

  /** Default constructor. Just clears the ivars. */
  ElementStd();

  /** Methods that define the geometry of an element. */
  unsigned int
  GetNumberOfNodes() const override
  {
    return NumberOfNodes;
  }

  /** Get/Set the Nodes that define the element. */
  NodeIDType
  GetNode(unsigned int n) const override
  {
    if (n >= NumberOfNodes)
    {
      return nullptr;
    }
    return this->m_node[n];
  }

  void
  SetNode(unsigned int n, NodeIDType node) override
  {
    this->SetNodeInternal(n, node);
  }
  void
  SetNode(unsigned int n, typename Superclass::Node::Pointer node) override
  {
    this->SetNodeInternal(n, node);
  }

  /** Get the nodal coordinates. */
  const VectorType &
  GetNodeCoordinates(unsigned int n) const override
  {
    return m_node[n]->GetCoordinates();
  }

  /** Get the number of spatial dimensions. */
  unsigned int
  GetNumberOfSpatialDimensions() const override
  {
    return NumberOfSpatialDimensions;
  }


protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  virtual void
  SetNodeInternal(unsigned int n, const Node * node)
  {
    if (n >= NumberOfNodes)
    {
      return;
    }
    this->m_node[n] = node;
  }

  // Array of pointers to point objects that define the element
  const Node * m_node[NumberOfNodes];
};

} // end namespace fem
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFEMElementStd.hxx"
#endif

#endif // itkFEMElementStd_h
