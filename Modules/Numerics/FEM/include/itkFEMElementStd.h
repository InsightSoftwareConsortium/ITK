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
template< unsigned int VNumberOfNodes, unsigned int VNumberOfSpatialDimensions, typename TBaseClass = Element >
class ITK_TEMPLATE_EXPORT ElementStd : public TBaseClass
{
public:
  /** Standard class typedefs. */
  typedef ElementStd               Self;
  typedef TBaseClass               Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ElementStd, TBaseClass);

// FIXME: Add concept cheking for TBaseClass, and TPointClass

  // Repeat typedefs and enums from parent class

  typedef typename Superclass::Float                 Float;
  typedef typename Superclass::MatrixType            MatrixType;
  typedef typename Superclass::VectorType            VectorType;
  typedef typename Superclass::LoadType              LoadType;
  typedef typename Superclass::LoadPointer           LoadPointer;
  typedef typename Superclass::NodeIDType            NodeIDType;
  typedef typename Superclass::DegreeOfFreedomIDType DegreeOfFreedomIDType;
  typedef typename Superclass::Node                  Node;
  enum { InvalidDegreeOfFreedomID = Superclass::InvalidDegreeOfFreedomID };

  /** Number of nodes that define the element. */
  enum { NumberOfNodes = VNumberOfNodes };

  /** Number of dimensions of space in which element can exist. */
  enum { NumberOfSpatialDimensions = VNumberOfSpatialDimensions };

  /** Default constructor. Just clears the ivars. */
  ElementStd();

  /** Methods that define the geometry of an element. */
  virtual unsigned int GetNumberOfNodes(void) const ITK_OVERRIDE
  {
    return NumberOfNodes;
  }

  /** Get/Set the Nodes that define the element. */
  virtual NodeIDType GetNode(unsigned int n) const ITK_OVERRIDE
  {
    if( n >= NumberOfNodes )
      {
      return ITK_NULLPTR;
      }
    return this->m_node[n];
  }

  virtual void SetNode(unsigned int n, NodeIDType node) ITK_OVERRIDE
  {
    this->SetNodeInternal(n,node);
  }
  virtual void SetNode(unsigned int n, typename Superclass::Node::Pointer node) ITK_OVERRIDE
  {
    this->SetNodeInternal(n,node);
  }

  /** Get the nodal coordinates. */
  virtual const VectorType & GetNodeCoordinates(unsigned int n) const ITK_OVERRIDE
  {
    return m_node[n]->GetCoordinates();
  }

  /** Get the number of spatial dimensions. */
  virtual unsigned int GetNumberOfSpatialDimensions() const ITK_OVERRIDE
  {
    return NumberOfSpatialDimensions;
  }


protected:

  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  virtual void SetNodeInternal(unsigned int n, const Node *node)
  {
    if( n >= NumberOfNodes )
      {
      return;
      }
    this->m_node[n] = node;
  }

  // Array of pointers to point objects that define the element
  const Node *m_node[NumberOfNodes];
};

}  // end namespace fem
}  // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFEMElementStd.hxx"
#endif

#endif // #ifndef itkFEMElementStd_h
