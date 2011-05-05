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
#ifndef __itkFEMElementStd_h
#define __itkFEMElementStd_h

#include "itkFEMElementBase.h"

namespace itk {
namespace fem {

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
 * You must specify three or four template parameters:
 *
 *   VNumberOfNodes - Number of nodes that define the element
 *                    (e.g. four for quadrilateral)
 *
 *   VNumberOfSpatialDimensions - Number of dimensions of space in which the
 *                    element is defined. This is also the size of a vector
 *                    returned by GetNodeCoordinates() member funtion.
 *
 *   TBaseClass - Class from which ElementStd is derived. TBaseClass must
 *                be derived from the Element base class. This enables you
 *                to use this class at any level of element definition.
 *                If not specified, it defaults to the Element class.
 * \ingroup ITK-FEM
 */
template<unsigned int VNumberOfNodes, unsigned int VNumberOfSpatialDimensions, class TBaseClass=Element>
class ElementStd : public TBaseClass
{
FEM_ABSTRACT_CLASS(ElementStd,TBaseClass)
public:

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
  enum{ InvalidDegreeOfFreedomID = Superclass::InvalidDegreeOfFreedomID };

  /**
   * Number of nodes that define the element.
   */
  enum { NumberOfNodes=VNumberOfNodes };

  /**
   * Number of dimensions of space in which element can exist.
   */
  enum { NumberOfSpatialDimensions=VNumberOfSpatialDimensions };

  /**
   * Default constructor just clears the ivars
   */
  ElementStd();

  //////////////////////////////////////////////////////////////////////////
  /**
   * Methods that define the geometry of an element
   */
  virtual unsigned int GetNumberOfNodes( void ) const
    { return NumberOfNodes; }

  virtual NodeIDType GetNode(unsigned int n) const
    {
    if(n>=NumberOfNodes)
      {
      return 0;
      }
    return this->m_node[n];
    }

  virtual void SetNode(unsigned int n, NodeIDType node)
    {
    if(n>=NumberOfNodes)
      {
      return;
      }
    this->m_node[n]=node;
    }

  virtual const VectorType& GetNodeCoordinates( unsigned int n ) const
    {
    return m_node[n]->GetCoordinates();
    }

  virtual unsigned int GetNumberOfSpatialDimensions() const
    {
    return NumberOfSpatialDimensions;
    }

  //////////////////////////////////////////////////////////////////////////
  /**
   * Methods related to I/O
   */

  /**
   * Read data for this class from input stream
   */
  virtual void Read( std::istream&, void* info );

  /**
   * Write data for this class to output stream
   */
  virtual void Write( std::ostream& f ) const;

protected:

  /**
   * Array of pointers to point objects that define the element
   */
  NodeIDType m_node[NumberOfNodes];

};

#ifdef _MSC_VER
// Declare a static dummy function to prevent a MSVC 6.0 SP5 from crashing.
// I have no idea why things don't work when this is not declared, but it
// looks like this declaration makes compiler forget about some of the
// troubles it has with templates.
static void Dummy( void );
#endif // #ifdef _MSC_VER

}} // end namespace itk::fem

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFEMElementStd.txx"
#endif

#endif // #ifndef __itkFEMElementStd_h
