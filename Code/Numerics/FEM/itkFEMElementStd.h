/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementStd.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElementStd_h
#define __itkFEMElementStd_h

#include "itkFEMElementNewBase.h"

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
 *   VNumberOfDegreesOfFreedomPerNode - Number of degrees of freedom at each
 *                    node. This is also equal to the number of unknown
 *                    functions that must be solved for at every point within
 *                    an element. Typically this number is only known, when
 *                    implementing the physical portion of the element's
 *                    functions. Therefore the abstract geometrical elements
 *                    are normally templated over this number.
 *
 *   TBaseClass - Class from which ElementStd is derived. TBaseClass must
 *                be derived from the Element base class. This enables you
 *                to use this class at any level of element definition.
 *                If not specified, it defaults to the Element class.
 */
template<unsigned int VNumberOfNodes, unsigned int VNumberOfDegreesOfFreedomPerNode, unsigned int VNumberOfSpatialDimensions, class TBaseClass=ElementNew>
class ElementStd : public TBaseClass
{
FEM_ABSTRACT_CLASS(ElementStd,TBaseClass)
public:

// FIXME: Add concept cheking for TBaseClass, and TPointClass

  // Repeat typedefs and enums from parent class
  typedef typename Superclass::Float Float;
  typedef typename Superclass::MatrixType MatrixType;
  typedef typename Superclass::VectorType VectorType;
  typedef typename Superclass::LoadElementType LoadElementType;
  typedef typename Superclass::LoadElementPointer LoadElementPointer;
  typedef typename Superclass::NodeIDType NodeIDType;
  typedef typename Superclass::DegreeOfFreedomIDType DegreeOfFreedomIDType;
  typedef typename Superclass::ReadInfoType ReadInfoType;
  typedef typename Superclass::Node Node;
  enum{ InvalidDegreeOfFreedomID = Superclass::InvalidDegreeOfFreedomID };

  /**
   * Number of nodes that define the element.
   */
  enum { NumberOfNodes=VNumberOfNodes };

  /**
   * Number of unknown variables that exist at every point
   * within an element. Also equal to number of degrees of
   * freedon at each node.
   */
  enum { NumberOfDegreesOfFreedomPerNode=VNumberOfDegreesOfFreedomPerNode };

  /**
   * Total number of degrees of freedom in an element
   */
  enum { NDOF=NumberOfNodes*NumberOfDegreesOfFreedomPerNode };

  /**
   * Number of dimensions of space in which element can exist.
   */
  enum { NumberOfSpatialDimensions=VNumberOfSpatialDimensions };




  /**
   * Default constructor just clears the ivars
   */
  ElementStd();



//////////////////////////////////////////////////////////////////////////
  /*
   * Methods that define the geometry of an element
   */
  virtual unsigned int GetNumberOfNodes( void ) const
  { return NumberOfNodes; }

  virtual unsigned int GetNumberOfDegreesOfFreedomPerNode( void ) const
  { return NumberOfDegreesOfFreedomPerNode; }

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
    if(n>=NumberOfNodes) { return; }
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
  /*
   * Methods related to I/O
   */

  /**
   * Read data for this class from input stream
   */
  virtual void Read( std::istream&, void* info );

  /**
   * Write data for this class to output stream
   */
  virtual void Write( std::ostream& f, int clid ) const;




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
