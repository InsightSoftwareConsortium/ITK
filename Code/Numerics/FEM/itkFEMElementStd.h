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
 *   VNumberOfPoints - Number of points that define the element
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
 *   TPointClass - Class of Point objects that the element uses. TPointClass
 *                 must be derived from the Point (old Node) base class.
 *
 *   TBaseClass - Class from which ElementStd is derived. TBaseClass must
 *                be derived from the Element base class. This enables you
 *                to use this class at any level of element definition.
 *                If not specified, it defaults to the Element class.
 */
template<unsigned int VNumberOfPoints, unsigned int VNumberOfDegreesOfFreedomPerNode, class TPointClass, class TBaseClass=ElementNew>
class ElementStd : public TBaseClass
{
FEM_CLASS_SP(ElementStd,TBaseClass)
public:

// FIXME: Add concept cheking for TBaseClass, and TPointClass

  // Repeat typedefs and enums from parent class
  typedef typename Superclass::Float Float;
  typedef typename Superclass::MatrixType MatrixType;
  typedef typename Superclass::VectorType VectorType;
  typedef typename Superclass::LoadElementType LoadElementType;
  typedef typename Superclass::LoadElementPointer LoadElementPointer;
  typedef typename Superclass::PointIDType PointIDType;
  typedef typename Superclass::DegreeOfFreedomIDType DegreeOfFreedomIDType;
  typedef typename Superclass::NodeDefinitionType NodeDefinitionType;
  typedef typename Superclass::ReadInfoType ReadInfoType;
  enum{ InvalidDegreeOfFreedomID = Superclass::InvalidDegreeOfFreedomID };

  /**
   * Number of geometrical points that define the element
   */
  enum { NumberOfPoints=VNumberOfPoints };

  /**
   * Number of nodes (points that hold degrees of freedom)
   * that define the element. In linear element number of
   * nodes is always equal to  number of points.
   */
  enum { NumberOfNodes=NumberOfPoints };

  /**
   * Number of unknown variables that exist at every point
   * within an element.
   */
  enum { NumberOfDegreesOfFreedomPerNode=VNumberOfDegreesOfFreedomPerNode };

  /**
   * Node class that is used to specify points that define
   * the geometry of the element.
   */
  typedef TPointClass PointClass;

  /**
   * Total number of degrees of freedom in an element
   */
  enum { NDOF=NumberOfNodes*NumberOfDegreesOfFreedomPerNode };




  /**
   * Default constructor just clears the ivars
   */
  ElementStd();



//////////////////////////////////////////////////////////////////////////
  /*
   * Methods that define the geometry of an element
   * FIXME: These should be implemented in Cell/Mesh
   */
  virtual unsigned int GetNumberOfPoints(void) const
  { return NumberOfPoints; }

  virtual PointIDType GetPoint(unsigned int pt) const
  {
    if(pt>=NumberOfPoints)
    {
      return 0;
    }
    return this->m_point[pt];
  }

  virtual void SetPoint(unsigned int pt, PointIDType point)
  {
    if(pt>=NumberOfPoints) { return; }
    if( (this->m_point[pt]=dynamic_cast<const PointClass*>(&*point)) == 0 )
    {
      throw FEMExceptionWrongClass(__FILE__,__LINE__,"ElementStd::ElementStd()");
    }
  }



//////////////////////////////////////////////////////////////////////////
  /*
   * Methods and typedefs related to DOF management 
   */
  virtual unsigned int GetNumberOfNodes( void ) const
  { return NumberOfNodes; }

  virtual unsigned int GetNumberOfDegreesOfFreedomPerNode( void ) const
  { return NumberOfDegreesOfFreedomPerNode; }

  virtual DegreeOfFreedomIDType GetDegreeOfFreedom( unsigned int local_dof ) const
  {
    if(local_dof>NDOF) { return static_cast<DegreeOfFreedomIDType>(InvalidDegreeOfFreedomID); } // error checking
    return this->m_dof[local_dof];
  }

  virtual void SetDegreeOfFreedom( unsigned int local_dof, DegreeOfFreedomIDType global_dof)
  {
    if (local_dof>NDOF) return; // error checking
    this->m_dof[local_dof]=global_dof;
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
  virtual void Write( std::ostream& f, int ofid ) const;




protected:

  /**
   * Array of pointers to point objects that define the element
   */
  typename PointClass::ConstPointer m_point[NumberOfPoints];

private:

  /**
   * Array that stores DOF ids for the element class.
   */
  DegreeOfFreedomIDType m_dof[NDOF];

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
