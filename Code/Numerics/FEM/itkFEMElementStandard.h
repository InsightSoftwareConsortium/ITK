/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementStandard.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElementStandard_h
#define __itkFEMElementStandard_h

#include "itkFEMElementBase.h"

namespace itk {
namespace fem {




/**
 * \class ElementStandard
 * \brief Implements standard node management in the element classes.
 *
 * If the derived element class has DOFs associated only with points
 * that define the geometry of the element, you can derive from this
 * class to automatically create all the functions required for proper
 * node management.
 *
 * You must specify two template parameters:
 *
 *   NumberOfPoints - number of points that define the element
 *                    (e.g. four for quadrilateral)
 *
 *   NumberOfDOFsPerPoint - number of DOFs that exist at every point
 *                          within an element. This is basically the
 *                          number of parameters that the unknown
 *                          function has.
 *
 *   NodeClass - class of Node objects that the element uses.
 */
template<unsigned int VNumberOfPoints,unsigned int VNumberOfDOFsPerPoint, class TNodeClass>
class ElementStandard : public Element
{
FEM_CLASS_SP(ElementStandard,Element)
public:

  /**
   * Number of geometrical points that define the element
   */
  enum { NumberOfPoints=VNumberOfPoints };

  /**
   * Number of degrees of freedom that exist at every point
   * within an element.
   */
  enum { NumberOfDOFsPerPoint=VNumberOfDOFsPerPoint };


  /**
   * Node class that is used to specify points that define
   * the geometry of the element.
   */
  typedef TNodeClass NodeClass;

  /**
   * Total number of degrees of freedom in an element
   */
  enum { NDOF=NumberOfPoints*NumberOfDOFsPerPoint };

  /**
   * Default constructor just clears the node pointers
   */
  ElementStandard()
  {
    for(int i=0; i<NDOF; i++)
    {
      m_node[i]=0;
    }
  }
  
  /*
   * Methods that define geometry of an element
   * FIXME: These should be implemented in Cell/Mesh
   */
  virtual unsigned int GetNumberOfPoints(void) const
  { return NumberOfPoints; }
  virtual Node::ConstPointer GetPoint(unsigned int pt) const
  {
    if(pt>=NumberOfPoints)
    {
      return 0;
    }
    return this->m_node[pt];
  }

  /*
   * Methods and typedefs related to Node management 
   */
  virtual unsigned int GetNumberOfDegreesOfFreedomPerPoint( void ) const
  { return NumberOfDOFsPerPoint; }

  virtual DegreeOfFreedomIDType GetDegreeOfFreedom( unsigned int local_dof ) const
  { 
    if(local_dof>NDOF) { return InvalidDegreeOfFreedomID; } // error checking
    return m_dof[local_dof];
  }

  virtual void SetDegreeOfFreedom( unsigned int local_dof, DegreeOfFreedomIDType global_dof)
  {
    if (local_dof>NDOF) return; // error checking
    m_dof[local_dof]=global_dof;
  }




  /**
   * Array of pointers to node objects that define the element
   */
  typename NodeClass::ConstPointer m_node[NDOF];

private:
  /**
   * Array that stores DOF ids for the element class.
   */
  DegreeOfFreedomIDType m_dof[NDOF];


};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMElementStandard_h
