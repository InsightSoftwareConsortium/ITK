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
 *   NumberOfDegreesOfFreedomPerNode - number of degrees of freedom at each
 *                    node. This is also equal to the number of unknown
 *                    functions that must be solved for at every point within
 *                    an element.
 *
 *   NodeClass - class of Node objects that the element uses.
 *
 *   NumberOfNodes - number of nodes in an element. Defaults to number
 *                   of points.
 */
template<unsigned int VNumberOfPoints,unsigned int VNumberOfDegreesOfFreedomPerNode, class TNodeClass>
class ElementStandard : public Element
{
FEM_CLASS_SP(ElementStandard,Element)
public:

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
  typedef TNodeClass NodeClass;

  /**
   * Total number of degrees of freedom in an element
   */
  enum { NDOF=NumberOfNodes*NumberOfDegreesOfFreedomPerNode };

  /**
   * Default constructor just clears the ivars
   */
  ElementStandard()
  {
    this->ClearDegreesOfFreedom();
    for(int i=0; i<NumberOfPoints; i++)
    {
      this->m_node[i]=0;
    }
  }
  
  /*
   * Methods that define geometry of an element
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
    return this->m_node[pt];
  }
  virtual void SetPoint(unsigned int pt, PointIDType node)
  {
    if(pt>=NumberOfPoints)
    {
      return;
    }
    this->m_node[pt]=dynamic_cast<const NodeClass*>(&*node);
  }

  /*
   * Methods and typedefs related to Node management 
   */
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



  
  /**
   * Array of pointers to node objects that define the element
   */
  typename NodeClass::ConstPointer m_node[NumberOfPoints];



private:

  /**
   * Array that stores DOF ids for the element class.
   */
  DegreeOfFreedomIDType m_dof[NDOF];

};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMElementStandard_h
