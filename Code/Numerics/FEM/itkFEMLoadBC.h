/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadBC.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFEMLoadBC_h
#define __itkFEMLoadBC_h

#include "itkFEMLoadBase.h"

namespace itk {
namespace fem {




/**
 * \class LoadBC
 * \brief Generic essential (Dirichlet) boundary conditions.
 *
 * Objects of this class specify, which DOFs in a system are fixed.
 */
class LoadBC : public Load
{
FEM_CLASS(LoadBC,Load)
public:

  /**
   * Pointer to an element, which holds the DOF that is affected
   * by boundary condition.
   */
  Element::ConstPointer m_element;

  /**
   * Local DOF number within the Element object.
   */    
  unsigned int m_dof;

  /**
   * Value which the DOF is being fixed.
   *
   * \note This is a vector so that implementation of BC on isotropic elements
   *       is easy. Which value is applied to the master force vector is
   *       defined by optional dim parameter (defaults to 0) in AssembleF
   *       function in solver.
   */
  vnl_vector<Element::Float> m_value;

  /** Default constructor */
  LoadBC() : m_element(0), m_dof(0), m_value() {}

  /** Read a LoadBC object from input stream.*/
  virtual void Read( std::istream& f, void* info );

  /** Write a LoadBC object to the output stream*/
  virtual void Write( std::ostream& f, int ofid ) const;

};

FEM_CLASS_INIT(LoadBC)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadBC_h
