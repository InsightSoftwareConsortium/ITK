/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadLandmark.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMLoadLandmark_h
#define __itkFEMLoadLandmark_h

#include "itkFEMLoadElementBase.h"
#include "vnl/vnl_vector.h"

namespace itk {
namespace fem {

/**
 * \class LoadLandmark
 * \brief This load is derived from the motion of a specific landmark
 *
 * This load depends on the motion of a point from an undeformed
 * configuration to a deformed configuration.
 */
class LoadLandmark : public LoadElement {
FEM_CLASS(LoadLandmark,LoadElement)
public:

  /**  
   * Square root of the variance (eta)
   */
  float eta; 

  /**
   * Point in __local coordinates__ in the undeformed configuration
   */
  vnl_vector<Float> m_pt;

  /**
   * the actual load vector
   */
  vnl_vector<Float> F;

  /**
   * Pointer to the element which contains the undeformed
   * configuration of the landmark
   */
  Element::ConstPointer m_element;

  /**
   * Pointer to the solution object
   */
  Solution::ConstPointer m_Solution;

  /**
   * Methods to access the most recent solution vector
   */
  void SetSolution(Solution::ConstPointer ptr) { m_Solution = ptr; }
  Solution::ConstPointer GetSolution() { return m_Solution; }
  Float GetSolution(unsigned int i, unsigned int v=0) { return m_Solution->GetSolutionValue(i,v); }

  /**
   * Access the location of the point load
   */
  Element::VectorType GetPoint() { return m_pt; }

  /**
   * Access the force vector
   */
  Element::VectorType GetForce() { return F; }

  /** 
   * Read a LoadLandmark object from the input stream
   */
  virtual void Read( std::istream& f, void* info );

  /**
   * Write a LoadLandmark object to the output stream
   */
  virtual void Write( std::ostream& f ) const;

  /**
   * Default constructors
   */
  LoadLandmark() : m_element(0) {}
/*   LoadLandmark() : */
/*     pointu(2), Flm_value(2) {} */
/*   LoadLandmark( int dim ) : */
/*     pointu(dim), Flm_value(dim) {}  */
  LoadLandmark( Element::ConstPointer el_, vnl_vector<Float> pu_, vnl_vector<Float> F_ ) : m_pt(pu_), F(F_), m_element(el_) {}

};

FEM_CLASS_INIT(LoadLandmark)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadLandmark_h
