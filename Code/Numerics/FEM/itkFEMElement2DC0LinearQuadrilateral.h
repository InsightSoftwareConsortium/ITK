/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DC0LinearQuadrilateral.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement2DC0LinearQuadrilateral_h
#define __itkFEMElement2DC0LinearQuadrilateral_h

#include "itkFEMElementStd.h"

namespace itk {
namespace fem {




/**
 * \class Element2DC0LinearQuadrilateral
 * \brief 4-noded, linear, C0 continuous finite element in 2D space.
 */
class Element2DC0LinearQuadrilateral : public ElementStd<4,2>
{
typedef ElementStd<4,2> TemplatedParentClass;
FEM_ABSTRACT_CLASS( Element2DC0LinearQuadrilateral, TemplatedParentClass )
public:

/**
   * Compute and return element mass matrix (Me) in global coordinate system.
   *
   *     b   T
   * int    N(x) (rho c) N(x) dx
   *     a
   *
   * where (rho c) is constant.  Implementation is similar to GetStiffnessMatrix.
   *
   *
   * 
   *
   */
  void GetMassMatrix(MatrixType& Me) const;


//////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to numeric integration
   */

  enum { DefaultIntegrationOrder = 2 };

  virtual void GetIntegrationPointAndWeight(unsigned int i, VectorType& pt, Float& w, unsigned int order) const;

  virtual unsigned int GetNumberOfIntegrationPoints(unsigned int order) const;



//////////////////////////////////////////////////////////////////////////
  /*
   * Methods related to the geometry of an element
   */

  virtual VectorType ShapeFunctions( const VectorType& pt ) const;

  virtual void ShapeFunctionDerivatives( const VectorType& pt, MatrixType& shapeD ) const;

  virtual bool GetLocalFromGlobalCoordinates( const VectorType& Gpt, VectorType& Lpt) const;

  /**
   * Draw the element on the specified device context
   */
#ifdef FEM_BUILD_VISUALIZATION
  void Draw(CDC* pDC, Solution::ConstPointer sol) const;
#endif

};




}} // end namespace itk::fem

#endif  // #ifndef __itkFEMElement2DC0LinearQuadrilateral_h
