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
#ifndef __itkFEMElement3DC0LinearTetrahedron_h
#define __itkFEMElement3DC0LinearTetrahedron_h


#include "itkFEMElementStd.h"
//to make some checks in GetLocalFromGlobalCoordinates
#include "vnl/vnl_matrix.h"
#include "vnl/algo/vnl_matrix_inverse.h"

namespace itk {
namespace fem {

/**
 * \class Element3DC0LinearTetrahedron
 * \brief 4-noded, linear, C0 continuous finite element in 3D space.
 * \ingroup ITK-FEM
 */
class Element3DC0LinearTetrahedron : public ElementStd<4,3>
{
  typedef ElementStd<4,3> TemplatedParentClass;
  FEM_ABSTRACT_CLASS( Element3DC0LinearTetrahedron, TemplatedParentClass )
public:


  //////////////////////////////////////////////////////////////////////////
  /**
   * Methods related to numeric integration
   */

  virtual void GetIntegrationPointAndWeight(unsigned int i, VectorType& pt, Float& w, unsigned int order) const;

  virtual unsigned int GetNumberOfIntegrationPoints(unsigned int order) const;

  //////////////////////////////////////////////////////////////////////////
  /**
   * Methods related to the geometry of an element
   */

  virtual VectorType ShapeFunctions( const VectorType& pt ) const;

  virtual void ShapeFunctionDerivatives( const VectorType& pt, MatrixType& shapeD ) const;

  virtual bool GetLocalFromGlobalCoordinates( const VectorType& globalPt, VectorType& localPt ) const;

  /**
   * Draw the element on the specified device context
   */
#ifdef FEM_BUILD_VISUALIZATION
  void Draw(CDC* pDC, Solution::ConstPointer sol) const;
#endif


};

}} // end namespace itk::fem

#endif  // #ifndef __itkFEMElement3DC0LinearTetrahedron_h
