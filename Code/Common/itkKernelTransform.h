/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKernelTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkKernelTransform_h
#define __itkKernelTransform_h

#include "itkTransform.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkMatrix.h"
#include "itkPointSet.h"
#include <deque>
#include <math.h>
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/algo/vnl_svd.h"

namespace itk
{

/** \class KernelTransform
 * Intended to be a base class for elastic body spline and thin plate spline.
 * This is implemented in as straightforward a manner as possible from the
 * IEEE TMI paper by Davis, Khotanzad, Flamig, and Harms,
 * Vol. 16 No. 3 June 1997
 * Notation closely follows their paper, so if you have it in front of you,
 * this code will make a lot more sense.
 *
 * KernelTransform:
 *  Provides support for defining source and target landmarks
 *  Defines a number of data types used in the computations
 *  Defines the mathematical framework used to compute all splines,
 *    so that subclasses need only provide a kernel specific to
 *    that spline
 *
 * \ingroup Transforms
 *
 */
template <class TScalarType,         // Only float and double make sense
          int NDimensions = 3>       // Number of dimensions
class ITK_EXPORT KernelTransform : public Transform<TScalarType, NDimensions>
{
public:
  /**
   * Standard Self typedef
   */
  typedef KernelTransform<TScalarType, NDimensions> Self;
  
  /**
   * Standard Superclass typedef
   */
  typedef Transform<TScalarType, NDimensions> Superclass;
  
  /**
   * Standard coordinate point type for this class
   */
  typedef typename Superclass::PointType   PointType;
  
  /**
   * Standard vector type for this class
   */
  typedef typename Superclass::VectorType  VectorType;

  /**
   * PointList typedef. This type is used for maintaining lists of points,
   * specifically, the source and target landmark lists.
   */
  typedef DefaultStaticMeshTraits<TScalarType,
                                  NDimensions,
                                  NDimensions,
                                  TScalarType,
                                  TScalarType> PointSetTraitsType;

  typedef PointSet<PointType, NDimensions, PointSetTraitsType> PointSetType;
  typedef typename PointSetType::Pointer PointSetPointer;
  typedef typename PointSetType::PointsContainerConstIterator PointsIterator;
  
  /**
   * VectorSet typedef
   */
  typedef itk::VectorContainer<unsigned long,VectorType> VectorSetType;
  typedef typename VectorSetType::Pointer           VectorSetPointer;

  /**
   * Get the source landmarks list, which we will denote \f$ p \f$
   */
  PointSetPointer GetSourceLandmarks(void) const;
  
  /**
   * Set the source landmarks list
   */
  void SetSourceLandmarks(const PointSetType * p);
  
  /**
   * Get the target landmarks list, which we will denote  \f$ q \f$
   */
  PointSetPointer GetTargetLandmarks(void) const;
  
  /**
   * Set the target landmarks list
   */
  void SetTargetLandmarks(const PointSetType * q);
  
  /**
   * Get the displacements list, which we will denote \f$ d \f$,
   * where \f$ d_i = q_i - p_i \f$
   */
  VectorSetPointer GetDisplacements(void) const;
  
  /**
   * Compute W matrix
   */
  void ComputeWMatrix(void);
  
  /**
   * Compute the position of point in the new space
   */
  virtual PointType TransformPoint(const PointType& thisPoint) const;
  
  /**
   * Compute the position of vector in the new space
   */
  virtual VectorType TransformVector(const VectorType& thisVector) const;
  
  /**
   * 'I' (identity) matrix typedef
   */
  typedef vnl_matrix_fixed<TScalarType, NDimensions, NDimensions> IMatrixType;
  
  /**
   * Default constructor
   */
  KernelTransform();
  /**
   * Destructor
   */
  virtual ~KernelTransform();

protected:
  /**
   * 'G' matrix typedef
   */
  typedef vnl_matrix_fixed<TScalarType, NDimensions, NDimensions> GMatrixType;
  /**
   * 'L' matrix typedef
   */
  typedef vnl_matrix<TScalarType> LMatrixType;
  /**
   * 'K' matrix typedef
   */
  typedef vnl_matrix<TScalarType> KMatrixType;
  /**
   * 'P' matrix typedef
   */
  typedef vnl_matrix<TScalarType> PMatrixType;
  /**
   * 'Y' matrix typedef
   */
  typedef vnl_matrix<TScalarType> YMatrixType;
  /**
   * 'W' matrix typedef
   */
  typedef vnl_matrix<TScalarType> WMatrixType;
  /**
   * Row matrix typedef
   */
  typedef vnl_matrix_fixed<TScalarType, 1, NDimensions> RowMatrixType;
  /**
   * Column matrix typedef
   */
  typedef vnl_matrix_fixed<TScalarType, NDimensions, 1> ColumnMatrixType;

  /**
   * Compute G(x)
   * This is essentially the kernel of the transform.
   * By overriding this method, we can obtain (among others):
   *    Elastic body spline
   *    Thin plate spline
   *    Volume spline
   */
  virtual GMatrixType ComputeG(const VectorType & x) const = 0;
  
  /**
   * Compute K matrix
   */
  void ComputeK();
  
  /**
   * Compute L matrix
   */
  void ComputeL();
  
  /**
   * Compute P matrix
   */
  void ComputeP();
  
  /**
   * Compute Y matrix
   */
  void ComputeY();
  
  /**
   * Compute displacements \f$ q_i - p_i \f$
   */
  void ComputeD();
  
  /**
   * The list of source landmarks, denoted 'p'
   */
  PointSetPointer m_SourceLandmarks;
  
  /**
   * The list of target landmarks, denoted 'q'
   */
  PointSetPointer m_TargetLandmarks;

  /**
   * The list of displacements.
   * d[i] = q[i] - p[i];
   */
  VectorSetPointer m_Displacements;

  /**
   * The L matrix
   */
  LMatrixType m_LMatrix;

  /**
   * The K matrix
   */
  KMatrixType m_KMatrix;
  
  /**
   * The P matrix
   */
  PMatrixType m_PMatrix;
  
  /**
   * The Y matrix
   */
  YMatrixType m_YMatrix;
  
  /**
   * The W matrix
   */
  WMatrixType m_WMatrix;
  
  /**
   * Has the W matrix been computed?
   */
  bool m_WMatrixComputed;
  
  /**
   * Identity matrix
   */
  static IMatrixType m_I;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKernelTransform.txx"
#endif

#endif // __itkKernelTransform_h
