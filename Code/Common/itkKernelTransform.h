/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKernelTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
template <class TScalarType, // probably only float and double make sense here
          int NDimensions,   // Number of dimensions
          class TParameters, class TJacobianType>       
class ITK_EXPORT KernelTransform : public Transform<TScalarType, 
          NDimensions,NDimensions,TParameters,TJacobianType>
{
public:
  /** Standard class typedefs. */
  typedef KernelTransform Self;
  typedef Transform<TScalarType, NDimensions,
                    NDimensions, TParameters,
                    TJacobianType>              Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( KernelTransform, Transform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Dimension of the domain space. */
  enum { SpaceDimension = NDimensions };

  /** Scalar type. */
  typedef typename Superclass::ScalarType  ScalarType;

  /** Parameters type. */
  typedef typename Superclass::ParametersType  ParametersType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType  JacobianType;

  /** Standard coordinate point type for this class. */
  typedef typename Superclass::InputPointType   InputPointType;
  typedef typename Superclass::OutputPointType  OutputPointType;
  
  /** Standard vector type for this class. */
  typedef typename Superclass::InputVectorType   InputVectorType;
  typedef typename Superclass::OutputVectorType  OutputVectorType;
  
  /** PointList typedef. This type is used for maintaining lists of points,
   * specifically, the source and target landmark lists. */
  typedef DefaultStaticMeshTraits<TScalarType,
                                  NDimensions,
                                  NDimensions,
                                  TScalarType,
                                  TScalarType> PointSetTraitsType;
  typedef PointSet<InputPointType, NDimensions, PointSetTraitsType> PointSetType;
  typedef typename PointSetType::Pointer PointSetPointer;
  typedef typename PointSetType::PointsContainerIterator PointsIterator;
  typedef typename PointSetType::PointsContainerConstIterator PointsConstIterator;
    
  /** VectorSet typedef. */
  typedef itk::VectorContainer<unsigned long,InputVectorType> VectorSetType;
  typedef typename VectorSetType::Pointer        VectorSetPointer;
  
  /** Get the source landmarks list, which we will denote \f$ p \f$. */
  itkGetObjectMacro( SourceLandmarks, PointSetType );
  
  /** Set the source landmarks list. */
  itkSetObjectMacro( SourceLandmarks, PointSetType );
  
  /** Get the target landmarks list, which we will denote  \f$ q \f$. */
  itkGetObjectMacro( TargetLandmarks, PointSetType );
  
  /** Set the target landmarks list. */
  itkSetObjectMacro( TargetLandmarks, PointSetType );
  
  /** Get the displacements list, which we will denote \f$ d \f$,
   * where \f$ d_i = q_i - p_i \f$. */
  itkGetObjectMacro( Displacements, VectorSetType );
  
  /** Compute W matrix. */
  void ComputeWMatrix(void);
  
  /** Compute the position of point in the new space */
  virtual OutputPointType TransformPoint(const InputPointType& thisPoint) const;
  
  /** 'I' (identity) matrix typedef. */
  typedef vnl_matrix_fixed<TScalarType, NDimensions, NDimensions> IMatrixType;

protected:
  KernelTransform();
  virtual ~KernelTransform();
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** 'G' matrix typedef. */
  typedef vnl_matrix_fixed<TScalarType, NDimensions, NDimensions> GMatrixType;
  
  /** 'L' matrix typedef. */
  typedef vnl_matrix<TScalarType> LMatrixType;
  
  /** 'K' matrix typedef. */
  typedef vnl_matrix<TScalarType> KMatrixType;
  
  /** 'P' matrix typedef. */
  typedef vnl_matrix<TScalarType> PMatrixType;
  
  /** 'Y' matrix typedef. */
  typedef vnl_matrix<TScalarType> YMatrixType;

  /** 'W' matrix typedef. */
  typedef vnl_matrix<TScalarType> WMatrixType;
  
  /** Row matrix typedef. */
  typedef vnl_matrix_fixed<TScalarType, 1, NDimensions> RowMatrixType;
  
  /** Column matrix typedef. */
  typedef vnl_matrix_fixed<TScalarType, NDimensions, 1> ColumnMatrixType;

  /** Compute G(x)
   * This is essentially the kernel of the transform.
   * By overriding this method, we can obtain (among others):
   *    Elastic body spline
   *    Thin plate spline
   *    Volume spline */
  virtual GMatrixType ComputeG(const InputVectorType & landmarkVector) const;
  
  /** Compute K matrix. */
  void ComputeK();
  
  /** Compute L matrix. */
  void ComputeL();
  
  /** Compute P matrix. */
  void ComputeP();
  
  /** Compute Y matrix. */
  void ComputeY();
  
  /** Compute displacements \f$ q_i - p_i \f$. */
  void ComputeD();
  
  /** The list of source landmarks, denoted 'p'. */
  PointSetPointer m_SourceLandmarks;
  
  /** The list of target landmarks, denoted 'q'. */
  PointSetPointer m_TargetLandmarks;
  
  /** The list of displacements.
   * d[i] = q[i] - p[i]; */
  VectorSetPointer m_Displacements;

  /** The L matrix. */
  LMatrixType m_LMatrix;

  /** The K matrix. */
  KMatrixType m_KMatrix;

  /** The P matrix. */
  PMatrixType m_PMatrix;

  /** The Y matrix. */
  YMatrixType m_YMatrix;
  
  /** The W matrix. */
  WMatrixType m_WMatrix;

  /** Has the W matrix been computed? */
  bool m_WMatrixComputed;

  /** Identity matrix. */
  IMatrixType m_I;

 private:
  KernelTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKernelTransform.txx"
#endif

#endif // __itkKernelTransform_h
