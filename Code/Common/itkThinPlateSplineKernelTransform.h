/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThinPlateSplineKernelTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkThinPlateSplineKernelTransform_h
#define __itkThinPlateSplineKernelTransform_h

#include "itkKernelTransform.h"

namespace itk
{
/** \class ThinPlateSplineKernelTransform
 * This class defines the thin plate spline (TPS) transformation.
 * It is implemented in as straightforward a manner as possible from
 * the IEEE TMI paper by Davis, Khotanzad, Flamig, and Harms,
 * Vol. 16 No. 3 June 1997
 *
 * \ingroup Transforms
 */
template <class TScalarType,         // Data type for scalars (float or double)
          unsigned int NDimensions = 3>          // Number of dimensions
class ITK_EXPORT ThinPlateSplineKernelTransform : 
                public KernelTransform<   TScalarType, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef ThinPlateSplineKernelTransform Self;
  typedef KernelTransform<    TScalarType, NDimensions>   Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( ThinPlateSplineKernelTransform, KernelTransform );

  /** Scalar type. */
  typedef typename Superclass::ScalarType  ScalarType;

  /** Parameters type. */
  typedef typename Superclass::ParametersType  ParametersType;

  /** Jacobian Type */
  typedef typename Superclass::JacobianType  JacobianType;

  /** Dimension of the domain space. */
  itkStaticConstMacro(SpaceDimension, unsigned int,Superclass::SpaceDimension);
                              
  /** These (rather redundant) typedefs are needed because on SGI, typedefs
   * are not inherited */
  typedef typename Superclass::InputPointType  InputPointType;
  typedef typename Superclass::OutputPointType  OutputPointType;
  typedef typename Superclass::InputVectorType InputVectorType;
  typedef typename Superclass::OutputVectorType OutputVectorType;
  typedef typename Superclass::InputCovariantVectorType InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType OutputCovariantVectorType;
  typedef typename Superclass::PointsIterator PointsIterator;
    

protected:
  ThinPlateSplineKernelTransform() {};
  virtual ~ThinPlateSplineKernelTransform() {}
  
  /** These (rather redundant) typedefs are needed because on SGI, typedefs
   * are not inherited. */
  typedef typename Superclass::GMatrixType GMatrixType;
  
  /** Compute G(x)
   * For the thin plate spline, this is:
   * G(x) = r(x)*I
   * \f$ G(x) = r(x)*I \f$
   * where
   * r(x) = Euclidean norm = sqrt[x1^2 + x2^2 + x3^2]
   * \f[ r(x) = \sqrt{ x_1^2 + x_2^2 + x_3^2 }  \f]
   * I = identity matrix. */
  const GMatrixType & ComputeG(const InputVectorType & x) const;


  /** Compute the contribution of the landmarks weighted by the kernel funcion
      to the global deformation of the space  */
  virtual void ComputeDeformationContribution( const InputPointType & inputPoint,
                                                     OutputPointType & result ) const;

 private:
  ThinPlateSplineKernelTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThinPlateSplineKernelTransform.txx"
#endif

#endif // __itkThinPlateSplineKernelTransform_h
