/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkElasticBodyReciprocalSplineKernelTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkElasticBodyReciprocalSplineKernelTransform_h
#define __itkElasticBodyReciprocalSplineKernelTransform_h

#include "itkKernelTransform.h"


namespace itk
{

/** \class ElasticBodyReciprocalSplineKernelTransform
 * This class defines the elastic body spline (EBS) transformation.
 * It is implemented in as straightforward a manner as possible from
 * the IEEE TMI paper by Davis, Khotanzad, Flamig, and Harms,
 * Vol. 16 No. 3 June 1997
 * Taken from the paper:
 * The EBS "is based on a physical model of a homogeneous, isotropic,
 * three-dimensional elastic body. The model can approximate the way
 * that some physical objects deform".
 *
 * \ingroup Transforms
 */
template <class TScalarType = double,   // Data type for scalars (float or double)
          unsigned int NDimensions = 3>          // Number of dimensions
class ITK_EXPORT ElasticBodyReciprocalSplineKernelTransform : 
          public KernelTransform<  TScalarType, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef ElasticBodyReciprocalSplineKernelTransform   Self;
  typedef KernelTransform<  TScalarType, 
                            NDimensions> Superclass;

  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** Run-time type information (and related methods). */
  itkTypeMacro( ThinPlateSplineKernelTransform, KernelTransform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Scalar type. */
  typedef typename Superclass::ScalarType  ScalarType;

  /** Parameters type. */
  typedef typename Superclass::ParametersType  ParametersType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType  JacobianType;

  /** Dimension of the domain space. */
  itkStaticConstMacro(SpaceDimension, unsigned int,Superclass::SpaceDimension);

  /** Set alpha.  Alpha is related to Poisson's Ratio (\f$\nu\f$) as
   * \f$\alpha = 8 ( 1 - \nu ) - 1\f$
   */
  itkSetMacro( Alpha, TScalarType );
  
  /** Get alpha */
  itkGetMacro( Alpha, TScalarType );
  
  /** These (rather redundant) typedefs are needed because on SGI, typedefs
   * are not inherited */
  typedef typename Superclass::InputPointType  InputPointType;
  typedef typename Superclass::OutputPointType  OutputPointType;
  typedef typename Superclass::InputVectorType InputVectorType;
  typedef typename Superclass::OutputVectorType OutputVectorType;
  typedef typename Superclass::InputCovariantVectorType InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType OutputCovariantVectorType;
    

protected:
  ElasticBodyReciprocalSplineKernelTransform();
  virtual ~ElasticBodyReciprocalSplineKernelTransform();
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** These (rather redundant) typedefs are needed because on SGI, typedefs
   * are not inherited */
  typedef typename Superclass::GMatrixType GMatrixType;

  /** Compute G(x)
   * For the elastic body spline, this is:
   * G(x) = [alpha*r(x)*I - 3*x*x'/r(x)]
   * \f$ G(x) = [\alpha*r(x)*I - 3*x*x'/r(x) ]\f$
   * where
   * \f$\alpha = 8 ( 1 - \nu ) - 1\f$
   * \f$\nu\f$ is Poisson's Ratio
   * r(x) = Euclidean norm = sqrt[x1^2 + x2^2 + x3^2]
   * \f[ r(x) = \sqrt{ x_1^2 + x_2^2 + x_3^2 }  \f]
   * I = identity matrix */
  const GMatrixType & ComputeG(const InputVectorType& x) const;

  /** alpha, Poisson's ratio */
  TScalarType m_Alpha;

  private:
  ElasticBodyReciprocalSplineKernelTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkElasticBodyReciprocalSplineKernelTransform.txx"
#endif

#endif // __itkElasticBodyReciprocalSplineKernelTransform_h
