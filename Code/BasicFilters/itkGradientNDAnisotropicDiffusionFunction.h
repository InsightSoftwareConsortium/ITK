/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientNDAnisotropicDiffusionFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGradientNDAnisotropicDiffusionFunction_h_
#define __itkGradientNDAnisotropicDiffusionFunction_h_

#include "itkScalarAnisotropicDiffusionFunction.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkDerivativeOperator.h"

namespace itk {

/** \class GradientNDAnisotropicDiffusionFunction
 *
 * This class implements an N-dimensional version of the classic Perona-Malik
 * anisotropic diffusion equation for scalar-valued images.  See
 * itkAnisotropicDiffusionFunction for an overview of the anisotropic diffusion 
 * framework and equation.
 *
 * \par
 * The conductance term for this implementation is chosen as a function of the
 * gradient magnitude of the image at each point, reducing the strength of
 * diffusion at edge pixels.
 *
 * \f[C(\mathbf{x}) = e^{-(\frac{\parallel \nabla U(\mathbf{x}) \parallel}{K})^2}\f].
 *
 * \par
 * The numerical implementation of this equation is similar to that described
 * in the Perona-Malik paper below, but uses a more robust technique
 * for gradient magnitude estimation and has been generalized to N-dimensions.
 *
 * \par References
 * Pietro Perona and Jalhandra Malik, ``Scale-space and edge detection using
 * anisotropic diffusion,'' IEEE Transactions on Pattern Analysis Machine
 * Intelligence, vol. 12, pp. 629-639, 1990.
 *
 * \sa AnisotropicDiffusionFunction
 * \sa VectorAnisotropicDiffusionFunction
 * \sa VectorGradientAnisotropicDiffusionFunction
 * \sa CurvatureNDAnisotropicDiffusionFunction
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ImageEnhancement
 */ 
template <class TImage>
class GradientNDAnisotropicDiffusionFunction :
    public ScalarAnisotropicDiffusionFunction<TImage>
{
public:
  /** Standard class typedefs. */
  typedef GradientNDAnisotropicDiffusionFunction Self;
  typedef ScalarAnisotropicDiffusionFunction<TImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( GradientNDAnisotropicDiffusionFunction,
                ScalarAnisotropicDiffusionFunction );
  
  /** Inherit some parameters from the superclass type. */
  typedef typename Superclass::ImageType        ImageType;
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::TimeStepType     TimeStepType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::BoundaryNeighborhoodType
                   BoundaryNeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;

  /** Inherit some parameters from the superclass type. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** Compute the equation value. */
  virtual PixelType ComputeUpdate(const NeighborhoodType &neighborhood,
                                  void *globalData,
                                  const FloatOffsetType& offset = FloatOffsetType(0.0)
                                  ) const;
  virtual PixelType ComputeUpdate(const BoundaryNeighborhoodType
                                  &neighborhood, void *globalData,
                                  const FloatOffsetType& offset = FloatOffsetType(0.0)
                                  ) const;

  /** This method is called prior to each iteration of the solver. */
  virtual void InitializeIteration()
    {
      m_K = static_cast<PixelType>(this->GetAverageGradientMagnitudeSquared() *
                                   this->GetConductanceParameter() * -1.0f);
    }
  
protected:
  GradientNDAnisotropicDiffusionFunction();
  ~GradientNDAnisotropicDiffusionFunction() {}

  void PrintSelf(std::ostream& os, Indent indent) const
    {      Superclass::PrintSelf(os,indent);    }
  
  /** Inner product function. */
  NeighborhoodInnerProduct<ImageType> m_InnerProduct;

  /** Boundary Inner product function. */
  SmartNeighborhoodInnerProduct<ImageType> m_SmartInnerProduct;

  /** Slices for the ND neighborhood. */
  std::slice  x_slice[ImageDimension];
  std::slice xa_slice[ImageDimension][ImageDimension];
  std::slice xd_slice[ImageDimension][ImageDimension];

  /** Derivative operator. */
  DerivativeOperator<PixelType, itkGetStaticConstMacro(ImageDimension)> dx_op;

  /** Modified global average gradient magnitude term. */
  PixelType m_K;

  unsigned long m_Center;
  unsigned long m_Stride[ImageDimension];

  static double m_MIN_NORM;
  
private:
  GradientNDAnisotropicDiffusionFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};
  
}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientNDAnisotropicDiffusionFunction.txx"
#endif

#endif
