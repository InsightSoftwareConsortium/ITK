/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientNDAnisotropicDiffusionEquation.h
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
#ifndef __itkGradientNDAnisotropicDiffusionEquation_h_
#define __itkGradientNDAnisotropicDiffusionEquation_h_

#include "itkScalarAnisotropicDiffusionEquation.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkDerivativeOperator.h"

namespace itk {

/**
 * \class GradientNDAnisotropicDiffusionEquation
 *  
 *  
 * This class implements an anisotropic equation with a conductance term that
 * varies spatially according to the gradient magnitude of the image.  See
 * AnisotropicDiffusionEquation for more information on anisotropic diffusion
 * equations.
 *
 * The conductance term of the equation has the following form:
 *
 * \f[C(\mathbf{x}) = e^{-(\frac{\parallel \nabla U(\mathbf{x}) \parallel}{K})^2}\f].
 *
 * Edge features tend to be preserved in the resulting, diffused image.
 *
 * This class implements the general case where image dimension is arbitrary.
 *
 * \sa AnisotropicDiffusionEquation
 * \sa Gradient2DAnisotropicDiffusionEquation
 * \ingroup Operators
 */ 
template <class TImage>
class GradientNDAnisotropicDiffusionEquation :
    public ScalarAnisotropicDiffusionEquation<TImage>
{
public:
 /**
   * Standard itk Self & Superclass typedefs
   */
  typedef GradientNDAnisotropicDiffusionEquation Self;
  typedef ScalarAnisotropicDiffusionEquation<TImage> Superclass;

  /**
   * Inherit some parameters from the superclass type
   */
  typedef typename Superclass::ImageType        ImageType;
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::TimeStepType     TimeStepType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::BoundaryNeighborhoodType
  BoundaryNeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;
  enum { ImageDimension = Superclass::ImageDimension };
  /** 
   * Smart pointer support for this class.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro( GradientNDAnisotropicDiffusionEquation,
                ScalarAnisotropicDiffusionEquation );
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   *
   */
  virtual PixelType ComputeUpdate(const NeighborhoodType &neighborhood,
                                  void *globalData,
                                  const FloatOffsetType& offset = m_ZeroOffset
                                  ) const;

  /**
   *
   */
  virtual PixelType ComputeUpdate(const BoundaryNeighborhoodType
                                  &neighborhood, void *globalData,
                                  const FloatOffsetType& offset = m_ZeroOffset
                                  ) const;


  /**
   * This method is called prior to each iteration of the solver.
   */
  virtual void InitializeIteration()
    {
      m_k = this->GetAverageGradientMagnitudeSquared() *
        this->GetConductanceParameter() * -1.0f;
    }
  
protected:
  GradientNDAnisotropicDiffusionEquation();
  ~GradientNDAnisotropicDiffusionEquation() {}
  GradientNDAnisotropicDiffusionEquation(const Self&) {}
  void operator=(const Self&) {}

  /**
   * Inner product function.
   */
  NeighborhoodInnerProduct<ImageType> m_InnerProduct;


  /**
   * Boundary Inner product function.
   */
  SmartNeighborhoodInnerProduct<ImageType> m_SmartInnerProduct;

  /**
   * Slices for the ND neighborhood.
   */
  std::slice  x_slice[ImageDimension];
  std::slice xa_slice[ImageDimension];
  std::slice xd_slice[ImageDimension];

  /**
   * Derivative operator
   */
  DerivativeOperator<PixelType, 2> dx_op;

  /**
   * Modified global average gradient magnitude term.
   */
  PixelType m_k;

  /**
   * 
   */
  unsigned long m_Center;

  /**
   *
   */
  unsigned long m_Stride[ImageDimension];

};


  
}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientNDAnisotropicDiffusionEquation.txx"
#endif

#endif
