/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorGradient2DAnisotropicDiffusionEquation.h
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
#ifndef __itkVectorGradient2DAnisotropicDiffusionEquation_h_
#define __itkVectorGradient2DAnisotropicDiffusionEquation_h_

#include "itkVectorAnisotropicDiffusionEquation.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkVectorNeighborhoodInnerProduct.h"
#include "itkDerivativeOperator.h"

namespace itk {

/**
 * \class VectorGradient2DAnisotropicDiffusionEquation
 *  
 * Requirements:
 *      1) Image PixelType must have an internal typedef of ValueType,
 *            i.e. TImage::PixelType::ValueType must be defined as the
 *                 as the type of one value (element/component) of a pixel.
 *                 Since we are talking about vector anisotropic diffusion,
 *                 this is the type of an element of the vector (float, etc.)
 *
 *
 * \todo Convert this class to ND and write a 2DGradientAnis....Equation
 */ 
template <class TImage>
class VectorGradient2DAnisotropicDiffusionEquation :
    public VectorAnisotropicDiffusionEquation<TImage>
{
public:
 /**
   * Standard itk Self & Superclass typedefs
   */
  typedef VectorGradient2DAnisotropicDiffusionEquation Self;
  typedef VectorAnisotropicDiffusionEquation<TImage> Superclass;

  /**
   * Inherit some parameters from the superclass type
   */
  typedef typename Superclass::ImageType        ImageType;
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::TimeStepType     TimeStepType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::BoundaryNeighborhoodType BoundaryNeighborhoodType;
  enum { ImageDimension = Superclass::ImageDimension };
  enum { VectorDimension= Superclass::VectorDimension };

  /**
   * Type of a value in a vector (double, float, etc.)
   */
  typedef typename PixelType::ValueType         ScalarValueType;


  /** 
   * Smart pointer support for this class.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro( VectorGradient2DAnisotropicDiffusionEquation,
                ScalarAnisotropicDiffusionEquation );
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   *
   */
  virtual PixelType ComputeUpdate(const NeighborhoodType &neighborhood,
                       TimeStepType &dt) const;

  /**
   *
   */
  virtual PixelType ComputeUpdate(const BoundaryNeighborhoodType
  &neighborhood, TimeStepType &dt) const;


  /**
   * This method is called prior to each iteration of the solver.
   */
  virtual void InitializeIteration()
    {
      m_k = this->GetAverageGradientMagnitudeSquared() *
        this->GetConductanceParameter() * -1.0f;
    }
  
protected:
  VectorGradient2DAnisotropicDiffusionEquation();
  ~VectorGradient2DAnisotropicDiffusionEquation() {}
  VectorGradient2DAnisotropicDiffusionEquation(const Self&) {}
  void operator=(const Self&) {}

  /**
   * Inner product function.
   */
  VectorNeighborhoodInnerProduct<ImageType> m_InnerProduct;


  /**
   * Boundary Inner product function.
   */
  SmartVectorNeighborhoodInnerProduct<ImageType> m_SmartInnerProduct;

  /**
   * Slices for the 2D neighborhood.
   * 0  1  2  3  4
   * 5  6 *7* 8  9
   * 10 11 12 13 14
   */
  std::slice  x_slice; // (6,3,1)
  std::slice  y_slice; // (2,3,5)
  std::slice xa_slice; // (7,3,1)
  std::slice ya_slice; // (3,3,5)
  std::slice xd_slice; // (5,3,1)
  std::slice yd_slice; // (1,3,5)

  /**
   * Derivative operators.
   */
  DerivativeOperator<ScalarValueType, 2> dx_op;
  DerivativeOperator<ScalarValueType, 2> dy_op;

  /**
   * Modified global average gradient magnitude term.
   */
  double m_k;
  
};


  
}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorGradient2DAnisotropicDiffusionEquation.txx"
#endif

#endif
