/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCurvature2DAnisotropicDiffusionEquation.h
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
#ifndef __itkCurvature2DAnisotropicDiffusionEquation_h_
#define __itkCurvature2DAnisotropicDiffusionEquation_h_

#include "itkScalarAnisotropicDiffusionEquation.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkDerivativeOperator.h"

namespace itk {

/**
 * \class Curvature2DAnisotropicDiffusionEquation
 *  
 * \ingroup Operators 
 *
 * \todo Convert this class to ND and write a 2DCurvatureAnis....Equation
 */ 
template <class TImage>
class Curvature2DAnisotropicDiffusionEquation :
    public ScalarAnisotropicDiffusionEquation<TImage>
{
public:
 /**
   * Standard itk Self & Superclass typedefs
   */
  typedef Curvature2DAnisotropicDiffusionEquation Self;
  typedef ScalarAnisotropicDiffusionEquation<TImage> Superclass;

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
  /** 
   * Smart pointer support for this class.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro( Curvature2DAnisotropicDiffusionEquation,
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
  Curvature2DAnisotropicDiffusionEquation();
  ~Curvature2DAnisotropicDiffusionEquation() {}
  Curvature2DAnisotropicDiffusionEquation(const Self&) {}
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
  DerivativeOperator<PixelType, 2> dx_op;
  DerivativeOperator<PixelType, 2> dy_op;

  /**
   * Modified global average gradient magnitude term.
   */
  PixelType m_k;
  
};


  
}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCurvature2DAnisotropicDiffusionEquation.txx"
#endif

#endif
