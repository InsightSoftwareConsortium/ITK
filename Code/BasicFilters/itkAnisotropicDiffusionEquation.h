/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnisotropicDiffusionEquation.h
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
#ifndef __itkAnisotropicDiffusionEquation_h_
#define __itkAnisotropicDiffusionEquation_h_


#include "itkFiniteDifferenceEquation.h"

namespace itk {

/**
 * \class AnisotropicDiffusionEquation
 *
 * \brief Base class for anisotropic diffusion equation objects.
 *
 * Anisotropic diffusion methods are a tools for calculating multi-scale
 * descriptions of images. Embed an image \f$U(/mathbf{x})\f$ in a higher
 * dimensional function of derived images, \f$U(/mathbf{x}, t)\f$.  This higher
 * dimensional function represents the solution of the heat diffusion equation,
 *
 * \f[\frac{d U(\mathbf{x})}{d t} = \nabla \cdot c \nabla U(\mathbf{x})\f]
 *
 * with constant \f$c\f$ and initial condition
 * \f$U(\mathbf{x}, 0) = U_0(\mathbf{x})\f$, the original image.
 *
 * Extending to the case where \f$c\f$ is not a constant, but a function of
 * \f$\mathbf{x}\f$, gives
 *
 * \f[\frac{d U(\mathbf{x})}{d t} = C(\mathbf{x})\Delta U(\mathbf{x})
 * + \nabla C(\mathbf{x}) \nabla U(\mathbf{x})\f]
 *
 * Our choice of \f$C\f$ now varies the strength of diffusion anisotropically.
 * Typically, \f$C\f$ is chosen as some function of image features to
 * selectively preserve or remove those features.  For example, edges tend to
 * be preserved over smoother regions where \f$C\f$ is inversely scaled
 * according to gradient magnitude as in
 *
 * \f[C(\mathbf{x}) = e^{-(\frac{\parallel \nabla U(\mathbf{x}) \parallel}{K})^2}\f].
 *
 * Several variations on the scheme presented above are implemented in Itk as
 * subclasses of this equation.  The equations are solved using an iterative,
 * finite forward difference technique (see the FiniteDifferenceImageFilter
 * class).
 *
 * \sa GradientAnisotropicDiffusionEquation
 * \sa CurvatureAnisotropicDiffusionEquation
 * \sa VectorGradientAnisotropicDiffusionEquation
 * 
 * \reference
 * \reference
 *
 * \todo Documentation, references */
template <class TImage>
class AnisotropicDiffusionEquation :
    public FiniteDifferenceEquation<TImage>
{
public:
 /**
   * Standard itk Self & Superclass typedefs
   */
  typedef AnisotropicDiffusionEquation Self;
  typedef FiniteDifferenceEquation<TImage> Superclass;

  /**
   * Inherit some parameters from the superclass type
   */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::PixelType PixelType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::BoundaryNeighborhoodType BoundaryNeighborhoodType;
  typedef typename Superclass::TimeStepType TimeStepType;

  //  typedef typename Superclass::FloatOffsetType FloatOffsetType;
  enum { ImageDimension = Superclass::ImageDimension };

  /** 
   * Smart pointer support for this class.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro( AnisotropicDiffusionEquation, FiniteDifferenceEquation );
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * This method is called before each iteration to recalculate the
   * average gradient magnitude of the region.  The value is used
   * internally by the AnisotropicDiffusionEquation during processing.
   */
  virtual void CalculateAverageGradientMagnitudeSquared(ImageType *) = 0;

  /**
   * Required by FiniteDifferenceImageFilter to set the initial value
   * of the time step for an iteration.
   */
  virtual TimeStepType GetInitialTimeStep() const
  {    return this->GetTimeStep();  }

  void SetTimeStep(const TimeStepType &t)
    { m_TimeStep = t; }
  const TimeStepType &GetTimeStep() const
    { return m_TimeStep; }
  void SetConductanceParameter(const double &c)
    { m_ConductanceParameter = c; }
  const double &GetConductanceParameter() const
    { return m_ConductanceParameter; }

  const double &GetAverageGradientMagnitudeSquared() const
    { return m_AverageGradientMagnitudeSquared;  }

  void SetAverageGradientMagnitudeSquared(const double &c)
    { m_AverageGradientMagnitudeSquared = c; }

protected:
  AnisotropicDiffusionEquation()
    {
      m_AverageGradientMagnitudeSquared = 0.0;
      m_ConductanceParameter     = 0.0;
      m_TimeStep                 = 0.125f;
    }
  ~AnisotropicDiffusionEquation() {}
  AnisotropicDiffusionEquation(const Self&) {}
  void operator=(const Self&) {}

  double m_AverageGradientMagnitudeSquared;
  double m_ConductanceParameter;
  TimeStepType    m_TimeStep;
};

}// end namespace itk

#endif
