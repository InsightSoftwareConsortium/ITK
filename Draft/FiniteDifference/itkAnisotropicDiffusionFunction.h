/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAcosImageAdaptor.h
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
#ifndef __itkAnisotropicDiffusionFunction_h_
#define __itkAnisotropicDiffusionFunction_h_


#include "itkFiniteDifferenceFunction.h"

namespace itk {

template <class TImage>
class AnisotropicDiffusionFunction :
    public FiniteDifferenceFunction<TImage>
{
public:
 /**
   * Standard itk Self & Superclass typedefs
   */
  typedef AnisotropicDiffusionFunction Self;
  typedef FiniteDifferenceFunction<TImage> Superclass;

  /**
   * Inherit some parameters from the superclass type
   */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::PixelType PixelType;
  typedef typename Superclass::RadiusType RadiusType;
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
  itkTypeMacro( AnisotropicDiffusionFunction, FiniteDifferenceFunction );
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * This method is called before each iteration to recalculate the
   * average gradient magnitude of the region.  The value is used
   * internally by the AnisotropicDiffusionFunction during processing.
   */
  virtual void CalculateAverageGradientMagnitudeSquared(ImageType *) = 0;

  /**
   * Required by FiniteDifferenceImageFilter to set the initial value
   * of the time step for an iteration.
   */
  virtual PixelType GetInitialTimeStep() const
  {    return this->GetTimeStep();  }

  void SetTimeStep(const PixelType &t)
    { m_TimeStep = t; }
  const PixelType &GetTimeStep() const
    { return m_TimeStep; }
  void SetConductanceParameter(const PixelType &c)
    { m_ConductanceParameter = c; }
  const PixelType &GetConductanceParameter() const
    { return m_ConductanceParameter; }

  const PixelType &GetAverageGradientMagnitudeSquared() const
    { return m_AverageGradientMagnitudeSquared;  }

  void SetAverageGradientMagnitudeSquared(const PixelType &c)
    { m_AverageGradientMagnitudeSquared = c; }

protected:
  AnisotropicDiffusionFunction()
    {
      m_AverageGradientMagnitudeSquared = NumericTraits<PixelType>::Zero;
      m_ConductanceParameter     = NumericTraits<PixelType>::One;
      m_TimeStep                 = 0.125f;
    }
  ~AnisotropicDiffusionFunction() {}
  AnisotropicDiffusionFunction(const Self&) {}
  void operator=(const Self&) {}

  PixelType m_AverageGradientMagnitudeSquared;
  PixelType m_ConductanceParameter;
  PixelType m_TimeStep;
};


  
}// end namespace itk

#endif
