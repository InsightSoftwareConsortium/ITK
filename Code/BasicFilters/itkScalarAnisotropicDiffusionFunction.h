/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarAnisotropicDiffusionFunction.h
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
#ifndef __itkScalarAnisotropicDiffusionFunction_h_
#define __itkScalarAnisotropicDiffusionFunction_h_


#include "itkAnisotropicDiffusionFunction.h"

namespace itk {

/**
 * \class ScalarAnisotropicDiffusionFunction    
 * This class forms the base for any anisotropic diffusion function that
 * operates on scalar data (see itkAnisotropicDiffusionFunction).  It provides
 * some common functionality used in classes like
 * CurvatureNDAnisotropicDiffusionFunction and
 * GradientNDAnisotropicDiffusionFunction.
 *
 * \sa AnisotropicDiffusionFunction
 * \sa AnisotropicDiffusionImageFilter
 * \sa VectorAnisotropicDiffusionFunction
 * \ingroup Functions
 * \ingroup ImageEnhancement */
template <class TImage>
class ScalarAnisotropicDiffusionFunction :
    public AnisotropicDiffusionFunction<TImage>
{
public:
 /** Standard class typedefs. */
  typedef ScalarAnisotropicDiffusionFunction Self;
  typedef AnisotropicDiffusionFunction<TImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Inherit some parameters from the superclass type. */
  enum { ImageDimension = Superclass::ImageDimension };

  /** Inherit some parameters from the superclass type. */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::PixelType PixelType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::BoundaryNeighborhoodType BoundaryNeighborhoodType;
  typedef typename Superclass::TimeStepType TimeStepType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarAnisotropicDiffusionFunction,
                AnisotropicDiffusionFunction);
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  virtual void CalculateAverageGradientMagnitudeSquared(TImage *);

protected:
  ScalarAnisotropicDiffusionFunction() {}
  ~ScalarAnisotropicDiffusionFunction() {}
  void PrintSelf(std::ostream& os, Indent indent) const
    { Superclass::PrintSelf(os,indent); }
private:
  ScalarAnisotropicDiffusionFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalarAnisotropicDiffusionFunction.txx"
#endif

#endif
