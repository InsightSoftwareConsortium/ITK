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
#ifndef __itkVectorAnisotropicDiffusionEquation_h_
#define __itkVectorAnisotropicDiffusionEquation_h_

#include "itkAnisotropicDiffusionEquation.h"
#include "itkVector.h"

namespace itk {

/**
 * \todo DOCUMENT!
 */ 
template <class TImage>
class VectorAnisotropicDiffusionEquation :
    public AnisotropicDiffusionEquation<TImage>
{
public:
 /**
   * Standard itk Self & Superclass typedefs
   */
  typedef VectorAnisotropicDiffusionEquation   Self;
  typedef AnisotropicDiffusionEquation<TImage> Superclass;

  /**
   * Inherit some parameters from the superclass type
   */
  typedef Superclass::ImageType        ImageType;
  enum { ImageDimension = Superclass::ImageDimension };
  typedef Superclass::PixelType        PixelType;
  typedef Superclass::ScalarValueType  ScalarValueType;
  typedef Superclass::TimeStepType     TimeStepType;
  typedef Superclass::RadiusType       RadiusType;
  typedef Superclass::NeighborhoodType NeighborhoodType;
  typedef Superclass::BoundaryNeighborhoodType BoundaryNeighborhoodType;
  enum { VectorDimension = PixelType::VectorDimension };


  /** 
   * Smart pointer support for this class.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(VectorAnisotropicDiffusionEquation,
               AnisotropicDiffusionEquation);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   *
   */
  virtual void CalculateAverageGradientMagnitudeSquared(TImage *);

protected:
  VectorAnisotropicDiffusionEquation() {}
  ~VectorAnisotropicDiffusionEquation() {}
  VectorAnisotropicDiffusionEquation(const Self&) {}
  void operator=(const Self&) {}
};

}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorAnisotropicDiffusionEquation.txx"
#endif

#endif
