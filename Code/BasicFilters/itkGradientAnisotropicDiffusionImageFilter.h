/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientAnisotropicDiffusionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGradientAnisotropicDiffusionImageFilter_h_
#define __itkGradientAnisotropicDiffusionImageFilter_h_

#include "itkAnisotropicDiffusionImageFilter.h"
#include "itkGradientNDAnisotropicDiffusionFunction.h"

namespace itk {

/** \class GradientAnisotropicDiffusionImageFilter
 *
 * This filter performs anisotropic diffusion on a scalar itk::Image using the
 * classic Perona-Malik, gradient magnitude based equation implemented in
 * itkGradientNDAnisotropicDiffusionFunction.  For detailed information on
 * anisotropic diffusion, see itkAnisotropicDiffusionFunction and
 * itkGradientNDAnisotropicDiffusionFunction.
 *
 * \par Inputs and Outputs
 * The input to this filter should be a scalar itk::Image of any
 * dimensionality.  The output image will be a diffused copy of the input.

 * \par Parameters
 * Please see the description of parameters given in
 * itkAnisotropicDiffusionImageFilter.
 *
 * \sa AnisotropicDiffusionImageFilter
 * \sa AnisotropicDiffusionFunction
 * \sa GradientAnisotropicDiffusionFunction
 * \ingroup ImageEnhancement
 * \ingroup ImageFilters
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT GradientAnisotropicDiffusionImageFilter
  : public AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef GradientAnisotropicDiffusionImageFilter Self;
  typedef AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>
    Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Standard method for creation through object factory. */
  itkNewMacro(Self);

  /** Run-time class information. */
  itkTypeMacro(GradientAnisotropicDiffusionImageFilter,
               AnisotropicDiffusionImageFilter);
  
  /** Extract information from the superclass. */
  typedef typename Superclass::UpdateBufferType UpdateBufferType;

  /** Extract information from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);
  
protected:
  GradientAnisotropicDiffusionImageFilter()
    {
    typename GradientNDAnisotropicDiffusionFunction<UpdateBufferType>::Pointer p
      = GradientNDAnisotropicDiffusionFunction<UpdateBufferType>::New();
    this->SetDifferenceFunction(p);
    }
  ~GradientAnisotropicDiffusionImageFilter() {}

private:  
  GradientAnisotropicDiffusionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namspace itk

#endif
