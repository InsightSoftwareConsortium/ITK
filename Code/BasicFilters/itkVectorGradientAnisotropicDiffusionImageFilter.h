/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorGradientAnisotropicDiffusionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorGradientAnisotropicDiffusionImageFilter_h_
#define __itkVectorGradientAnisotropicDiffusionImageFilter_h_

#include "itkExceptionObject.h"
#include "itkAnisotropicDiffusionImageFilter.h"
#include "itkVectorGradientNDAnisotropicDiffusionFunction.h"

namespace itk {

/** \class VectorGradientAnisotropicDiffusionImageFilter
 *
 * This filter performs anisotropic diffusion on a vector itk::Image using the
 * anisotropic diffusion function implemented implemented in
 * itkVectorGradientNDAnisotropicDiffusionFunction.  For detailed information on
 * anisotropic diffusion see itkAnisotropicDiffusionFunction,
 * itkVectorGradientNDAnisotropicDiffusionFunction, and
 * itkGradientAnisotropicDiffusionFunction.
 * 
 * \par Inputs and Outputs
 * The input to this filter must be an itk::Image with pixel
 * type which is either an itk::Vector, or a subclass of an itk::Vector.
 * Additionally, the component type of the vector should be a numerical type
 * (float or double, or a user defined type which correctly defines
 * arithmetic operations with floating point accuracy).  The output image type
 * also has these requirements.
 *
 * \par Parameters
 * Please read all the documentation found in
 * AnisotropicDiffusionImageFilter and AnisotropicDiffusionFunction.  Also see
 * VectorGradientNDAnisotropicDiffusionFunction.
 *
 * The maximum allowable time step for this filter is 1/2^N, where N is the
 * dimensionality of the image.  For 2D images any value below 0.250 is stable,
 * and for 3D images, any value below 0.125 is stable.
 *
 * \ingroup ImageEnhancement
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT VectorGradientAnisotropicDiffusionImageFilter
  : public AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef VectorGradientAnisotropicDiffusionImageFilter Self;
  typedef AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>
  Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Instantiation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information. */
  itkTypeMacro(VectorGradientAnisotropicDiffusionImageFilter,
               AnisotropicDiffusionImageFilter);
  
  /** Extract information from the superclass. */
  typedef typename Superclass::UpdateBufferType UpdateBufferType;

  /** Determine the image dimension from the  superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension );
  
protected:
  VectorGradientAnisotropicDiffusionImageFilter()
  {
    typename VectorGradientNDAnisotropicDiffusionFunction<UpdateBufferType>::Pointer p        
      = VectorGradientNDAnisotropicDiffusionFunction<UpdateBufferType>::New();
    this->SetDifferenceFunction(p);
  }
  ~VectorGradientAnisotropicDiffusionImageFilter() {}

private:
  VectorGradientAnisotropicDiffusionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namspace itk

#endif
