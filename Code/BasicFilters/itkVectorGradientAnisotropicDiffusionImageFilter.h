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
 * \ingroup ImageEnhancement
 * \ingroup GradientFilters
 *
 *\todo Document.
 */
template <class TInputImage, class TOutputImage>
class VectorGradientAnisotropicDiffusionImageFilter
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
