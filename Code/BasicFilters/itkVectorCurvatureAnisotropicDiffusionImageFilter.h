/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorCurvatureAnisotropicDiffusionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorCurvatureAnisotropicDiffusionImageFilter_h_
#define __itkVectorCurvatureAnisotropicDiffusionImageFilter_h_

#include "itkExceptionObject.h"
#include "itkAnisotropicDiffusionImageFilter.h"
#include "itkVectorCurvatureNDAnisotropicDiffusionFunction.h"

namespace itk {

/** \class VectorCurvatureAnisotropicDiffusionImageFilter
 * 
 * \ingroup ImageEnhancement
 *
 *\todo Document.
 */
template <class TInputImage, class TOutputImage>
class VectorCurvatureAnisotropicDiffusionImageFilter
  : public AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard itk typedefs */
  typedef VectorCurvatureAnisotropicDiffusionImageFilter Self;
  typedef AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>
   Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Instantiation through object factory. */
  itkNewMacro(Self);

  /** Run-time type information. */
  itkTypeMacro(VectorCurvatureAnisotropicDiffusionImageFilter,
               AnisotropicDiffusionImageFilter);
  
  /** Convenient typedef. */
  typedef typename Superclass::UpdateBufferType UpdateBufferType;

  /** Determine the image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension );
  
protected:
  VectorCurvatureAnisotropicDiffusionImageFilter()
    {
    typename VectorCurvatureNDAnisotropicDiffusionFunction<UpdateBufferType>::Pointer q
        = VectorCurvatureNDAnisotropicDiffusionFunction<UpdateBufferType>::New();
      this->SetDifferenceFunction(q);
    }
  ~VectorCurvatureAnisotropicDiffusionImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const
    { Superclass::PrintSelf(os,indent);  }
private:
  VectorCurvatureAnisotropicDiffusionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namspace itk

#endif
