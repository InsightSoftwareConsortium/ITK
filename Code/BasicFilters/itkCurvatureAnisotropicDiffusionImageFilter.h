/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCurvatureAnisotropicDiffusionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCurvatureAnisotropicDiffusionImageFilter_h_
#define __itkCurvatureAnisotropicDiffusionImageFilter_h_

#include "itkAnisotropicDiffusionImageFilter.h"
#include "itkCurvatureNDAnisotropicDiffusionFunction.h"

namespace itk {

/**
 * \class CurvatureAnisotropicDiffusionImageFilter
 *
 * This filter performs anisotropic diffusion on a scalar itk::Image using the
 * modified curvature diffusion equation (MCDE) implemented in
 * itkCurvatureNDAnisotropicDiffusionFunction.  For detailed information on
 * anisotropic diffusion and the MCDE see itkAnisotropicDiffusionFunction and
 * itkCurvatureNDAnisotropicDiffusionFunction.
 *
 * \par Inputs and Outputs
 * The input to this filter should be a scalar itk::Image of any
 * dimensionality.  The output image will be a diffused copy of the input.
 *
 * \sa AnisotropicDiffusionImageFilter
 * \sa AnisotropicDiffusionFunction
 * \sa CurvatureNDAnisotropicDiffusionFunction
 * \ingroup ImageEnhancement */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT CurvatureAnisotropicDiffusionImageFilter
  : public AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef CurvatureAnisotropicDiffusionImageFilter Self;
  typedef AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>
   Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Standard method for creation through object factory. */
  itkNewMacro(Self);

  /** Run-time information. */
  itkTypeMacro(CurvatureAnisotropicDiffusionImageFilter,
               AnisotropicDiffusionImageFilter);
  
  /** Extract superclass information. */
  typedef typename Superclass::UpdateBufferType UpdateBufferType;
  
  /** Extract superclass image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);
  
protected:
  CurvatureAnisotropicDiffusionImageFilter()
    {
      typename CurvatureNDAnisotropicDiffusionFunction<UpdateBufferType>::Pointer q
          = CurvatureNDAnisotropicDiffusionFunction<UpdateBufferType>::New();
      this->SetDifferenceFunction(q);
    }
  ~CurvatureAnisotropicDiffusionImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const
    {
      Superclass::PrintSelf(os,indent);
    }

private:
  CurvatureAnisotropicDiffusionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namspace itk

#endif
