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
#include "itkMacro.h"
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
 * The input and output to this filter must be a scalar itk::Image with
 * numerical pixel types (float or double).  A user defined type which
 * correctly defines arithmetic operations with floating point accuracy should
 * also give correct results.
 *
 * \par Parameters
 * Please first read all the documentation found in
 * AnisotropicDiffusionImageFilter and AnisotropicDiffusionFunction.  Also see
 * CurvatureNDAnisotropicDiffusionFunction.
 *
 * The default time step for this filter is set to the maximum theoretically
 * stable value: 0.5 / 2^N, where N is the dimensionality of the image.  For a
 * 2D image, this means valid time steps are below 0.1250.  For a 3D image,
 * valid time steps are below 0.0625.
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

  virtual void InitializeIteration()
  {
    Superclass::InitializeIteration();
    if (this->GetTimeStep() >  0.5 / pow(2.0, static_cast<double>(ImageDimension))  )
      {
      itkWarningMacro(<< "Anisotropic diffusion is using a time step which may introduce instability into the solution." );
      }
  }
  
private:
  CurvatureAnisotropicDiffusionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namspace itk

#endif
