/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarAnisotropicDiffusionFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
 * \ingroup FiniteDifferenceFunctions
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
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension );

  /** Inherit some parameters from the superclass type. */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::PixelType PixelType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::TimeStepType TimeStepType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarAnisotropicDiffusionFunction,
                AnisotropicDiffusionFunction);
  
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
