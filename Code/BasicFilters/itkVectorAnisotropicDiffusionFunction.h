/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorAnisotropicDiffusionFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorAnisotropicDiffusionFunction_h_
#define __itkVectorAnisotropicDiffusionFunction_h_

#include "itkAnisotropicDiffusionFunction.h"
#include "itkVector.h"

namespace itk {

/** \class VectorAnisotropicDiffusionFunction
 *
 * This class implements a vector-valued version of
 * AnisotropicDiffusionFunction.  Typically in vector-valued diffusion, vector
 * components are diffused independently of one another using a conductance
 * term that is linked across the components. Refer to the the documentation of
 * AnisotropicDiffusionFunction for an overview of anisotropic diffusion.  The
 * way that the conductance term is calculated is specific to the specific type
 * of diffusion function.
 *
 * \par Data type requirements
 * This filter was designed to process itk::Images of itk::Vector type.  The code 
 * relies on various typedefs and overloaded operators defined in itk::Vector.
 * It is perfectly reasonable, however, to apply this filter to images of other,
 * user-defined types as long as the appropriate typedefs and operator overloads
 * are in place.  As a general rule, follow the example of itk::Vector in
 * defining your data types.
 *
 *  \ingroup FiniteDifferenceFunctions
 *  \ingroup ImageEnhancement
 *
 * \sa AnisotropicDiffusionFunction
 * \sa ScalarAnisotropicDiffusionFunction
 * */
template <class TImage>
class ITK_EXPORT VectorAnisotropicDiffusionFunction :
    public AnisotropicDiffusionFunction<TImage>
{
public:
  /** Standard class typedefs. */
  typedef VectorAnisotropicDiffusionFunction   Self;
  typedef AnisotropicDiffusionFunction<TImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(VectorAnisotropicDiffusionFunction,
               AnisotropicDiffusionFunction);
  
  /** Inherit some parameters from the superclass type */
  typedef typename Superclass::ImageType        ImageType;
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::TimeStepType     TimeStepType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;

  /** Inherit some parameters from the superclass type */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension );
  itkStaticConstMacro(VectorDimension, unsigned int, PixelType::Dimension );

  /** Compute the average gradient magnitude squared. */
  virtual void CalculateAverageGradientMagnitudeSquared(TImage *);

protected:
  VectorAnisotropicDiffusionFunction() {}
  ~VectorAnisotropicDiffusionFunction() {}
  void PrintSelf(std::ostream& os, Indent indent) const
    { Superclass::PrintSelf(os,indent); }
private:
  VectorAnisotropicDiffusionFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorAnisotropicDiffusionFunction.txx"
#endif

#endif
