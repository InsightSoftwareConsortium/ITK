/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinMaxCurvatureFlowImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMinMaxCurvatureFlowImageFilter_h_
#define __itkMinMaxCurvatureFlowImageFilter_h_

#include "itkCurvatureFlowImageFilter.h"
#include "itkMinMaxCurvatureFlowFunction.h"

namespace itk {

/** \class MinMaxCurvatureFlowImageFilter
  * \brief Denoise an image using min/max curvature flow.
  *
  * MinMaxCurvatureFlowImageFilter implements a curvature driven image denoising 
  * algorithm. Iso-brightness contours in the grayscale input image are viewed 
  * as a level set. The level set is then evolved using a curvature-based speed 
  * function:
  *
  * \f[  I_t = F_{\mbox{minmax}} |\nabla I| \f]
  *
  * where \f$ F_{\mbox{minmax}} = \max(\kappa,0) \f$ if 
  * \f$ \mbox{Avg}_{\mbox{stencil}}(x) \f$ is less than or equal to \f$ T_{thresold} \f$ 
  * and \f$ \min(\kappa,0) \f$, otherwise. 
  * \f$ \kappa \f$ is the mean curvature of the iso-brightness contour
  * at point \f$ x \f$.
  *
  * In min/max curvature flow, movement is turned on or off depending
  * on the scale of the noise one wants to remove. Switching depends on
  * the average image value of a region of radius \f$ R \f$ around each 
  * point. The choice of \f$ R \f$, the stencil radius, governs the scale of
  * the noise to be removed.
  *
  * The threshold value \f$ T_{threshold} \f$ is the average intensity obtained
  * in the direction perpendicular to the gradient at point \f$ x \f$ at the
  * extrema of the local neighborhood.
  *
  * This filter make use of the multi-threaded finite difference solver
  * hierarchy.  Updates are computed using a MinMaxCurvatureFlowFunction object.
  * A zero flux Neumann boundary condition is used when computing derivatives 
  * near the data boundary.
  *
  * \warning This filter assumes that the input and output types have the
  * same dimensions. This filter also requires that the output image pixels
  * are of a real type. This filter works for any dimensional images, 
  * however for dimensions greater than 3D, an expensive brute-force search 
  * is used to compute the local threshold.
  *
  * Reference:
  * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
  * Cambridge Press, Chapter 16, Second edition, 1999.
  *
  * \sa MinMaxCurvatureFlowFunction
  * \sa CurvatureFlowImageFilter
  * \sa BinaryMinMaxCurvatureFlowImageFilter
  *
  * \ingroup ImageEnhancement 
  * \ingroup Multithreaded
  * 
  */
template <class TInputImage, class TOutputImage>
class MinMaxCurvatureFlowImageFilter
  : public CurvatureFlowImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef MinMaxCurvatureFlowImageFilter Self;
  typedef CurvatureFlowImageFilter<TInputImage, TOutputImage>
   Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MinMaxCurvatureFlowImageFilter,
               CurvatureFlowImageFilter);

  /** Inherit typedefs from Superclass. */
  typedef typename Superclass::FiniteDifferenceFunctionType
    FiniteDifferenceFunctionType;
  typedef typename Superclass::OutputImageType OutputImageType;

  /** MinMaxCurvatureFlowFunction type. */
  typedef MinMaxCurvatureFlowFunction<OutputImageType>
    MinMaxCurvatureFlowFunctionType;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Typedef support for the neighbour radius. */
  typedef typename FiniteDifferenceFunctionType::RadiusType RadiusType;
  typedef typename RadiusType::SizeValueType RadiusValueType;

  /** Set/Get the stencil radius. */
  itkSetMacro( StencilRadius, RadiusValueType );
  itkGetMacro( StencilRadius, RadiusValueType );
  
protected:
  MinMaxCurvatureFlowImageFilter();
  ~MinMaxCurvatureFlowImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** Initialize the state of filter and equation before each iteration.
   * Progress feeback is implemented as part of this method. */
  virtual void InitializeIteration();
  
private:
  MinMaxCurvatureFlowImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  RadiusValueType    m_StencilRadius;
  
};

} // end namspace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMinMaxCurvatureFlowImageFilter.txx"
#endif

#endif
