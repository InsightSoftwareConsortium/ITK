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
  * algorithm. Iso-brightness contours in the input image are viewed as a 
  * level set. The level set is then evolved using a curvature-based speed 
  * function:
  *
  * \f[  I_t = \kappa |\nabla I| \f]
  *
  * where \f$ \kappa \f$ is the curvature.
  * 
  * However, continuous application of this scheme will result in the
  * eventual removal of all information as each contour shrinks to zero
  * and disappear.
  *
  * In min/max curvature flow, curvature flow is turned on or off depending
  * on the scale of the noise one wants to remove. Switching depends on
  * the average image value of a region of radius \f$ R \f$ around each 
  * point. The choice of \f$ R \f$, the stencil radius, governs the scale of
  * the noise to be removed.
  *
  * This filter make use of the multi-threaded finite difference solver
  * hierarchy.  Updates are computed using a MinMaxCurvatureFlowFunction object. A
  * zero flux Neumann boundary condition when computing derivatives near the
  * data boundary.
  *
  * \warning This filter assumes that the input and output types have the
  * same dimensions. This filter also requires that the output image pixels
  * are of a floating point type. This filter works for any dimensional images, 
  * however for dimensions greater than 3D, an expensive brute-force search is used
  * to compute the local threshold.
  *
  * Reference:
  * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
  * Cambridge Press, Chapter 16, Second edition, 1999.
  *
  * \sa MinMaxCurvatureFlowFunction
  * \sa CurvatureFlowImageFilter
  *
  * \ingroup ImageEnhancement Multithreaded
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
  enum { ImageDimension = Superclass::ImageDimension };

  /** Typedef support for the neighbour radius. */
  typedef typename FiniteDifferenceFunctionType::RadiusType RadiusType;
  typedef typename RadiusType::SizeValueType RadiusValueType;

  /** Set/Get the stencil radius. */
  itkSetMacro( StencilRadius, RadiusValueType );
  itkGetMacro( StencilRadius, RadiusValueType );

  /** Set/Get use block neighborhood boolean flag. If true,
   * an N-dimensional block neighborhood is used to compute the
   * neighborhood average. If false, an N-dimensional spherical
   * neighborhood is used. Using a spherical neighborhood is more
   * computationally demanding than using a block neighborhood. */
  itkSetMacro( UseBlockNeighborhood, bool );
  itkGetMacro( UseBlockNeighborhood, bool );
  itkBooleanMacro( UseBlockNeighborhood );
  
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
  bool               m_UseBlockNeighborhood;
  
};

} // end namspace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMinMaxCurvatureFlowImageFilter.txx"
#endif

#endif
