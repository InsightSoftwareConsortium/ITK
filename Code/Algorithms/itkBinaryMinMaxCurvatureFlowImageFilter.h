/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryMinMaxCurvatureFlowImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryMinMaxCurvatureFlowImageFilter_h_
#define __itkBinaryMinMaxCurvatureFlowImageFilter_h_

#include "itkMinMaxCurvatureFlowImageFilter.h"
#include "itkBinaryMinMaxCurvatureFlowFunction.h"

namespace itk {

/** \class BinaryMinMaxCurvatureFlowImageFilter
  * \brief Denoise a binary image using min/max curvature flow.
  *
  * BinaryMinMaxCurvatureFlowImageFilter implements a curvature driven image  
  * denosing algorithm. This filter assumes that the image is essentially 
  * binary: consisting of two classes. Iso-brightness contours in the input 
  * image are viewed as a level set. The level set is then evolved using 
  * a curvature-based speed function:
  *
  * \f[  I_t = F_{\mbox{minmax}} |\nabla I| \f]
  *
  * where \f$ F_{\mbox{minmax}} = \min(\kappa,0) \f$ if 
  * \f$ \mbox{Avg}_{\mbox{stencil}}(x) \f$ 
  * is less than or equal to \f$ T_{thresold} \f$ 
  * and \f$ \max(\kappa,0) \f$, otherwise. 
  * \f$ \kappa \f$ is the mean curvature of the iso-brightness contour
  * at point \f$ x \f$.
  *
  * In min/max curvature flow, movement is turned on or off depending
  * on the scale of the noise one wants to remove. Switching depends on
  * the average image value of a region of radius \f$ R \f$ around each 
  * point. The choice of \f$ R \f$, the stencil radius, governs the scale of
  * the noise to be removed.
  *
  * The threshold value \f$ T_{threshold} \f$ is a user specified value which
  * discriminates between the two pixel classes.
  *
  * This filter make use of the multi-threaded finite difference solver
  * hierarchy.  Updates are computed using a BinaryMinMaxCurvatureFlowFunction 
  * object. A zero flux Neumann boundary condition is used when computing 
  * derivatives near the data boundary.
  *
  * \warning This filter assumes that the input and output types have the
  * same dimensions. This filter also requires that the output image pixels
  * are of a real type. This filter works for any dimensional images. 
  *
  * Reference:
  * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
  * Cambridge Press, Chapter 16, Second edition, 1999.
  *
  * \sa BinaryMinMaxCurvatureFlowFunction
  * \sa CurvatureFlowImageFilter
  * \sa MinMaxCurvatureFlowImageFilter
  *
  * \ingroup ImageEnhancement 
  * \ingroup Multithreaded
  * 
  */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT BinaryMinMaxCurvatureFlowImageFilter
  : public MinMaxCurvatureFlowImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef BinaryMinMaxCurvatureFlowImageFilter Self;
  typedef MinMaxCurvatureFlowImageFilter<TInputImage, TOutputImage>
  Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryMinMaxCurvatureFlowImageFilter,
               MinMaxCurvatureFlowImageFilter);

  /** Inherit typedefs from Superclass. */
  typedef typename Superclass::FiniteDifferenceFunctionType
  FiniteDifferenceFunctionType;
  typedef typename Superclass::OutputImageType OutputImageType;

  /** BinaryMinMaxCurvatureFlowFunction type. */
  typedef BinaryMinMaxCurvatureFlowFunction<OutputImageType>
  BinaryMinMaxCurvatureFlowFunctionType;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** Set/Get the threshold value. */
  itkSetMacro( Threshold, double );
  itkGetMacro( Threshold, double );
  
protected:
  BinaryMinMaxCurvatureFlowImageFilter();
  ~BinaryMinMaxCurvatureFlowImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** Initialize the state of filter and equation before each iteration.
   * Progress feeback is implemented as part of this method. */
  virtual void InitializeIteration();
  
private:
  BinaryMinMaxCurvatureFlowImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  double             m_Threshold;
  
};

} // end namspace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryMinMaxCurvatureFlowImageFilter.txx"
#endif

#endif
