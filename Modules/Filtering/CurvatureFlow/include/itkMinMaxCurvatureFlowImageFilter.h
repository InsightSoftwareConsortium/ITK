/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMinMaxCurvatureFlowImageFilter_h
#define itkMinMaxCurvatureFlowImageFilter_h

#include "itkCurvatureFlowImageFilter.h"
#include "itkMinMaxCurvatureFlowFunction.h"

namespace itk
{
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
 * \ingroup MultiThreaded
 *
 * \ingroup ITKCurvatureFlow
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT MinMaxCurvatureFlowImageFilter:
  public CurvatureFlowImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef MinMaxCurvatureFlowImageFilter                        Self;
  typedef CurvatureFlowImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MinMaxCurvatureFlowImageFilter, CurvatureFlowImageFilter);

  /** Inherit typedefs from Superclass. */
  typedef typename Superclass::FiniteDifferenceFunctionType FiniteDifferenceFunctionType;
  typedef typename Superclass::OutputImageType              OutputImageType;

  /** MinMaxCurvatureFlowFunction type. */
  typedef MinMaxCurvatureFlowFunction< OutputImageType >
  MinMaxCurvatureFlowFunctionType;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Typedef support for the neighbour radius. */
  typedef typename FiniteDifferenceFunctionType::RadiusType RadiusType;
  typedef typename RadiusType::SizeValueType                RadiusValueType;

  /** Set/Get the stencil radius. */
  itkSetMacro(StencilRadius, RadiusValueType);
  itkGetConstMacro(StencilRadius, RadiusValueType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( UnsignedLongConvertibleToOutputCheck,
                   ( Concept::Convertible< unsigned long, typename TOutputImage::PixelType > ) );
  itkConceptMacro( OutputLessThanComparableCheck,
                   ( Concept::LessThanComparable< typename TOutputImage::PixelType > ) );
  itkConceptMacro( LongConvertibleToOutputCheck,
                   ( Concept::Convertible< long, typename TOutputImage::PixelType > ) );
  itkConceptMacro( OutputDoubleComparableCheck,
                   ( Concept::Comparable< typename TOutputImage::PixelType, double > ) );
  itkConceptMacro( OutputDoubleMultiplyAndAssignOperatorCheck,
                   ( Concept::MultiplyAndAssignOperator< typename TOutputImage::PixelType,
                                                         double > ) );
  itkConceptMacro( OutputGreaterThanUnsignedLongCheck,
                   ( Concept::GreaterThanComparable< typename TOutputImage::PixelType,
                                                     unsigned long > ) );
  itkConceptMacro( UnsignedLongOutputAditiveOperatorsCheck,
                   ( Concept::AdditiveOperators< unsigned long,
                                                 typename TOutputImage::PixelType > ) );
  // End concept checking
#endif

protected:
  MinMaxCurvatureFlowImageFilter();
  ~MinMaxCurvatureFlowImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Initialize the state of filter and equation before each iteration.
   * Progress feeback is implemented as part of this method. */
  virtual void InitializeIteration() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MinMaxCurvatureFlowImageFilter);

  RadiusValueType m_StencilRadius;
};
} // end namspace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMinMaxCurvatureFlowImageFilter.hxx"
#endif

#endif
