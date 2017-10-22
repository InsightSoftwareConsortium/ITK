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
#ifndef itkBinaryMinMaxCurvatureFlowImageFilter_h
#define itkBinaryMinMaxCurvatureFlowImageFilter_h

#include "itkMinMaxCurvatureFlowImageFilter.h"
#include "itkBinaryMinMaxCurvatureFlowFunction.h"

namespace itk
{
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
 * \ingroup MultiThreaded
 *
 * \ingroup ITKCurvatureFlow
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT BinaryMinMaxCurvatureFlowImageFilter:
  public MinMaxCurvatureFlowImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef BinaryMinMaxCurvatureFlowImageFilter                        Self;
  typedef MinMaxCurvatureFlowImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                        Pointer;
  typedef SmartPointer< const Self >                                  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryMinMaxCurvatureFlowImageFilter, MinMaxCurvatureFlowImageFilter);

  /** Inherit typedefs from Superclass. */
  typedef typename Superclass::FiniteDifferenceFunctionType FiniteDifferenceFunctionType;
  typedef typename Superclass::OutputImageType              OutputImageType;

  /** BinaryMinMaxCurvatureFlowFunction type. */
  typedef BinaryMinMaxCurvatureFlowFunction< OutputImageType > BinaryMinMaxCurvatureFlowFunctionType;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Set/Get the threshold value. */
  itkSetMacro(Threshold, double);
  itkGetConstMacro(Threshold, double);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< typename TInputImage::PixelType,
                                           typename TOutputImage::PixelType > ) );
  // End concept checking
#endif

protected:

protected:
  BinaryMinMaxCurvatureFlowImageFilter();
  ~BinaryMinMaxCurvatureFlowImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Initialize the state of filter and equation before each iteration.
   * Progress feeback is implemented as part of this method. */
  virtual void InitializeIteration() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryMinMaxCurvatureFlowImageFilter);

  double m_Threshold;
};
} // end namspace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryMinMaxCurvatureFlowImageFilter.hxx"
#endif

#endif
