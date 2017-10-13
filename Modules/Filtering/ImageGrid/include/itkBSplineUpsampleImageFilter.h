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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkBSplineUpsampleImageFilter_h
#define itkBSplineUpsampleImageFilter_h

// include .h for each ResamplerType
#include "itkBSplineL2ResampleImageFilterBase.h"
#include "itkBSplineCenteredL2ResampleImageFilterBase.h"

namespace itk
{
/** \class BSplineUpsampleImageFilter
 * \brief Uses B-Spline interpolation to upsample an image by a factor of 2.
 * This class is the public interface for spline upsampling as defined by the
 * ResamplerType.
 *
 * Requires the use of a resampler type.  If in doubt, the basic itkBSplineResampleImageFilterBase
 *   should work fine for most applications.
 *
 * This class defines N-Dimension B-Spline transformation.
 * It is based on:
 *    [1] M. Unser,
 *       "Splines: A Perfect Fit for Signal and Image Processing,"
 *        IEEE Signal Processing Magazine, vol. 16, no. 6, pp. 22-38,
 *        November 1999.
 *    [2] M. Unser, A. Aldroubi and M. Eden,
 *        "B-Spline Signal Processing: Part I--Theory,"
 *        IEEE Transactions on Signal Processing, vol. 41, no. 2, pp. 821-832,
 *        February 1993.
 *    [3] M. Unser, A. Aldroubi and M. Eden,
 *        "B-Spline Signal Processing: Part II--Efficient Design and Applications,"
 *        IEEE Transactions on Signal Processing, vol. 41, no. 2, pp. 834-848,
 *        February 1993.
 * And code obtained from bigwww.epfl.ch by Philippe Thevenaz
 *
 * Limitations:  This class requires specification of a resampler type which may
 *                      be one of:
 *                        BSplineResampleImageFilterBase,
 *                        BSplineL2ResampleImageFilterBase
 *                        BSplineSplineCenteredResampleImageFilterBase,
 *                        BSplineCenteredL2ResampleImageFilterBase
 *               The limitations of these resampler types will apply to this filter.
 *               Upsamples only by a factor of 2.
 *
 * \sa BSplineDownsampleImageFilter
 * \sa BSplineL2ResampleImageFilter
 * \sa BSplineResampleImageFilterBase
 * \sa BSplineCenteredResampleImageFilterBase
 * \sa BSplineCenteredL2ResampleImageFilterBase
 *
 * \ingroup GeometricTransformationFilters
 * \ingroup SingleThreaded
 * \ingroup CannotBeStreamed
 * \ingroup ITKImageGrid
 */

template< typename TInputImage, typename TOutputImage, typename ResamplerType =
            BSplineResampleImageFilterBase< TInputImage, TOutputImage > >
class ITK_TEMPLATE_EXPORT BSplineUpsampleImageFilter:
  public ResamplerType
{
public:
  /** Standard class typedefs. */
  typedef BSplineUpsampleImageFilter Self;
  typedef ResamplerType              Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineUpsampleImageFilter, ReamplerType);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** InputImagePointer typedef support. */
  typedef typename Superclass::InputImagePointer InputImagePointer;

  /** OutputImageIterator typedef support. */
  typedef typename Superclass::OutputImageIterator OutputImageIterator;

  /** OutputImagePointer typedef support. */
  typedef typename Superclass::OutputImagePointer OutputImagePointer;

  /** Creates an image twice the size of the input image with spacing half the
    * input image. */
  void GenerateOutputInformation() ITK_OVERRIDE;

  /** This filter requires all of the input image */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( DoubleConvertibleToOutputCheck,
                   ( Concept::Convertible< double, typename TOutputImage::PixelType > ) );
  // End concept checking
#endif

protected:

  void GenerateData() ITK_OVERRIDE;

  void EnlargeOutputRequestedRegion(DataObject *output) ITK_OVERRIDE;

  BSplineUpsampleImageFilter();
  virtual ~BSplineUpsampleImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineUpsampleImageFilter);
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineUpsampleImageFilter.hxx"
#endif

#endif
