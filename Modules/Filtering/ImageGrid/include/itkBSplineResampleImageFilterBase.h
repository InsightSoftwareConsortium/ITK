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
#ifndef itkBSplineResampleImageFilterBase_h
#define itkBSplineResampleImageFilterBase_h

#include <vector>

#include "itkImageLinearIteratorWithIndex.h"
#include "itkImageRegionIterator.h"   // Used for the output iterator needs to
                                      // match filter program
#include "itkProgressReporter.h"
#include "itkImageToImageFilter.h"

namespace itk
{
/** \class BSplineResampleImageFilterBase
 *  \brief Uses the "l2" spline pyramid implementation of B-Spline Filters to
 *        up/down sample an image by a factor of 2.
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
 * Limitations:  Spline order for the l2 pyramid must be between 0 and 3.
 *               This code cannot be multi-threaded since the entire image must be
 *                      traversed in the proper order.
 *               This code cannot be streamed and requires the all of the input image.
 *               Only up/down samples by a factor of 2.
 *               This is a base class and is not meant to be instantiated on its own.
 *                    It requires one of the itkBSplineDownsampleImageFilter or
 *                    itkBSplineUpsampleImageFilter classes.
 *
 * \sa itkBSplineDownsampleImageFilter
 * \sa itkBSplineUpsampleImageFilter
 * \sa itkBSplineCenteredL2ResampleImageFilterBase
 * \sa itkBSplineCenteredResampleImageFilterBase
 * \sa itkBSplineL2ResampleImageFilterBase
 *
 * \ingroup GeometricTransformationFilters
 * \ingroup SingleThreaded
 * \ingroup CannotBeStreamed
 * \ingroup ITKImageGrid
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT BSplineResampleImageFilterBase:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef BSplineResampleImageFilterBase                  Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineResampleImageFilterBase, ImageToImageFilter);

  /** New macro for creation of through a Smart Pointer */
  //  Must be sustantiated through another class. itkNewMacro( Self );

  /** InputInputImage typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Index typedef support. */
  typedef typename TInputImage::IndexType IndexType;

  /** Size typedef support. */
  typedef typename TInputImage::SizeType SizeType;

  /** Size typedef support. */
  typedef typename TInputImage::RegionType RegionType;

  /** OutputImagePixelType typedef support. */
  typedef typename Superclass::OutputImagePixelType OutputImagePixelType;

  /** Iterator typedef support */
  typedef itk::ImageLinearConstIteratorWithIndex< TInputImage > ConstInputImageIterator;

  /** Iterator typedef support */
  typedef itk::ImageLinearConstIteratorWithIndex< TOutputImage > ConstOutputImageIterator;

  /** Output Iterator typedef support */
  typedef itk::ImageLinearIteratorWithIndex< TOutputImage > OutputImageIterator;

  /** Set the spline order for interpolation.  Value must be between 0 and 3 with a
   * default of 0. */
  void SetSplineOrder(int SplineOrder);

  /** Get the spline order */
  itkGetConstMacro(SplineOrder, int);

protected:
  /** Reduces an N-dimension image by a factor of 2 in each dimension. */
  void ReduceNDImage(OutputImageIterator & OutItr);

  /** Expands an N-dimension image by a factor of 2 in each dimension. */
  void ExpandNDImage(OutputImageIterator & OutItr);

  /** Initializes the pyramid spline coefficients.  Called when Spline order
   *   has been set. */
  virtual void InitializePyramidSplineFilter(int SplineOrder);

  /** The basic operator for reducing a line of data by a factor of 2 */
  virtual void Reduce1DImage(
    const std::vector< double > & In,
    OutputImageIterator & Iter,
    unsigned int traverseSize,
    ProgressReporter & progress
    );

  /** The basic operator for expanding a line of data by a factor of 2 */
  virtual void Expand1DImage(
    const std::vector< double > & In,
    OutputImageIterator & Iter,
    unsigned int traverseSize,
    ProgressReporter & progress
    );

  BSplineResampleImageFilterBase();
  virtual ~BSplineResampleImageFilterBase() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  int m_SplineOrder;                      // User specified spline order
  int m_GSize;                            // downsampling filter size
  int m_HSize;                            // upsampling filter size

  std::vector< double >       m_G;        // downsampling filter coefficients
  std::vector< double >       m_H;        // upsampling filter coefficients

private:

  // Resizes m_Scratch Variable based on image sizes
  void InitializeScratch(SizeType DataLength);

  // Copies a line of data from the input to the m_Scratch for subsequent
  // processing
  void CopyInputLineToScratch(ConstInputImageIterator & Iter);

  void CopyOutputLineToScratch(ConstOutputImageIterator & Iter);

  void CopyLineToScratch(ConstInputImageIterator & Iter);

  std::vector< double >       m_Scratch;        // temp storage for processing
                                                // of Coefficients

  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineResampleImageFilterBase);
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineResampleImageFilterBase.hxx"
#endif

#endif
