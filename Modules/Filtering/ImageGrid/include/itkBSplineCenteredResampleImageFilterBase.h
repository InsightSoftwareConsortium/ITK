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
#ifndef itkBSplineCenteredResampleImageFilterBase_h
#define itkBSplineCenteredResampleImageFilterBase_h

#include "itkBSplineResampleImageFilterBase.h"
#include "itkProgressReporter.h"
#include <vector>

namespace itk
{

/** \class BSplineCenteredResampleImageFilterBase
 * \brief Evaluates the Centered B-Spline interpolation of an image.  Spline order may be from 0 to 5.
 *
 * This class defines N-Dimension CenteredB-Spline transformation.
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
 * Limitations:  Spline order must be between 0 and 5.
 *               Spline order must be set before setting the image.
 *               Uses mirror boundary conditions.
 *               Requires the same order of Spline for each dimension.
 * \ingroup ITKImageGrid
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT BSplineCenteredResampleImageFilterBase:
  public BSplineResampleImageFilterBase< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef BSplineCenteredResampleImageFilterBase                      Self;
  typedef BSplineResampleImageFilterBase< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                        Pointer;
  typedef SmartPointer< const Self >                                  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineCenteredResampleImageFilterBase, BSplineResampleImageFilterBase);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** OutputImagePixelType typedef support. */
  typedef typename Superclass::OutputImagePixelType OutputImagePixelType;

  /** OutputImageIterator typedef support. */
  typedef typename Superclass::OutputImageIterator OutputImageIterator;

protected:

  virtual void InitializePyramidSplineFilter(int SplineOrder) ITK_OVERRIDE;

  virtual void Reduce1DImage(
    const std::vector< double > & In,
    OutputImageIterator & Iter,
    unsigned int traverseSize,
    ProgressReporter & progress
    ) ITK_OVERRIDE;

  virtual void Expand1DImage(
    const std::vector< double > & In,
    OutputImageIterator & Iter,
    unsigned int traverseSize,
    ProgressReporter & progress
    ) ITK_OVERRIDE;

protected:
  BSplineCenteredResampleImageFilterBase();
  virtual ~BSplineCenteredResampleImageFilterBase() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineCenteredResampleImageFilterBase);

  typedef typename TInputImage::IndexValueType    IndexValueType;
  typedef typename TInputImage::SizeValueType     SizeValueType;

  // implemented
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineCenteredResampleImageFilterBase.hxx"
#endif

#endif
