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
#ifndef itkBSplineL2ResampleImageFilterBase_h
#define itkBSplineL2ResampleImageFilterBase_h

#include <vector>
#include "itkBSplineResampleImageFilterBase.h"

namespace itk
{
/** \class BSplineL2ResampleImageFilterBase
 * \brief Uses the "Centered l2" B-Spline pyramid implementation of B-Spline Filters
 *        to up/down sample an image by a factor of 2.
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
 *    [4] P. Brigger, F. Miller, K. Illgner, M. Unser, "Centered Pyramids,"
 *        IEEE Transactions on Image Processing, vol. 8, no. 9, pp. 1254-1264,
 *        September 1999.
 * And code obtained from bigwww.epfl.ch by Philippe Thevenaz
 *
 * Limitations:  Spline order for the centered l2 pyramid must be 0,1,3, or 5.
 *               This code cannot be multi-threaded since the entire image must be
 *                      traversed in the proper order.
 *               This code cannot be streamed and requires the all of the input image.
 *               Only up/down samples by a factor of 2.
 *               This is a base class and is not meant to be instantiated on its own.
 *                    It requires one of the itkBSplineDownsampleImageFilter or
 *                    itkBSplineUpsampleImageFilter classes.
 *               Spline order must be set before setting the image.
 *               Uses mirror boundary conditions.
 *               Requires the same order of Spline for each dimension.
 *
 * \sa itkBSplineDownsampleImageFilter
 * \sa itkBSplineUpsampleImageFilter
 * \sa itkBSplineResampleImageFilterBase
 * \sa itkBSplineCenteredResampleImageFilterBase
 * \sa itkBSplineCenteredL2ResampleImageFilterBase
 *
 * \ingroup GeometricTransformationFilters
 * \ingroup SingleThreaded
 * \ingroup CannotBeStreamed
 * \ingroup ITKImageGrid
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT BSplineL2ResampleImageFilterBase:
  public BSplineResampleImageFilterBase< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef BSplineL2ResampleImageFilterBase                            Self;
  typedef BSplineResampleImageFilterBase< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                        Pointer;
  typedef SmartPointer< const Self >                                  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineL2ResampleImageFilterBase, BSplineResampleImageFilterBase);

protected:

  virtual void InitializePyramidSplineFilter(int SplineOrder) ITK_OVERRIDE;

  BSplineL2ResampleImageFilterBase();
  virtual ~BSplineL2ResampleImageFilterBase() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineL2ResampleImageFilterBase);
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineL2ResampleImageFilterBase.hxx"
#endif

#endif
