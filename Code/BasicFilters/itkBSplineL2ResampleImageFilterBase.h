/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineL2ResampleImageFilterBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBSplineL2ResampleImageFilterBase_h
#define __itkBSplineL2ResampleImageFilterBase_h

#include <vector>

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
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT BSplineL2ResampleImageFilterBase : 
    public BSplineResampleImageFilterBase<TInputImage,TOutputImage>  
{
public:
  /** Standard class typedefs. */
  typedef BSplineL2ResampleImageFilterBase       Self;
  typedef BSplineResampleImageFilterBase<TInputImage, TOutputImage>  Superclass;
  typedef SmartPointer<Self>                    Pointer;
  typedef SmartPointer<const Self>              ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineL2ResampleImageFilterBase, BSplineResampleImageFilterBase);

 
protected:

  virtual void InitializePyramidSplineFilter(int SplineOrder);

  BSplineL2ResampleImageFilterBase();
  virtual ~BSplineL2ResampleImageFilterBase() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
    

private:
  BSplineL2ResampleImageFilterBase( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented
  
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineL2ResampleImageFilterBase.txx"
#endif

#endif
