/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineUpsampleImageFilter.h
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

#ifndef __itkBSplineUpsampleImageFilter_h
#define __itkBSplineUpsampleImageFilter_h

// include .h for each ResamplerType
#include "itkBSplineResampleImageFilterBase.h"
#include "itkBSplineL2ResampleImageFilterBase.h"
#include "itkBSplineCenteredResampleImageFilterBase.h"
#include "itkBSplineCenteredL2ResampleImageFilterBase.h"

namespace itk
{
/** \class BSplineUpsampleImageFilterBase
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
 *                        itkBSplineResampleImageFilterBase,
 *                        itkBSplineL2ResampleImageFilterBase
 *                        itkBSplineSplineCenteredResampleImageFilterBase,
 *                        itkBSplineCenteredL2ResampleImageFilterBase
 *               The limitations of these resampler types will apply to this filter.
 *               Upsamples only by a factor of 2.
 *
 * \sa itkBSplineDownsampleImageFilter
 * \sa itkBSplineL2ResampleImageFilter
 * \sa itkBSplineResampleImageFilterBase
 * \sa itkBSplineCenteredResampleImageFilterBase
 * \sa itkBSplineCenteredL2ResampleImageFilterBase
 *
 * \ingroup GeometricTransformationFilters
 * \ingroup SingleThreaded
 * \ingroup CannotBeStreamed 
 */

//= ITK_TYPENAME BSplineResampleImageFilterBase<TInputImage, TOutputImage> 
template <class TInputImage, class TOutputImage, class ResamplerType = BSplineResampleImageFilterBase<TInputImage, TOutputImage> >
class ITK_EXPORT BSplineUpsampleImageFilter : 
    public ResamplerType 
{
public:
  /** Standard class typedefs. */
  typedef BSplineUpsampleImageFilter       Self;
  typedef ResamplerType                    Superclass;
  typedef SmartPointer<Self>               Pointer;
  typedef SmartPointer<const Self>         ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineUpsampleImageFilter, ReamplerType);

 
  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

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
  void GenerateOutputInformation();

  /** This filter requires all of the input image */
  void GenerateInputRequestedRegion();

protected:

  void GenerateData();
  void EnlargeOutputRequestedRegion( DataObject *output );

  BSplineUpsampleImageFilter();
  virtual ~BSplineUpsampleImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
    
private:
  BSplineUpsampleImageFilter( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented
  
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineUpsampleImageFilter.txx"
#endif

#endif
