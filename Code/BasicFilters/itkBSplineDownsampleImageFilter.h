/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineDownsampleImageFilter.h
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

#ifndef __itkBSplineDownsampleImageFilter_h
#define __itkBSplineDownsampleImageFilter_h

// include .h for each ResamplerType
#include "itkBSplineResampleImageFilterBase.h"
#include "itkBSplineL2ResampleImageFilterBase.h"
#include "itkBSplineCenteredResampleImageFilterBase.h"
#include "itkBSplineCenteredL2ResampleImageFilterBase.h"

namespace itk
{
/** \class BSplineDownsampleImageFilter
 * \brief Down-samples an image by a factor of 2 using B-Spline filter interpolation.
 *
 * This class is the public interface for spline down-sampling as defined by the
 *   ResamplerType.
 * Requires the use of a resampler type.  If in doubt, the basic itkBSplineResampleImageFilterBase
 *   should work fine for most applications.
 *
 *  This class may also be used to create a smoother by combining it with the upSampler
 *  as in the following example:
 *      typedef itk::BSplineResampleImageFilterBase<ImageType2D, ImageType2D> ResamplerType;
 *      typedef itk::BSplineDownsampleImageFilter<ImageType2D,ImageType2D,ResamplerType> DownsamplerType2D;
 *      typedef itk::BSplineUpsampleImageFilter<ImageType2D,ImageType2D,ResamplerType> UpsamplerType2D;
 *
 *      DownsamplerType2D::Pointer downSampler = DownsamplerType2D::New();
 *      UpsamplerType2D::Pointer   upSampler =   UpsamplerType2D::New();
 *      int splineOrder = 3;
 *      downSampler->SetSplineOrder(splineOrder);
 *      upSampler->SetSplineOrder(splineOrder);
 *
 *      downSampler->SetInput(image);
 *      downSampler->Update();
 *
 *      upSampler->SetInput( downSampler->GetOutput() );   // output of downSampler is input to upSampler
 *      upSampler->Update();
 *
 *      ImageTypePtr2D outImage2 = upSampler->GetOutput();  // outImage2 is the smoothed imaged
 *
 * Limitations:  This class requires specification of a resampler type which may 
 *                      be one of:
 *                        itkBSplineResampleImageFilterBase,
 *                        itkBSplineL2ResampleImageFilterBase
 *                        itkBSplineSplineCenteredResampleImageFilterBase,
 *                        itkBSplineCenteredL2ResampleImageFilterBase
 *               The limitations of these resampler types will apply to this filter.
 *               Downsamples only by a factor of 2.
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
template <class TInputImage, class TOutputImage, 
          class ResamplerType = BSplineResampleImageFilterBase<TInputImage, TOutputImage> >
class ITK_EXPORT BSplineDownsampleImageFilter : 
    public ResamplerType
{
public:
  /** Standard class typedefs. */
  typedef BSplineDownsampleImageFilter       Self;
  typedef ResamplerType                      Superclass;
  typedef SmartPointer<Self>                 Pointer;
  typedef SmartPointer<const Self>           ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineDownsampleImageFilter, ResamplerType);

 
  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** InputImagePointer typedef support. */
  typedef typename Superclass::InputImagePointer InputImagePointer;

  /** OutputImagePointer typedef support. */
  typedef typename Superclass::OutputImagePointer OutputImagePointer;

  /** OutputImageIterator typedef support. */
  typedef typename Superclass::OutputImageIterator OutputImageIterator;

  /** Creates an image half the size of the input image with spacing twice the 
    * input image. */
  void GenerateOutputInformation();

  /** This filter requires all of the input image */
  void GenerateInputRequestedRegion();

protected:

  void GenerateData();
  void EnlargeOutputRequestedRegion( DataObject *output );

  BSplineDownsampleImageFilter();
  virtual ~BSplineDownsampleImageFilter() {};
    
private:
  BSplineDownsampleImageFilter( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented
  
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineDownsampleImageFilter.txx"
#endif

#endif
