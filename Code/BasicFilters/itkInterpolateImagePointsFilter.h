/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInterpolateImagePointsFilter.h
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

#ifndef __itkInterpolateImagePointsFilter_h
#define __itkInterpolateImagePointsFilter_h

#include "itkBSplineInterpolateImageFunction.h"
#include "itkImageToImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{
/** \class InterpolateImagePointsFilter
 *  \brief Resamples an image at the coordinates specified by the user.
 *
 *  This class may be templated over the Interpolate Function but defaults
 *    to the BSplineInterpolateImageFunction for cubic interpolation.
 *    The user must set the image using the SetInputImage function and they 
 *    must set the coordinates (one coordinate "image" for each dimension) using 
 *    SetInterpolationCoordinate().  The coordinates may be any number of points and can 
 *    be randomly organized.  The interpolated output will correspond to the 
 *    ordering of the coordinate points.  The coordinates must be of type
 *    ContinuousIndexType and not of PointType.
 *
 *  This function is different from the resampleImageFilter class in that the 
 *    resampleImageFilter applies a transform to the original input image
 *    coordinates.  This function takes an arbitrary set of point coordinates
 *    and applies the transform at these locations.
 * 
 * Limitations:  The coordinates must be in an image of the same dimension as the 
 *       input image.  There is no reason why this should be the case and future 
 *       revisions should look at eliminating this limitation. 
 *    Currently TCoordType must be the same as the input pixel type (TInputImage).  
 *       Again future revisions should look at eliminating this limitation.
 *    Though the output generation may be streamed the entire input image, 
 *       must be supplied. The coordinates may be streamed in smaller blocks.
 *    The coordinates are specified as seperate images for each dimension.  
 *    The coordinates are treated as Continuous Indicies. If coordinates are
 *    supplied as Points then they must be converted to an Index before passing
 *       to this class.
 *
 * \sa BSplineInterpolateImageFunction
 * \sa ResampleImageFilter
 *
 * \ingroup GeometricTransformationFilters
 * \ingroup MultiThreaded
 * \ingroup CanBeStreamed 
 *
 */

template <class TInputImage, 
  class TOutputImage, 
  class TCoordType = float, 
  class InterpolatorType = BSplineInterpolateImageFunction<TInputImage, TCoordType> > 
class ITK_EXPORT InterpolateImagePointsFilter : 
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef InterpolateImagePointsFilter       Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                 Pointer;
  typedef SmartPointer<const Self>           ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(InterpolateImagePointsFilter, ImageToImageFilter);

 
  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** ImageDimension enumeration. */
  enum { ImageDimension = TInputImage::ImageDimension };

  /** Typedefs to describe and access input image */
  typedef typename Superclass::InputImageType InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::InputImagePointer InputImagePointer;

  /** Typedefs to describe and access output image. */
  typedef typename TOutputImage::Pointer OutputImagePointer;
  typedef itk::ImageRegionIterator<InputImageType> OutputImageIterator;
  typedef typename OutputImageType::RegionType             OutputImageRegionType;

  /** Image pixel value typedef. */
  typedef typename TOutputImage::PixelType   PixelType;



  /** Typedefs to describe and access Interpolator */
  typedef typename InterpolatorType::Pointer InterpolatorPointer;
  typedef typename InterpolatorType::ContinuousIndexType    ContinuousIndexType;

  /** Typedefs to describe and access coordinate images */
  typedef typename itk::Image< TCoordType, ImageDimension > CoordImageType;

  /** Typedef for region copier */
  typedef typename ImageToImageFilterDetail
    ::ImageRegionCopier<ImageDimension,ImageDimension>     RegionCopierType; 

  /** SetInputImage is used to set the image to be interpolated.
    * Note that this should be used instead of the direct setInput
    * as multiple inputs are needed and this class keeps track of
    * the ordering. */
  void SetInputImage (const TInputImage * inputImage);

  /** SetInterpolationCoordinate must be called for each dimension.  The variable setDimension
    * is used to identify the dimension being set, i.e. 0,1,2...N */
  void SetInterpolationCoordinate (const CoordImageType * coordinate, unsigned int setDimension);

  /** Set the pixel value when a transformed pixel is outside of the image */
  itkSetMacro(DefaultPixelValue,PixelType);

  /** Get the pixel value when a transformed pixel is outside of the image */
  itkGetMacro(DefaultPixelValue,PixelType);

  /** Returns a pointer to the  interpolator. */
  InterpolatorPointer GetInterpolator()
    {  return m_Interpolator;  }
   
  /** Overloaded to ensure that output is sized the same as the coordinate inputs
    * and not the size of the input image. */
  void GenerateOutputInformation();
  
  /**  Overloaded to set the input image to the largest possible region */
  void GenerateInputRequestedRegion();

protected:
  /** Main function for calculating interpolated values at each coordinate 
    * set.  Access is through the update() call. */
//  void EvaluateAtContinuousIndex( OutputImagePointer );

  /** TODO:  This needs to be modified for a threaded implementation.
    */
  void ThreadedGenerateData( const OutputImageRegionType& outputRegionForThread,
                            int threadId );
  void BeforeThreadedGenerateData();

  InterpolateImagePointsFilter();
  ~InterpolateImagePointsFilter(){};
  void PrintSelf(std::ostream& os, Indent indent) const;
    
private:

  /** Typedefs to describe and access coordinate images */
  typedef typename CoordImageType::Pointer CoordImageTypePointer;
  typedef itk::ImageRegionConstIterator<CoordImageType>   CoordImageIterator;
  typedef typename CoordImageType::RegionType              CoordImageRegionType;

  InterpolateImagePointsFilter( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  InterpolatorPointer      m_Interpolator;
  PixelType                m_DefaultPixelValue; // default pixel value if the
                                                // point is outside the image
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkInterpolateImagePointsFilter.txx"
#endif

#endif
