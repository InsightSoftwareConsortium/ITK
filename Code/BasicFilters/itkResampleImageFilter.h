/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkResampleImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkResampleImageFilter_h
#define __itkResampleImageFilter_h

#include "itkTransform.h"
#include "itkImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkImageToImageFilter.h"
#include "itkInterpolateImageFunction.h"
#include "itkSize.h"

namespace itk
{

/** \class ResampleImageFilter
 * \brief Resample an image via a coordinate transform
 *
 * ResampleImageFilter resamples an existing image through some coordinate
 * transform, interpolating via some image function.  The class is templated
 * over the type of the input image, the type of the output image, the
 * type of the coordinate transformation, and the type of the interpolation
 * method or image function.
 *
 * Since this filter produces an image which is a different size than
 * its input, it needs to override several of the methods defined
 * in ProcessObject in order to properly manage the pipeline execution model.
 * In particular, this filter overrides
 * ProcessObject::GenerateInputRequestedRegion() and
 * ProcessObject::GenerateOutputInformation().
 *
 * This filter is implemented as a multithreaded filter.  It provides a 
 * ThreadedGenerateData() method for its implementation.
 *
 * \ingroup GeometricTransforms
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT ResampleImageFilter:
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ResampleImageFilter         Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename InputImageType::RegionType InputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(ResampleImageFilter, ImageToImageFilter);

  /** Number of dimensions. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Transform typedef.
   *
   * \todo Check that input and output images have the same number of 
   * dimensions; this is required by the current implementation of 
   * AffineTransform. */
  typedef Transform<double, itkGetStaticConstMacro(ImageDimension), itkGetStaticConstMacro(ImageDimension)> TransformType;
  typedef typename TransformType::Pointer TransformPointerType;

  /** Interpolator typedef. */
  typedef InterpolateImageFunction<InputImageType, double> InterpolatorType;
  typedef typename InterpolatorType::Pointer  InterpolatorPointerType;

  /** Image size typedef. */
  typedef Size<itkGetStaticConstMacro(ImageDimension)> SizeType;

  /** Image index typedef. */
  typedef typename TOutputImage::IndexType IndexType;

  /** Image point typedef. */
  typedef typename InterpolatorType::PointType    PointType;

  /** Image pixel value typedef. */
  typedef typename TOutputImage::PixelType   PixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Set the coordinate transformation.
   * Set the coordinate transform to use for resampling.  Note that this
   * must be in index coordinates and is the output-to-input transform,
   * NOT the input-to-output transform that you might naively expect. */
  itkSetObjectMacro( Transform, TransformType ); 

  /** Get a pointer to the coordinate transform. */
  itkGetObjectMacro( Transform, TransformType );

  /** Set the interpolator function */
  itkSetObjectMacro( Interpolator, InterpolatorType );

  /** Get a pointer to the interpolator function. */
  itkGetObjectMacro( Interpolator, InterpolatorType );

  /** Set the size of the output image. */
  itkSetMacro( Size, SizeType );

  /** Get the size of the output image. */
  itkGetConstReferenceMacro( Size, SizeType );
     
  /** Set the pixel value when a transformed pixel is outside of the image */
  itkSetMacro(DefaultPixelValue,PixelType);

  /** Get the pixel value when a transformed pixel is outside of the image */
  itkGetMacro(DefaultPixelValue,PixelType);

  /** Set the output image spacing. */
  virtual void SetOutputSpacing( const double values[ImageDimension] );

  /** Get the output image spacing. */
  const double * GetOutputSpacing()
    { return m_OutputSpacing; }

  /** Set the output image origin. */
  virtual void SetOutputOrigin( const double values[ImageDimension] );

  /** Get the output image origin. */
  const double * GetOutputOrigin()
    { return m_OutputSpacing; }


  /** ResampleImageFilter produces an image which is a different size
   * than its input.  As such, it needs to provide an implementation
   * for GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below. \sa ProcessObject::GenerateOutputInformaton() */
  virtual void GenerateOutputInformation();

  /** ResampleImageFilter needs a different input requested region than
   * the output requested region.  As such, ResampleImageFilter needs
   * to provide an implementation for GenerateInputRequestedRegion()
   * in order to inform the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion();

  /** This method is used to set the state of the filter before 
   * multi-threading. */
  virtual void BeforeThreadedGenerateData();

  /** Method Compute the Modified Time based on changed to the components. */
  unsigned long GetMTime( void ) const;

protected:
  ResampleImageFilter();
  ~ResampleImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** ResampleImageFilter can be implemented as a multithreaded filter.  Therefore,
   * this implementation provides a ThreadedGenerateData() routine which
   * is called for each processing thread. The output image data is allocated
   * automatically by the superclass prior to calling ThreadedGenerateData().
   * ThreadedGenerateData can only write to the portion of the output image
   * specified by the parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

private:
  ResampleImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  SizeType                m_Size;       // Size of the output image
  TransformPointerType    m_Transform;  // Coordinate transform to use
  InterpolatorPointerType m_Interpolator;
                                        // Image function for interpolation
  PixelType               m_DefaultPixelValue; 
                                        // default pixel value if the point 
                                        // is outside the image
  double                  m_OutputSpacing[ImageDimension]; // output image spacing
  double                  m_OutputOrigin[ImageDimension];  // output image origin

};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkResampleImageFilter.txx"
#endif
  
#endif
