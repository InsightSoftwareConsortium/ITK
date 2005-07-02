/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkResampleImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkResampleImageFilter_h
#define __itkResampleImageFilter_h

#include "itkFixedArray.h"
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
 * over the types of the input and output images.
 *
 * Note that the choice of interpolator function can be important.
 * This function is set via SetInterpolator().  The default is
 * itk::LinearInterpolateImageFunction<InputImageType, TInterpolatorPrecisionType>, which
 * is reasonable for ordinary medical images.  However, some synthetic
 * images have pixels drawn from a finite prescribed set.  An example
 * would be a mask indicating the segmentation of a brain into a small
 * number of tissue types.  For such an image, one does not want to
 * interpolate between different pixel values, and so
 * itk::NearestNeighborInterpolateImageFunction< InputImageType,
 * TCoordRep > would be a better choice.
 *
 * Output information (spacing, size and direction) for the output
 * image should be set. This information has the normal defaults of
 * unit spacing, zero origin and identity direction. Optionally, the
 * output information can be obtained from a reference image. If the
 * reference image is provided and UseReferenceImage is On, then the
 * spacing, origin and direction of the reference image will be used.
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
template <class TInputImage, class TOutputImage, class TInterpolatorPrecisionType=double>
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
  typedef Transform<TInterpolatorPrecisionType, itkGetStaticConstMacro(ImageDimension), itkGetStaticConstMacro(ImageDimension)> TransformType;
  typedef typename TransformType::ConstPointer TransformPointerType;

  /** Interpolator typedef. */
  typedef InterpolateImageFunction<InputImageType, TInterpolatorPrecisionType> InterpolatorType;
  typedef typename InterpolatorType::Pointer  InterpolatorPointerType;

  /** Image size typedef. */
  typedef Size<itkGetStaticConstMacro(ImageDimension)> SizeType;

  /** Image index typedef. */
  typedef typename TOutputImage::IndexType IndexType;

  /** Image point typedef. */
  typedef typename InterpolatorType::PointType    PointType;
  //typedef typename TOutputImage::PointType    PointType;

  /** Image pixel value typedef. */
  typedef typename TOutputImage::PixelType   PixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Image spacing,origin and direction typedef */
  typedef typename TOutputImage::SpacingType SpacingType;
  typedef typename TOutputImage::PointType   OriginPointType;
  typedef typename TInputImage::DirectionType DirectionType;
  
  /** Set the coordinate transformation.
   * Set the coordinate transform to use for resampling.  Note that this
   * must be in index coordinates and is the output-to-input transform,
   * NOT the input-to-output transform that you might naively expect.
   * The default is itk::AffineTransform<TInterpolatorPrecisionType, ImageDimension>. */
  itkSetConstObjectMacro( Transform, TransformType ); 

  /** Get a pointer to the coordinate transform. */
  itkGetConstObjectMacro( Transform, TransformType );

  /** Set the interpolator function.  The default is
   * itk::LinearInterpolateImageFunction<InputImageType, TInterpolatorPrecisionType>. Some
   * other options are itk::NearestNeighborInterpolateImageFunction
   * (useful for binary masks and other images with a small number of
   * possible pixel values), and itk::BSplineInterpolateImageFunction
   * (which provides a higher order of interpolation).  */
  itkSetObjectMacro( Interpolator, InterpolatorType );

  /** Get a pointer to the interpolator function. */
  itkGetConstObjectMacro( Interpolator, InterpolatorType );

  /** Set the size of the output image. */
  itkSetMacro( Size, SizeType );

  /** Get the size of the output image. */
  itkGetConstReferenceMacro( Size, SizeType );
     
  /** Set the pixel value when a transformed pixel is outside of the
   * image.  The default default pixel value is 0. */
  itkSetMacro(DefaultPixelValue,PixelType);

  /** Get the pixel value when a transformed pixel is outside of the image */
  itkGetMacro(DefaultPixelValue,PixelType);

  /** Set the output image spacing. */
  itkSetMacro(OutputSpacing, SpacingType);
  virtual void SetOutputSpacing( const double values[ImageDimension] );

  /** Get the output image spacing. */
  itkGetConstReferenceMacro( OutputSpacing, SpacingType );

  /** Set the output image origin. */
  itkSetMacro(OutputOrigin, PointType);
  virtual void SetOutputOrigin( const double values[ImageDimension] );

  /** Get the output image origin. */
  itkGetConstReferenceMacro( OutputOrigin, PointType );

  /** Set the output direciton cosine matrix. */
  itkSetMacro(OutputDirection, DirectionType);
  itkGetConstReferenceMacro(OutputDirection, DirectionType);

  /** Helper method to set the output parameters based on this image */
  void SetOutputParametersFromImage ( typename OutputImageType::Pointer Image ) {
    this->SetOutputOrigin ( Image->GetOrigin() );
    this->SetOutputSpacing ( Image->GetSpacing() );
    this->SetSize ( Image->GetLargestPossibleRegion().GetSize() );
  }

  /** Set the start index of the output largest possible region. 
   * The default is an index of all zeros. */
  itkSetMacro( OutputStartIndex, IndexType );

  /** Get the start index of the output largest possible region. */
  itkGetConstReferenceMacro( OutputStartIndex, IndexType );

  /** Copy the output information from another Image.  By default,
   *  the information is specified with the SetOutputSpacing, Origin,
   *  and Direction methods. UseReferenceImage must be On and a
   *  Reference image must be present to override the defaul behavior.*/
  void SetReferenceImage (TInputImage *image)
  {
    if (image != m_ReferenceImage)
      {
      m_ReferenceImage = image;
      this->ProcessObject::SetNthInput(1, image);
      this->Modified();
      }
  }
  itkGetObjectMacro(ReferenceImage, TInputImage);

  itkSetMacro(UseReferenceImage, bool);
  itkBooleanMacro(UseReferenceImage);
  itkGetMacro(UseReferenceImage, bool);

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

  /** This method is used to set the state of the filter after 
   * multi-threading. */
  virtual void AfterThreadedGenerateData();

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

  OutputImagePointer      m_ReferenceImage;

  SizeType                m_Size;       // Size of the output image
  TransformPointerType    m_Transform;  // Coordinate transform to use
  InterpolatorPointerType m_Interpolator; // Image function for interpolation
  PixelType               m_DefaultPixelValue; // default pixel value if the point 
                                               // is outside the image
  SpacingType             m_OutputSpacing; // output image spacing
  PointType               m_OutputOrigin;  // output image origin
  DirectionType           m_OutputDirection; // output image direction cosines
  IndexType               m_OutputStartIndex; // output image start index
  bool m_UseReferenceImage;

};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkResampleImageFilter.txx"
#endif
  
#endif
