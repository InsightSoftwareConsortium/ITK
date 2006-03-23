/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkZeroCrossingBasedEdgeDetectionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkZeroCrossingBasedEdgeDetectionImageFilter_h
#define __itkZeroCrossingBasedEdgeDetectionImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkFixedArray.h"
#include "itkImage.h"


namespace itk
{
  
/** \class ZeroCrossingBasedEdgeDetectionImageFilter
 *
 * This filter implements a zero-crossing based edge detecor. The zero-crossing
 * based edge detector looks for pixels in the Laplacian of an image where the
 * value of the Laplacian passes through zero --- points where the Laplacian
 * changes sign.  Such points often occur at ``edges'' in images ---
 * i.e. points where the intensity of the image changes rapidly, but they also
 * occur at places that are not as easy to associate with edges. It is best to
 * think of the zero crossing detector as some sort of feature detector rather
 * than as a specific edge detector.
 *
 * /par
 * Zero crossings always lie on closed contours and so the output from the zero
 * crossing detector is usually a binary image with single pixel thickness
 * lines showing the positions of the zero crossing points.
 *
 * /par
 * In this implementation, the input image is first smoothed with a Gaussian
 * filter, then the LaplacianImageFilter is applied to smoothed image. Finally
 * the zero-crossing of the Laplacian of the smoothed image is detected. The
 * output is a binary image.
 *
 * /par Inputs and Outputs
 * The input to the filter should be a scalar, itk::Image of arbitrary
 * dimension.  The output image is a binary, labeled image.  See
 * itkZeroCrossingImageFilter for more information on requirements of the data
 * type of the output.
 *
 * /par
 * To use this filter, first set the parameters (variance and maximum error)
 * needed by the embedded DiscreteGaussianImageFilter, i.e.  See
 * DiscreteGaussianImageFilter for information about these parameters.
 * Optionally, you may also set foreground and background values for the
 * zero-crossing filter.  The default label values are Zero for the background
 * and One for the foreground, as defined in NumericTraits for the data type of
 * the output image.
 * 
 * \sa DiscreteGaussianImageFilter
 * \sa LaplacianImageFilter
 * \sa ZeroCrossingImageFilter 
 * \ingroup ImageFeatureExtraction */
template<class TInputImage, class TOutputImage>
class ITK_EXPORT ZeroCrossingBasedEdgeDetectionImageFilter
  : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard "Self" & Superclass typedef.   */
  typedef ZeroCrossingBasedEdgeDetectionImageFilter    Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  
  /** Image typedef support   */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;
  
  /** SmartPointer typedef support  */    
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Define pixel type  */
  typedef typename TInputImage::PixelType  InputImagePixelType;
  typedef typename TOutputImage::PixelType  OutputImagePixelType;
  
  /** Method for creation through the object factory.   */
  itkNewMacro(Self);  
  
  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;
  
  /** Run-time type information (and related methods).   */
  itkTypeMacro(ZeroCrossingBasedEdgeDetectionImageFilter, ImageToImageFilter);
  
  /** ImageDimension enumeration   */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension );
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension );
  
  /** Typedef of double containers */
  typedef FixedArray<double, itkGetStaticConstMacro(ImageDimension)> ArrayType;

  /** Standard get/set macros for Gaussian filter parameters.  */
  itkSetMacro(Variance, ArrayType);
  itkGetMacro(Variance, const ArrayType);
  itkSetMacro(MaximumError, ArrayType);
  itkGetMacro(MaximumError, const ArrayType);

  /** Get/Set the label values for the ZeroCrossingImageFilter */
  itkGetMacro(BackgroundValue, OutputImagePixelType);
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetMacro(ForegroundValue, OutputImagePixelType);
  itkSetMacro(ForegroundValue, OutputImagePixelType);
  
  /** Set the variance parameter needed by the embedded gaussian filter  */ 
  void SetVariance(const typename ArrayType::ValueType v)
  {
    m_Variance.Fill(v);
  }
  
  /** Set the MaximumError parameter needed by the embedded gaussian filter */
  void SetMaximumError(const typename ArrayType::ValueType v)
  {
    m_MaximumError.Fill(v);
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(OutputEqualityComparableCheck,
    (Concept::EqualityComparable<OutputImagePixelType>));
  itkConceptMacro(SameDimensionCheck,
    (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(SameTypeCheck,
    (Concept::SameType<InputImagePixelType, OutputImagePixelType>));
  /** End concept checking */
#endif

protected:
  ZeroCrossingBasedEdgeDetectionImageFilter()
  {
    m_Variance.Fill(1.0);
    m_MaximumError.Fill(0.01);
    m_BackgroundValue = NumericTraits<OutputImagePixelType>::Zero;
    m_ForegroundValue = NumericTraits<OutputImagePixelType>::One;
  }
  ~ZeroCrossingBasedEdgeDetectionImageFilter(){}
  ZeroCrossingBasedEdgeDetectionImageFilter(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** Standard pipeline method. While this class does not implement a
   * ThreadedGenerateData(), its GenerateData() delegates all
   * calculations to the pipeline of a DiscreteGaussianImageFilter, 
   * a LaplacianImageFilter and a ZeroCrossingImageFilter.  Since these
   * filters are multithreaded, this filter is multithreaded by default.
   */
  void GenerateData();
  
private:
  /** The variance of the Gaussian Filter used in this filter */
  ArrayType m_Variance;

  /** The maximum error of the gaussian blurring kernel in each dimensional
   * direction.  */
  ArrayType m_MaximumError;
 
  OutputImagePixelType m_BackgroundValue;
  OutputImagePixelType m_ForegroundValue;
};
  
} //end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkZeroCrossingBasedEdgeDetectionImageFilter.txx"
#endif
  
#endif

