/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHistogramMatchingImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHistogramMatchingImageFilter_h
#define __itkHistogramMatchingImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkHistogram.h"
#include "vnl/vnl_matrix.h"

namespace itk
{

/** \class HistogramMatchingImageFilter
 * \brief Normalize the grayscale values between two image by histogram 
 * matching.
 *
 * HistogramMatchingImageFilter normalizes the grayscale values of a source
 * image based on the grayscale values of a reference image. 
 * This filter uses a histogram matching technique where the histograms of the 
 * two images are matched only at a specified number of quantile values.
 *
 * This filter was orginally designed to normalize MR images of the same
 * MR protocol and same body part. The algorithm works best if background
 * pixels are excluded from both the source and reference histograms.
 * A simple background exclusion method is to exclude all pixels whose
 * grayscale values are smaller than the mean grayscale value.
 * ThresholdAtMeanIntensityOn() switches on this simple background
 * exclusion method.
 *
 * The source image can be set via either SetInput() or SetSourceImage().
 * The reference image can be set via SetReferenceImage().
 *
 * SetNumberOfHistogramLevels() sets the number of bins used when
 * creating histograms of the source and reference images.
 * SetNumberOfMatchPoints() governs the number of quantile values to be
 * matched.
 *
 * This filter assumes that both the source and reference are of the same
 * type and that the input and output image type have the same number of
 * dimension and have scalar pixel types.
 *
 * \ingroup IntensityImageFilters Multithreaded
 *
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT HistogramMatchingImageFilter:
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef HistogramMatchingImageFilter         Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(HistogramMatchingImageFilter, ImageToImageFilter);

  /** ImageDimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Inherited typedefs. */
  typedef typename Superclass::InputImageType InputImageType;
  typedef typename Superclass::InputImagePointer InputImagePointer;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::OutputImagePointer OutputImagePointer;
  
  /** Pixel related typedefs. */
  typedef typename InputImageType::PixelType InputPixelType;
  typedef typename OutputImageType::PixelType OutputPixelType;

  /** Histogram related typedefs. */
  typedef Statistics::Histogram<InputPixelType, 1> HistogramType;
  typedef typename HistogramType::Pointer HistogramPointer;

  /** Set/Get the source image. */
  void SetSourceImage( const InputImageType * source )
  { this->SetInput( source ); }
  const InputImageType * GetSourceImage(void)
  { return this->GetInput(); }

  /** Set/Get the reference image. */
  void SetReferenceImage( const InputImageType * reference );
  const InputImageType * GetReferenceImage(void);

  /** Set/Get the number of histogram levels used. */
  itkSetMacro( NumberOfHistogramLevels, unsigned long );
  itkGetMacro( NumberOfHistogramLevels, unsigned long );

  /** Set/Get the number of match points used. */
  itkSetMacro( NumberOfMatchPoints, unsigned long );
  itkGetMacro( NumberOfMatchPoints, unsigned long );

  /** Set/Get the threshold at mean intensity flag.
   * If true, only source (reference) pixels which are greater
   * than the mean source (reference) intensity is used in
   * the histogram matching. If false, all pixels are
   * used. */
  itkSetMacro( ThresholdAtMeanIntensity, bool );
  itkGetMacro( ThresholdAtMeanIntensity, bool );
  itkBooleanMacro( ThresholdAtMeanIntensity );

  /** This filter requires all of the input to be in the buffer. */
  virtual void GenerateInputRequestedRegion();

  /** Methods to get the histograms of the source, reference, and
   * output. Objects are only valid after Update() has been called
   * on this filter. */
  itkGetObjectMacro(SourceHistogram, HistogramType);
  itkGetObjectMacro(ReferenceHistogram, HistogramType);
  itkGetObjectMacro(OutputHistogram, HistogramType);
   

protected:
  HistogramMatchingImageFilter();
  ~HistogramMatchingImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  void BeforeThreadedGenerateData();
  void AfterThreadedGenerateData();
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

  /** Compute min, max and mean of an image. */
  void ComputeMinMaxMean( const InputImageType * image,
                          double& minValue, double& maxValue, double& meanValue ); 

  /** Construct a histogram from an image. */
  void ConstructHistogram( const InputImageType * image,
                           HistogramType * histogram, double minValue,
                           double maxValue );

private:
  HistogramMatchingImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  unsigned long         m_NumberOfHistogramLevels;
  unsigned long         m_NumberOfMatchPoints;
  bool                  m_ThresholdAtMeanIntensity;

  InputPixelType        m_SourceIntensityThreshold;
  InputPixelType        m_ReferenceIntensityThreshold;
  OutputPixelType       m_OutputIntensityThreshold;

  double                m_SourceMinValue;
  double                m_SourceMaxValue;
  double                m_SourceMeanValue;
  double                m_ReferenceMinValue;
  double                m_ReferenceMaxValue;
  double                m_ReferenceMeanValue;
  double                m_OutputMinValue;
  double                m_OutputMaxValue;
  double                m_OutputMeanValue;

  HistogramPointer      m_SourceHistogram;
  HistogramPointer      m_ReferenceHistogram;
  HistogramPointer      m_OutputHistogram;
  
  typedef vnl_matrix<double>  TableType;
  TableType             m_QuantileTable;
  
  typedef vnl_vector<double>  GradientArrayType;
  GradientArrayType     m_Gradients;
  double                m_LowerGradient;
  double                m_UpperGradient;

};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHistogramMatchingImageFilter.txx"
#endif
  
#endif
