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
#ifndef itkVoronoiSegmentationRGBImageFilter_h
#define itkVoronoiSegmentationRGBImageFilter_h

#include "itkVoronoiSegmentationImageFilterBase.h"

namespace itk
{
/** \class VoronoiSegmentationRGBImageFilter
 *
 * Segmentation of 2D RGB images using Voronoi Diagram.
 * This is not a standard 3 channel image filter, it also investigates the
 * HSV color space information. from RGBHSV, the user can specify or by giving
 * a prior binary mask, the algorithm will decide which 3 channels out of the
 * 6 channels will be used for homogeneity testing.
 * the homogeneity testing requires all the three testing channels to have the
 * similar mean and standard deviation value from the gold-standard in the sense that the
 * difference will be under the tolerance value.
 *
 * Input parameters are:
 * (1) Image data, in the format: itkImage<itkVector<PixelType,3>, 2>.
 * (2) Object statistics: mean and standard deviation
 * (3) Tolerance level for the classifier. This level is usually set
 *     around the mean and standard deviation values.
 *
 * These parameters can also be automatically set by providing a binary image prior.
 *
 * Detailed information about this algorithm can be found in:
 *  " Semi-automated color segmentation of anatomical tissue,"
 *   C. Imelinska, M. Downes, and W. Yuan
 *  Computerized Medical Imaging and Graphics, Vol.24, pp 173-180, 2000.
 *
 *
 * \ingroup HybridSegmentation
 * \ingroup ITKVoronoi
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT VoronoiSegmentationRGBImageFilter:
  public VoronoiSegmentationImageFilterBase< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef VoronoiSegmentationRGBImageFilter                               Self;
  typedef VoronoiSegmentationImageFilterBase< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                            Pointer;
  typedef SmartPointer< const Self >                                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VoronoiSegmentationRGBImageFilter,
               VoronoiSegmentationImageFilterBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Convenient typedefs. */
  typedef typename Superclass::BinaryObjectImage BinaryObjectImage;
  typedef typename Superclass::IndexList         IndexList;
  typedef typename Superclass::IndexType         IndexType;
  typedef typename Superclass::RegionType        RegionType;
  typedef typename Superclass::PixelType         PixelType;
  typedef typename Superclass::InputImagePointer InputImagePointer;
  typedef typename Superclass::InputImageType    InputImageType;
  typedef Vector< float, 6 >                     RGBHCVPixel;
  typedef Image< RGBHCVPixel >                   RGBHCVImage;

  /** \todo Document. */
  void SetMeanPercentError(double x[6]);
  void SetSTDPercentError(double x[6]);
  void GetMeanPercentError(double x[6]){ for ( int i = 0; i < 6; i++ ) { x[i] = m_MeanPercentError[i]; } }
  void GetSTDPercentError(double x[6]){ for ( int i = 0; i < 6; i++ ) { x[i] = m_STDPercentError[i]; } }
  void GetMean(double x[6]){ for ( int i = 0; i < 6; i++ ) { x[i] = m_Mean[i]; } }
  void GetSTD(double x[6]){ for ( int i = 0; i < 6; i++ ) { x[i] = m_STD[i]; } }
  void SetMean(double x[6]){ for ( int i = 0; i < 6; i++ ) { m_Mean[i] = x[i]; } }
  void SetSTD(double x[6]){ for ( int i = 0; i < 6; i++ ) { m_STD[i] = x[i]; } }
  void GetMeanTolerance(double x[6]){ for ( int i = 0; i < 6; i++ ) { x[i] = m_MeanTolerance[i]; } }
  void GetSTDTolerance(double x[6]){ for ( int i = 0; i < 6; i++ ) { x[i] = m_STDTolerance[i]; } }

  /** Maximum value of the RGB, needed for color space coversions.
   * default as 8 bit per channel, if it is different, need to be
   * set before doing anything. */
  itkSetMacro(MaxValueOfRGB, double);
  itkGetConstMacro(MaxValueOfRGB, double);

  /** Set the three channels to test the mean and STD respectivley
   * 0:red, 1:green, 2:blue, 3:hue, 4:chroma, 5:value. */
  void SetTestMean(unsigned int t1, unsigned int t2, unsigned int t3)
  {
    m_TestMean[0] = t1;
    m_TestMean[1] = t2;
    m_TestMean[2] = t3;
  }

  void SetTestSTD(unsigned int t1, unsigned int t2, unsigned int t3)
  {
    m_TestSTD[0] = t1;
    m_TestSTD[1] = t2;
    m_TestSTD[2] = t3;
  }

  void GetTestMean(unsigned int x[3])
  {
    x[0] = m_TestMean[0]; x[1] = m_TestMean[1]; x[2] = m_TestMean[2];
  }

  void GetTestSTD(unsigned int x[3])
  {
    x[0] = m_TestSTD[0]; x[1] = m_TestSTD[1]; x[2] = m_TestSTD[2];
  }

  void TakeAPrior(const BinaryObjectImage *aprior) ITK_OVERRIDE;

  using Superclass::SetInput;
  virtual void SetInput(const InputImageType *input) ITK_OVERRIDE;

  virtual void SetInput(unsigned int, const InputImageType *image) ITK_OVERRIDE;

  /** ImageDimension enumeration   */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< InputImageDimension, OutputImageDimension > ) );
  itkConceptMacro( IntConvertibleToOutputCheck,
                   ( Concept::Convertible< int, typename TOutputImage::PixelType > ) );
  itkConceptMacro( PixelDimensionCheck,
                   ( Concept::SameDimension< PixelType::Dimension, 3u > ) );
  // End concept checking
#endif

protected:
  VoronoiSegmentationRGBImageFilter();
  ~VoronoiSegmentationRGBImageFilter() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  double m_Mean[6];
  double m_STD[6];
  double m_MeanTolerance[6];
  double m_STDTolerance[6];
  double m_MeanPercentError[6];
  double m_STDPercentError[6];
  double m_MaxValueOfRGB;
  unsigned int m_TestMean[3];
  unsigned int m_TestSTD[3];
  typename RGBHCVImage::Pointer m_WorkingImage;

  virtual bool TestHomogeneity(IndexList & Plist) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VoronoiSegmentationRGBImageFilter);
};
} //end namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVoronoiSegmentationRGBImageFilter.hxx"
#endif

#endif
