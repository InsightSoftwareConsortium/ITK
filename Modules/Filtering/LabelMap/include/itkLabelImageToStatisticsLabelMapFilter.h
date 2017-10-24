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
#ifndef itkLabelImageToStatisticsLabelMapFilter_h
#define itkLabelImageToStatisticsLabelMapFilter_h

#include "itkStatisticsLabelObject.h"
#include "itkLabelImageToLabelMapFilter.h"
#include "itkStatisticsLabelMapFilter.h"

namespace itk
{
/** \class LabelImageToStatisticsLabelMapFilter
 * \brief a convenient class to convert a label image to a label map and valuate the statistics attributes at once
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa StatisticsLabelObject, LabelStatisticsOpeningImageFilter, LabelStatisticsOpeningImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template< typename TInputImage, typename TFeatureImage, typename TOutputImage =
            LabelMap< StatisticsLabelObject< typename TInputImage::PixelType,
                                             TInputImage::ImageDimension > > >
class ITK_TEMPLATE_EXPORT LabelImageToStatisticsLabelMapFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef LabelImageToStatisticsLabelMapFilter            Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;

  typedef TOutputImage                              OutputImageType;
  typedef typename OutputImageType::Pointer         OutputImagePointer;
  typedef typename OutputImageType::ConstPointer    OutputImageConstPointer;
  typedef typename OutputImageType::RegionType      OutputImageRegionType;
  typedef typename OutputImageType::PixelType       OutputImagePixelType;
  typedef typename OutputImageType::LabelObjectType LabelObjectType;

  typedef TFeatureImage                           FeatureImageType;
  typedef typename FeatureImageType::Pointer      FeatureImagePointer;
  typedef typename FeatureImageType::ConstPointer FeatureImageConstPointer;
  typedef typename FeatureImageType::PixelType    FeatureImagePixelType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  typedef LabelImageToLabelMapFilter< InputImageType, OutputImageType > LabelizerType;
  typedef StatisticsLabelMapFilter< OutputImageType, FeatureImageType > LabelObjectValuatorType;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelImageToStatisticsLabelMapFilter,
               ImageToImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputEqualityComparableCheck,
                   ( Concept::EqualityComparable< InputImagePixelType > ) );
  itkConceptMacro( IntConvertibleToInputCheck,
                   ( Concept::Convertible< int, InputImagePixelType > ) );
  itkConceptMacro( InputOStreamWritableCheck,
                   ( Concept::OStreamWritable< InputImagePixelType > ) );
  // End concept checking
#endif

  /**
   * Set/Get the value used as "background" in the output image.
   * Defaults to NumericTraits<PixelType>::NonpositiveMin().
   */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

  /**
   * Set/Get whether the maximum Feret diameter should be computed or not. The
   * defaut value is false, because of the high computation time required.
   */
  itkSetMacro(ComputeFeretDiameter, bool);
  itkGetConstReferenceMacro(ComputeFeretDiameter, bool);
  itkBooleanMacro(ComputeFeretDiameter);

  /**
   * Set/Get whether the perimeter should be computed or not. The defaut value
   * is false, because of the high computation time required.
   */
  itkSetMacro(ComputePerimeter, bool);
  itkGetConstReferenceMacro(ComputePerimeter, bool);
  itkBooleanMacro(ComputePerimeter);

  /** Set the feature image */
  void SetFeatureImage(const TFeatureImage *input)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput( 1, const_cast< TFeatureImage * >( input ) );
  }

  /** Get the feature image */
  const FeatureImageType * GetFeatureImage()
  {
    return static_cast< FeatureImageType * >( this->ProcessObject::GetInput(1) );
  }

  /** Set the input image */
  void SetInput1(const InputImageType *input)
  {
    this->SetInput(input);
  }

  /** Set the feature image */
  void SetInput2(const FeatureImageType *input)
  {
    this->SetFeatureImage(input);
  }

  /**
   * Set/Get whether the histogram should be attached to the label object or not.
   * This option defaults to `true`, but because the histogram may take a lot of memory
   * compared to the other attributes, this option is useful to reduce the memory usage
   * when the histogram is not required.
   */
  itkSetMacro(ComputeHistogram, bool);
  itkGetConstReferenceMacro(ComputeHistogram, bool);
  itkBooleanMacro(ComputeHistogram);

  /**
   * Set/Get the number of bins in the histogram. Note that the histogram is used
   * to compute the median value, and that this option may have an effect on the
   * value of the median.
   */
  itkSetMacro(NumberOfBins, unsigned int);
  itkGetConstReferenceMacro(NumberOfBins, unsigned int);

protected:
  LabelImageToStatisticsLabelMapFilter();
  ~LabelImageToStatisticsLabelMapFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** LabelImageToStatisticsLabelMapFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** LabelImageToStatisticsLabelMapFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) ) ITK_OVERRIDE;

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleGeodesicErodeImageFilter. */
  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelImageToStatisticsLabelMapFilter);

  OutputImagePixelType m_BackgroundValue;
  bool                 m_ComputeFeretDiameter;
  bool                 m_ComputePerimeter;
  unsigned int         m_NumberOfBins;
  bool                 m_ComputeHistogram;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelImageToStatisticsLabelMapFilter.hxx"
#endif

#endif
