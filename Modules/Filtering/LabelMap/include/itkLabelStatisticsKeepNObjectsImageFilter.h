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
#ifndef itkLabelStatisticsKeepNObjectsImageFilter_h
#define itkLabelStatisticsKeepNObjectsImageFilter_h

#include "itkLabelImageToLabelMapFilter.h"
#include "itkStatisticsLabelMapFilter.h"
#include "itkStatisticsKeepNObjectsLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

namespace itk
{
/** \class LabelStatisticsKeepNObjectsImageFilter
 * \brief keep N objects according to their statistics attributes
 *
 * LabelStatisticsKeepNObjectsImageFilter keep the N objects in a labeled image
 * with the highest (or lowest) attribute value. The attributes are the ones
 * of the StatisticsLabelObject.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa StatisticsLabelObject, BinaryStatisticsKeepNObjectsImageFilter, LabelShapeKeepNObjectsImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template< typename TInputImage, typename TFeatureImage >
class ITK_TEMPLATE_EXPORT LabelStatisticsKeepNObjectsImageFilter:
  public ImageToImageFilter< TInputImage, TInputImage >
{
public:
  /** Standard class typedefs. */
  typedef LabelStatisticsKeepNObjectsImageFilter         Self;
  typedef ImageToImageFilter< TInputImage, TInputImage > Superclass;
  typedef SmartPointer< Self >                           Pointer;
  typedef SmartPointer< const Self >                     ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                            InputImageType;
  typedef TInputImage                            OutputImageType;
  typedef typename InputImageType::Pointer       InputImagePointer;
  typedef typename InputImageType::ConstPointer  InputImageConstPointer;
  typedef typename InputImageType::RegionType    InputImageRegionType;
  typedef typename InputImageType::PixelType     InputImagePixelType;
  typedef typename OutputImageType::Pointer      OutputImagePointer;
  typedef typename OutputImageType::ConstPointer OutputImageConstPointer;
  typedef typename OutputImageType::RegionType   OutputImageRegionType;
  typedef typename OutputImageType::PixelType    OutputImagePixelType;

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

  typedef StatisticsLabelObject< InputImagePixelType, itkGetStaticConstMacro(ImageDimension) > LabelObjectType;
  typedef LabelMap< LabelObjectType >                                                          LabelMapType;
  typedef LabelImageToLabelMapFilter< InputImageType, LabelMapType >                           LabelizerType;
  typedef StatisticsLabelMapFilter< LabelMapType, TFeatureImage >                              LabelObjectValuatorType;
  typedef typename LabelObjectType::AttributeType                                              AttributeType;
  typedef StatisticsKeepNObjectsLabelMapFilter< LabelMapType >                                 KeepNObjectsType;
  typedef LabelMapToLabelImageFilter< LabelMapType, OutputImageType >                          BinarizerType;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelStatisticsKeepNObjectsImageFilter,
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
   * Set/Get the number of objects to keep
   */
  itkGetConstMacro(NumberOfObjects, SizeValueType);
  itkSetMacro(NumberOfObjects, SizeValueType);

  /**
   * Set/Get the ordering of the objects. By default, the ones with the
   * highest value are kept. Turming ReverseOrdering to true make this filter
   * keep the objects with the smallest values
   */
  itkGetConstMacro(ReverseOrdering, bool);
  itkSetMacro(ReverseOrdering, bool);
  itkBooleanMacro(ReverseOrdering);

  /**
   * Set/Get the attribute to use to select the object to keep. The default
   * is "Mean".
   */
  itkGetConstMacro(Attribute, AttributeType);
  itkSetMacro(Attribute, AttributeType);
  void SetAttribute(const std::string & s)
  {
    this->SetAttribute( LabelObjectType::GetAttributeFromName(s) );
  }

  /** Set the feature image */
  void SetFeatureImage(const TFeatureImage *input)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput( 1, const_cast< TFeatureImage * >( input ) );
  }

  /** Get the feature image */
  const FeatureImageType * GetFeatureImage()
  {
    return static_cast< const FeatureImageType * >( this->ProcessObject::GetInput(1) );
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

protected:
  LabelStatisticsKeepNObjectsImageFilter();
  ~LabelStatisticsKeepNObjectsImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** LabelStatisticsKeepNObjectsImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** LabelStatisticsKeepNObjectsImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) ) ITK_OVERRIDE;

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleGeodesicErodeImageFilter. */
  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelStatisticsKeepNObjectsImageFilter);

  OutputImagePixelType m_BackgroundValue;
  SizeValueType        m_NumberOfObjects;
  bool                 m_ReverseOrdering;
  AttributeType        m_Attribute;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelStatisticsKeepNObjectsImageFilter.hxx"
#endif

#endif
