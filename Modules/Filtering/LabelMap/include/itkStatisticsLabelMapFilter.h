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
#ifndef itkStatisticsLabelMapFilter_h
#define itkStatisticsLabelMapFilter_h

#include "itkShapeLabelMapFilter.h"

namespace itk
{
/** \class StatisticsLabelMapFilter
 * \brief The valuator class for the StatisticsLabelObject
 *
 * StatisticsLabelMapFilter can be used to set the attributes values
 * of the StatisticsLabelObject in a LabelMap.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template< typename TImage, typename TFeatureImage >
class ITK_TEMPLATE_EXPORT StatisticsLabelMapFilter:
  public ShapeLabelMapFilter< TImage,
                              Image< typename TImage::PixelType,  TImage ::ImageDimension > >
{
public:
  /** Standard class typedefs. */
  typedef StatisticsLabelMapFilter      Self;
  typedef ShapeLabelMapFilter< TImage > Superclass;
  typedef SmartPointer< Self >          Pointer;
  typedef SmartPointer< const Self >    ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                               ImageType;
  typedef typename ImageType::Pointer          ImagePointer;
  typedef typename ImageType::ConstPointer     ImageConstPointer;
  typedef typename ImageType::PixelType        PixelType;
  typedef typename ImageType::IndexType        IndexType;
  typedef typename ImageType::PointType        PointType;
  typedef typename ImageType::LabelObjectType  LabelObjectType;
  typedef typename LabelObjectType::MatrixType MatrixType;
  typedef typename LabelObjectType::VectorType VectorType;

  typedef TFeatureImage                           FeatureImageType;
  typedef typename FeatureImageType::Pointer      FeatureImagePointer;
  typedef typename FeatureImageType::ConstPointer FeatureImageConstPointer;
  typedef typename FeatureImageType::PixelType    FeatureImagePixelType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(StatisticsLabelMapFilter,
               ShapeLabelMapFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
/*  itkConceptMacro(InputEqualityComparableCheck,
    (Concept::EqualityComparable<InputImagePixelType>));
  itkConceptMacro(IntConvertibleToInputCheck,
    (Concept::Convertible<int, InputImagePixelType>));
  itkConceptMacro(InputOStreamWritableCheck,
    (Concept::OStreamWritable<InputImagePixelType>));*/
// End concept checking
#endif

  /** Set the feature image */
  void SetFeatureImage(const TFeatureImage *input)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput( 1, const_cast< TFeatureImage * >( input ) );
  }

  /** Get the feature image */
  FeatureImageType * GetFeatureImage()
  {
    return static_cast< FeatureImageType * >( const_cast< DataObject * >( this->ProcessObject::GetInput(1) ) );
  }

  /** Set the input image */
  void SetInput1(TImage *input)
  {
    this->SetInput(input);
  }

  /** Set the feature image */
  void SetInput2(const TFeatureImage *input)
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
  StatisticsLabelMapFilter();
  ~StatisticsLabelMapFilter() ITK_OVERRIDE {}

  virtual void ThreadedProcessLabelObject(LabelObjectType *labelObject) ITK_OVERRIDE;

  virtual void BeforeThreadedGenerateData() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(StatisticsLabelMapFilter);

  FeatureImagePixelType m_Minimum;
  FeatureImagePixelType m_Maximum;
  unsigned int          m_NumberOfBins;
  bool                  m_ComputeHistogram;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatisticsLabelMapFilter.hxx"
#endif

#endif
