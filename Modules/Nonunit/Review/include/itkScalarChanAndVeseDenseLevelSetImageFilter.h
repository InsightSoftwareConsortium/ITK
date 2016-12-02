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
#ifndef itkScalarChanAndVeseDenseLevelSetImageFilter_h
#define itkScalarChanAndVeseDenseLevelSetImageFilter_h

#include "itkMultiphaseDenseFiniteDifferenceImageFilter.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkScalarChanAndVeseLevelSetFunction.h"

namespace itk
{
/** \class ScalarChanAndVeseDenseLevelSetImageFilter
 * \brief Dense implementation of the Chan and Vese multiphase level set image filter.
 *
 * This code was adapted from the paper:
 *
 *        "An active contour model without edges"
 *         T. Chan and L. Vese.
 *         In Scale-Space Theories in Computer Vision, pages 141-151, 1999.
 *
 * \author Mosaliganti K., Smith B., Gelas A., Gouaillard A., Megason S.
 *
 *  This code was taken from the Insight Journal paper:
 *
 *      "Cell Tracking using Coupled Active Surfaces for Nuclei and Membranes"
 *      http://www.insight-journal.org/browse/publication/642
 *      https://hdl.handle.net/10380/3055
 *
 *  That is based on the papers:
 *
 *      "Level Set Segmentation: Active Contours without edge"
 *      http://www.insight-journal.org/browse/publication/322
 *      https://hdl.handle.net/1926/1532
 *
 *      and
 *
 *      "Level set segmentation using coupled active surfaces"
 *      http://www.insight-journal.org/browse/publication/323
 *      https://hdl.handle.net/1926/1533
 *
 * \ingroup ITKReview
 *
 * \wiki
 * \wikiexample{Segmentation/SinglephaseChanAndVeseDenseFieldLevelSetSegmentation,Single-phase Chan And Vese Dense Field Level Set Segmentation}
 * \endwiki
 */
template< typename TInputImage, typename TFeatureImage, typename TOutputImage,
          typename TFunction = ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage >,
          class TSharedData = typename TFunction::SharedDataType >
class ITK_TEMPLATE_EXPORT ScalarChanAndVeseDenseLevelSetImageFilter:
  public MultiphaseDenseFiniteDifferenceImageFilter< TInputImage, TFeatureImage, TOutputImage,
                                                     TFunction >
{
public:

  typedef ScalarChanAndVeseDenseLevelSetImageFilter Self;
  typedef MultiphaseDenseFiniteDifferenceImageFilter< TInputImage,
                                                      TFeatureImage, TOutputImage,
                                                      TFunction >                Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarChanAndVeseDenseLevelSetImageFilter,
               MultiphaseDenseFiniteDifferenceImageFilter);

  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  /** Inherited typedef from the superclass. */
  typedef typename Superclass::InputImageType    InputImageType;
  typedef typename Superclass::InputImagePointer InputImagePointer;
  typedef typename Superclass::InputPointType    InputPointType;
  typedef typename Superclass::ValueType         ValueType;
  typedef typename InputImageType::SpacingType   InputSpacingType;

  typedef TFeatureImage                             FeatureImageType;
  typedef typename FeatureImageType::Pointer        FeatureImagePointer;
  typedef typename FeatureImageType::PixelType      FeaturePixelType;
  typedef typename FeatureImageType::IndexType      FeatureIndexType;
  typedef typename FeatureIndexType::IndexValueType FeatureIndexValueType;
  typedef typename FeatureImageType::RegionType     FeatureRegionType;

  /** Output image type typedefs */
  typedef TOutputImage                        OutputImageType;
  typedef typename OutputImageType::IndexType IndexType;
  typedef typename OutputImageType::PixelType OutputPixelType;

  typedef typename Superclass::TimeStepType TimeStepType;
  typedef typename Superclass::FiniteDifferenceFunctionType
  FiniteDifferenceFunctionType;

  typedef TFunction                      FunctionType;
  typedef typename FunctionType::Pointer FunctionPointer;

  typedef TSharedData                      SharedDataType;
  typedef typename SharedDataType::Pointer SharedDataPointer;

  typedef RegionOfInterestImageFilter< FeatureImageType, FeatureImageType > ROIFilterType;
  typedef typename ROIFilterType::Pointer                                   ROIFilterPointer;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< OutputPixelType > ) );
  // End concept checking
#endif

  /** Set/Get the feature image to be used for speed function of the level set
   *  equation.  Equivalent to calling Set/GetInput(1, ..) */
  virtual void SetFeatureImage(const FeatureImagePointer f)
  {
    this->SetInput(f);
  }

protected:
  ScalarChanAndVeseDenseLevelSetImageFilter()
  {
    this->m_SharedData = SharedDataType::New();
  }

  ~ScalarChanAndVeseDenseLevelSetImageFilter(){}

  SharedDataPointer m_SharedData;

  virtual void Initialize() ITK_OVERRIDE;

  virtual void InitializeIteration() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScalarChanAndVeseDenseLevelSetImageFilter);
};
} //end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalarChanAndVeseDenseLevelSetImageFilter.hxx"
#endif

#endif
