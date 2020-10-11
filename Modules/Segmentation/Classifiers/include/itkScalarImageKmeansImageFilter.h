/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkScalarImageKmeansImageFilter_h
#define itkScalarImageKmeansImageFilter_h


#include "itkKdTree.h"
#include "itkKdTreeBasedKmeansEstimator.h"
#include "itkWeightedCentroidKdTreeGenerator.h"

#include "itkSampleClassifierFilter.h"
#include "itkImageToListSampleAdaptor.h"
#include "itkMinimumDecisionRule.h"

#include "itkRegionOfInterestImageFilter.h"

#include <vector>

namespace itk
{
/**
 *\class ScalarImageKmeansImageFilter
 * \brief Classifies the intensity values of a scalar image using the K-Means algorithm.
 *
 * Given an input image with scalar values, it uses the K-Means statistical
 * classifier in order to define labels for every pixel in the image. The
 * filter is templated over the type of the input image. The output image is
 * predefined as having the same dimension of the input image and pixel type
 * unsigned char, under the assumption that the classifier will generate less
 * than 256 classes.
 *
 * You may want to look also at the RelabelImageFilter that may be used as a
 * postprocessing stage, in particular if you are interested in ordering the
 * labels by their relative size in number of pixels.
 *
 * \sa Image
 * \sa ImageKmeansModelEstimator
 * \sa KdTreeBasedKmeansEstimator, WeightedCentroidKdTreeGenerator, KdTree
 * \sa RelabelImageFilter
 *
 * \ingroup ClassificationFilters
 * \ingroup ITKClassifiers
 *
 * \sphinx
 * \sphinxexample{Segmentation/Classifiers/ClusterPixelsInGrayscaleImage,Cluster Pixels In Grayscale Image}
 * \sphinxexample{Segmentation/Classifiers/KMeansClustering,K-Means Clustering}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage = Image<unsigned char, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT ScalarImageKmeansImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ScalarImageKmeansImageFilter);

  /** Extract dimension from input and output image. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Convenient type alias for simplifying declarations. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  /** Standard class type aliases. */
  using Self = ScalarImageKmeansImageFilter;
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarImageKmeansImageFilter, ImageToImageFilter);

  /** Image type alias support */
  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;

  /** Type used for representing the Mean values. */
  using RealPixelType = typename NumericTraits<InputPixelType>::RealType;

  /** Create a List from the scalar image. */
  using AdaptorType = itk::Statistics::ImageToListSampleAdaptor<InputImageType>;

  /** Define the Measurement vector type from the AdaptorType. */
  using MeasurementVectorType = typename AdaptorType::MeasurementVectorType;

  using MembershipFunctionType = itk::Statistics::DistanceToCentroidMembershipFunction<MeasurementVectorType>;
  using ClassifierType = itk::Statistics::SampleClassifierFilter<AdaptorType>;
  using DecisionRuleType = itk::Statistics::MinimumDecisionRule;

  using ClassLabelVectorType = typename ClassifierType::ClassLabelVectorType;

  using MembershipFunctionVectorType = typename ClassifierType::MembershipFunctionVectorType;
  using MembershipFunctionOriginType = typename MembershipFunctionType::CentroidType;

  using MembershipFunctionPointer = typename MembershipFunctionType::Pointer;

  /** Create the K-d tree structure. */
  using TreeGeneratorType = itk::Statistics::WeightedCentroidKdTreeGenerator<AdaptorType>;
  using TreeType = typename TreeGeneratorType::KdTreeType;
  using EstimatorType = itk::Statistics::KdTreeBasedKmeansEstimator<TreeType>;

  using ParametersType = typename EstimatorType::ParametersType;

  using ImageRegionType = typename InputImageType::RegionType;

  using RegionOfInterestFilterType = RegionOfInterestImageFilter<InputImageType, InputImageType>;

  /** Add a new class to the classification by specifying its initial mean. */
  void
  AddClassWithInitialMean(RealPixelType mean);

  /** Return the array of Means found after the classification. */
  itkGetConstReferenceMacro(FinalMeans, ParametersType);

  /** Set/Get the UseNonContiguousLabels flag. When this is set to false the
   * labels are numbered contiguously, like in {0,1,3..N}. When the flag is set
   * to true, the labels are selected in order to span the dynamic range of the
   * output image. This last option is useful when the output image is intended
   * only for display. The default value is false. */
  itkSetMacro(UseNonContiguousLabels, bool);
  itkGetConstReferenceMacro(UseNonContiguousLabels, bool);
  itkBooleanMacro(UseNonContiguousLabels);

  /** Set Region method to constrain classification to a certain region */
  void
  SetImageRegion(const ImageRegionType & region);

  /** Get the region over which the statistics will be computed */
  itkGetConstReferenceMacro(ImageRegion, ImageRegionType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));
  // End concept checking
#endif

protected:
  ScalarImageKmeansImageFilter();
  ~ScalarImageKmeansImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** This method runs the statistical methods that identify the means of the
   * classes and the use the distances to those means in order to label the
   * image pixels.
   * \sa ImageToImageFilter::GenerateData()
   */
  void
  GenerateData() override;

  /* See superclass for doxygen. This methods additionally checks that
   * the number of means is not 0. */
  void
  VerifyPreconditions() ITKv5_CONST override;

private:
  using MeansContainer = std::vector<RealPixelType>;

  MeansContainer m_InitialMeans;

  ParametersType m_FinalMeans;

  bool m_UseNonContiguousLabels{ false };

  ImageRegionType m_ImageRegion;

  bool m_ImageRegionDefined{ false };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkScalarImageKmeansImageFilter.hxx"
#endif

#endif
