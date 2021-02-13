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
#ifndef itkScalarImageToRunLengthFeaturesFilter_h
#define itkScalarImageToRunLengthFeaturesFilter_h

#include "itkDataObjectDecorator.h"

#include "itkHistogramToRunLengthFeaturesFilter.h"
#include "itkScalarImageToRunLengthMatrixFilter.h"

namespace itk
{
namespace Statistics
{
/**
 *\class ScalarImageToRunLengthFeaturesFilter
 *  \brief This class computes run length descriptions from an image.
 *
 * By default, run length features are computed for each spatial
 * direction and then averaged afterward, so it is possible to access the
 * standard deviations of the texture features. These values give a clue as
 * to texture anisotropy. However, doing this is much more work, because it
 * involved computing one for each offset given. To compute a single
 * matrix using the first offset, call FastCalculationsOn(). If this is called,
 * then the texture standard deviations will not be computed (and will be set
 * to zero), but texture computation will be much faster.
 *
 * This class is templated over the input image type.
 *
 * Template Parameters:
 * The image type, and the type of histogram frequency container. If you are
 * using a large number of bins per axis, a sparse frequency container may be
 * advisable.  The default is to use a dense frequency container.
 *
 * Inputs and parameters:
 * -# An image
 * -# A mask defining the region over which texture features will be
 *    calculated. (Optional)
 * -# The pixel value that defines the "inside" of the mask. (Optional, defaults
 *    to 1 if a mask is set.)
 * -# The set of features to be calculated. These features are defined
 *    in the HistogramToRunLengthFeaturesFilter class.
 * -# The number of intensity bins. (Optional, defaults to 256.)
 * -# The set of directions (offsets) to average across. (Optional, defaults to
 *    {(-1, 0), (-1, -1), (0, -1), (1, -1)} for 2D images and scales analogously
 *    for ND images.)
 * -# The pixel intensity range over which the features will be calculated.
 *    (Optional, defaults to the full dynamic range of the pixel type.)
 * -# The distance range over which the features will be calculated.
 *    (Optional, defaults to the full dynamic range of double type.)
 *
 * In general, the default parameter values should be sufficient.
 *
 * Outputs:
 * (1) The average value of each feature.
 * (2) The standard deviation in the values of each feature.
 *
 * Print references:
 * M. M. Galloway. Texture analysis using gray level run lengths. Computer
 * Graphics and Image Processing, 4:172-179, 1975.
 *
 * A. Chu, C. M. Sehgal, and J. F. Greenleaf. Use of gray value distribution of
 * run lengths for texture analysis.  Pattern Recognition Letters, 11:415-420,
 * 1990.
 *
 * B. R. Dasarathy and E. B. Holder. Image characterizations based on joint
 * gray-level run-length distributions. Pattern Recognition Letters, 12:490-502,
 * 1991.
 *
 * IJ article: https://www.insight-journal.org/browse/publication/231
 *
 * \sa ScalarImageToRunLengthFeaturesFilter
 * \sa ScalarImageToRunLengthMatrixFilter
 * \sa HistogramToRunLengthFeaturesFilter
 *
 * \author: Nick Tustison
 * \ingroup ITKStatistics
 */

template <typename TImageType, typename THistogramFrequencyContainer = DenseFrequencyContainer2>
class ITK_TEMPLATE_EXPORT ScalarImageToRunLengthFeaturesFilter : public ProcessObject
{
public:
  /** Standard type alias */
  using Self = ScalarImageToRunLengthFeaturesFilter;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarImageToRunLengthFeaturesFilter, ProcessObject);

  /** standard New() method support */
  itkNewMacro(Self);

  using FrequencyContainerType = THistogramFrequencyContainer;
  using ImageType = TImageType;
  using ImagePointer = typename ImageType::Pointer;

  using PixelType = typename ImageType::PixelType;
  using OffsetType = typename ImageType::OffsetType;
  using OffsetVector = VectorContainer<unsigned char, OffsetType>;
  using OffsetVectorPointer = typename OffsetVector::Pointer;
  using OffsetVectorConstPointer = typename OffsetVector::ConstPointer;

  using RunLengthMatrixFilterType = ScalarImageToRunLengthMatrixFilter<ImageType, FrequencyContainerType>;

  using HistogramType = typename RunLengthMatrixFilterType::HistogramType;

  using RunLengthFeaturesFilterType = HistogramToRunLengthFeaturesFilter<HistogramType>;

  // More work needs to be done to fix wrapping
  // using RunLengthFeatureName = itk::Statistics::RunLengthFeatureEnum;
  using RunLengthFeatureName = uint8_t;
  using FeatureNameVector = VectorContainer<unsigned char, RunLengthFeatureName>;
  using FeatureNameVectorPointer = typename FeatureNameVector::Pointer;
  using FeatureNameVectorConstPointer = typename FeatureNameVector::ConstPointer;
  using FeatureValueVector = VectorContainer<unsigned char, double>;
  using FeatureValueVectorPointer = typename FeatureValueVector::Pointer;

  /** Smart Pointer type to a DataObject. */
  using DataObjectPointer = DataObject::Pointer;

  /** Type of DataObjects used for scalar outputs */
  using FeatureValueVectorDataObjectType = DataObjectDecorator<FeatureValueVector>;

  const FeatureValueVectorDataObjectType *
  GetFeatureMeansOutput() const;

  const FeatureValueVectorDataObjectType *
  GetFeatureStandardDeviationsOutput() const;

  /** Connects the input image for which the features are going to be computed
   */
  using Superclass::SetInput;
  void
  SetInput(const ImageType *);

  const ImageType *
  GetInput() const;

  /** Return the feature means and deviations.  */
  itkGetConstReferenceObjectMacro(FeatureMeans, FeatureValueVector);
  itkGetConstReferenceObjectMacro(FeatureStandardDeviations, FeatureValueVector);

  /** Set the desired feature set. Optional, for default value see above. */
  itkSetConstObjectMacro(RequestedFeatures, FeatureNameVector);
  itkGetConstObjectMacro(RequestedFeatures, FeatureNameVector);

  /** Set the  offsets over which the co-occurrence pairs will be computed.
      Optional; for default value see above. */
  itkSetConstObjectMacro(Offsets, OffsetVector);
  itkGetConstObjectMacro(Offsets, OffsetVector);

  /** Set number of histogram bins along each axis.
      Optional; for default value see above. */
  void
  SetNumberOfBinsPerAxis(unsigned int);

  /** Set the min and max (inclusive) pixel value that will be used for
      feature calculations. Optional; for default value see above. */
  void
  SetPixelValueMinMax(PixelType min, PixelType max);

  /** Set the min and max (inclusive) pixel value that will be used for
      feature calculations. Optional; for default value see above. */
  void
  SetDistanceValueMinMax(double min, double max);

  /** Connects the mask image for which the histogram is going to be computed.
      Optional; for default value see above. */
  void
  SetMaskImage(const ImageType *);

  const ImageType *
  GetMaskImage() const;

  /** Set the pixel value of the mask that should be considered "inside" the
      object. Optional; for default value see above. */
  void
  SetInsidePixelValue(PixelType insidePixelValue);

  itkGetConstMacro(FastCalculations, bool);
  itkSetMacro(FastCalculations, bool);
  itkBooleanMacro(FastCalculations);

protected:
  ScalarImageToRunLengthFeaturesFilter();
  ~ScalarImageToRunLengthFeaturesFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  FastCompute();

  void
  FullCompute();

  /** This method causes the filter to generate its output. */
  void
  GenerateData() override;

  /** Make a DataObject to be used for output output. */
  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObjectPointer MakeOutput(DataObjectPointerArraySizeType) override;

private:
  typename RunLengthMatrixFilterType::Pointer m_RunLengthMatrixGenerator;

  FeatureValueVectorPointer     m_FeatureMeans;
  FeatureValueVectorPointer     m_FeatureStandardDeviations;
  FeatureNameVectorConstPointer m_RequestedFeatures;
  OffsetVectorConstPointer      m_Offsets;
  bool                          m_FastCalculations;
};
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkScalarImageToRunLengthFeaturesFilter.hxx"
#endif

#endif
