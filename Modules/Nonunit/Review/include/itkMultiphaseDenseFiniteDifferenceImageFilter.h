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
#ifndef itkMultiphaseDenseFiniteDifferenceImageFilter_h
#define itkMultiphaseDenseFiniteDifferenceImageFilter_h

#include "itkMultiphaseFiniteDifferenceImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkSignedMaurerDistanceMapImageFilter.h"
#include "itkNumericTraits.h"
#include "itkNeighborhoodAlgorithm.h"

#include <list>

namespace itk
{
/**
 * \class MultiphaseDenseFiniteDifferenceImageFilter
 *
 * This filter implements a layer of the finite difference solver hierarchy that
 * performs "dense" iteration, ie. iteration over all pixels in the input and
 * output at each change calculation and update step. Dense iteration is in
 * contrast to a "sparse" iteration over a subset of the pixels.  See
 * documentation for FiniteDifferenceImageFilter for an overview of the
 * iterative finite difference algorithm:
 *
 * \par
 * \f$u_{\mathbf{i}}^{n+1}=u^n_{\mathbf{i}}+\Delta u^n_{\mathbf{i}}\Delta t\f$
 *
 * \par
 * The generic code for performing iterations and updates at each time
 * step is inherited from the parent class.  This class defines an update
 * buffer for \f$ \Delta \f$ and the methods CalculateChange() and
 * ApplyUpdate(). These methods are designed to automatically thread their
 * execution.  \f$ \Delta \f$ is defined as an image of identical size and type
 * as the output image.
 *
 * \par
 * As we descend through each layer in the hierarchy, we know more and more
 * about the specific application of our filter.  At this level, we
 * have committed to iteration over each pixel in an image. We take advantage
 * of that knowledge to multithread the iteration and update methods.
 *
 * \par Inputs and Outputs
 * This is an image to image filter.  The specific types of the images are not
 * fixed at this level in the hierarchy.
 *
 * \par How to use this class
 * This filter is only one layer in a branch the finite difference solver
 * hierarchy.  It does not define the function used in the CalculateChange() and
 * it does not define the stopping criteria (Halt method).  To use this class,
 * subclass it to a specific instance that supplies a function and Halt()
 * method.
 *
 * Based on the paper:
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
 *
 * \ingroup ImageFilters
 * \sa FiniteDifferenceImageFilter
 * \ingroup ITKReview
 */
template <typename TInputImage,
          typename TFeatureImage,
          typename TOutputImage,
          typename TFunction,
          typename TIdCell = unsigned int>
class ITK_TEMPLATE_EXPORT MultiphaseDenseFiniteDifferenceImageFilter
  : public MultiphaseFiniteDifferenceImageFilter<TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MultiphaseDenseFiniteDifferenceImageFilter);

  /** Standard class type aliases */
  using Self = MultiphaseDenseFiniteDifferenceImageFilter;
  using Superclass =
    MultiphaseFiniteDifferenceImageFilter<TInputImage, TFeatureImage, TOutputImage, TFunction, TIdCell>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(MultiphaseDenseFiniteDifferenceImageFilter, MultiphaseFiniteDifferenceImageFilter);

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Convenient type alias */
  using InputImageType = typename Superclass::InputImageType;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using InputRegionType = typename Superclass::InputRegionType;
  using InputSizeType = typename Superclass::InputSizeType;
  using InputSizeValueType = typename Superclass::InputSizeValueType;
  using InputIndexType = typename Superclass::InputIndexType;
  using InputIndexValueType = typename Superclass::InputIndexValueType;
  using InputPixelType = typename Superclass::InputPixelType;
  using InputPointType = typename Superclass::InputPointType;
  using InputSpacingType = typename Superclass::InputSpacingType;
  using ValueType = typename InputImageType::ValueType;

  using FeatureImageType = typename Superclass::FeatureImageType;
  using FeatureSizeType = typename Superclass::FeatureSizeType;
  using FeatureImagePointer = typename Superclass::FeatureImagePointer;
  using FeatureRegionType = typename Superclass::FeatureRegionType;
  using FeatureSpacingType = typename Superclass::FeatureSpacingType;
  using FeaturePointType = typename Superclass::FeaturePointType;
  using FeaturePixelType = typename Superclass::FeaturePixelType;

  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImagePointer = typename Superclass::OutputImagePointer;
  using OutputRegionType = typename Superclass::OutputRegionType;
  using OutputSizeType = typename Superclass::OutputSizeType;
  using SizeValueType = typename Superclass::OutputSizeValueType;
  using OutputIndexType = typename Superclass::OutputIndexType;
  using OutputIndexValueType = typename Superclass::OutputIndexValueType;
  using OutputPixelType = typename OutputImageType::PixelType;

  using IdCellType = typename Superclass::IdCellType;

  using ThresholdFilterType = BinaryThresholdImageFilter<InputImageType, InputImageType>;
  using ThresholdFilterPointer = typename ThresholdFilterType::Pointer;

  using MaurerType = SignedMaurerDistanceMapImageFilter<InputImageType, InputImageType>;
  using MaurerPointer = typename MaurerType::Pointer;

  using FiniteDifferenceFunctionType = typename Superclass::FiniteDifferenceFunctionType;
  using FiniteDifferenceFunctionPointer = typename Superclass::FiniteDifferenceFunctionPointer;
  using NeighborhoodIteratorType = typename FiniteDifferenceFunctionType::NeighborhoodType;

  /** The value type of a time step.  Inherited from the superclass. */
  using TimeStepType = typename Superclass::TimeStepType;

  using FaceCalculatorType = NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>;
  using FaceListType = typename FaceCalculatorType::FaceListType;

  void
  SetFunctionCount(const IdCellType & n);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputTimesDoubleCheck, (Concept::MultiplyOperator<OutputPixelType, double>));
  itkConceptMacro(OutputAdditiveOperatorsCheck, (Concept::AdditiveOperators<OutputPixelType>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<FeaturePixelType, OutputPixelType>));
  // End concept checking
#endif

  itkSetMacro(ReinitializeCounter, unsigned int);
  itkGetMacro(ReinitializeCounter, unsigned int);

protected:
  MultiphaseDenseFiniteDifferenceImageFilter()
  {
    this->m_ReinitializeCounter = 1;
    // FIXME: this->m_UpdateCounter really used?
    // this->m_UpdateCounter = 0;        // FIXME: Should this be a bool ?
  }

  ~MultiphaseDenseFiniteDifferenceImageFilter() override = default;

  void
  PrintSelf(std::ostream &, Indent indent) const override;

  /** A simple method to copy the data from the input to the output.  ( Supports
   * "read-only" image adaptors in the case where the input image type converts
   * to a different output image type. )  */
  void
  CopyInputToOutput() override;

  void
  PostProcessOutput() override;

  /** This method applies changes from the m_UpdateBuffer to the output using
   * the ThreadedApplyUpdate() method and a multithreading mechanism.  "dt" is
   * the time step to use for the update of each pixel. */
  void
  ApplyUpdate(TimeStepType dt) override;

  unsigned int m_ReinitializeCounter; // FIXME: Should this be a boolean ?
  // unsigned int m_UpdateCounter;        // FIXME: Should this be a boolean ?

private:
  /** This method allocates storage in m_UpdateBuffer.  It is called from
   * Superclass::GenerateData(). */
  void
  AllocateUpdateBuffer() override;

  /** This method populates an update buffer with changes for each pixel in the
   * output using the ThreadedCalculateChange() method and a multithreading
   * mechanism. Returns value is a time step to be used for the update. */
  TimeStepType
  CalculateChange() override;

  /** The buffer that holds the updates for an iteration of the algorithm. */
  std::vector<InputImagePointer> m_UpdateBuffers;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMultiphaseDenseFiniteDifferenceImageFilter.hxx"
#endif

#endif
