/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkProgressAccumulator_h
#define itkProgressAccumulator_h

#include "itkCommand.h"
#include "itkProcessObject.h"
#include <vector>

namespace itk
{
/**
 * \class ProgressAccumulator
 * \brief Facilitates progress reporting for filters that wrap around
 *        multiple other filters.
 *
 * This object allows a mini-pipeline filters to easily keep track of the
 * progress performed by the internal filters.
 * See DiscreteGaussianImageFilter.hxx for an implementation example.
 *
 * \sa DiscreteGaussianImageFilter
 *
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT ProgressAccumulator : public Object
{
public:
  /** Standard class type aliases. */
  using Self = ProgressAccumulator;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Typedef for inputting filters */
  using GenericFilterType = ProcessObject;
  using GenericFilterPointer = GenericFilterType::Pointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(ProgressAccumulator);

  /** Get the total progress accumulated by this object */
  itkGetConstMacro(AccumulatedProgress, float);

  /** Set the mini-pipeline filter */
  itkSetObjectMacro(MiniPipelineFilter, ProcessObject);

  /** Set the mini-pipeline filter */
  itkGetModifiableObjectMacro(MiniPipelineFilter, ProcessObject);

  /**
   * Register a filter with the progress accumulator and specify the
   * fraction of the overall progress associated with this filter.
   * The sum of the weights for all filters that are registered should
   * be 1.
   * However, if streaming is used, the weight should be divided by the
   * number of streams because each stream will reset the filter
   * after capturing its progress.
   * For example, if the desired weight of a filter is 0.4, but it is
   * streamed into 4 streams, the weight should be 0.1.
   * The ProgressAccumulator will then ensure that each of the 4 runs
   * of this filter will add 0.1 to the filter's progress.
   */
  void
  RegisterInternalFilter(GenericFilterType * filter, float weight);

  /**
   * Unregister all filters that have been registered with this object
   */
  void
  UnregisterAllFilters();

protected:
  ProgressAccumulator();
  ~ProgressAccumulator() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /**  Command for observing progress of pipeline filters */
  using CommandType = MemberCommand<Self>;
  using CommandPointer = CommandType::Pointer;

  /** Structure associated with each filter in the pipeline */
  struct FilterRecord
  {
    // Pointer to the filter
    GenericFilterPointer Filter;

    // The weight of the filter in total progress of the mini-pipeline
    float Weight;

    // The tags for adding/removing observers to mini-pipeline filter
    unsigned long ProgressObserverTag;
    unsigned long StartObserverTag;

    float AccumulatedProgress{ 0.0 };
  };

  /** A callback function that is called by the progressing filters */
  void
  ReportProgress(Object * who, const EventObject & event);

  /** The client mini-pipeline filter */
  GenericFilterPointer m_MiniPipelineFilter{};

  /** An array of record structures */
  using FilterRecordVector = std::vector<struct FilterRecord>;

  /** The total accumulated progress */
  float m_AccumulatedProgress{};

  /** The total accumulated progress for multiple runs of the mini-pipeline */
  float m_BaseAccumulatedProgress{};

  /**
   * A list of progress proportions of the different filters in the
   * pipeline
   */
  FilterRecordVector m_FilterRecord{};

  /** The callback command */
  CommandPointer m_CallbackCommand{};
};
} // End namespace itk

#endif // itkProgressAccumulator_h_
