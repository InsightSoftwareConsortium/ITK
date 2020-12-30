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
#ifndef itkSampleToHistogramFilter_h
#define itkSampleToHistogramFilter_h

#include "itkMacro.h"
#include "itkProcessObject.h"
#include "itkMeasurementVectorTraits.h"
#include "itkSimpleDataObjectDecorator.h"

itkDeclareExceptionMacro(SampleToHistogramFilterException, ExceptionObject, "Histogram-related Exception");
itkDeclareExceptionMacro(MissingHistogramSizeInput,
                         SampleToHistogramFilterException,
                         "Histogram Size input is missing");
itkDeclareExceptionMacro(MissingHistogramMarginalScaleInput,
                         SampleToHistogramFilterException,
                         "Histogram marginal scale input is missing");
itkDeclareExceptionMacro(NullSizeHistogramInputMeasurementVectorSize,
                         SampleToHistogramFilterException,
                         "Input sample MeasurementVectorSize is zero");
itkDeclareExceptionMacro(MissingHistogramBinMaximumInput,
                         SampleToHistogramFilterException,
                         "Histogram Bin Maximum input is missing");
itkDeclareExceptionMacro(MissingHistogramBinMinimumInput,
                         SampleToHistogramFilterException,
                         "Histogram Bin Minimum input is missing");
itkDeclareExceptionMacro(HistogramWrongNumberOfComponents,
                         SampleToHistogramFilterException,
                         "Histogram has wrong number of components");

namespace itk
{
namespace Statistics
{

/**
 *\class SampleToHistogramFilter
 *  \brief Computes the Histogram corresponding to a Sample.
 *
 * This filter produces as output the histogram corresponding to
 * the values of a Sample.
 *
 * \sa Sample, Histogram
 *
 * \ingroup ITKStatistics
 *
 * \sphinx
 * \sphinxexample{Numerics/Statistics/CreateHistogramFromListOfMeasurements,Create Histogram From List Of Measurements}
 * \endsphinx
 */

template <typename TSample, typename THistogram>
class ITK_TEMPLATE_EXPORT SampleToHistogramFilter : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SampleToHistogramFilter);

  /** Standard class type aliases */
  using Self = SampleToHistogramFilter;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(SampleToHistogramFilter, ProcessObject);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** MeasurementVector type alias support */
  using SampleType = TSample;
  using HistogramType = THistogram;
  using MeasurementVectorType = typename SampleType::MeasurementVectorType;
  using MeasurementType = typename MeasurementVectorType::ValueType;
  using HistogramSizeType = typename HistogramType::SizeType;
  using HistogramMeasurementType = typename HistogramType::MeasurementType;
  using HistogramMeasurementVectorType = typename HistogramType::MeasurementVectorType;

  /** Type for the data object output */
  using DataObjectPointer = typename Superclass::DataObjectPointer;

  using Superclass::SetInput;

  /** Set/Get the input sample */
  virtual void
  SetInput(const SampleType * sample);

  virtual const SampleType *
  GetInput() const;

  /** Get the output Histogram */
  const HistogramType *
  GetOutput() const;

  /** Type of DataObjects to use for Size inputs */
  using InputHistogramSizeObjectType = SimpleDataObjectDecorator<HistogramSizeType>;

  /** Type of DataObjects to use for Marginal Scale inputs */
  using InputHistogramMeasurementObjectType = SimpleDataObjectDecorator<HistogramMeasurementType>;

  /** Type of DataObjects to use for Minimum and Maximums values of the
   * histogram bins. */
  using InputHistogramMeasurementVectorObjectType = SimpleDataObjectDecorator<HistogramMeasurementVectorType>;

  /** Type of DataObjects to use for AutoMinimumMaximum input */
  using InputBooleanObjectType = SimpleDataObjectDecorator<bool>;

  /** Methods for setting and getting the histogram size.  The histogram size
   * is encapsulated inside a decorator class. For this reason, it is possible
   * to set and get the decorator class, but it is only possible to set the
   * histogram size by value. This macro declares the methods
   * SetHistogramSize(), SetHistogramSizeInput(), GetHistogramSizeInput().
   */
  itkSetGetDecoratedInputMacro(HistogramSize, HistogramSizeType);

  /** Methods for setting and getting the Marginal scale value.  The marginal
   * scale is used when the type of the measurement vector components are of
   * integer type. */
  itkSetGetDecoratedInputMacro(MarginalScale, HistogramMeasurementType);

  /** Methods for setting and getting the Minimum and Maximum values of the
   * histogram bins.
   \warning To use those values you need to set the AutoMinimumMaximum flag to false.*/
  itkSetGetDecoratedInputMacro(HistogramBinMinimum, HistogramMeasurementVectorType);
  itkSetGetDecoratedInputMacro(HistogramBinMaximum, HistogramMeasurementVectorType);

  /** Methods for setting and getting the boolean flag that defines whether the
   * minimum and maximum of the histogram are going to be computed
   * automatically from the values of the sample.
   \warning If the flag is set to false, it is required to set HistogramBinMinimum and HistogramBinMaximum. */
  itkSetGetDecoratedInputMacro(AutoMinimumMaximum, bool);

  /** Method that facilitates the use of this filter in the internal
   * pipeline of another filter. */
  virtual void
  GraftOutput(DataObject * graft);

protected:
  SampleToHistogramFilter();
  ~SampleToHistogramFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Make a DataObject of the correct type to used as the specified
   * output. This method
   * is automatically called when DataObject::DisconnectPipeline() is
   * called.
   * \sa ProcessObject
   */
  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObjectPointer
  MakeOutput(DataObjectPointerArraySizeType idx) override;

  // Where the histogram is actually computed
  void
  GenerateData() override;

private:
  /** SafeAssign -- avoid numeric overflow/underflow */
  HistogramMeasurementType
  SafeAssign(MeasurementType from) const
  {
    if (NumericTraits<HistogramMeasurementType>::is_integer)
    {
      auto fromMax = static_cast<MeasurementType>(NumericTraits<HistogramMeasurementType>::max());
      auto fromMin = static_cast<MeasurementType>(NumericTraits<HistogramMeasurementType>::min());

      if (from >= fromMax)
      {
        return NumericTraits<HistogramMeasurementType>::max();
      }
      else if (from <= fromMin)
      {
        return NumericTraits<HistogramMeasurementType>::min();
      }
    }
    return static_cast<HistogramMeasurementType>(from);
  }

}; // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSampleToHistogramFilter.hxx"
#endif

#endif
