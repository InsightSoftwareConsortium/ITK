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
#ifndef itkChainCodeToFourierSeriesPathFilter_h
#define itkChainCodeToFourierSeriesPathFilter_h

#include "itkPathToPathFilter.h"
#include "itkOffset.h"
// Templates require interfaces conforming to itkPath.h and itkChainCodePath.h

namespace itk
{
/**
 *\class ChainCodeToFourierSeriesPathFilter
 * \brief Filter that produces a Fourier series version of a chain code path
 *
 * ChainCodeToFourierSeriesPathFilter produces a Fourier series representation
 * of a chain code path. By default, the first 8 harmonics (frequency
 * coefficients, which include the "DC" term) are computed.
 * SetNumberOfHarmonics() can be used to override this value.
 * Because the requested number of harmonics may not be able to be computed,
 * it is advisable to check the number of harmonics in the actual output.
 *
 * \ingroup PathFilters
 * \ingroup ITKPath
 */
template <typename TInputChainCodePath, typename TOutputFourierSeriesPath>
class ITK_TEMPLATE_EXPORT ChainCodeToFourierSeriesPathFilter
  : public PathToPathFilter<TInputChainCodePath, TOutputFourierSeriesPath>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ChainCodeToFourierSeriesPathFilter);

  /** Standard class type aliases. */
  using Self = ChainCodeToFourierSeriesPathFilter;
  using Superclass = PathToPathFilter<TInputChainCodePath, TOutputFourierSeriesPath>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ChainCodeToFourierSeriesPathFilter, PathToPathFilter);

  /** Some convenient type alias. */
  using InputPathType = TInputChainCodePath;
  using InputPathPointer = typename InputPathType::Pointer;
  using InputPathInputType = typename InputPathType::InputType;
  using OutputPathType = TOutputFourierSeriesPath;
  using OutputPathPointer = typename OutputPathType::Pointer;
  using OutputPathInputType = typename OutputPathType::InputType;
  using IndexType = typename InputPathType::IndexType;
  using OffsetType = typename InputPathType::OffsetType;
  using VectorType = typename OutputPathType::VectorType;

  /** Set the number of harmonics to try to compute.  By default, the first 8
   * harmonics (frequency coefficients, which include the "DC" term) are
   * computed.  SetNumberOfHarmonics() can be used to override this value.  If
   * the chain code has too few steps to calculate the desired number of
   * harmonics (due to the Nyquist criterion), then as many harmonics as are
   * possible (input->NumberOfSteps()/2) will be calculated, but at least 2
   * harmonics will always be calculated.*/
  itkSetMacro(NumberOfHarmonics, unsigned int);

protected:
  ChainCodeToFourierSeriesPathFilter();
  ~ChainCodeToFourierSeriesPathFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

private:
  unsigned int m_NumberOfHarmonics;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkChainCodeToFourierSeriesPathFilter.hxx"
#endif

#endif
