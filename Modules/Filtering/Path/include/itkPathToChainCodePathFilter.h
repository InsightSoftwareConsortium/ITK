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
#ifndef itkPathToChainCodePathFilter_h
#define itkPathToChainCodePathFilter_h

#include "itkPathToPathFilter.h"
#include "itkOffset.h"
// Templates require interfaces conforming to itkPath.h and itkChainCodePath.h

namespace itk
{
/**
 *\class PathToChainCodePathFilter
 * \brief Filter that produces a chain code version of a path.
 *
 * PathToChainCodePathFilter produces a chain code representation of a path.
 * If MaximallyConnectedOn() is called, then the resulting chain code will be
 * maximally connected (for example, 4-connected instead of 8-connected in 2D).
 *
 * \ingroup PathFilters
 * \ingroup ITKPath
 */
template <typename TInputPath, typename TOutputChainCodePath>
class ITK_TEMPLATE_EXPORT PathToChainCodePathFilter : public PathToPathFilter<TInputPath, TOutputChainCodePath>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PathToChainCodePathFilter);

  /** Standard class type aliases. */
  using Self = PathToChainCodePathFilter;
  using Superclass = PathToPathFilter<TInputPath, TOutputChainCodePath>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PathToChainCodePathFilter, PathToPathFilter);

  /** Some convenient type alias. */
  using InputPathType = TInputPath;
  using InputPathPointer = typename InputPathType::Pointer;
  using InputPathInputType = typename InputPathType::InputType;
  using OutputPathType = TOutputChainCodePath;
  using OutputPathPointer = typename OutputPathType::Pointer;
  using OutputPathInputType = typename OutputPathType::InputType;
  using IndexType = typename InputPathType::IndexType;
  using OffsetType = typename InputPathType::OffsetType;

  /** Set/Get the direction in which to reflect the data. Default is "Off". */
  itkSetMacro(MaximallyConnected, bool);
  itkGetConstMacro(MaximallyConnected, bool);
  itkBooleanMacro(MaximallyConnected);

protected:
  PathToChainCodePathFilter();
  ~PathToChainCodePathFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

private:
  bool m_MaximallyConnected{ false };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPathToChainCodePathFilter.hxx"
#endif

#endif
