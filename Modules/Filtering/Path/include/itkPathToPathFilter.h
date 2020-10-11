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
#ifndef itkPathToPathFilter_h
#define itkPathToPathFilter_h

#include "itkPathSource.h"

namespace itk
{
/**
 *\class PathToPathFilter
 * \brief Base class for filters that take a path as input and produce a path as output.
 *
 * PathToPathFilter is the base class for all process objects that output
 * path data and require path data as input. Specifically, this class
 * defines the SetInput() method for defining the input to a filter.
 *
 * \ingroup PathFilters
 * \ingroup ITKPath
 */

template <typename TInputPath, typename TOutputPath>
class ITK_TEMPLATE_EXPORT PathToPathFilter : public PathSource<TOutputPath>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PathToPathFilter);

  /** Standard class type aliases. */
  using Self = PathToPathFilter;
  using Superclass = PathSource<TOutputPath>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PathToPathFilter, PathSource);

  /** Some convenient type alias. */
  using InputPathType = TInputPath;
  using InputPathPointer = typename InputPathType::Pointer;
  using InputPathConstPointer = typename InputPathType::ConstPointer;

  /** Set/Get the path input of this process object.  */
  using Superclass::SetInput;
  virtual void
  SetInput(const InputPathType * path);

  virtual void
  SetInput(unsigned int, const TInputPath * path);

  const InputPathType *
  GetInput();

  const InputPathType *
  GetInput(unsigned int idx);

protected:
  PathToPathFilter();
  ~PathToPathFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** What is the input requested region that is required to produce the output
   * requested region?  Up till and including now, the base assumption is that
   * the largest possible region will be requested of the input.  If this method
   * is overridden, the new method should call its superclass' implementation as
   * its first step.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPathToPathFilter.hxx"
#endif

#endif
