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
#ifndef itkWatershedMiniPipelineProgressCommand_h
#define itkWatershedMiniPipelineProgressCommand_h

#include "itkProcessObject.h"
#include "itkCommand.h"
#include "ITKWatershedsExport.h"

namespace itk
{
/** \class WatershedMiniPipelineProgressCommand
 * A specialized Command object for updating the progress of a
 *  MiniPipeline.  Follows the progress of a series of filters
 *  and calls UpdateProgress on another filter (i.e. the filter
 * implementing the mini-pipeline).
 * \ingroup ITKWatersheds
 */
class ITKWatersheds_EXPORT WatershedMiniPipelineProgressCommand : public Command
{
public:
  /** Smart pointer declaration methods */
  using Self = WatershedMiniPipelineProgressCommand;
  using Superclass = Command;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;
  itkTypeMacro(WatershedMiniPipelineProgressCommand, Command);
  itkNewMacro(Self);

  /** Standard Command virtual methods */
  void
  Execute(Object * caller, const EventObject & event) override;

  void
  Execute(const Object * caller, const EventObject & event) override;

  /** Set/Get the filter whose UpdateProgress will be set by this
   * command object */
  void
  SetFilter(ProcessObject * p)
  {
    m_Filter = p;
  }
  const ProcessObject *
  GetFilter()
  {
    return m_Filter;
  }

  /** Set/Get the base count for stepping through filter progress values */
  itkSetMacro(Count, double);
  itkGetConstMacro(Count, double);

  /** Set/Get the number of filters that this command will expect to be
   * observing */
  itkSetMacro(NumberOfFilters, unsigned int);
  itkGetConstMacro(NumberOfFilters, unsigned int);

protected:
  WatershedMiniPipelineProgressCommand() = default;
  ~WatershedMiniPipelineProgressCommand() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  double          m_Count{ 0.0 };
  ProcessObject * m_Filter{ nullptr };
  unsigned int    m_NumberOfFilters{ 1 };
};
} // end namespace itk

#endif
