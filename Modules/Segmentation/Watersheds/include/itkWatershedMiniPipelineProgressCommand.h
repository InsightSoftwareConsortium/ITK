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
class ITKWatersheds_EXPORT WatershedMiniPipelineProgressCommand:public Command
{
public:
  /** Smart pointer declaration methods */
  typedef WatershedMiniPipelineProgressCommand Self;
  typedef Command                              Superclass;
  typedef itk::SmartPointer< Self >            Pointer;
  typedef itk::SmartPointer< const Self >      ConstPointer;
  itkTypeMacro(WatershedMiniPipelineProgressCommand, Command);
  itkNewMacro(Self);

  /** Standard Command virtual methods */
  virtual void Execute(Object *caller, const EventObject & event) ITK_OVERRIDE;

  virtual void Execute(const Object *caller, const EventObject & event) ITK_OVERRIDE;

  /** Set/Get the filter whose UpdateProgress will be set by this
   * command object */
  void SetFilter(ProcessObject *p)
  { m_Filter = p; }
  const ProcessObject * GetFilter()
  { return m_Filter; }

  /** Set/Get the base count for stepping through filter progress values */
  itkSetMacro(Count, double);
  itkGetConstMacro(Count, double);

  /** Set/Get the number of filters that this command will expect to be
   * observing */
  itkSetMacro(NumberOfFilters, unsigned int);
  itkGetConstMacro(NumberOfFilters, unsigned int);

protected:
  WatershedMiniPipelineProgressCommand():m_Count(0.0), m_Filter(ITK_NULLPTR),
    m_NumberOfFilters(1) {}
  virtual ~WatershedMiniPipelineProgressCommand() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  double         m_Count;
  ProcessObject *m_Filter;
  unsigned int   m_NumberOfFilters;
};
} // end namespace itk

#endif
