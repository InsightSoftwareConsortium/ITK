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
#include "itkEventObject.h"

namespace itk
{
void
EventObject
::Print(std::ostream & os) const
{
  Indent indent;

  this->PrintHeader(os, 0);
  this->PrintSelf( os, indent.GetNextIndent() );
  this->PrintTrailer(os, 0);
}

/**
 * Define a default print header for all objects.
 */
void
EventObject
::PrintHeader(std::ostream & os, Indent indent) const
{
  os << std::endl;
  os << indent << "itk::" << this->GetEventName() << " (" << this << ")\n";
}

/**
 * Define a default print trailer for all objects.
 */
void
EventObject
::PrintTrailer(std::ostream & os, Indent indent) const
{
  os << indent << std::endl;
}

void
EventObject
::PrintSelf(std::ostream &, Indent) const
{}

/**
 * Define some common ITK events
 */
itkEventMacroDefinition(NoEvent, EventObject)
itkEventMacroDefinition(AnyEvent, EventObject)
itkEventMacroDefinition(DeleteEvent, AnyEvent)
itkEventMacroDefinition(StartEvent, AnyEvent)
itkEventMacroDefinition(EndEvent, AnyEvent)
itkEventMacroDefinition(ProgressEvent, AnyEvent)
itkEventMacroDefinition(ExitEvent, AnyEvent)
itkEventMacroDefinition(AbortEvent, AnyEvent)
itkEventMacroDefinition(ModifiedEvent, AnyEvent)
itkEventMacroDefinition(InitializeEvent, AnyEvent)
itkEventMacroDefinition(IterationEvent, AnyEvent)
itkEventMacroDefinition(MultiResolutionIterationEvent,IterationEvent)
itkEventMacroDefinition(PickEvent, AnyEvent)
itkEventMacroDefinition(StartPickEvent, PickEvent)
itkEventMacroDefinition(EndPickEvent, PickEvent)
itkEventMacroDefinition(AbortCheckEvent, PickEvent)
itkEventMacroDefinition(FunctionEvaluationIterationEvent, IterationEvent)
itkEventMacroDefinition(GradientEvaluationIterationEvent, IterationEvent)
itkEventMacroDefinition(FunctionAndGradientEvaluationIterationEvent, IterationEvent)
itkEventMacroDefinition(UserEvent, AnyEvent)

} // end namespace itk
