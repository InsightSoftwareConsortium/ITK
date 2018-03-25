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
#ifndef itkLevelSetIterationUpdateCommand_h
#define itkLevelSetIterationUpdateCommand_h

#include "itkCommand.h"
#include "itkWeakPointer.h"
#include "itkIntTypes.h"

namespace itk
{

/** \class LevelSetIterationUpdateCommand
 * \brief Call update on one filter when another filter iterates.
 *
 * \tparam TIteratingFilter Filter that invokes iteration events.
 * \tparam TFilterToUpdate  Filter to call update on when the iteration event
 * occurs.
 *
 * \ingroup ITKLevelSetsv4Visualization
 */
template< typename TIteratingFilter, typename TFilterToUpdate >
class ITK_TEMPLATE_EXPORT LevelSetIterationUpdateCommand : public Command
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetIterationUpdateCommand);

  using Self = LevelSetIterationUpdateCommand;
  using Superclass = Command;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  using IteratingFilterType = TIteratingFilter;
  using FilterToUpdateType = TFilterToUpdate;

  /** Run-time type information (and related methods). */
  itkTypeMacro( LevelSetIterationUpdateCommand, Command );

  itkNewMacro( Self );

  void Execute( const Object* caller, const EventObject& event ) override;

  void Execute( Object* caller, const EventObject& event ) override;

  /** Set/Get the filter to call Update() on. */
  itkSetObjectMacro( FilterToUpdate, FilterToUpdateType );
  itkGetModifiableObjectMacro(FilterToUpdate, FilterToUpdateType );

  /** Set/Get the period that Update() is called on the FilterToUpdate.  It is
   * in units of iterations. */
  itkSetMacro( UpdatePeriod, IdentifierType );
  itkGetConstMacro( UpdatePeriod, IdentifierType );

protected:
  LevelSetIterationUpdateCommand();
  ~LevelSetIterationUpdateCommand() override;

private:
  WeakPointer< FilterToUpdateType >  m_FilterToUpdate;
  IdentifierType                     m_UpdatePeriod;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetIterationUpdateCommand.hxx"
#endif

#endif
