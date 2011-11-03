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
#ifndef __itkIterationUpdateCommand_hxx
#define __itkIterationUpdateCommand_hxx

#include "itkIterationUpdateCommand.h"

namespace itk
{

template< class TIteratingFilter, class TFilterToUpdate >
void
IterationUpdateCommand< TIteratingFilter, TFilterToUpdate >
::Execute( const Object* caller, const EventObject& event )
{
  this->Execute( const_cast< Object* >( caller ), event );
}

template< class TIteratingFilter, class TFilterToUpdate >
void
IterationUpdateCommand< TIteratingFilter, TFilterToUpdate >
::Execute( Object* caller, const EventObject& event )
{
  IteratingFilterType * filter = dynamic_cast< IteratingFilterType * >( caller );

  // Was filter->AddObserver() called correctly?
  itkAssertInDebugAndIgnoreInReleaseMacro( filter != NULL );

  // If we have the right event.
  if( IterationEvent().CheckEvent( &event ) )
    {
    // Was the FilterToUpdate set?
    itkAssertInDebugAndIgnoreInReleaseMacro( this->m_FilterToUpdate );
    this->m_FilterToUpdate->Update();
    }
}

} // end namespace itk

#endif
