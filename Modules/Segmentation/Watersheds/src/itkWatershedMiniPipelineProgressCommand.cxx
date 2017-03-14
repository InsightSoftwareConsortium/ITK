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
#include "itkWatershedMiniPipelineProgressCommand.h"

namespace itk
{
void WatershedMiniPipelineProgressCommand
::Execute(Object *caller, const EventObject & event)
{
  ProcessObject *po = dynamic_cast< ProcessObject * >( caller );

  if ( !po ) { return; }

  if ( typeid( event ) == typeid( ProgressEvent ) )
    {
    m_Filter->UpdateProgress(
      static_cast< float >( ( m_Count + po->GetProgress() ) /
      static_cast< float >( m_NumberOfFilters ) ) );
    if ( po->GetProgress() == 1.0 )
      {
      m_Count += 1.0;
      }
    }
}

void WatershedMiniPipelineProgressCommand
::Execute(const Object *caller, const EventObject & event)
{
  ProcessObject *po = dynamic_cast< ProcessObject * >( const_cast< Object * >( caller ) );

  if ( !po ) { return; }

  if ( typeid( event ) == typeid( ProgressEvent ) )
    {
    m_Filter->UpdateProgress(
      static_cast< float >( ( m_Count + po->GetProgress() ) /
      static_cast< float >( m_NumberOfFilters ) ) );
    if ( po->GetProgress() == 1.0 )
      {
      m_Count += 1.0;
      }
    }
}

void WatershedMiniPipelineProgressCommand
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "NumberOfFilters: " << m_NumberOfFilters << std::endl;
  os << indent << "Count: " << m_Count << std::endl;
}
} // end namespace itk
