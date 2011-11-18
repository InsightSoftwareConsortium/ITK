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
#include "itkLightProcessObject.h"

namespace itk
{
/**
 * Instantiate object with no start, end, or progress methods.
 */
LightProcessObject
::LightProcessObject()
{
  m_AbortGenerateData = false;
  m_Progress = 0.0f;
}

/**
 * Destructor for the LightProcessObject class. We've got to
 * UnRegister() the use of any input classes.
 */
LightProcessObject
::~LightProcessObject()
{}

/**
 * Update the progress of the process object. If a ProgressMethod exists,
 * executes it. Then set the Progress ivar to amount. The parameter amount
 * should range between (0,1).
 */
void
LightProcessObject
::UpdateProgress(float amount)
{
  m_Progress = amount;
  this->InvokeEvent( ProgressEvent() );
}

/**
 *
 */
void
LightProcessObject
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "AbortGenerateData: " << ( m_AbortGenerateData ? "On\n" : "Off\n" );
  os << indent << "Progress: " << m_Progress << "\n";
}

/**
   *
   */
void
LightProcessObject
::UpdateOutputData()
{
  this->InvokeEvent( StartEvent() );

  /**
   * GenerateData this object - we have not aborted yet, and our progress
   * before we start to execute is 0.0.
   */
  m_AbortGenerateData = 0;
  m_Progress = 0.0f;

  this->GenerateData();

  /**
   * If we ended due to aborting, push the progress up to 1.0 (since
   * it probably didn't end there)
   */
  if ( !m_AbortGenerateData )
    {
    this->UpdateProgress(1.0f);
    }

  // Notify end event observers
  this->InvokeEvent( EndEvent() );
}
} // end namespace itk
