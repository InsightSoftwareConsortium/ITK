/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLightProcessObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "itkLightProcessObject.h"
#include "itkCommand.h"

namespace itk
{

  /**
   * Instantiate object with no start, end, or progress methods.
   */
  LightProcessObject
  ::LightProcessObject()
  {
    m_AbortGenerateData = false;
    m_Progress = 0.0;
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
    this->InvokeEvent(Command::ProgressEvent);
  }


  /**
   *
   */
  void 
  LightProcessObject
  ::PrintSelf(std::ostream& os, Indent indent) const
  {
    Superclass::PrintSelf(os,indent);

    os << indent << "AbortGenerateData: " << (m_AbortGenerateData ? "On\n" : "Off\n");
    os << indent << "Progress: " << m_Progress << "\n";
  }


  /**
   *
   */
  void 
  LightProcessObject
  ::UpdateOutputData()
  {
    
    this->InvokeEvent(Command::StartEvent);

    /**
     * GenerateData this object - we have not aborted yet, and our progress
     * before we start to execute is 0.0.
     */
    m_AbortGenerateData = 0;
    m_Progress = 0.0;

    this->GenerateData();

    /**
     * If we ended due to aborting, push the progress up to 1.0 (since
     * it probably didn't end there)
     */
    if ( !m_AbortGenerateData )
      {
        this->UpdateProgress(1.0);
      }

    // Notify end event observers
    this->InvokeEvent(Command::EndEvent);
  }

} // end namespace itk
