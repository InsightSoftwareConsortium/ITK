/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkXMLFileOutputWindow.h
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
#ifndef __itkXMLFileOutputWindow_h
#define __itkXMLFileOutputWindow_h

#include "itkFileOutputWindow.h"

namespace itk
{
/** \class XMLFileOutputWindow
 * \brief Messages sent from the system are sent to a file with each message enclosed by XML tags.
 *
 * Writes debug/warning/error output to an XML file.  Uses predefined XML
 * tags for each text display method.  The text is processed to replace
 * XML markup characters.
 *
 *   DisplayText - <Text>
 * 
 *   DisplayErrorText - <Error>
 * 
 *   DisplayWarningText - <Warning>
 * 
 *   DisplayGenericOutputText - <GenericOutput>
 * 
 *   DisplayDebugText - <Debug>
 * 
 * The method DisplayTag outputs the text unprocessed.  To use this
 * class, instantiate it and then call SetInstance(this).
 *
 * \ingroup OSSystemObjects
 * 
 */

class ITK_EXPORT XMLFileOutputWindow : public FileOutputWindow
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef XMLFileOutputWindow        Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef FileOutputWindow  Superclass;

  /** 
   * Smart pointer typedef support. 
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(XMLFileOutputWindow, FileOutputWindow);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Send a string to the XML file.
   */
  virtual void DisplayText(const char*);

  /**
   * Send an error string to the XML file.
   */
  virtual void DisplayErrorText(const char*);

  /**
   * Send a warning string to the XML file.
   */
  virtual void DisplayWarningText(const char*);

  /**
   * Send a generic output string to the XML file.
   */
  virtual void DisplayGenericOutputText(const char*);

  /**
   * Send a debug string to the XML file.
   */
  virtual void DisplayDebugText(const char*);

  /**
   *  Put the text into the log file without processing it.
   */
  virtual void DisplayTag(const char*);

protected:
  XMLFileOutputWindow();
  virtual ~XMLFileOutputWindow();
  XMLFileOutputWindow(const Self&) {}
  void operator=(const Self&) {}
  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  void Initialize();
  virtual void DisplayXML(const char*, const char*);

private:

};
  
} // end namespace itk

#endif
