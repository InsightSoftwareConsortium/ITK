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
#ifndef itkXMLFileOutputWindow_h
#define itkXMLFileOutputWindow_h

#include "itkFileOutputWindow.h"

namespace itk
{
/** \class XMLFileOutputWindow
 * \brief Messages sent from the system are sent to a file with each message
 *        enclosed by XML tags.
 *
 * Writes debug/warning/error output to an XML file.  Uses predefined XML
 * tags for each text display method.  The text is processed to replace
 * XML markup characters.
 *
 *   DisplayText - \<Text\>
 *
 *   DisplayErrorText - \<Error\>
 *
 *   DisplayWarningText - \<Warning\>
 *
 *   DisplayGenericOutputText - \<GenericOutput\>
 *
 *   DisplayDebugText - \<Debug\>
 *
 * The method DisplayTag outputs the text unprocessed.  To use this
 * class, instantiate it and then call SetInstance(this).
 *
 * \ingroup OSSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT XMLFileOutputWindow : public FileOutputWindow
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(XMLFileOutputWindow);

  /** Standard class type aliases. */
  using Self = XMLFileOutputWindow;
  using Superclass = FileOutputWindow;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(XMLFileOutputWindow, FileOutputWindow);

  /** Send a string to the XML file. */
  void
  DisplayText(const char *) override;

  /** Send an error string to the XML file. */
  void
  DisplayErrorText(const char *) override;

  /** Send a warning string to the XML file. */
  void
  DisplayWarningText(const char *) override;

  /** Send a generic output string to the XML file. */
  void
  DisplayGenericOutputText(const char *) override;

  /** Send a debug string to the XML file. */
  void
  DisplayDebugText(const char *) override;

  /**  Put the text into the log file without processing it. */
  virtual void
  DisplayTag(const char *);

protected:
  XMLFileOutputWindow();
  ~XMLFileOutputWindow() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  Initialize();

  virtual void
  DisplayXML(const char *, const char *);
};
} // end namespace itk

#endif
