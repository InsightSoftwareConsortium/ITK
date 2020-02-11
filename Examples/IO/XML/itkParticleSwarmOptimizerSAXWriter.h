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

/**
 * This is a fully working example of SAX-based writer for the ITK object
 * itk::ParticleSwarmOptimizer. It performs the same function as
 * ParticleSwarmOptimizerDOMWriter; however, this writer directly generates the output
 * XML document, which is more complicated and error prone.
 *
 * Please see [ITK_HOME]/Testing/Data/InputXML/test.pso.xml for an example of our XML
 * format for the PSO object.
 */

#ifndef itkParticleSwarmOptimizerSAXWriter_h
#define itkParticleSwarmOptimizerSAXWriter_h

#include "itkXMLFile.h"
#include "itkParticleSwarmOptimizer.h"

namespace itk
{

class ParticleSwarmOptimizerSAXWriter : public XMLWriterBase<ParticleSwarmOptimizer>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ParticleSwarmOptimizerSAXWriter);

  /** Standard class type aliases. */
  using Self = ParticleSwarmOptimizerSAXWriter;
  using Superclass = XMLWriterBase<ParticleSwarmOptimizer>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ParticleSwarmOptimizerSAXWriter, XMLWriterBase);

  /**
   * Virtual method defined in itk::XMLWriterBase.
   * Check that whether the file with given name is writable.
   */
  int
  CanWriteFile(const char * name) override;

  /**
   * Method for performing XML file generation from the input object.
   */
  int
  WriteFile() override;

protected:
  ParticleSwarmOptimizerSAXWriter() = default;
};

} // namespace itk

#endif // itkParticleSwarmOptimizerSAXWriter_h
