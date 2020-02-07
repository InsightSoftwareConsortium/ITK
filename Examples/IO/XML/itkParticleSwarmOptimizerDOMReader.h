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
 * This is a fully working example of DOM-based reader for the ITK object
 * itk::ParticleSwarmOptimizer. To demonstrate the power and flexibility of the new
 * DOM-based object reading, we formulate our XML format for the PSO object in a special
 * way, e.g. using children and text nodes (instead of attributes) for some data fields.
 * This may not be the favouriate way for some users, but users can always define their
 * own XML format, and customize this implementation.
 *
 * Please see [ITK_HOME]/Testing/Data/InputXML/test.pso.xml for an example of our XML
 * format for the PSO object.
 */

#ifndef itkParticleSwarmOptimizerDOMReader_h
#define itkParticleSwarmOptimizerDOMReader_h

#include "itkDOMReader.h"
#include "itkParticleSwarmOptimizer.h"

namespace itk
{

class ParticleSwarmOptimizerDOMReader : public DOMReader<ParticleSwarmOptimizer>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ParticleSwarmOptimizerDOMReader);

  /** Standard class type aliases. */
  using Self = ParticleSwarmOptimizerDOMReader;
  using Superclass = DOMReader<ParticleSwarmOptimizer>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ParticleSwarmOptimizerDOMReader, DOMReader);

protected:
  ParticleSwarmOptimizerDOMReader() = default;

  /**
   * This function is called automatically when update functions are performed.
   * It should fill the contents of the output object by pulling information from the
   * intermediate DOM object.
   */
  void
  GenerateData(const DOMNodeType * inputdom, const void *) override;
};

} // namespace itk

#endif // itkParticleSwarmOptimizerDOMReader_h
