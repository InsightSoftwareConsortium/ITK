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

/**
 * This is a fully working example of DOM-based writer for the ITK object itk::ParticleSwarmOptimizer.
 * To demonstrate the power and flexibility of the new DOM-based object writing/reading, we formulate our XML
 * format for the PSO object in a special way, e.g. using children and text nodes (instead of attributes)
 * for some data fields. This may not be the favouriate way for some users, but users can always
 * define their own XML format, and customize this implementation.
 *
 * Please see [ITK_HOME]/Testing/Data/InputXML/test.pso.xml for an example of our XML format for the PSO object.
 */

#ifndef itkParticleSwarmOptimizerDOMWriter_h
#define itkParticleSwarmOptimizerDOMWriter_h

#include "itkDOMWriter.h"
#include "itkParticleSwarmOptimizer.h"

namespace itk
{

class ParticleSwarmOptimizerDOMWriter : public DOMWriter<ParticleSwarmOptimizer>
{
public:
  /** Standard class typedefs. */
  typedef ParticleSwarmOptimizerDOMWriter     Self;
  typedef DOMWriter<ParticleSwarmOptimizer>   Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ParticleSwarmOptimizerDOMWriter, DOMWriter);

protected:
  ParticleSwarmOptimizerDOMWriter() {}

  /**
   * This function is called automatically when update functions are performed.
   * It should fill the contents of the intermediate DOM object by pulling information from the input object.
   */
  virtual void GenerateData( DOMNodeType* outputdom, const void* ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ParticleSwarmOptimizerDOMWriter);
};

} // namespace itk

#endif // itkParticleSwarmOptimizerDOMWriter_h
