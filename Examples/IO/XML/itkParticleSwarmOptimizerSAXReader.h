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
 * This is a fully working example of SAX-based reader for the ITK object itk::ParticleSwarmOptimizer.
 * It performs the same function as ParticleSwarmOptimizerDOMReader; however, the traditional SAX
 * (Simple API for XML) is used during the reading process, which is more complicated and error prone.
 *
 * Please see [ITK_HOME]/Testing/Data/InputXML/test.pso.xml for an example of our XML format for the PSO object.
 */

#ifndef itkParticleSwarmOptimizerSAXReader_h
#define itkParticleSwarmOptimizerSAXReader_h

#include "itkXMLFile.h"
#include "itkParticleSwarmOptimizer.h"

#include "itkArray.h"
#include <vector>

namespace itk
{

class ParticleSwarmOptimizerSAXReader : public XMLReader<ParticleSwarmOptimizer>
{
public:
  /** Standard class typedefs */
  typedef ParticleSwarmOptimizerSAXReader     Self;
  typedef XMLReader<ParticleSwarmOptimizer>   Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ParticleSwarmOptimizerSAXReader, XMLReader);

  /**
   * Virtual method defined in itk::XMLReaderBase.
   * Check that whether the file with given name is readable.
   */
  virtual int CanReadFile( const char* name ) ITK_OVERRIDE;

  /**
   * Virtual method defined in itk::XMLReaderBase.
   * Called when a new xml tag start is encountered.
   */
  virtual void StartElement( const char* name, const char** atts ) ITK_OVERRIDE;

  /**
   * Virtual method defined in itk::XMLReaderBase.
   * Called when an xml tag end is encountered.
   */
  virtual void EndElement( const char* name ) ITK_OVERRIDE;

  /**
   * Virtual method defined in itk::XMLReaderBase.
   * Called when handling character data inside an xml tag.
   */
  virtual void CharacterDataHandler( const char* inData, int inLength ) ITK_OVERRIDE;

  /**
   * Method for performing XML reading and output generation.
   */
  virtual int ReadFile();

protected:
  ParticleSwarmOptimizerSAXReader() {}

  /** Process tag 'optimizer' attributes. */
  void ProcessOptimizerAttributes( const char** atts, ParticleSwarmOptimizer* opt );

  /** Process tag 'bound' attributes. */
  void ProcessBoundAttributes( const char** atts, std::vector<double>& bound );

  /** Search for and return a particular attribute from the attribute list. */
  const char* GetAttribute( const char** atts, const char* key );

  /** Check the current tags to see whether it matches a user input. */
  bool ContextIs( const char* test ) const;

  /** During the parsing process, current tags are stored in a LIFO stack. */
  std::vector< const char* > m_CurrentTags;

  // other temporary variables used during XML parsing
  std::vector<double> m_LowerBound;
  std::vector<double> m_UpperBound;
  Array<double>       m_ParametersConvergenceTolerance;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ParticleSwarmOptimizerSAXReader);
};

} // namespace itk

#endif // itkParticleSwarmOptimizerSAXReader_h
