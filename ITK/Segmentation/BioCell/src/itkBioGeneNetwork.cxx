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
#include "itkBioGeneNetwork.h"

namespace itk
{
namespace bio
{
/**
 *    Constructor
 */
GeneNetwork
::GeneNetwork()
{}

/**
 *    Destructor
 */
GeneNetwork
::~GeneNetwork()
{}

/**
 *    Copy from another genome
 */
void
GeneNetwork
::Copy(const GeneNetwork & geneNetwork)
{
  m_ProteinConcentration.clear();
  m_ProteinConcentration.insert(
    m_ProteinConcentration.begin(),
    geneNetwork.m_ProteinConcentration.begin(),
    geneNetwork.m_ProteinConcentration.end() );

  m_DomainConcentration.clear();
  m_DomainConcentration.insert(
    m_DomainConcentration.begin(),
    geneNetwork.m_DomainConcentration.begin(),
    geneNetwork.m_DomainConcentration.end() );
}
}  // end namespace bio
}  // end namespace itk
