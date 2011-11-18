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
#include "itkBioGene.h"

namespace itk
{
namespace bio
{
/**
 *    Constructor
 */
Gene
::Gene()
{
  m_Name = "Unknown";
}

/**
 *    Destructor
 */
Gene
::~Gene()
{}

/**
 *    Copy from another genome
 */
void
Gene
::Copy(const Gene & gene)
{
  m_Name = gene.m_Name;

  m_ControlDomains.clear();

  m_ControlDomains.insert( m_ControlDomains.begin(),
                           gene.m_ControlDomains.begin(),
                           gene.m_ControlDomains.end() );

  m_ProteinDomains.clear();

  ProteinDomainsType::const_iterator proteinDomain = gene.m_ProteinDomains.begin();
  ProteinDomainsType::const_iterator last          = gene.m_ProteinDomains.end();

  while ( proteinDomain != last )
    {
    m_ProteinDomains[( *proteinDomain ).first] = ( *proteinDomain ).second;
    ++proteinDomain;
    }
}

/**
 *    Set the name of the gene
 */
void
Gene
::SetName(const NameType & name)
{
  m_Name = name;
}

/**
 *    Set the name of the gene
 */
void
Gene
::SetName(const char *name)
{
  m_Name = name;
}

/**
 *    Get the name of the gene
 */
const char *
Gene
::GetName() const
{
  return m_Name.c_str();
}

/**
 *    Add a protein domain
 */
void
Gene
::AddProteinDomain(const DomainType & domain, AffinityType affinity)
{
  m_ProteinDomains[domain] = affinity;
}

/**
 *    Add a protein domain
 */
void
Gene
::AddGeneControlDomain(const DomainType & domain, bool type)
{
  ControlDomainType controlDomain;

  controlDomain.m_Domain = domain;
  controlDomain.m_Type   = type;
  m_ControlDomains.push_back(controlDomain);
}
}  // end namespace bio
}  // end namespace itk
