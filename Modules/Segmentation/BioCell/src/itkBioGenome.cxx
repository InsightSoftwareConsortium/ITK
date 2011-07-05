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
#include "itkBioGenome.h"

namespace itk
{
namespace bio
{
/**
 *    Constructor
 */
Genome
::Genome()
{}

/**
 *    Destructor
 */
Genome
::~Genome()
{}

/**
 *    Copy from another genome
 */
void
Genome
::Copy(const Genome & genome)
{
  m_Map.clear();
  typedef  Genome::MapType::const_iterator IteratorType;

  IteratorType begin = genome.m_Map.begin();
  IteratorType end   = genome.m_Map.end();

  IteratorType gene = begin;
  while ( gene != end )
    {
    m_Map[gene->first] = gene->second;
    ++gene;
    }
}

/**
 *    Insert a gene in the genome
 */
void
Genome
::InsertGene(const GeneIdType & geneId)
{
  // operator[] will create the geneId if
  // it doesn't exist yet.
  // By default the gene is inhibited
  m_Map[geneId] = 0.0;
}

/**
 *    Knockout a gene in the genome
 */
void
Genome
::KnockOutGene(const GeneIdType & geneId)
{
  m_Map.erase(geneId);
}

/**
 *    Return the level of expression of a particular gene
 */
double
Genome
::GetExpressionLevel(const GeneIdType & geneId)
{
  return m_Map[geneId];
}

/**
 *    Set the level of expression of a particular gene
 */
void
Genome
::SetExpressionLevel(const GeneIdType & geneId, double level)
{
  m_Map[geneId] = level;
}
}  // end namespace bio
}  // end namespace itk
