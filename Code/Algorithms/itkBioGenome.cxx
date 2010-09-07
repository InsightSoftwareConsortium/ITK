/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioGenome.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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
 *    Inset a gene in the genome
 */
void
Genome
::InsertGene(const GeneIdType & geneId)
{
  // operator[] will create the geneId if
  // it doesn't exist yet
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
