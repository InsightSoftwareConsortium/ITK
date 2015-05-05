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
#ifndef itkBioGeneNetwork_h
#define itkBioGeneNetwork_h


#include "itkMacro.h"
#include "ITKBioCellExport.h"

#include <vector>

namespace itk
{
namespace bio
{
/** \class GeneNetwork
 * \brief This class implements the abstraction of a biological gene network.
 *
 * This class is implemented to iteratively compute the state of the gene network
 * following update rules (typical Partial Differential Equations).
 *
 * \ingroup ITKBioCell
 */
class ITKBioCell_EXPORT GeneNetwork
{
public:
  typedef   std::vector< float > ProteomeType;
  typedef   std::vector< float > DomainsType;

public:
  GeneNetwork();
  virtual ~GeneNetwork();

  void Copy(const GeneNetwork & genome);

private:

  // This array contains the concentrations of each protein
  // in the cell at a certain time.
  ProteomeType m_ProteinConcentration;

  // This array contains the concentrations of domain affinities
  // in the cell at a certain time. Since each protein may
  // have multiple domains and each domain has affinities
  // for standard domains in the regulatory section of genes,
  // this array is indexed by the domains in the regulatory
  // section. The final value on each entry indicates the
  // probability of a domain to be bound during a certain time
  // interval.
  DomainsType m_DomainConcentration;
};
} // end namespace bio
} // end namespace itk

#endif
