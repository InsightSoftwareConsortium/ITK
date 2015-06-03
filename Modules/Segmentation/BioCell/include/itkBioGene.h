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
#ifndef itkBioGene_h
#define itkBioGene_h


#include <string>
#include <vector>
#include <map>
#include "itkMacro.h"
#include "ITKBioCellExport.h"

namespace itk
{
namespace bio
{
/** \class Gene
 * \brief This class implements the abstraction of a biological gene.
 *
 * The gene has a name and a specification of promoter and represor binding
 * sites.  Promoters and represors encode a boolean expression that controls
 * the expression of the gene. These boolean expressions will result in
 * a polynome in a finite different equation determining the rate at which
 * protein is syntethizied by this gene.
 *
 * \ingroup ITKBioCell
 */
class ITKBioCell_EXPORT Gene
{
public:
  typedef   std::string                          NameType;
  typedef   std::string                          DomainType;
  typedef   double                               AffinityType;
  typedef   std::map< DomainType, AffinityType > ProteinDomainsType;

  typedef   struct {
    DomainType m_Domain;
    bool m_Type;
  }                                             ControlDomainType;

  typedef   std::vector< ControlDomainType > GeneControlDomainsType;

public:
  Gene();
  virtual ~Gene();

  void Copy(const Gene & genome);

  /** Set/Get the name of the gene. This uses a std::string type. */
  void SetName(const NameType & name);

  void SetName(const char *name);

  const char * GetName() const;

  /** Add a protein domain along with an affinity. The name of the domain
      here actually refers to the one for which this protein will have
      binding affinity.  For example, adding "SH2" with affinity 0.05 will
      mean that this gene will synthetize a proteing with a domain that has
      0.05 affinity for a standard SH2 domain. */
  void AddProteinDomain(const DomainType & domain, AffinityType affinity);

  /** Add a control domain to the gene. A control domain is an enhancer or silencer.
      A gene can have any number of them. The type will indicate if they are enhancers
      or silencers. */
  void AddGeneControlDomain(const DomainType & domain, bool type);

private:
  NameType               m_Name;
  GeneControlDomainsType m_ControlDomains;
  ProteinDomainsType     m_ProteinDomains;
};
} // end namespace bio
} // end namespace itk

#endif
