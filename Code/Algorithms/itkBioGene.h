/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioGene.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBioGene_h_
#define __itkBioGene_h_

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include <string>
#include <vector>
#include <map>

namespace itk {

namespace bio {

/** \class Gene
 * \brief This class implement the abstraction of a biological gene.
 * 
 * The gene has a name and a specification of promoters and represor binding
 * sites.  Promoter and represors encode a boolean expression that allows to
 * control the expression of the gene. These boolean expression will result in
 * a polynome in a finite different equation determining the rate at which
 * protein is syntethizied by this gene.
 */
class Gene  
{
public:
  typedef   std::string                           NameType;
  typedef   std::string                           DomainType;
  typedef   double                                AffinityType;
  typedef   std::map< DomainType, AffinityType >  ProteinDomainsType;

  typedef   struct { DomainType domain; bool type; } ControlDomainType;
  typedef   std::vector< ControlDomainType >         GeneControlDomainsType;

public:
  Gene();
  virtual ~Gene();

  void Copy( const Gene & genome );

  /** Set/Get the name of the gene. This uses a std::string type. */
  void SetName( const NameType & name );
  void SetName( const char * name );
  const char * GetName() const;

  /** Add a protein domain along with an affinity. The name of the domain 
      here actually refers to the one for which this protein will have 
      binding affinity.  For example, adding "SH2" with affinity 0.05 will
      means that this gene will synthetize a proteing with a domain that has
      0.05 affinity for a standard SH2 domain. */
  void AddProteinDomain( const DomainType & domain, AffinityType affinity );


  /** Add a control domain to the gene. A control domain is an enhancer or silencer.
      A gene can have any number of them. The type will indicate if they are enhancers
      or silencers. */
  void AddGeneControlDomain( const DomainType & domain, bool type );

private:
  NameType                  m_Name;
  GeneControlDomainsType    m_ControlDomains;
  ProteinDomainsType        m_ProteinDomains; 
};


} // end namespace bio

} // end namespace itk

#endif


