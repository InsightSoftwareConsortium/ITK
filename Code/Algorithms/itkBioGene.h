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
  typedef   std::string                     NameType;
  typedef   std::string                     DomainType;
  typedef   std::map< DomainType, float >   ProteinDomainsType;
  typedef   std::vector< DomainType >       GeneControlDomainsType;

public:
  Gene();
  virtual ~Gene();

  void Copy( const Gene & genome );


private:
  NameType                  m_Name;
  GeneControlDomainsType    m_ControlDomains;
  ProteinDomainsType        m_ProteinDomains; 
};


} // end namespace bio

} // end namespace itk

#endif


