/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioGeneNetwork.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBioGeneNetwork_h_
#define __itkBioGeneNetwork_h_

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include <vector>

namespace itk {

namespace bio {

/** \class GeneNetwork
 * \brief This class implement the abstraction of a biological gene network.
 * 
 * This class is implemented to iteratively compute the state of the gene network
 * following update rules (typicall Partial Differential Equations).
 * 
 */
class GeneNetwork  
{
public:
  typedef   std::vector< float >       ProteomeType;
  typedef   std::vector< float >       DomainsType;

public:
  GeneNetwork();
  virtual ~GeneNetwork();

  void Copy( const GeneNetwork & genome );


private:

  // This array contains the concentrations each protein
  // in the cell at a certain time.
  ProteomeType              m_ProteinConcentration;

  // This array contains the concentrations domain affinities 
  // in the cell at a certain time. Since each protein may 
  // have multiple domains and each domain have affinities
  // for standard domains in the regulatory section of genes,
  // this array is indexed by the domains in the regulatory
  // section. The final value on each entry indicates the 
  // probability of a domain to be bound during a certain time
  // interval.
  DomainsType               m_DomainConcentration;

};


} // end namespace bio

} // end namespace itk

#endif


