/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioGene.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#include "itkBioGene.h"
#include <algorithm>



namespace itk {


namespace bio {


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
{
}



/**
 *    Copy from another genome
 */ 
void
Gene
::Copy( const Gene & gene )
{

  m_Name = gene.m_Name;

  m_ControlDomains.clear();

  m_ControlDomains.insert( m_ControlDomains.begin(),
                           gene.m_ControlDomains.begin(),
                           gene.m_ControlDomains.end()    );


  m_ProteinDomains.clear();

  ProteinDomainsType::value_type first = (*gene.m_ProteinDomains.begin());
  ProteinDomainsType::value_type last = (*gene.m_ProteinDomains.end());
  m_ProteinDomains.insert(
                           &first,
                           &last);
  
}



/**
 *    Set the name of the gene
 */ 
void
Gene
::SetName( const NameType & name )
{
   m_Name = name;
}
 


/**
 *    Set the name of the gene
 */ 
void
Gene
::SetName( const char * name )
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
::AddProteinDomain( const DomainType & domain, AffinityType affinity )
{
   m_ProteinDomains[ domain ] = affinity;
}
 



/**
 *    Add a protein domain
 */ 
void
Gene
::AddGeneControlDomain( const DomainType & domain, bool type )
{
   ControlDomainType controlDomain;
   controlDomain.domain = domain;
   controlDomain.type   = type;
   m_ControlDomains.push_back( controlDomain );
}
 

}  // end namespace bio

}  // end namespace itk


