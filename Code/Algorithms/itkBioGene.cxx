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

  m_ControlDomains.insert( m_ControlDomains.begin(),
                           gene.m_ControlDomains.begin(),
                           gene.m_ControlDomains.end()    );

  m_ProteinDomains.insert( gene.m_ProteinDomains.begin(),
                           gene.m_ProteinDomains.end()    );
  
}


 
}  // end namespace bio

}  // end namespace itk


