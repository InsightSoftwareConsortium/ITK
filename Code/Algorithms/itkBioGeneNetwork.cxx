/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioGeneNetwork.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#include "itkBioGeneNetwork.h"



namespace itk {


namespace bio {


/**
 *    Constructor 
 */ 
GeneNetwork
::GeneNetwork()
{
}




/**
 *    Destructor   
 */ 
GeneNetwork
::~GeneNetwork()
{
}



/**
 *    Copy from another genome
 */ 
void
GeneNetwork
::Copy( const GeneNetwork & geneNetwork )
{

  m_ProteinConcentration.clear();
  m_ProteinConcentration.insert( 
                      m_ProteinConcentration.begin(),
          geneNetwork.m_ProteinConcentration.begin(),
          geneNetwork.m_ProteinConcentration.end()    );

  m_DomainConcentration.clear();
  m_DomainConcentration.insert( 
                      m_DomainConcentration.begin(),
          geneNetwork.m_DomainConcentration.begin(),
          geneNetwork.m_DomainConcentration.end()    );



}


 
}  // end namespace bio

}  // end namespace itk


