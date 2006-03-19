/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioGenome.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBioGenome_h
#define __itkBioGenome_h

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#if defined(_WIN32)
#include "itkWindows.h"
#endif

#include <map>
#include <string>
#include "vcl_cmath.h"

namespace itk {

namespace bio {

/** \class Genome
 * \brief This class implement the abstraction of a biological genome.
 * 
 * The genome is considered to be a sequence of genes each one having
 * a name and a level of expression. This class is intended to be used
 * by artificial cells implementing cellular algorithms for image processing.
 */
class Genome  
{
public:
  typedef   std::string                       GeneIdType;
  typedef   std::map< GeneIdType, double >    MapType;

public:
  Genome();
  virtual ~Genome();

  void Copy( const Genome & genome );

  void InsertGene( const GeneIdType & geneId );
  void KnockOutGene( const GeneIdType & geneId );

  double GetExpressionLevel( const GeneIdType & geneId );
  void   SetExpressionLevel( const GeneIdType & geneId, double level );

  /** This method computes a normalized Sigmoide function that can
   *  be used for gene network computations.  */
  static double Sigmoide( double threshold, double slant, double value )
    {
    return vcl_atan(( value - threshold ) / slant   ) / 3.1416 + 0.5001;
    }

private:
  MapType         m_Map;
   
};


} // end namespace bio

} // end namespace itk

#endif


