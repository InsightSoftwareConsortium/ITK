/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadLandmark.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMLoadLandmark.h"

namespace itk {
namespace fem {

/**
 * Read a LoadLandmark object from the input stream
 */
void LoadLandmark::Read( std::istream& f, void* info )
{
  int n1, n2;
  vnl_vector<Float> pu;
  vnl_vector<Float> pd;
  bool isFound = false;

  // Convert the info pointer to a usable objects
  ReadInfoType::ElementArrayPointer elements=static_cast<ReadInfoType*>(info)->m_el;

  // first call the parent's read function 
  Superclass::Read(f,info);

  // Read the landmark ID - obsolete
  // SkipWhiteSpace(f); f>>id; if(!f) goto out;

  // read the dimensions of the undeformed point and set the size of the point accordingly
  SkipWhiteSpace(f); f>>n1; if(!f) goto out;
  pu.resize(n1);  
  this->m_pt.resize(n1);

  // read the undeformed point in global coordinates
  SkipWhiteSpace(f); f>>pu; if(!f) goto out;

  // Read the dimensions of the deformed point and set the size of the point accordingly
  SkipWhiteSpace(f); f>>n2; if(!f) goto out;
  pd.resize(n2);

  // read the deformed point in global coordinates
  SkipWhiteSpace(f); f>>pd; if(!f) goto out;
  m_target = pu;

  // read the square root of the variance associated with this landmark
  SkipWhiteSpace(f); f>>eta; if(!f) goto out;

  // Verify that the undeformed and deformed points are of the same size.
  if (n1 != n2) { goto out; } else { this->F.resize(n2); }

  // Calculate and save the initial force imposed by this landmark
  this->F = ( (pu - pd) / (this->eta * this->eta) );

  // Compute & store the local coordinates of the undeformed point and
  // the pointer to the element
  for (Element::ArrayType::const_iterator n = elements->begin(); n!=elements->end() && !isFound; n++) {
    if ( (*n)->GetLocalFromGlobalCoordinates(pd, this->m_pt) ) { 
      isFound = true; 
      this->el.push_back( ( &**n ) );
    }
  }

  // If the corresponding local coordinates aren't found, complain
  if (!isFound) {
    throw FEMException(__FILE__,__LINE__,"LoadLandmark() - error in global to local conversion!");
  }

out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadLandmark::Read()","Error reading landmark load!");
  }
}

/**
 * Write the LoadLandmark object to the output stream
 */
void LoadLandmark::Write( std::ostream& f ) const 
{

  /** first call the parent's write function */
  Superclass::Write(f);

  /**
   * Write the actual LoadLandmark data
   */

  /** Information */
  f<<"\t"<<"Each vector below is preceded by its size"<<"\n";

  /** Write the point coordinates in the undeformed state */
  f<<"\t"<<m_pt.size()<<" "<<m_pt<<"\t%Point (local) coordinates, undeformed state"<<"\n";

  /** Write the force vector */
  f<<"\t"<<F.size()<<" "<<F<<"\t%Force vector"<<"\n";

  /** check for errors */
  if (!f)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadBCMFC::Write()","Error writing FEM load!");
  }

}

FEM_CLASS_REGISTER(LoadLandmark)



}} // end namespace itk::fem
