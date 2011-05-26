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
void LoadLandmark::Read( std::istream& f, void*)
{
  int n1, n2;
  vnl_vector<Float> pu;
  vnl_vector<Float> pd;

  // first call the parent's read function
  //Superclass::Read(f,info);

  // read the dimensions of the undeformed point and set the size of the point accordingly
  this->SkipWhiteSpace(f); f>>n1; if(!f) goto out;
  pu.set_size(n1);
  this->m_pt.set_size(n1);

  // read the undeformed point in global coordinates
  this->SkipWhiteSpace(f); f>>pu; if(!f) goto out;

  // Read the dimensions of the deformed point and set the size of the point accordingly
  this->SkipWhiteSpace(f); f>>n2; if(!f) goto out;
  pd.set_size(n2);
  m_force.set_size(n2);

  // read the deformed point in global coordinates
  this->SkipWhiteSpace(f); f>>pd; if(!f) goto out;

  m_source = pd;
  m_pt = pd;
  m_target = pu;
  m_force  = pu-pd;

  //std::cout << m_source << std::endl << m_pt << std::endl << m_target << std::endl << m_force << std::endl;

  // read the square root of the variance associated with this landmark
  this->SkipWhiteSpace(f); f>>eta; if(!f) goto out;

  // Verify that the undeformed and deformed points are of the same size.
  if (n1 != n2) { goto out; }

  this->el.resize(1);

  out:

  if( !f )
    {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadLandmark::Read()","Error reading landmark load!");
    }
}

/**
 * Find the Element to which the LoadLandmark belongs
 */

void LoadLandmark::AssignToElement(Element::ArrayType::Pointer elements)
{
  bool isFound = false;

  // Compute & store the local coordinates of the undeformed point and
  // the pointer to the element

  for (Element::ArrayType::const_iterator n = elements->begin();
       n != elements->end() && !isFound; n++)
    {
    if ( (*n)->GetLocalFromGlobalCoordinates(m_source, this->m_pt) )
      {
      isFound = true;
      std::cout << "Found: " << (&**n) << std::endl;
      this->el[0] = *n;
      }
    }

  if (!isFound)
    {
    throw FEMException(__FILE__,__LINE__,"LoadLandmark::Read() - could not find element containing landmark!");
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
  f << "\t% Each vector below is preceded by its size" << std::endl;

  /** Write the point coordinates in the undeformed state */
  f<<"\t"<<m_pt.size()<<" "<<m_pt<<"\t%Point (local) coordinates, undeformed state"<<"\n";


  /** check for errors */
  if (!f)
    {
    throw FEMExceptionIO(__FILE__,__LINE__,"LoadBCMFC::Write()","Error writing FEM load!");
    }

}

FEM_CLASS_REGISTER(LoadLandmark)

}} // end namespace itk::fem
