/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMMaterialLinearElasticity.cxx
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

#include "itkFEMMaterialLinearElasticity.h"

namespace itk {
namespace fem {




/*
 * Default constructor
 */
MaterialLinearElasticity::MaterialLinearElasticity() :
  E(100.0), A(1.0), I(1.0), nu(0.2), h(1.0), RhoC(1.0)
{}




/*
 * Read Linear elasticity material object form stream
 */
void MaterialLinearElasticity::Read(std::istream& f, void* info) {

double d;

std::streampos l;
char buf[256];
std::string s;
std::string::size_type b,e;
  
  // first call the parent's read function
  Superclass::Read(f, info);

  // clear the data already inside the object
  E=0.0; A=0.0; I=0.0; nu=0.0; h=1.0; RhoC=1.0;

  /*
   * Next we read any known constant from stream. This allows a user to
   * specify only constants which are actually required by elements in
   * a system. This makes creating input files a bit easier.
   */
  while(f) {
#ifndef __sgi
    l=f.tellg();            // remember the stream position
#endif
    SkipWhiteSpace(f);      // skip comments and whitespaces

    /*
     * All Constants are in the following format:
     *    constant_name : value
     */
    f.getline(buf,256,':');  // read up to 256 characters until ':' is reached. we read and discard the ':'
    if (!f) { goto out; }    // no : was found
    s=std::string(buf);

    // Get rid of the whitespaces in front of and the back of token
    b=s.find_first_not_of(whitespaces);    // end of whitespaces in the beginning 
    if ( (e=s.find_first_of(whitespaces,b))==std::string::npos )  // beginning of whitespaces at the end
      e=s.size();

    /*
     * s now contains just the name of the constant.
     * The value is ready to be read next from the stream
     */
    s=s.substr(b,e-b);
    
    if (s=="E") {
      f>>d; if(!f) goto out;
      E=d;
      continue;
    }

    if (s=="A") {
      f>>d; if(!f) goto out;
      A=d;
      continue;
    }

    if (s=="I") {
      SkipWhiteSpace(f); f>>d; if(!f) goto out;
      I=d;
      continue;
    }

    if (s=="nu") {
      SkipWhiteSpace(f); f>>d; if(!f) goto out;
      nu=d;
      continue;
    }

    if (s=="h") {
      SkipWhiteSpace(f); f>>d; if(!f) goto out;
      h=d;
      continue;
    }

    if (s=="RhoC") {
      SkipWhiteSpace(f); f>>d; if(!f) goto out;
      RhoC=d;
      continue;
    }

    if (s=="END") {
      // End of constants in material definition
      goto out;
    }


    /*
     * If we got here an unknown constant was reached.
     * We reset the stream position and set the stream error.
     */
#ifndef __sgi
    f.seekg(l);
    f.clear(std::ios::failbit);
#endif
  }

out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"MaterialLinearElasticity::Read()","Error reading FEM material!");
  }

}




/*
 * Write linear elasticity material object to stream
 */
void MaterialLinearElasticity::Write( std::ostream& f ) const
{
  // First call the parent's write function
  Superclass::Write(f);

  /// Then write the actual data
  f<<"\tE  : "<<E<<"\t% Young modulus\n";
  f<<"\tA  : "<<A<<"\t% Beam crossection area\n";
  f<<"\tI  : "<<I<<"\t% Moment of inertia\n";
  f<<"\tnu : "<<nu<<"\t% Poisson's ratio\n";
  f<<"\th : "<<h<<"\t% Plate thickness\n";
  f<<"\tRhoC : "<<RhoC<<"\t% Density times capacity\n";
  f<<"\tEND:\t% End of material definition\n";

  // check for errors
  if (!f)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"MaterialLinearElasticity::Write()","Error writing FEM material!");
  }

}

FEM_CLASS_REGISTER(MaterialLinearElasticity)




}} // end namespace itk::fem
