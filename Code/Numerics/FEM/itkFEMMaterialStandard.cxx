/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMMaterialStandard.cxx
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

#include "itkFEMMaterialStandard.h"
#include "itkFEMUtility.h"

namespace itk {
namespace fem {




/**
 * Read Standard material object form stream
 */
void MaterialStandard::Read(std::istream& f, void* info) {

double d;

std::streampos l;
char buf[256];
std::string s;
std::string::size_type b,e;
  
  /** first call the parent's read function */
  Superclass::Read(f, info);

  /**  clear the data already inside the object */
  E=0.0; A=0.0; I=0.0; ni=0.0;

  /**
   * Next we read any known constant from stream. This allows a user to
   * specify only constants which are actually required by elements in
   * a system. This makes creating input files a bit easier.
   */
  while(f) {
    l=f.tellg();            // remember the stream position
    SkipWhiteSpace(f);      // skip comments and whitespaces

    /**
     * All Constants are in the following format:
     *    constant_name : value
     */
    f.getline(buf,256,':');  /** read up to 256 characters until ':' is reached. we read and discard the ':' */
    if (!f) { goto out; }    /** no : was found */
    s=std::string(buf);

    /**
     * Get rid of the whitespaces in front of and the back of token
     */
    b=s.find_first_not_of(whitespaces);    // end of whitespaces in the beginning 
    if ( (e=s.find_first_of(whitespaces,b))==std::string::npos )  // beginning of whitespaces at the end
      e=s.size();

    /**
     * s now contains just the name of the constant.
     * the value is ready to be read next from the stream
     */
    s=s.substr(b,e-b);
    
    if (s=="E") {
      /** read and set E */
      f>>d; if(!f) goto out;
      E=d;
      continue;
    }

    if (s=="A") {
      /** read and set A */
      f>>d; if(!f) goto out;
      A=d;
      continue;
    }

    if (s=="I") {
      /** read and set I */
      SkipWhiteSpace(f); f>>d; if(!f) goto out;
      I=d;
      continue;
    }

    if (s=="ni") {
      /** read and set ni */
      SkipWhiteSpace(f); f>>d; if(!f) goto out;
      ni=d;
      continue;
    }
    if (s=="END") {
      /** end of constants in material definition */
      goto out;
    }


    /**
     * If we got here an unknown constant was reached.
     * We reset the stream position and set the stream error.
     */
    f.seekg(l);
    f.clear(std::ios::failbit);

  }

out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"MaterialStandard::Read()","Error reading FEM material!");
  }

}




/**
 * Write standard material object to stream
 */
void MaterialStandard::Write( std::ostream& f, int ofid ) const {

  /** if not set already, se set the ofid */
  if (ofid<0) ofid=OFID;

  /** first call the parent's write function */
  Superclass::Write(f,ofid);

  /** then the actual data (node, and properties numbers) */
  f<<"\tE  : "<<E<<"\t% Young modulus\n";
  f<<"\tA  : "<<A<<"\t% Crossection area\n";
  f<<"\tI  : "<<I<<"\t% Moment of inertia\n";
  f<<"\tni : "<<ni<<"\t% Poisson's ratio\n";
  f<<"\tEND:\t% End of material definition\n";

  /** check for errors */
  if (!f)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"MaterialStandard::Write()","Error writing FEM material!");
  }

}

FEM_CLASS_REGISTER(MaterialStandard)



}} // end namespace itk::fem
