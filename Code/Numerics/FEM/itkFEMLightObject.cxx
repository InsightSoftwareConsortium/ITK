/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLightObject.cxx
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

#include "itkFEMLightObject.h"
#include "itkNumericTraits.h"

namespace itk {
namespace fem {




/*
 * Here we just read the global number from the stream.
 * This should be the first function called when reading object data.
 */
void FEMLightObject::Read( std::istream& f, void* )
{
  int n;

  /** Read and set the global object number */
  SkipWhiteSpace(f); f>>n; if(!f) { goto out; }
  this->GN=n;

out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"FEMLightObject::Read","Error reading FEM object!");
  }

}




/*
 * Here we just write the class name and GN.
 * This should be the first function called when writing object data, so
 * every derived class should first call the parent's write function.
 * The Write function in base (this one) class knows which class is
 * being written by calling the virtual ClassID() function and can write
 * the class name properly.
 */
void FEMLightObject::Write( std::ostream& f ) const
{
  // first write the class name
  f<<'<'<<FEMObjectFactory<Self>::ID2ClassName(this->ClassID())<<">\n";

  // then the global object number
  f<<"\t"<<GN<<"\t% Global object number\n";

  // check for errors
  if (!f)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"FEMLightObject::Write","Error writing FEM object!");
  }
}




/*
 * Read and create object of any derived class from stream
 */
FEMLightObject::Pointer
FEMLightObject::
CreateFromStream( std::istream& f, void *info )
{

// local variables
std::streampos l(0);
char buf[256];
std::string s;
std::string::size_type b,e;
int clID;
FEMLightObject::Pointer a=0;

start:
#ifndef __sgi
  l=f.tellg();    // remember the stream position
#endif
  SkipWhiteSpace(f);      // skip comments and whitespaces
  if ( f.eof() ) return 0; // end of stream. all was good

  if ( f.get()!='<' ) goto out; // we expect a token
  f.getline(buf,256,'>');  // read up to 256 characters until '>' is reached. we read and discard the '>'
  s=std::string(buf);

  // get rid of the whitespaces in front of and the back of token
  b=s.find_first_not_of(whitespaces); // end of whitespaces in the beginning 
  if ( (e=s.find_first_of(whitespaces,b))==std::string::npos )  // beginning of whitespaces at the end
  {
    e=s.size();
  }
  s=s.substr(b,e-b);

  if ( s=="END" )
  {
    /*
     * We can ignore this token. Start again by reading the next object.
     */
    goto start;
  }
  clID=FEMOF::ClassName2ID(s);  // obtain the class ID from FEMObjectFactory
  if (clID<0) goto out;  // class not found

  // create a new object of the correct class
  a=FEMOF::Create(clID);
  if (!a) goto out;    // error creating new object of the derived class

  /*
   * Now we have to read additional data, which is
   * specific to the class of object we just created
   */
  try
  {
    a->Read(f,info);
  }
  /*
   * Catch possible exceptions while 
   * reading object's data from stream
   */
  catch (...)
  {
    #ifndef FEM_USE_SMART_POINTERS
    delete a;  // if something went wrong, we need to destroy the already created object
    #endif
    a=0;
    throw;     // rethrow the same exception
  }

  /*
   * Return a pointer to a newly created object if all was OK
   * Technically everithing should be fine here (a!=0), but we
   * check again, just in case.
   */
  if (a) { return a; }

out:

  /*
   * Something went wrong.
   * Reset the stream position to where it was before reading the object.
   */
#ifndef __sgi
  f.seekg(l);
#endif
  
  /*
   * Throw an IO exception
   */
  throw FEMExceptionIO(__FILE__,__LINE__,"FEMLightObject::ReadAnyObjectFromStream()","Error reading object from stream!");

}




// Helper function to skip all whitespaces and comments in input stream
void
FEMLightObject::
SkipWhiteSpace(std::istream& f)
{
  while(f && !f.eof() && (std::ws(f).peek())=='%' )
  {
    f.ignore(NumericTraits<int>::max(), '\n');
  }
}

// string containing all whitespace characters
const std::string
FEMLightObject
::whitespaces=" \t\n\r";




}} // end namespace itk::fem
