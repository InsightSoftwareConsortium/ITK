/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformFileReader.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTransformFileReader_cxx
#define __itkTransformFileReader_cxx

#include "itkTransformFileReader.h"
#include "itkTransformBase.h"
#include "itkTransformFactoryBase.h"

/**
 * itk::IStringStream wrapper to hide differences between
 * std::istringstream and the old istrstream.  Necessary for
 * portability.
 */
#if !defined(ITK_NO_ANSI_STRING_STREAM)
class IStringStream: public std::istringstream
{
public:
  IStringStream() {}
  IStringStream(std::string &s) : std::istringstream(s) {}
private:
  IStringStream(const IStringStream&);
  void operator=(const IStringStream&);
};
#else
class IStringStream: public std::istrstream
{
public:
  IStringStream(std::string &s) : istrstream(s.c_str()) {}
private:
  IStringStream(const IStringStream&);
  void operator=(const IStringStream&);
};
#endif

namespace itk
{

std::string trim(std::string const& source, char const* delims = " \t\r\n") {
  std::string result(source);
  std::string::size_type index = result.find_last_not_of(delims);
  if(index != std::string::npos)
    result.erase(++index);

  index = result.find_first_not_of(delims);
  if(index != std::string::npos)
    result.erase(0, index);
  else
    result.erase();
  return result;
}

/** Constructor */
TransformFileReader
::TransformFileReader()
{
  m_FileName = "";
  TransformFactoryBase::RegisterDefaultTransforms();
}

/** Destructor */
TransformFileReader
::~TransformFileReader()
{
}

/** Update the Reader */
void TransformFileReader
::Update()
{  
  TransformPointer transform;

  std::ifstream in ( m_FileName.c_str() );

  OStringStream InData;

  // in.get ( InData );
  char* buffer = new char[1024];
  while ( !in.eof() )
    {
    // Read a buffer full
    in.getline ( buffer, 1024 );
    InData << buffer << std::endl;
    }
  delete[] buffer;
  std::string data = InData.str();
  in.close();

  // Read line by line
  vnl_vector<double> VectorBuffer;
  unsigned int position = 0;
  while ( position < data.size() )
    {
    // Find the next string
    unsigned int end = data.find ( "\n", position );
    std::string line = trim ( data.substr ( position, end - position ) );
    position = end+1;
    itkDebugMacro ("Found line: \"" << line << "\"" );
    if ( line.length() == 0 )
      {
      continue;
      }
    if ( line[0] == '#' || std::string::npos == line.find_first_not_of ( " \t" ) )
      {
      // Skip lines beginning with #, or blank lines
      continue;
      }

    // Get the name
    end = line.find ( ":" );
    if ( end == std::string::npos )
      {
      // Throw an error
      itkExceptionMacro ( "Tags must be delimited by :" );
      }
    std::string Name = trim ( line.substr ( 0, end ) );
    std::string Value = trim ( line.substr ( end + 1, line.length() ) );
    // Push back 
    itkDebugMacro ( "Name: \"" << Name << "\"" );
    itkDebugMacro ( "Value: \"" << Value << "\"" );
    IStringStream parse ( Value );
    VectorBuffer.clear();
    if ( Name == "Transform" )
      {
      // Instantiate the transform
      LightObject::Pointer i = ObjectFactoryBase::CreateInstance ( Value.c_str() );
      TransformType* ptr = dynamic_cast<TransformBase*> ( i.GetPointer() );
      if ( ptr == NULL )
        {
        itkExceptionMacro ( "Could not create an instance of " << Value );
        }
      transform = ptr;
      transform->Register();
      // transform->Print ( std::cout );
      // ptr->UnRegister();
      m_TransformList.push_back ( transform );
      }
    else if ( Name == "Parameters" || Name == "FixedParameters" )
      {
      Array<double> TmpArray;
      TmpArray.clear();
      VectorBuffer.clear();

      // Read them
      parse >> VectorBuffer;
      itkDebugMacro ( "Parsed: " << VectorBuffer );
      if ( Name == "Parameters" )
        {
        TmpArray = VectorBuffer;
        itkDebugMacro ( "Setting Parameters: " << TmpArray );
        if ( !transform )
          {
          itkExceptionMacro ( "Please set the transform before parameters" );
          }
        transform->SetParameters ( TmpArray );
        itkDebugMacro ( "Parameters set Parameters" );
        }
      else if ( Name == "FixedParameters" )
        {
        TmpArray = VectorBuffer;
        itkDebugMacro ( "Setting Fixed Parameters: " << TmpArray );
        if ( !transform )
          {
          itkExceptionMacro ( "Please set the transform before fixed parameters" );
          }
        transform->SetFixedParameters ( TmpArray );
        itkDebugMacro ( "Set Fixed Parameters" );
        }
      }
    }
}


} // namespace itk

#endif
