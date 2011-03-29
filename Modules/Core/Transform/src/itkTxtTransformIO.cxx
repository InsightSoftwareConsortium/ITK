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
#include "itkTxtTransformIO.h"
#include "itksys/SystemTools.hxx"
#include "vnl/vnl_matlab_read.h"
#include "vnl/vnl_matlab_write.h"
#include "itkTransformFileReader.h"

namespace itk
{
TxtTransformIO::TxtTransformIO()
{}

TxtTransformIO::
~TxtTransformIO()
{}

bool
TxtTransformIO::CanReadFile(const char *fileName)
{
  bool recognizedExtension = false;

  recognizedExtension |= ( itksys::SystemTools::GetFilenameLastExtension(fileName) == ".txt" );
  recognizedExtension |= ( itksys::SystemTools::GetFilenameLastExtension(fileName) == ".tfm" );
  return recognizedExtension;
}

bool
TxtTransformIO::CanWriteFile(const char *fileName)
{
  bool recognizedExtension = false;

  recognizedExtension |= ( itksys::SystemTools::GetFilenameLastExtension(fileName) == ".txt" );
  recognizedExtension |= ( itksys::SystemTools::GetFilenameLastExtension(fileName) == ".tfm" );
  return recognizedExtension;
}

std::string
TxtTransformIO::trim(std::string const & source, char const *delims)
{
  std::string            result(source);
  std::string::size_type index = result.find_last_not_of(delims);

  if ( index != std::string::npos )
    {
    result.erase(++index);
    }

  index = result.find_first_not_of(delims);
  if ( index != std::string::npos )
    {
    result.erase(0, index);
    }
  else
    {
    result.erase();
    }
  return result;
}

void
TxtTransformIO::ReadComponentFile( std::string Value )
{
  /* Used for reading component files listed in a composite transform
   * file, which should be read by CompositeTransformReader for assembly
   * into a CompositeTransform.
   * The component filenames are listed w/out paths, and are expected
   * to be in the same path as the master file. */
  std::string filePathUnix =
    itksys::SystemTools::GetFilenamePath( this->GetFileName() ) + "/";
  std::string filePath =
    itksys::SystemTools::ConvertToOutputPath( filePathUnix.c_str() );

  /* Use TransformFileReader to read each component file. */
  TransformFileReader::Pointer reader = TransformFileReader::New();
  std::string componentFullPath = filePath + Value;
  reader->SetFileName( componentFullPath );
  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    itkExceptionMacro("Error reading component file: " << Value
                      << std::endl << ex );
    }
  TransformPointer transform = reader->GetTransformList()->front().GetPointer();
  this->GetReadTransformList().push_back (transform);
}

void
TxtTransformIO::Read()
{
  TransformPointer transform;
  std::ifstream    in;

  in.open (this->GetFileName(), std::ios::in | std::ios::binary);
  if ( in.fail() )
    {
    in.close();
    itkExceptionMacro ("The file could not be opened for read access "
                       << std::endl << "Filename: \"" << this->GetFileName() << "\"");
    }

  std::ostringstream InData;

  // in.get ( InData );
  std::filebuf *pbuf;
  pbuf = in.rdbuf();

  // get file size using buffer's members
  int size = pbuf->pubseekoff (0, std::ios::end, std::ios::in);
  pbuf->pubseekpos (0, std::ios::in);

  // allocate memory to contain file data
  char *buffer = new char[size + 1];

  // get file data
  pbuf->sgetn (buffer, size);
  buffer[size] = '\0';
  itkDebugMacro ("Read file transform Data");
  InData << buffer;

  delete[] buffer;
  std::string data = InData.str();
  in.close();

  // Read line by line
  TransformType::ParametersType   VectorBuffer;
  std::string::size_type position = 0;

  TransformType::ParametersType TmpParameterArray;
  TransformType::ParametersType TmpFixedParameterArray;
  TmpParameterArray.clear();
  TmpFixedParameterArray.clear();
  bool haveFixedParameters = false;
  bool haveParameters = false;

  //
  // check for line end convention
  std::string line_end("\n");

  if ( data.find('\n') == std::string::npos )
    {
    if ( data.find('\r') == std::string::npos )
      {
      itkExceptionMacro ("No line ending character found, not a valid ITK Transform TXT file");
      }
    line_end = "\r";
    }
  while ( position != std::string::npos && position < data.size() )
    {
    // Find the next string
    std::string::size_type end = data.find (line_end, position);

    if( end == std::string::npos )
      {
      itkExceptionMacro("Couldn't find end of line in " << data );
      }

    std::string            line = trim ( data.substr (position, end - position) );
    position = end + 1;
    itkDebugMacro ("Found line: \"" << line << "\"");

    if ( line.length() == 0 )
      {
      continue;
      }
    if ( line[0] == '#' || std::string::npos == line.find_first_not_of (" \t") )
      {
      // Skip lines beginning with #, or blank lines
      continue;
      }

    // Get the name
    end = line.find (":");
    if ( end == std::string::npos )
      {
      // Throw an error
      itkExceptionMacro ("Tags must be delimited by :");
      }
    std::string Name = trim ( line.substr (0, end) );
    std::string Value = trim ( line.substr ( end + 1, line.length() ) );
    // Push back
    itkDebugMacro ("Name: \"" << Name << "\"");
    itkDebugMacro ("Value: \"" << Value << "\"");
    itksys_ios::istringstream parse (Value);
    VectorBuffer.clear();
    if ( Name == "Transform" )
      {
      this->CreateTransform(transform, Value);
      this->GetReadTransformList().push_back (transform);
      }
    else if ( Name == "ComponentTransformFile" )
      {
      /* Used by CompositeTransform file */
      ReadComponentFile( Value );
      }
    else if ( Name == "Parameters" || Name == "FixedParameters" )
      {
      VectorBuffer.clear();

      // Read them
      parse >> VectorBuffer;
      itkDebugMacro ("Parsed: " << VectorBuffer);
      if ( Name == "Parameters" )
        {
        TmpParameterArray = VectorBuffer;
        itkDebugMacro ("Setting Parameters: " << TmpParameterArray);
        if ( haveFixedParameters )
          {
          transform->SetFixedParameters (TmpFixedParameterArray);
          itkDebugMacro ("Set Transform Fixed Parameters");
          transform->SetParametersByValue (TmpParameterArray);
          itkDebugMacro ("Set Transform Parameters");
          TmpParameterArray.clear();
          TmpFixedParameterArray.clear();
          haveFixedParameters = false;
          haveParameters = false;
          }
        else
          {
          haveParameters = true;
          }
        }
      else if ( Name == "FixedParameters" )
        {
        TmpFixedParameterArray = VectorBuffer;
        itkDebugMacro ("Setting Fixed Parameters: " << TmpFixedParameterArray);
        if ( !transform )
          {
          itkExceptionMacro ("Please set the transform before parameters"
                             "or fixed parameters");
          }
        if ( haveParameters )
          {
          transform->SetFixedParameters (TmpFixedParameterArray);
          itkDebugMacro ("Set Transform Fixed Parameters");
          transform->SetParametersByValue (TmpParameterArray);
          itkDebugMacro ("Set Transform Parameters");
          TmpParameterArray.clear();
          TmpFixedParameterArray.clear();
          haveFixedParameters = false;
          haveParameters = false;
          }
        else
          {
          haveFixedParameters = true;
          }
        }
      }
    }
}

void
TxtTransformIO::Write()
{
  ConstTransformListType::iterator it = this->GetWriteTransformList().begin();

  vnl_vector< double > TempArray;
  std::ofstream        out;
  this->OpenStream(out, false);

  out << "#Insight Transform File V1.0" << std::endl;
  int count = 0;
  while ( it != this->GetWriteTransformList().end() )
    {
    out << "#Transform " << count << std::endl;
    out << "Transform: " << ( *it )->GetTransformTypeAsString() << std::endl;

    TempArray = ( *it )->GetParameters();
    out << "Parameters: " << TempArray << std::endl;
    TempArray = ( *it )->GetFixedParameters();
    out << "FixedParameters: " << TempArray << std::endl;
    it++;
    count++;
    }
  out.close();
}
}
