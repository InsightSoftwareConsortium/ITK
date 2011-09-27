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
#ifndef __itkCompositeTransformWriter_hxx
#define __itkCompositeTransformWriter_hxx

#include "itkCompositeTransformWriter.h"
#include "itkTransformFileWriter.h"
#include "itksys/SystemTools.hxx"
#include <stdio.h>
#include <sstream>

namespace itk
{
//---------------------------------------------------------
template<class TScalar, unsigned int NDimensions>
CompositeTransformWriter<TScalar, NDimensions>
::CompositeTransformWriter()
{
  m_CompositeTransform = NULL;
}

//---------------------------------------------------------
template<class TScalar, unsigned int NDimensions>
CompositeTransformWriter<TScalar, NDimensions>
::~CompositeTransformWriter()
{}

//---------------------------------------------------------
template<class TScalar, unsigned int NDimensions>
void
CompositeTransformWriter<TScalar, NDimensions>
::SetComponentFileNames(const FileNamesContainer & fileNames)
{
  FileNamesContainer paths;

  //Note, path separator is converted to system-specific separator as
  // needed at time of writing in Update().
  std::string path =
    itksys::SystemTools::GetFilenamePath( this->m_MasterFullPath ) + "/";
  for( FileNamesContainer::const_iterator it = fileNames.begin();
        it != fileNames.end(); it++ )
    {
    paths.push_back( path + *it );
    }

  SetComponentFullPaths( paths );
}

//---------------------------------------------------------
template<class TScalar, unsigned int NDimensions>
void
CompositeTransformWriter<TScalar, NDimensions>
::SetComponentFileNamesByCommonExtension(const std::string & extension)
{
 if( this->m_CompositeTransform.IsNull() )
    {
    itkExceptionMacro(
      << "CompositeTransform must be set before setting component filenames.");
    }
  FileNamesContainer extensions(
    this->m_CompositeTransform->GetNumberOfTransforms(), extension);
  SetComponentFileNamesByExtensions( extensions );
}

//---------------------------------------------------------
template<class TScalar, unsigned int NDimensions>
void
CompositeTransformWriter<TScalar, NDimensions>
::SetComponentFileNamesByExtensions(const FileNamesContainer & extensions)
{
  if( this->m_MasterFullPath.empty() )
    {
    itkExceptionMacro(
      << "MasterFileName must be set before setting component filenames.");
    }

  FileNamesContainer paths;

  //Create prefix from path and filename w/out extension.
  std::string prefix =
    itksys::SystemTools::GetFilenamePath( this->m_MasterFullPath ) + "/" +
    itksys::SystemTools::GetFilenameWithoutLastExtension( this->m_MasterFullPath );

  std::ostringstream stream;
  //Setup for formatting
  size_t digits = extensions.size();
  size_t numberOfDigits=0;
  while( digits > 0 )
    {
    numberOfDigits++;
    digits /= 10;
    }

  //Make the filenames.
  size_t count = 1; //1-based numbering.
  for( FileNamesContainer::const_iterator it = extensions.begin();
        it != extensions.end(); it++ )
    {
      stream.str( "" );
      stream << prefix << "_";
      stream.width( numberOfDigits );
      stream.fill('0');
      stream << std::right << count++;
      if( (*it)[0] != '.' )
        {
        stream << ".";
        }
      stream << *it;
      paths.push_back( stream.str() );
    }

  SetComponentFullPaths( paths );
}

//---------------------------------------------------------
template<class TScalar, unsigned int NDimensions>
void
CompositeTransformWriter<TScalar, NDimensions>
::Update()
{
  itkDebugMacro(<< "Writing a CompositeTransform file.");

  //Verify filenames are set
  if( this->m_CompositeTransform.IsNull() )
    {
    itkExceptionMacro(
      << "CompositeTransform must be set before writing.");
    }
  if( this->m_MasterFullPath.empty() )
    {
    itkExceptionMacro(
      << "MasterFileName must be set before writing.");
    }
  if( this->m_CompositeTransform->GetNumberOfTransforms()
        != this->m_ComponentFullPaths.size() )
    {
    itkExceptionMacro(
      << "Number of transforms must match number of component filenames.");
    }

  vnl_vector< double > TempArray;
  std::ofstream        out;
  this->OpenStream(out, false/*binary*/);

  //Header
  out << "#Insight Composite Transform File V1.0" << std::endl
      << "#Transform 0" << std::endl
      << "Transform: "
      << m_CompositeTransform->GetTransformTypeAsString() << std::endl;

  //Write out each component transform to its own file, and put the
  // name of that file in the main transform file.
  typedef TransformFileWriter WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetAppendOff();
  for( size_t tn=0;
        tn < this->m_CompositeTransform->GetNumberOfTransforms(); tn++ )
    {
    //Add only the filename, without path, to the master file
    out << "ComponentTransformFile: ";
    out << itksys::SystemTools::
      GetFilenameName( this->m_ComponentFullPaths[tn] ) << std::endl;

    //Convert path to platform-specific style. We use "/" to auto-gen these
    // paths above.
    std::string filenameConverted = itksys::SystemTools::ConvertToOutputPath
      ( this->m_ComponentFullPaths[tn].c_str() );
    writer->SetFileName( filenameConverted );
    writer->SetInput( this->m_CompositeTransform->GetNthTransform(tn) );
    writer->Update();
    }

  out.close();

}

//---------------------------------------------------------
template<class TScalar, unsigned int NDimensions>
void
CompositeTransformWriter<TScalar, NDimensions>
::OpenStream(std::ofstream & outputStream, bool binary)
{
  std::ios::openmode mode(std::ios::out);

  if ( binary )
    {
    mode |= std::ios::binary;
    }

  outputStream.open(m_MasterFullPath.c_str(), mode);

  if ( outputStream.fail() )
    {
    outputStream.close();
    itkExceptionMacro("Failed opening file" << m_MasterFullPath);
    }
}

//---------------------------------------------------------
template<class TScalar, unsigned int NDimensions>
void
CompositeTransformWriter<TScalar, NDimensions>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Composite Transform IO: ";
  if ( m_CompositeTransform.IsNull() )
    {
    os << indent << "(none)\n";
    }
  else
    {
    os << indent << m_CompositeTransform << "\n";
    }

  os << indent << "MasterFileName: " << m_MasterFullPath << std::endl;
  os << indent << "Component FileNames: " << std::endl;
  for( FileNamesContainer::const_iterator it = this->m_ComponentFullPaths.begin();
        it != m_ComponentFullPaths.end(); it++ )
    {
    os << indent << *it << std::endl;
    }
}
} // end namespace itk

#endif
