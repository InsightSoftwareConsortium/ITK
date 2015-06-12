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
#ifndef itkMatlabTransformIO_hxx
#define itkMatlabTransformIO_hxx

#include "itkMatlabTransformIO.h"
#include "itksys/SystemTools.hxx"
#include "vnl/vnl_matlab_read.h"
#include "vnl/vnl_matlab_write.h"

namespace itk
{
template<typename ParametersValueType>
MatlabTransformIOTemplate<ParametersValueType>
::MatlabTransformIOTemplate()
{}

template<typename ParametersValueType>
MatlabTransformIOTemplate<ParametersValueType>
::~MatlabTransformIOTemplate()
{}

template<typename ParametersValueType>
bool
MatlabTransformIOTemplate<ParametersValueType>
::CanReadFile(const char *fileName)
{
  return itksys::SystemTools::GetFilenameLastExtension(fileName) == ".mat";
}

template<typename ParametersValueType>
bool
MatlabTransformIOTemplate<ParametersValueType>
::CanWriteFile(const char *fileName)
{
  return itksys::SystemTools::GetFilenameLastExtension(fileName) == ".mat";
}

//
// ReadMat -- we always want double precision,
// but handle single precision as well.
template<typename ParametersValueType>
static void
ReadMat(vnl_matlab_readhdr & mathdr,
        typename MatlabTransformIOTemplate<ParametersValueType>::TransformType::ParametersType & array)
{
  if ( mathdr.is_single() )
    {
    vnl_vector< float > fv( mathdr.rows() );
    mathdr.read_data( fv.begin() );
    for ( int i = 0; i < mathdr.rows(); i++ )
      {
      array[i] = (ParametersValueType)(fv[i]);
      }
    }
  else
    {
    vnl_vector< double > dv( mathdr.rows() );
    mathdr.read_data( dv.begin() );
    for ( int i = 0; i < mathdr.rows(); i++ )
      {
      array[i] = (ParametersValueType)(dv[i]);
      }
    }
}

template<typename ParametersValueType>
void
MatlabTransformIOTemplate<ParametersValueType>
::Read()
{
  std::ifstream matfile(this->GetFileName(),
                        std::ios::in | std::ios::binary);

  if ( matfile.fail() )
    {
    matfile.close();
    itkExceptionMacro("The file could not be opened for read access "
                      << std::endl << "Filename: \"" << this->GetFileName() << "\"");
    }
  while ( !matfile.eof() )
    {
    vnl_matlab_readhdr mathdr(matfile);
    if ( !mathdr )
      {
      break;
      }
    if ( mathdr.cols() != 1 )
      {
      matfile.close();
      itkExceptionMacro
        ("Only vector parameters supported");
      }
    typename TransformType::ParametersType TmpParameterArray( mathdr.rows() );
    ReadMat<ParametersValueType>(mathdr, TmpParameterArray);
    std::string classname( mathdr.name() );
    // Transform name should be modified to have the output precision type.
    Superclass::CorrectTransformPrecisionType( classname );

    // create transform based on name of first vector
    TransformPointer transform;
    this->CreateTransform(transform, classname);
    this->GetReadTransformList().push_back(transform);
    vnl_matlab_readhdr mathdr2(matfile);
    if ( mathdr2.cols() != 1 )
      {
      matfile.close();
      itkExceptionMacro
        ("Only vector parameters supported");
      }
    typename TransformType::FixedParametersType TmpFixedParameterArray( mathdr2.rows() );
    ReadMat<typename TransformType::FixedParametersValueType>(mathdr2, TmpFixedParameterArray);
    transform->SetFixedParameters(TmpFixedParameterArray);
    transform->SetParametersByValue(TmpParameterArray);
    }
  matfile.close();
}

template<typename ParametersValueType>
void
MatlabTransformIOTemplate<ParametersValueType>
::Write()
{
  typename ConstTransformListType::iterator it = this->GetWriteTransformList().begin();

  typename MatlabTransformIOTemplate<ParametersValueType>::TransformType::ParametersType TempArray;
  std::ofstream        out;
  this->OpenStream(out, true);
  while ( it != this->GetWriteTransformList().end() )
    {
    std::string xfrmType( ( *it )->GetTransformTypeAsString() );
    TempArray = ( *it )->GetParameters();
    vnl_matlab_write( out, TempArray.begin(), TempArray.size(), xfrmType.c_str() );
    TempArray = ( *it )->GetFixedParameters();
    vnl_matlab_write(out, TempArray.begin(), TempArray.size(), "fixed");
    ++it;
    }
  out.close();
}
}

#endif
