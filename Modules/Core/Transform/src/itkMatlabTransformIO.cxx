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
#include "itkMatlabTransformIO.h"
#include "itksys/SystemTools.hxx"
#include "vnl/vnl_matlab_read.h"
#include "vnl/vnl_matlab_write.h"

namespace itk
{
MatlabTransformIO::MatlabTransformIO()
{}

MatlabTransformIO::
~MatlabTransformIO()
{}

bool
MatlabTransformIO::CanReadFile(const char *fileName)
{
  return itksys::SystemTools::GetFilenameLastExtension(fileName) == ".mat";
}

bool
MatlabTransformIO::CanWriteFile(const char *fileName)
{
  return itksys::SystemTools::GetFilenameLastExtension(fileName) == ".mat";
}

//
// ReadMat -- we always want double precision,
// but handle single precision as well.
static void
ReadMat(vnl_matlab_readhdr & mathdr,
        MatlabTransformIO::TransformType::ParametersType & array)
{
  if ( mathdr.is_single() )
    {
    vnl_vector< float > fv( mathdr.rows() );
    mathdr.read_data( fv.begin() );
    for ( int i = 0; i < mathdr.rows(); i++ )
      {
      array[i] = fv[i];
      }
    }
  else
    {
    mathdr.read_data( array.begin() );
    }
}

void
MatlabTransformIO::Read()
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
    TransformType::ParametersType TmpParameterArray( mathdr.rows() );
    ReadMat(mathdr, TmpParameterArray);
    std::string classname( mathdr.name() );
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
    TransformType::ParametersType TmpFixedParameterArray( mathdr2.rows() );
    ReadMat(mathdr2, TmpFixedParameterArray);
    transform->SetFixedParameters(TmpFixedParameterArray);
    transform->SetParametersByValue(TmpParameterArray);
    }
  matfile.close();
}

void
MatlabTransformIO::Write()
{
  ConstTransformListType::iterator it = this->GetWriteTransformList().begin();

  MatlabTransformIO::TransformType::ParametersType TempArray;
  std::ofstream        out;
  this->OpenStream(out, true);
  while ( it != this->GetWriteTransformList().end() )
    {
    std::string xfrmType( ( *it )->GetTransformTypeAsString() );
    TempArray = ( *it )->GetParameters();
    vnl_matlab_write( out, TempArray.begin(), TempArray.size(), xfrmType.c_str() );
    TempArray = ( *it )->GetFixedParameters();
    vnl_matlab_write(out, TempArray.begin(), TempArray.size(), "fixed");
    it++;
    }
  out.close();
}
}
