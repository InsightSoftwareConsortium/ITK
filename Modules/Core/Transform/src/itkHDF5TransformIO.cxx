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
#include "itkHDF5TransformIO.h"
#include "itksys/SystemTools.hxx"
#include "itksys/SystemInformation.hxx"
#include "itkVersion.h"
#include <sstream>

#include "itk_H5Cpp.h"

namespace itk
{
HDF5TransformIO
::HDF5TransformIO()
{
  this->m_H5File = 0;
}

HDF5TransformIO
::~HDF5TransformIO()
{
  if(this->m_H5File)
    {
    delete this->m_H5File;
    }
}

bool
HDF5TransformIO
::CanReadFile(const char *fileName)
{
  // call standard method to determine HDF-ness
  bool rval;
  // HDF5 is so exception happy, we have to worry about
  // it throwing a wobbly here if the file doesn't exist
  // or has some other problem.
  try
    {
    rval = H5::H5File::isHdf5(fileName);
    }
  catch(...)
    {
    rval = false;
    }
  return rval;
}

bool
HDF5TransformIO
::CanWriteFile(const char *fileName)
{
  //
  // all extensions mentioned in wikipedia + 'hd5'
  // actually HDF doesn't care about extensions at
  // all and this is just by convention.
  const char *extensions[] =
  {
    ".hdf",".h4",".hdf4",".h5",".hdf5",".he4",".he5",".hd5",0,
  };
  std::string ext
    (itksys::SystemTools::GetFilenameLastExtension(fileName));
  for(unsigned i = 0; extensions[i] != 0; i++)
    {
    if(ext == extensions[i])
      {
      return true;
      }
    }
  return false;
}


/** Write a parameter array to the location specified by name
 */
void
HDF5TransformIO
::WriteParameters(const std::string &name,
                  const ParametersType &parameters)
{
  hsize_t dim(parameters.Size());
  double *buf(new double[dim]);

  for(unsigned i(0); i < dim; i++)
    {
    buf[i] = parameters[i];
    }

  H5::DataSpace paramSpace(1,&dim);
  H5::DataSet paramSet = this->m_H5File->createDataSet(name,
                                                       H5::PredType::NATIVE_DOUBLE,
                                                       paramSpace);
  paramSet.write(buf,H5::PredType::NATIVE_DOUBLE);
  paramSet.close();
  delete [] buf;
}

/** read a parameter array from the location specified by name */
HDF5TransformIO::ParametersType
HDF5TransformIO
::ReadParameters(const std::string &DataSetName)
{
  ParametersType ParameterArray;
  hsize_t dim;
  H5::DataSet paramSet = this->m_H5File->openDataSet(DataSetName);
  H5T_class_t Type = paramSet.getTypeClass();
  if(Type != H5T_FLOAT)
    {
    itkExceptionMacro(<< "Wrong data type for "
                      << DataSetName
                      << "in HDF5 File");
    }
  H5::DataSpace Space = paramSet.getSpace();
  if(Space.getSimpleExtentNdims() != 1)
    {
    itkExceptionMacro(<< "Wrong # of dims for TransformType "
                      << "in HDF5 File");
    }
  Space.getSimpleExtentDims(&dim,0);
  ParameterArray.SetSize(dim);
  H5::FloatType ParamType = paramSet.getFloatType();
  if(ParamType.getSize() == sizeof(double))
    {
    double *buf = new double[dim];
    paramSet.read(buf,H5::PredType::NATIVE_DOUBLE);
    ParameterArray.SetData(buf,dim,true);
    }
  else
    {
    float *buf = new float[dim];
    paramSet.read(buf,H5::PredType::NATIVE_FLOAT);
    for(unsigned i = 0; i < dim; i++)
      {
      ParameterArray.SetElement(i,buf[i]);
      }
    delete [] buf;
    }
  paramSet.close();
  return ParameterArray;
}

void
HDF5TransformIO
::WriteString(const std::string &path,
              const std::string &value)
{
  hsize_t numStrings(1);
  H5::DataSpace strSpace(1,&numStrings);
  H5::StrType strType(H5::PredType::C_S1,H5T_VARIABLE);
  H5::DataSet strSet = this->m_H5File->createDataSet(path,strType,strSpace);
  strSet.write(value,strType);
  strSet.close();
}

void
HDF5TransformIO
::WriteString(const std::string &path,
                 const char *s)
{
  const std::string _s(s);
  WriteString(path,_s);
}

namespace
{
//
// HDF uses hierarchical paths to find particular data
// in a file. These strings are used by both reading and
// writing.
const std::string transformGroupName("/TransformGroup");
const std::string transformTypeName("/TransformType");
const std::string transformFixedName("/TranformFixedParameters");
const std::string transformParamsName("/TranformParameters");
const std::string ItkVersion("/ITKVersion");
const std::string HDFVersion("/HDFVersion");
const std::string OSName("/OSName");
const std::string OSVersion("/OSVersion");

//
// I couldn't figure out a way to represent transforms
// excepts as groups -- the HDF5 composite only allows
// fixed-size structures.
// Since (for now) transforms are ordered in a file, but
// not nameed, I name them by their order in the file,
// beginning with zero.
const std::string
GetTransformName(int i)
{
  std::stringstream s;
  s << transformGroupName;
  s << "/";
  s << i;
  return s.str();
}
}

/*
 File layout
 /TransformGroup
 /TransformGroup/N/TransformType -- string
 /TransformGroup/N/TransformFixedParameters -- list of double
 /TransformGroup/N//TransformParameters -- list of double
 */
void
HDF5TransformIO::Read()
{
  //
  // HDF5 is pretty heavily exception-oriented. Some
  // exceptions might be recovered from, theoretically,
  // but in our case, either the data we want is there
  // and of the right type, or we give up.  So everything
  // happens in a big try/catch clause
  try
    {
    this->m_H5File = new H5::H5File(this->GetFileName(),H5F_ACC_RDONLY);
    // open /TransformGroup
    H5::Group transformGroup = this->m_H5File->openGroup(transformGroupName);

    for(unsigned int i = 0; i < transformGroup.getNumObjs(); i++)
      {
      std::string transformName(GetTransformName(i));
      // open /TransformGroup/N
      H5::Group currentTransformGroup = this->m_H5File->openGroup(transformName);
      //
      // read transform type
      std::string transformType;
      {
      hsize_t numStrings(1);
      H5::DataSpace strSpace(1,&numStrings);
      H5::StrType typeType(H5::PredType::C_S1,H5T_VARIABLE);
      std::string typeName(transformName);
      typeName += transformTypeName;
      H5::DataSet typeSet = this->m_H5File->openDataSet(typeName);
      typeSet.read(transformType,typeType,strSpace);
      typeSet.close();
      }

      TransformPointer transform;
      this->CreateTransform(transform,transformType);
      this->GetReadTransformList().push_back (transform);

      std::string fixedParamsName(transformName);
      fixedParamsName += transformFixedName;
      ParametersType params(this->ReadParameters(fixedParamsName));
      transform->SetFixedParameters(params);

      std::string paramsName(transformName);
      paramsName += transformParamsName;
      params = this->ReadParameters(paramsName);
      transform->SetParametersByValue(params);
      currentTransformGroup.close();
      }
    transformGroup.close();
    }
  // catch failure caused by the H5File operations
  catch( H5::FileIException error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  // catch failure caused by the DataSet operations
  catch( H5::DataSetIException error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  // catch failure caused by the DataSpace operations
  catch( H5::DataSpaceIException error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  // catch failure caused by the DataSpace operations
  catch( H5::DataTypeIException error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
}

void
HDF5TransformIO
::Write()
{
  itksys::SystemInformation sysInfo;
  sysInfo.RunOSCheck();
  try
    {
    this->m_H5File = new H5::H5File(this->GetFileName(),H5F_ACC_TRUNC);

    this->WriteString(ItkVersion, Version::GetITKVersion());
    this->WriteString(HDFVersion, H5_VERS_INFO);
    this->WriteString(OSName,sysInfo.GetOSName());
    this->WriteString(OSVersion,sysInfo.GetOSRelease());

    H5::Group transformGroup = this->m_H5File->createGroup(transformGroupName);
    int i = 0;
    for(ConstTransformListType::iterator it =
          this->GetWriteTransformList().begin();
        it != this->GetWriteTransformList().end();
        it++,i++)
      {
      std::string transformName(GetTransformName(i));
      H5::Group transform = this->m_H5File->createGroup(transformName);
      //
      // write out transform type.
      {
      hsize_t numStrings(1);
      H5::DataSpace typeSpace(1,&numStrings);
      H5::StrType typeType(H5::PredType::C_S1,H5T_VARIABLE);
      std::string typeName(transformName);
      typeName += transformTypeName;
      H5::DataSet typeSet = this->m_H5File->createDataSet(typeName,typeType,typeSpace);
      const std::string transformType =
        (*it)->GetTransformTypeAsString();
      typeSet.write(transformType,typeType);
      typeSet.close();
      }
      //
      // write out Fixed Parameters
      ParametersType tmpArray = (*it)->GetFixedParameters();
      std::string fixedParamsName(transformName);
      fixedParamsName += transformFixedName;
      this->WriteParameters(fixedParamsName,tmpArray);
      // parameters
      tmpArray = (*it)->GetParameters();
      std::string paramsName(transformName);
      paramsName += transformParamsName;
      this->WriteParameters(paramsName,tmpArray);
      }
    }
  // catch failure caused by the H5File operations
  catch( H5::FileIException error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  // catch failure caused by the DataSet operations
  catch( H5::DataSetIException error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  // catch failure caused by the DataSpace operations
  catch( H5::DataSpaceIException error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  // catch failure caused by the DataSpace operations
  catch( H5::DataTypeIException error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
}
}
