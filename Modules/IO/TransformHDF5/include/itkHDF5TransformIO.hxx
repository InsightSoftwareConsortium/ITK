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
#ifndef __itkHDF5TransformIO_hxx
#define __itkHDF5TransformIO_hxx

#include "itkHDF5TransformIO.h"
#include "itksys/SystemTools.hxx"
#include "itksys/SystemInformation.hxx"
#include "itkCompositeTransform.h"
#include "itkCompositeTransformIOHelper.h"
#include "itkVersion.h"
#include <sstream>
#include "itk_H5Cpp.h"

namespace itk
{
template< class TInternalComputationValueType >
HDF5TransformIOTemplate< TInternalComputationValueType >
::HDF5TransformIOTemplate()
{
  this->m_H5File = 0;
}

template< class TInternalComputationValueType >
HDF5TransformIOTemplate< TInternalComputationValueType >
::~HDF5TransformIOTemplate()
{
  delete this->m_H5File;
}

template< class TInternalComputationValueType >
bool
HDF5TransformIOTemplate< TInternalComputationValueType >
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

template< class TInternalComputationValueType >
bool
HDF5TransformIOTemplate< TInternalComputationValueType >
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


/** Write a parameter array to the location specified by name */
template< class TInternalComputationValueType >
void
HDF5TransformIOTemplate< TInternalComputationValueType >
::WriteParameters(const std::string &name,
                  const ParametersType &parameters)
{
  hsize_t dim(parameters.Size());

  const char* nameOfComputationType = TypeName<TInternalComputationValueType>::Get();
  TInternalComputationValueType *buf = new TInternalComputationValueType[dim];
  if( !strcmp( nameOfComputationType, "double" ) )
    {
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
    }
  else if( !strcmp( nameOfComputationType, "float" ) )
    {
    for(unsigned i(0); i < dim; i++)
      {
      buf[i] = parameters[i];
      }
    H5::DataSpace paramSpace(1,&dim);
    H5::DataSet paramSet = this->m_H5File->createDataSet(name,
                                                         H5::PredType::NATIVE_FLOAT,
                                                         paramSpace);
    paramSet.write(buf,H5::PredType::NATIVE_FLOAT);
    paramSet.close();
    }
  else
    {
    itkExceptionMacro(<< "Wrong data precision type "
                      << nameOfComputationType
                      << "for writing in HDF5 File");
    }
  delete [] buf;
}

/** read a parameter array from the location specified by name */
template< class TInternalComputationValueType >
typename HDF5TransformIOTemplate< TInternalComputationValueType >::ParametersType
HDF5TransformIOTemplate< TInternalComputationValueType >
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

  if( ParamType.getSize() == sizeof(double) )
    {
    double *buf = new double[dim];
    paramSet.read(buf,H5::PredType::NATIVE_DOUBLE);
    for(unsigned i = 0; i < dim; i++)
      {
      ParameterArray.SetElement(i,(TInternalComputationValueType)(buf[i]));
      }
    delete[] buf;
    }
  else
    {
    float *buf = new float[dim];
    paramSet.read(buf,H5::PredType::NATIVE_FLOAT);
    for(unsigned i = 0; i < dim; i++)
      {
      ParameterArray.SetElement(i,(TInternalComputationValueType)(buf[i]));
      }
    delete[] buf;
    }
  paramSet.close();
  return ParameterArray;
}

template< class TInternalComputationValueType >
void
HDF5TransformIOTemplate< TInternalComputationValueType >
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

template< class TInternalComputationValueType >
void
HDF5TransformIOTemplate< TInternalComputationValueType >
::WriteString(const std::string &path,
                 const char *s)
{
  const std::string _s(s);
  WriteString(path,_s);
}

/*
 File layout
 /TransformGroup
 /TransformGroup/N/TransformType -- string
 /TransformGroup/N/TransformFixedParameters -- list of double
 /TransformGroup/N//TransformParameters -- list of double
 */
template< class TInternalComputationValueType >
void
HDF5TransformIOTemplate< TInternalComputationValueType >
::Read()
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
      // Transform name should be modified to have the output precision type.
      TransformName<TInternalComputationValueType>::CorrectPrecisionType( transformType );

      TransformPointer transform;
      this->CreateTransform(transform,transformType);
      this->GetReadTransformList().push_back (transform);
      //
      // Composite transform doesn't store its own parameters
      if(transformType.find("CompositeTransform") == std::string::npos)
        {
        std::string fixedParamsName(transformName);
        fixedParamsName += transformFixedName;
        ParametersType params(this->ReadParameters(fixedParamsName));
        transform->SetFixedParameters(params);

        std::string paramsName(transformName);
        paramsName += transformParamsName;
        params = this->ReadParameters(paramsName);
        transform->SetParametersByValue(params);
        }
      currentTransformGroup.close();
      }
    transformGroup.close();
    }
  // catch failure caused by the H5File operations
  catch( H5::Exception & error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
}

template< class TInternalComputationValueType >
void
HDF5TransformIOTemplate< TInternalComputationValueType >
::WriteOneTransform(const int transformIndex,
                    const TransformType *curTransform)
{
  std::string transformName(GetTransformName(transformIndex));
  this->m_H5File->createGroup(transformName);
  const std::string transformType = curTransform->GetTransformTypeAsString();
  //
  // write out transform type.
  {
  std::string typeName(transformName);
  typeName += transformTypeName;
  this->WriteString(typeName,transformType);
  }
  //
  // composite transform doesn't store own parameters
  if(transformType.find("CompositeTransform") != std::string::npos)
    {
    if(transformIndex != 0)
      {
      itkExceptionMacro(<< "Composite Transform can only be 1st transform in a file");
      }
    }
  else
    {
    //
    // write out Fixed Parameters
    ParametersType tmpArray = curTransform->GetFixedParameters();
    std::string fixedParamsName(transformName);
    fixedParamsName += transformFixedName;
    this->WriteParameters(fixedParamsName,tmpArray);
    // parameters
    tmpArray = curTransform->GetParameters();
    std::string paramsName(transformName);
    paramsName += transformParamsName;
    this->WriteParameters(paramsName,tmpArray);
    }
}

template< class TInternalComputationValueType >
void
HDF5TransformIOTemplate< TInternalComputationValueType >
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

    this->m_H5File->createGroup(transformGroupName);

    ConstTransformListType &transformList =
      this->GetWriteTransformList();

    std::string compositeTransformType = transformList.front()->GetTransformTypeAsString();

    CompositeTransformIOHelperTemplate<TInternalComputationValueType> helper;
    //
    // if the first transform in the list is a
    // composite transform, use its internal list
    // instead of the IO
    if(compositeTransformType.find("CompositeTransform") != std::string::npos)
      {
      transformList = helper.GetTransformList(transformList.front().GetPointer());
      }

    typename ConstTransformListType::const_iterator end = transformList.end();

    int count = 0;

    for (typename ConstTransformListType::const_iterator it = transformList.begin();
         it != end; ++it,++count )
      {
      this->WriteOneTransform(count,(*it).GetPointer());
      }
    }
  // catch failure caused by the H5File operations
  catch( H5::Exception & error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
}
}

#endif
