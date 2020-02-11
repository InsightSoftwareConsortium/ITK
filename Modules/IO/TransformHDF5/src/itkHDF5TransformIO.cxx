/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#define ITK_TEMPLATE_EXPLICIT_HDF5TransformIO
#include "itkHDF5TransformIO.h"
#include "itk_H5Cpp.h"
#include "itksys/SystemTools.hxx"
#include "itksys/SystemInformation.hxx"
#include "itkCompositeTransform.h"
#include "itkCompositeTransformIOHelper.h"
#include "itkVersion.h"
#include <sstream>

namespace itk
{
template <typename TParametersValueType>
HDF5TransformIOTemplate<TParametersValueType>::HDF5TransformIOTemplate() = default;

template <typename TParametersValueType>
HDF5TransformIOTemplate<TParametersValueType>::~HDF5TransformIOTemplate() = default;

template <typename TParametersValueType>
bool
HDF5TransformIOTemplate<TParametersValueType>::CanReadFile(const char * fileName)
{
  // HDF5 is so exception happy, we have to worry about
  // it throwing a wobbly here if the file doesn't exist
  // or has some other problem.
  bool rval = true;
  try
  {
    htri_t ishdf5 = H5Fis_hdf5(fileName);

    if (ishdf5 <= 0)
    {
      return false;
    }

    H5::H5File h5file(fileName, H5F_ACC_RDONLY);

#if (H5_VERS_MAJOR == 1) && (H5_VERS_MINOR < 10)
    // check the file has the "TransformGroup"
    htri_t exists = H5Lexists(h5file.getId(), transformGroupName.c_str(), H5P_DEFAULT);
    if (exists <= 0)
#else
    if (!h5file.exists(transformGroupName))
#endif
    {
      rval = false;
    }
  }
  catch (...)
  {
    rval = false;
  }
  return rval;
}

template <typename TParametersValueType>
bool
HDF5TransformIOTemplate<TParametersValueType>::CanWriteFile(const char * fileName)
{
  //
  // all extensions mentioned in wikipedia + 'hd5'
  // actually HDF doesn't care about extensions at
  // all and this is just by convention.
  const char * extensions[] = {
    ".hdf", ".h4", ".hdf4", ".h5", ".hdf5", ".he4", ".he5", ".hd5", nullptr,
  };
  std::string ext(itksys::SystemTools::GetFilenameLastExtension(fileName));
  for (unsigned i = 0; extensions[i] != nullptr; i++)
  {
    if (ext == extensions[i])
    {
      return true;
    }
  }
  return false;
}

template <typename TParametersValueType>
H5::PredType
HDF5TransformIOTemplate<TParametersValueType>::GetH5TypeFromString() const
{
  const std::string & NameParametersValueTypeString = Superclass::GetTypeNameString();
  if (!NameParametersValueTypeString.compare(std::string("double")))
  {
    return H5::PredType::NATIVE_DOUBLE;
  }
  else if (!NameParametersValueTypeString.compare(std::string("float")))
  {
    return H5::PredType::NATIVE_FLOAT;
  }

  itkExceptionMacro(<< "Wrong data precision type " << NameParametersValueTypeString << "for writing in HDF5 File");
}

/** Write a Parameter array to the location specified by name */
template <typename TParametersValueType>
void
HDF5TransformIOTemplate<TParametersValueType>::WriteParameters(const std::string &    name,
                                                               const ParametersType & parameters)
{
  const hsize_t dim(parameters.Size());
  H5::DataSpace paramSpace(1, &dim);

  H5::DataSet paramSet;

  // Set the storage format type
  const H5::PredType h5StorageIdentifier{ GetH5TypeFromString() };
  if (this->GetUseCompression())
  {
    // Set compression information
    // set up properties for chunked, compressed writes.
    // in this case, set the chunk size to be the N-1 dimension
    // region
    H5::DSetCreatPropList plist;
    plist.setDeflate(5); // Set intermediate compression level
    constexpr hsize_t oneMegabyte = 1024 * 1024;
    const hsize_t     chunksize = (dim > oneMegabyte) ? oneMegabyte : dim; // Use chunks of 1 MB if large, else use dim
    plist.setChunk(1, &chunksize);

    paramSet = this->m_H5File->createDataSet(name, h5StorageIdentifier, paramSpace, plist);
  }
  else
  {
    paramSet = this->m_H5File->createDataSet(name, h5StorageIdentifier, paramSpace);
  }
  paramSet.write(parameters.data_block(), h5StorageIdentifier);
  paramSet.close();
}

template <typename TParametersValueType>
void
HDF5TransformIOTemplate<TParametersValueType>::WriteFixedParameters(const std::string &         name,
                                                                    const FixedParametersType & fixedParameters)
{
  const hsize_t dim(fixedParameters.Size());
  H5::DataSpace paramSpace(1, &dim);
  H5::DataSet   paramSet = this->m_H5File->createDataSet(name, H5::PredType::NATIVE_DOUBLE, paramSpace);
  paramSet.write(fixedParameters.data_block(), H5::PredType::NATIVE_DOUBLE);
  paramSet.close();
}

/** read a parameter array from the location specified by name */
template <typename TParametersValueType>
typename HDF5TransformIOTemplate<TParametersValueType>::ParametersType
HDF5TransformIOTemplate<TParametersValueType>::ReadParameters(const std::string & DataSetName) const
{

  H5::DataSet       paramSet = this->m_H5File->openDataSet(DataSetName);
  const H5T_class_t Type = paramSet.getTypeClass();
  if (Type != H5T_FLOAT)
  {
    itkExceptionMacro(<< "Wrong data type for " << DataSetName << "in HDF5 File");
  }
  const H5::DataSpace Space = paramSet.getSpace();
  if (Space.getSimpleExtentNdims() != 1)
  {
    itkExceptionMacro(<< "Wrong # of dims for TransformType "
                      << "in HDF5 File");
  }
  hsize_t dim;
  Space.getSimpleExtentDims(&dim, nullptr);
  ParametersType ParameterArray;
  ParameterArray.SetSize(dim);
  H5::FloatType ParamType = paramSet.getFloatType();

  if (ParamType.getSize() == sizeof(double))
  {
    const std::unique_ptr<double[]> buf(new double[dim]);
    paramSet.read(buf.get(), H5::PredType::NATIVE_DOUBLE);
    for (unsigned i = 0; i < dim; i++)
    {
      ParameterArray.SetElement(i, static_cast<ParametersValueType>(buf[i]));
    }
  }
  else
  {
    const std::unique_ptr<float[]> buf(new float[dim]);
    paramSet.read(buf.get(), H5::PredType::NATIVE_FLOAT);
    for (unsigned i = 0; i < dim; i++)
    {
      ParameterArray.SetElement(i, static_cast<ParametersValueType>(buf[i]));
    }
  }
  paramSet.close();
  return ParameterArray;
}

/** read a parameter array from the location specified by name */
template <typename TParametersValueType>
typename HDF5TransformIOTemplate<TParametersValueType>::FixedParametersType
HDF5TransformIOTemplate<TParametersValueType>::ReadFixedParameters(const std::string & DataSetName) const
{

  H5::DataSet paramSet = this->m_H5File->openDataSet(DataSetName);
  H5T_class_t Type = paramSet.getTypeClass();
  if (Type != H5T_FLOAT)
  {
    itkExceptionMacro(<< "Wrong data type for " << DataSetName << "in HDF5 File");
  }
  const H5::DataSpace Space = paramSet.getSpace();
  if (Space.getSimpleExtentNdims() != 1)
  {
    itkExceptionMacro(<< "Wrong # of dims for TransformType "
                      << "in HDF5 File");
  }
  hsize_t dim;
  Space.getSimpleExtentDims(&dim, nullptr);
  FixedParametersType FixedParameterArray;

  FixedParameterArray.SetSize(dim);
  const H5::FloatType ParamType = paramSet.getFloatType();

  if (ParamType.getSize() == sizeof(double))
  {
    const std::unique_ptr<double[]> buf(new double[dim]);
    paramSet.read(buf.get(), H5::PredType::NATIVE_DOUBLE);
    for (unsigned i = 0; i < dim; i++)
    {
      FixedParameterArray.SetElement(i, static_cast<FixedParametersValueType>(buf[i]));
    }
  }
  else
  {
    const std::unique_ptr<float[]> buf(new float[dim]);
    paramSet.read(buf.get(), H5::PredType::NATIVE_FLOAT);
    for (unsigned i = 0; i < dim; i++)
    {
      FixedParameterArray.SetElement(i, static_cast<FixedParametersValueType>(buf[i]));
    }
  }
  paramSet.close();
  return FixedParameterArray;
}


template <typename TParametersValueType>
void
HDF5TransformIOTemplate<TParametersValueType>::WriteString(const std::string & path, const std::string & value)
{
  hsize_t       numStrings(1);
  H5::DataSpace strSpace(1, &numStrings);
  H5::StrType   strType(H5::PredType::C_S1, H5T_VARIABLE);
  H5::DataSet   strSet = this->m_H5File->createDataSet(path, strType, strSpace);
  strSet.write(value, strType);
  strSet.close();
}

template <typename TParametersValueType>
void
HDF5TransformIOTemplate<TParametersValueType>::WriteString(const std::string & path, const char * s)
{
  const std::string _s(s);
  WriteString(path, _s);
}

/*
 File layout
 /TransformGroup
 /TransformGroup/N/TransformType -- string
 /TransformGroup/N/TransformFixedParameters -- list of double
 /TransformGroup/N//TransformParameters -- list of double
 */
template <typename TParametersValueType>
void
HDF5TransformIOTemplate<TParametersValueType>::Read()
{
  //
  // HDF5 is pretty heavily exception-oriented. Some
  // exceptions might be recovered from, theoretically,
  // but in our case, either the data we want is there
  // and of the right type, or we give up.  So everything
  // happens in a big try/catch clause
  try
  {
    this->m_H5File.reset(new H5::H5File(this->GetFileName(), H5F_ACC_RDONLY));
    // open /TransformGroup
    H5::Group transformGroup = this->m_H5File->openGroup(transformGroupName);

    for (unsigned int i = 0; i < transformGroup.getNumObjs(); i++)
    {
      std::string transformName(GetTransformName(i));

      // open /TransformGroup/N
      H5::Group currentTransformGroup = this->m_H5File->openGroup(transformName);
      //
      // read transform type
      std::string transformType;
      {
        hsize_t       numStrings(1);
        H5::DataSpace strSpace(1, &numStrings);
        H5::StrType   typeType(H5::PredType::C_S1, H5T_VARIABLE);
        std::string   typeName(transformName);
        typeName += transformTypeName;
        H5::DataSet typeSet = this->m_H5File->openDataSet(typeName);
        typeSet.read(transformType, typeType, strSpace);
        typeSet.close();
      }
      // Transform name should be modified to have the output precision type.
      Superclass::CorrectTransformPrecisionType(transformType);

      TransformPointer transform;
      this->CreateTransform(transform, transformType);
      this->GetReadTransformList().push_back(transform);
      //
      // Composite transform doesn't store its own parameters
      if (transformType.find("CompositeTransform") == std::string::npos)
      {
        std::string fixedParamsName(transformName + transformFixedName);
#if (H5_VERS_MAJOR == 1) && (H5_VERS_MINOR < 10)
        // check if group exists
        htri_t exists = H5Lexists(this->m_H5File->getId(), fixedParamsName.c_str(), H5P_DEFAULT);
        if (exists == 0)
        {
#else
        if (!this->m_H5File->exists(fixedParamsName))
        {
#endif
          fixedParamsName = transformName + transformFixedNameMisspelled;
        }
        FixedParametersType fixedparams(this->ReadFixedParameters(fixedParamsName));
        transform->SetFixedParameters(fixedparams);

        std::string paramsName(transformName + transformParamsName);
#if (H5_VERS_MAJOR == 1) && (H5_VERS_MINOR < 10)
        exists = H5Lexists(this->m_H5File->getId(), paramsName.c_str(), H5P_DEFAULT);
        if (exists == 0)
        {
#else
        if (!this->m_H5File->exists(paramsName))
        {
#endif
          paramsName = transformName + transformParamsNameMisspelled;
        }
        ParametersType params = this->ReadParameters(paramsName);
        transform->SetParametersByValue(params);
      }
      currentTransformGroup.close();
    }
    transformGroup.close();
    this->m_H5File->close();
  }
  // catch failure caused by the H5File operations
  catch (H5::Exception & error)
  {
    itkExceptionMacro(<< error.getCDetailMsg());
  }
}

template <typename TParametersValueType>
void
HDF5TransformIOTemplate<TParametersValueType>::WriteOneTransform(const int             transformIndex,
                                                                 const TransformType * curTransform)
{
  std::string transformName(GetTransformName(transformIndex));
  this->m_H5File->createGroup(transformName);
  const std::string transformType = curTransform->GetTransformTypeAsString();
  //
  // write out transform type.
  {
    std::string typeName(transformName);
    typeName += transformTypeName;
    this->WriteString(typeName, transformType);
  }
  //
  // composite transform doesn't store own parameters
  if (transformType.find("CompositeTransform") != std::string::npos)
  {
    if (transformIndex != 0)
    {
      itkExceptionMacro(<< "Composite Transform can only be 1st transform in a file");
    }
  }
  else
  {
    //
    // write out Fixed Parameters
    FixedParametersType FixedtmpArray = curTransform->GetFixedParameters();
    const std::string   fixedParamsName(transformName + transformFixedName);
    this->WriteFixedParameters(fixedParamsName, FixedtmpArray);
    // parameters
    ParametersType    tmpArray = curTransform->GetParameters();
    const std::string paramsName(transformName + transformParamsName);
    this->WriteParameters(paramsName, tmpArray);
  }
}

template <typename TParametersValueType>
void
HDF5TransformIOTemplate<TParametersValueType>::Write()
{
  itksys::SystemInformation sysInfo;
  sysInfo.RunOSCheck();
  try
  {
    H5::FileAccPropList fapl;
#if (H5_VERS_MAJOR > 1) || (H5_VERS_MAJOR == 1) && (H5_VERS_MINOR > 10) ||                                             \
  (H5_VERS_MAJOR == 1) && (H5_VERS_MINOR == 10) && (H5_VERS_RELEASE >= 2)
    // File format which is backwards compatible with HDF5 version 1.8
    // Only HDF5 v1.10.2 has both setLibverBounds method and H5F_LIBVER_V18 constant
    fapl.setLibverBounds(H5F_LIBVER_V18, H5F_LIBVER_V18);
#elif (H5_VERS_MAJOR == 1) && (H5_VERS_MINOR == 10) && (H5_VERS_RELEASE < 2)
#  error The selected version of HDF5 library does not support setting backwards compatibility at run-time.\
  Please use a different version of HDF5, e.g. the one bundled with ITK (by setting ITK_USE_SYSTEM_HDF5 to OFF).
#endif
    this->m_H5File.reset(new H5::H5File(this->GetFileName(), H5F_ACC_TRUNC, H5::FileCreatPropList::DEFAULT, fapl));

    this->WriteString(ItkVersion, Version::GetITKVersion());
    this->WriteString(HDFVersion, H5_VERS_INFO);
    this->WriteString(OSName, sysInfo.GetOSName());
    this->WriteString(OSVersion, sysInfo.GetOSRelease());

    this->m_H5File->createGroup(transformGroupName);

    ConstTransformListType & transformList = this->GetWriteTransformList();

    std::string compositeTransformType = transformList.front()->GetTransformTypeAsString();

    CompositeTransformIOHelperTemplate<TParametersValueType> helper;
    //
    // if the first transform in the list is a
    // composite transform, use its internal list
    // instead of the IO
    if (compositeTransformType.find("CompositeTransform") != std::string::npos)
    {
      transformList = helper.GetTransformList(transformList.front().GetPointer());
    }

    typename ConstTransformListType::const_iterator end = transformList.end();

    int count = 0;

    for (typename ConstTransformListType::const_iterator it = transformList.begin(); it != end; ++it, ++count)
    {
      this->WriteOneTransform(count, (*it).GetPointer());
    }
    this->m_H5File->close();
  }
  // catch failure caused by the H5File operations
  catch (H5::Exception & error)
  {
    itkExceptionMacro(<< error.getCDetailMsg());
  }
}

} // end namespace itk

namespace itk
{

// HDF uses hierarchical paths to find particular data
// in a file. These strings are used by both reading and
// writing.
const std::string HDF5CommonPathNames::transformGroupName(std::string("/TransformGroup"));
const std::string HDF5CommonPathNames::transformTypeName("/TransformType");

const std::string HDF5CommonPathNames::transformFixedNameMisspelled("/TranformFixedParameters");
const std::string HDF5CommonPathNames::transformParamsNameMisspelled("/TranformParameters");
const std::string HDF5CommonPathNames::transformFixedName("/TransformFixedParameters");
const std::string HDF5CommonPathNames::transformParamsName("/TransformParameters");

const std::string HDF5CommonPathNames::ItkVersion("/ITKVersion");
const std::string HDF5CommonPathNames::HDFVersion("/HDFVersion");
const std::string HDF5CommonPathNames::OSName("/OSName");
const std::string HDF5CommonPathNames::OSVersion("/OSVersion");


// I couldn't figure out a way to represent transforms
// excepts as groups -- the HDF5 composite only allows
// fixed-size structures.
// Since (for now) transforms are ordered in a file, but
// not named, I name them by their order in the file,
// beginning with zero.
const std::string
GetTransformName(int i)
{
  std::stringstream s;
  s << HDF5CommonPathNames::transformGroupName;
  s << "/";
  s << i;
  return s.str();
}

ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

template class ITKIOTransformHDF5_EXPORT HDF5TransformIOTemplate<double>;
template class ITKIOTransformHDF5_EXPORT HDF5TransformIOTemplate<float>;

ITK_GCC_PRAGMA_DIAG_POP()

} // end namespace itk
