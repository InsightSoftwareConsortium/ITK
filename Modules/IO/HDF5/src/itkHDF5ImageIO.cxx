/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkVersion.h"
#include "itkHDF5ImageIO.h"
#include "itkMetaDataObject.h"
#include "itkArray.h"
#include "itksys/SystemTools.hxx"
#include "itk_H5Cpp.h"
#include "itkMakeUniqueForOverwrite.h"

#include <algorithm>

namespace itk
{

HDF5ImageIO::HDF5ImageIO()

{

  const char * extensions[] = { ".hdf", ".h4", ".hdf4", ".h5", ".hdf5", ".he4", ".he5", ".hd5" };


  for (auto ext : extensions)
  {
    this->AddSupportedWriteExtension(ext);
    this->AddSupportedReadExtension(ext);
  }
  this->Self::SetMaximumCompressionLevel(9);
  this->Self::SetCompressionLevel(5);
}

HDF5ImageIO::~HDF5ImageIO()
{
  this->ResetToInitialState();
}

void
HDF5ImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  // just prints out the pointer value.
  os << indent << "H5File: " << this->m_H5File << std::endl;
}

//
// strings defining HDF file layout for image data.
namespace
{
const std::string ItkVersion("/ITKVersion");
const std::string HDFVersion("/HDFVersion");
const std::string ImageGroup("/ITKImage");
const std::string Origin("/Origin");
const std::string Directions("/Directions");
const std::string Spacing("/Spacing");
const std::string Dimensions("/Dimension");
const std::string VoxelType("/VoxelType");
const std::string VoxelData("/VoxelData");
const std::string MetaDataName("/MetaData");

template <typename TScalar>
H5::PredType
GetType()
{
  itkGenericExceptionMacro(<< "Type not handled "
                           << "in HDF5 File: " << typeid(TScalar).name());
}
#define GetH5TypeSpecialize(CXXType, H5Type) \
  template <>                                \
  H5::PredType GetType<CXXType>()            \
  {                                          \
    return H5Type;                           \
  }

GetH5TypeSpecialize(float, H5::PredType::NATIVE_FLOAT) GetH5TypeSpecialize(double, H5::PredType::NATIVE_DOUBLE)

  GetH5TypeSpecialize(char, H5::PredType::NATIVE_CHAR) GetH5TypeSpecialize(unsigned char, H5::PredType::NATIVE_UCHAR)

    GetH5TypeSpecialize(short, H5::PredType::NATIVE_SHORT)
      GetH5TypeSpecialize(short unsigned int, H5::PredType::NATIVE_USHORT)

        GetH5TypeSpecialize(int, H5::PredType::NATIVE_INT) GetH5TypeSpecialize(unsigned int, H5::PredType::NATIVE_UINT)

          GetH5TypeSpecialize(long, H5::PredType::NATIVE_LONG)
            GetH5TypeSpecialize(long unsigned int, H5::PredType::NATIVE_ULONG)

              GetH5TypeSpecialize(long long, H5::PredType::NATIVE_LLONG)
                GetH5TypeSpecialize(unsigned long long, H5::PredType::NATIVE_ULLONG)

/* The following types are not implemented.  This comment serves
 * to indicate that the full complement of possible H5::PredType
 * types are not implemented int the ITK IO reader/writer
 * GetH5TypeSpecialize(bool,              H5::PredType::NATIVE_HBOOL)
 */

#undef GetH5TypeSpecialize

                  inline IOComponentEnum PredTypeToComponentType(H5::DataType & type)
{
  if (type == H5::PredType::NATIVE_UCHAR)
  {
    return IOComponentEnum::UCHAR;
  }
  else if (type == H5::PredType::NATIVE_CHAR)
  {
    return IOComponentEnum::CHAR;
  }
  else if (type == H5::PredType::NATIVE_USHORT)
  {
    return IOComponentEnum::USHORT;
  }
  else if (type == H5::PredType::NATIVE_SHORT)
  {
    return IOComponentEnum::SHORT;
  }
  else if (type == H5::PredType::NATIVE_UINT)
  {
    return IOComponentEnum::UINT;
  }
  else if (type == H5::PredType::NATIVE_INT)
  {
    return IOComponentEnum::INT;
  }
  else if (type == H5::PredType::NATIVE_ULONG)
  {
    return IOComponentEnum::ULONG;
  }
  else if (type == H5::PredType::NATIVE_LONG)
  {
    return IOComponentEnum::LONG;
  }
  else if (type == H5::PredType::NATIVE_LLONG)
  {
    return IOComponentEnum::LONGLONG;
  }
  else if (type == H5::PredType::NATIVE_ULLONG)
  {
    return IOComponentEnum::ULONGLONG;
  }
  else if (type == H5::PredType::NATIVE_FLOAT)
  {
    return IOComponentEnum::FLOAT;
  }
  else if (type == H5::PredType::NATIVE_DOUBLE)
  {
    return IOComponentEnum::DOUBLE;
  }
  itkGenericExceptionMacro(<< "unsupported HDF5 data type with id " << type.getId());
}

H5::PredType
ComponentToPredType(IOComponentEnum cType)
{
  switch (cType)
  {
    case IOComponentEnum::UCHAR:
      return H5::PredType::NATIVE_UCHAR;
    case IOComponentEnum::CHAR:
      return H5::PredType::NATIVE_CHAR;
    case IOComponentEnum::USHORT:
      return H5::PredType::NATIVE_USHORT;
    case IOComponentEnum::SHORT:
      return H5::PredType::NATIVE_SHORT;
    case IOComponentEnum::UINT:
      return H5::PredType::NATIVE_UINT;
    case IOComponentEnum::INT:
      return H5::PredType::NATIVE_INT;
    case IOComponentEnum::ULONG:
      return H5::PredType::NATIVE_ULONG;
    case IOComponentEnum::LONG:
      return H5::PredType::NATIVE_LONG;
    case IOComponentEnum::ULONGLONG:
      return H5::PredType::NATIVE_ULLONG;
    case IOComponentEnum::LONGLONG:
      return H5::PredType::NATIVE_LLONG;
    case IOComponentEnum::FLOAT:
      return H5::PredType::NATIVE_FLOAT;
    case IOComponentEnum::DOUBLE:
      return H5::PredType::NATIVE_DOUBLE;
    case IOComponentEnum::LDOUBLE:
    case IOComponentEnum::UNKNOWNCOMPONENTTYPE:
      itkGenericExceptionMacro(<< "unsupported IOComponentEnum" << static_cast<char>(cType));
  }

  itkGenericExceptionMacro(<< "unsupported IOComponentEnum" << static_cast<char>(cType));
}

std::string
ComponentToString(IOComponentEnum cType)
{
  std::string rval;
  switch (cType)
  {
    case IOComponentEnum::UCHAR:
      rval = "UCHAR";
      break;
    case IOComponentEnum::CHAR:
      rval = "CHAR";
      break;
    case IOComponentEnum::USHORT:
      rval = "USHORT";
      break;
    case IOComponentEnum::SHORT:
      rval = "SHORT";
      break;
    case IOComponentEnum::UINT:
      rval = "UINT";
      break;
    case IOComponentEnum::INT:
      rval = "INT";
      break;
    case IOComponentEnum::ULONG:
      rval = "ULONG";
      break;
    case IOComponentEnum::LONG:
      rval = "LONG";
      break;
    case IOComponentEnum::ULONGLONG:
      rval = "ULONGLONG";
      break;
    case IOComponentEnum::LONGLONG:
      rval = "LONGLONG";
      break;
    case IOComponentEnum::FLOAT:
      rval = "FLOAT";
      break;
    case IOComponentEnum::DOUBLE:
      rval = "DOUBLE";
      break;
    default:
      itkGenericExceptionMacro(<< "unsupported IOComponentEnum" << static_cast<char>(cType));
  }
  return rval;
}
// Function:    H5Object::doesAttrExist
///\brief       test for existence of attribut
///\param       name - IN: Name of the attribute
///\return      true if attribute exists, false otherwise
///\exception   none
// Programmer   Kent Williams 2011
//--------------------------------------------------------------------------
static bool
doesAttrExist(const H5::H5Object & object, const char * const name)
{
  return (H5Aexists(object.getId(), name) > 0 ? true : false);
}

} // namespace

void
HDF5ImageIO::WriteScalar(const std::string & path, const bool value)
{
  hsize_t       numScalars(1);
  H5::DataSpace scalarSpace(1, &numScalars);
  H5::PredType  scalarType = H5::PredType::NATIVE_HBOOL;
  H5::DataSet   scalarSet = this->m_H5File->createDataSet(path, scalarType, scalarSpace);
  //
  // HDF5 can't distinguish
  // between bool and int datasets
  // in a disk file. So add an attribute
  // labeling this as a bool
  const std::string isBoolName("isBool");
  H5::Attribute     isBool = scalarSet.createAttribute(isBoolName, scalarType, scalarSpace);
  bool              trueVal(true);
  isBool.write(scalarType, &trueVal);
  isBool.close();
  auto tempVal = static_cast<int>(value);
  scalarSet.write(&tempVal, scalarType);
  scalarSet.close();
}

void
HDF5ImageIO::WriteScalar(const std::string & path, const long value)
{
  hsize_t       numScalars(1);
  H5::DataSpace scalarSpace(1, &numScalars);
  H5::PredType  scalarType = H5::PredType::NATIVE_INT;
  H5::PredType  attrType = H5::PredType::NATIVE_HBOOL;
  H5::DataSet   scalarSet = this->m_H5File->createDataSet(path, scalarType, scalarSpace);
  //
  // HDF5 can't distinguish
  // between long and int datasets
  // in a disk file. So add an attribute
  // labeling this as a long.
  const std::string isLongName("isLong");
  H5::Attribute     isLong = scalarSet.createAttribute(isLongName, attrType, scalarSpace);
  bool              trueVal(true);
  isLong.write(attrType, &trueVal);
  isLong.close();
  auto tempVal = static_cast<int>(value);
  scalarSet.write(&tempVal, scalarType);
  scalarSet.close();
}

void
HDF5ImageIO::WriteScalar(const std::string & path, const unsigned long value)
{
  hsize_t       numScalars(1);
  H5::DataSpace scalarSpace(1, &numScalars);
  H5::PredType  scalarType = H5::PredType::NATIVE_UINT;
  H5::PredType  attrType = H5::PredType::NATIVE_HBOOL;
  H5::DataSet   scalarSet = this->m_H5File->createDataSet(path, scalarType, scalarSpace);
  //
  // HDF5 can't distinguish
  // between unsigned long and unsigned int datasets
  // in a disk file. So add an attribute
  // labeling this as an unsigned long.
  const std::string isUnsignedLongName("isUnsignedLong");
  H5::Attribute     isUnsignedLong = scalarSet.createAttribute(isUnsignedLongName, attrType, scalarSpace);
  bool              trueVal(true);
  isUnsignedLong.write(attrType, &trueVal);
  isUnsignedLong.close();
  auto tempVal = static_cast<int>(value);
  scalarSet.write(&tempVal, scalarType);
  scalarSet.close();
}

void
HDF5ImageIO::WriteScalar(const std::string & path, const long long value)
{
  hsize_t       numScalars(1);
  H5::DataSpace scalarSpace(1, &numScalars);
  H5::PredType  scalarType = H5::PredType::STD_I64LE;
  H5::PredType  attrType = H5::PredType::NATIVE_HBOOL;
  H5::DataSet   scalarSet = this->m_H5File->createDataSet(path, scalarType, scalarSpace);
  //
  // HDF5 can't distinguish
  // between long and long long datasets
  // in a disk file. So add an attribute
  // labeling this as a long long
  const std::string isLLongName("isLLong");
  H5::Attribute     isLLong = scalarSet.createAttribute(isLLongName, attrType, scalarSpace);
  bool              trueVal(true);
  isLLong.write(attrType, &trueVal);
  isLLong.close();
  scalarSet.write(&value, scalarType);
  scalarSet.close();
}

void
HDF5ImageIO::WriteScalar(const std::string & path, const unsigned long long value)
{
  hsize_t       numScalars(1);
  H5::DataSpace scalarSpace(1, &numScalars);
  H5::PredType  scalarType = H5::PredType::STD_U64LE;
  H5::PredType  attrType = H5::PredType::NATIVE_HBOOL;
  H5::DataSet   scalarSet = this->m_H5File->createDataSet(path, scalarType, scalarSpace);
  //
  // HDF5 can't distinguish
  // between unsigned long and unsigned long long
  // datasets in a disk file. So add an attribute
  // labeling this as an unsigned long long
  const std::string isULLongName("isULLong");
  H5::Attribute     isULLong = scalarSet.createAttribute(isULLongName, attrType, scalarSpace);
  bool              trueVal(true);
  isULLong.write(attrType, &trueVal);
  isULLong.close();
  scalarSet.write(&value, scalarType);
  scalarSet.close();
}

template <typename TScalar>
void
HDF5ImageIO::WriteScalar(const std::string & path, const TScalar & value)
{
  hsize_t       numScalars(1);
  H5::DataSpace scalarSpace(1, &numScalars);
  H5::PredType  scalarType = GetType<TScalar>();
  H5::DataSet   scalarSet = this->m_H5File->createDataSet(path, scalarType, scalarSpace);
  scalarSet.write(&value, scalarType);
  scalarSet.close();
}

template <typename TScalar>
TScalar
HDF5ImageIO::ReadScalar(const std::string & DataSetName)
{
  hsize_t       dim[1];
  H5::DataSet   scalarSet = this->m_H5File->openDataSet(DataSetName);
  H5::DataSpace Space = scalarSet.getSpace();

  if (Space.getSimpleExtentNdims() != 1)
  {
    itkExceptionMacro(<< "Wrong # of dims for TransformType "
                      << "in HDF5 File");
  }
  Space.getSimpleExtentDims(dim, nullptr);
  if (dim[0] != 1)
  {
    itkExceptionMacro(<< "Elements > 1 for scalar type "
                      << "in HDF5 File");
  }
  TScalar      scalar;
  H5::PredType scalarType = GetType<TScalar>();
  scalarSet.read(&scalar, scalarType);
  scalarSet.close();
  return scalar;
}


void
HDF5ImageIO::WriteString(const std::string & path, const std::string & value)
{
  hsize_t       numStrings(1);
  H5::DataSpace strSpace(1, &numStrings);
  H5::StrType   strType(H5::PredType::C_S1, H5T_VARIABLE);
  H5::DataSet   strSet = this->m_H5File->createDataSet(path, strType, strSpace);
  strSet.write(value, strType);
  strSet.close();
}

void
HDF5ImageIO::WriteString(const std::string & path, const char * s)
{
  std::string _s(s);
  WriteString(path, _s);
}

std::string
HDF5ImageIO::ReadString(const std::string & path)
{
  std::string   rval;
  hsize_t       numStrings(1);
  H5::DataSpace strSpace(1, &numStrings);
  H5::StrType   strType(H5::PredType::C_S1, H5T_VARIABLE);
  H5::DataSet   strSet = this->m_H5File->openDataSet(path);
  strSet.read(rval, strType, strSpace);
  strSet.close();
  return rval;
}

template <typename TScalar>
void
HDF5ImageIO::WriteVector(const std::string & path, const std::vector<TScalar> & vec)
{
  hsize_t       dim(vec.size());
  H5::DataSpace vecSpace(1, &dim);
  H5::PredType  vecType = GetType<TScalar>();
  H5::DataSet   vecSet = this->m_H5File->createDataSet(path, vecType, vecSpace);
  vecSet.write(vec.data(), vecType);
  vecSet.close();
}

template <typename TScalar>
std::vector<TScalar>
HDF5ImageIO::ReadVector(const std::string & DataSetName)
{
  std::vector<TScalar> vec;
  hsize_t              dim[1];
  H5::DataSet          vecSet = this->m_H5File->openDataSet(DataSetName);
  H5::DataSpace        Space = vecSet.getSpace();

  if (Space.getSimpleExtentNdims() != 1)
  {
    itkExceptionMacro(<< "Wrong # of dims for TransformType "
                      << "in HDF5 File");
  }
  Space.getSimpleExtentDims(dim, nullptr);
  vec.resize(dim[0]);
  H5::PredType vecType = GetType<TScalar>();
  vecSet.read(vec.data(), vecType);
  vecSet.close();
  return vec;
}

void
HDF5ImageIO::WriteDirections(const std::string & path, const std::vector<std::vector<double>> & dir)
{
  hsize_t dim[2];
  dim[1] = dir.size();
  dim[0] = dir[0].size();
  const auto   buf = make_unique_for_overwrite<double[]>(dim[0] * dim[1]);
  unsigned int k(0);
  for (unsigned int i = 0; i < dim[1]; ++i)
  {
    for (unsigned int j = 0; j < dim[0]; ++j)
    {
      buf[k] = dir[i][j];
      ++k;
    }
  }

  H5::DataSpace dirSpace(2, dim);
  H5::DataSet   dirSet = this->m_H5File->createDataSet(path, H5::PredType::NATIVE_DOUBLE, dirSpace);
  dirSet.write(buf.get(), H5::PredType::NATIVE_DOUBLE);
  dirSet.close();
}

std::vector<std::vector<double>>
HDF5ImageIO::ReadDirections(const std::string & path)
{
  std::vector<std::vector<double>> rval;
  H5::DataSet                      dirSet = this->m_H5File->openDataSet(path);
  H5::DataSpace                    dirSpace = dirSet.getSpace();
  hsize_t                          dim[2];
  if (dirSpace.getSimpleExtentNdims() != 2)
  {
    itkExceptionMacro(<< " Wrong # of dims for Image Directions "
                      << "in HDF5 File");
  }
  dirSpace.getSimpleExtentDims(dim, nullptr);
  rval.resize(dim[1]);
  for (unsigned int i = 0; i < dim[1]; ++i)
  {
    rval[i].resize(dim[0]);
  }
  H5::FloatType dirType = dirSet.getFloatType();
  if (dirType.getSize() == sizeof(double))
  {
    const auto buf = make_unique_for_overwrite<double[]>(dim[0] * dim[1]);
    dirSet.read(buf.get(), H5::PredType::NATIVE_DOUBLE);
    int k = 0;
    for (unsigned int i = 0; i < dim[1]; ++i)
    {
      for (unsigned int j = 0; j < dim[0]; ++j)
      {
        rval[i][j] = buf[k];
        ++k;
      }
    }
  }
  else
  {
    const auto buf = make_unique_for_overwrite<float[]>(dim[0] * dim[1]);
    dirSet.read(buf.get(), H5::PredType::NATIVE_FLOAT);
    int k = 0;
    for (unsigned int i = 0; i < dim[1]; ++i)
    {
      for (unsigned int j = 0; j < dim[0]; ++j)
      {
        rval[i][j] = buf[k];
        ++k;
      }
    }
  }
  dirSet.close();
  return rval;
}

template <typename TType>
void
HDF5ImageIO::StoreMetaData(MetaDataDictionary * metaDict,
                           const std::string &  HDFPath,
                           const std::string &  name,
                           unsigned long        numElements)
{
  if (numElements == 1)
  {
    auto val = this->ReadScalar<TType>(HDFPath);
    EncapsulateMetaData<TType>(*metaDict, name, val);
  }
  else
  {
    //
    // store as itk::Array -- consistent with how
    // metadatadictionary actually is used in ITK
    std::vector<TType> valVec = this->ReadVector<TType>(HDFPath);
    Array<TType>       val(static_cast<typename Array<TType>::SizeValueType>(valVec.size()));
    for (unsigned int i = 0; i < val.GetSize(); ++i)
    {
      val[i] = valVec[i];
    }
    EncapsulateMetaData<Array<TType>>(*metaDict, name, val);
  }
}

bool
HDF5ImageIO::CanWriteFile(const char * name)
{
  return this->HasSupportedWriteExtension(name);
}

// This method will only test if the header looks like an
// HDF5 Header.  Some code is redundant with ReadImageInformation
// a StateMachine could provide a better implementation
bool
HDF5ImageIO::CanReadFile(const char * FileNameToRead)
{
  // HDF5 is overly verbose in complaining that
  //     a file does not exist.
  if (!itksys::SystemTools::FileExists(FileNameToRead))
  {
    return false;
  }

  // HDF5 is so exception happy, we have to worry about
  // it throwing a wobbly here if the file doesn't exist
  // or has some other problem.
  bool rval = true;
  try
  {

    htri_t ishdf5 = H5Fis_hdf5(FileNameToRead);

    if (ishdf5 <= 0)
    {
      return false;
    }

    H5::H5File h5file(FileNameToRead, H5F_ACC_RDONLY);

#if (H5_VERS_MAJOR == 1) && (H5_VERS_MINOR < 10)
    // check the file has the ITK ImageGroup
    htri_t exists = H5Lexists(h5file.getId(), ImageGroup.c_str(), H5P_DEFAULT);
    if (exists <= 0)
#else
    if (!h5file.exists(ImageGroup))
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

void
HDF5ImageIO::ResetToInitialState()
{
  // close the H5 File
  {
    if (this->m_H5File != nullptr)
    {
      this->m_H5File->close();
      delete this->m_H5File;
      this->m_H5File = nullptr;
    }
  }

  // close the dataset
  {
    if (this->m_VoxelDataSet != nullptr)
    {
      m_VoxelDataSet->close();
      delete m_VoxelDataSet;
      this->m_VoxelDataSet = nullptr;
    }
  }
  // Need to reset m_ImageInformationWritten so that
  // the IO object is returned to
  // a state similar to constructing
  // anew.
  this->m_ImageInformationWritten = false;
}

void
HDF5ImageIO::ReadImageInformation()
{
  try
  {
    this->ResetToInitialState();
    this->m_H5File = new H5::H5File(this->GetFileName(), H5F_ACC_RDONLY);
    this->m_VoxelDataSet = new H5::DataSet();

    // not sure what to do with this initially
    // eventually it will be needed if the file versions change
    // std::string fileVersion = this->ReadString(ItkVersion);
    // std::string hdfVersion = this->ReadString(HDFVersion);

    std::string groupName(ImageGroup);
    // H5::Group imageGroup(this->m_H5File->openGroup(groupName));
    groupName += "/0";

    std::string DirectionsName(groupName);
    DirectionsName += Directions;

    std::vector<std::vector<double>> directions = this->ReadDirections(DirectionsName);

    auto numDims = static_cast<int>(directions.size());
    this->SetNumberOfDimensions(numDims);

    // H5::Group instanceGroup(this->m_H5File->openGroup(groupName));
    std::string OriginName(groupName);
    OriginName += Origin;
    this->m_Origin = this->ReadVector<double>(OriginName);

    for (int i = 0; i < numDims; ++i)
    {
      this->SetDirection(i, directions[i]);
    }

    std::string SpacingName(groupName);
    SpacingName += Spacing;
    std::vector<double> spacing = this->ReadVector<double>(SpacingName);
    for (int i = 0; i < numDims; ++i)
    {
      this->SetSpacing(i, spacing[i]);
    }

    std::string DimensionsName(groupName);
    DimensionsName += Dimensions;

    {
      std::vector<ImageIOBase::SizeValueType> Dims = this->ReadVector<ImageIOBase::SizeValueType>(DimensionsName);
      for (int i = 0; i < numDims; ++i)
      {
        this->SetDimensions(i, Dims[i]);
      }
    }

    std::string VoxelDataName(groupName);
    VoxelDataName += VoxelData;
    *(this->m_VoxelDataSet) = this->m_H5File->openDataSet(VoxelDataName);
    H5::DataSet   imageSet = *(this->m_VoxelDataSet);
    H5::DataSpace imageSpace = imageSet.getSpace();
    //
    // set the componentType
    H5::DataType imageVoxelType = imageSet.getDataType();
    this->m_ComponentType = PredTypeToComponentType(imageVoxelType);
    //
    // if this isn't a scalar image, deduce the # of components
    // by comparing the size of the Directions matrix with the
    // reported # of dimensions in the voxel dataset
    {
      hsize_t    nDims = imageSpace.getSimpleExtentNdims();
      const auto Dims = make_unique_for_overwrite<hsize_t[]>(nDims);
      imageSpace.getSimpleExtentDims(Dims.get());
      if (nDims > this->GetNumberOfDimensions())
      {
        this->SetNumberOfComponents(Dims[nDims - 1]);
      }
    }
    //
    // read out metadata
    MetaDataDictionary & metaDict = this->GetMetaDataDictionary();
    // Necessary to clear dict if ImageIO object is re-used
    metaDict.Clear();
    std::string MetaDataGroupName(groupName);
    MetaDataGroupName += MetaDataName;
    MetaDataGroupName += "/";
    H5::Group metaGroup(this->m_H5File->openGroup(MetaDataGroupName));
    for (unsigned int i = 0; i < metaGroup.getNumObjs(); ++i)
    {
      H5std_string name = metaGroup.getObjnameByIdx(i);

      std::string localMetaDataName(MetaDataGroupName);
      localMetaDataName += name;
      H5::DataSet   metaDataSet = this->m_H5File->openDataSet(localMetaDataName);
      H5::DataType  metaDataType = metaDataSet.getDataType();
      H5::DataSpace metaDataSpace = metaDataSet.getSpace();
      if (metaDataSpace.getSimpleExtentNdims() != 1)
      {
        // ignore > 1D metadata
        continue;
      }
      hsize_t metaDataDims[1];
      metaDataSpace.getSimpleExtentDims(metaDataDims);
      //
      // work around bool/int confusion on disk.
      if (metaDataType == H5::PredType::NATIVE_INT)
      {
        if (doesAttrExist(metaDataSet, "isBool"))
        {
          // itk::Array<bool> apparently can't
          // happen because vnl_vector<bool> isn't
          // instantiated
          bool val;
          auto tmpVal = this->ReadScalar<int>(localMetaDataName);
          val = tmpVal != 0;
          EncapsulateMetaData<bool>(metaDict, name, val);
        }
        else if (doesAttrExist(metaDataSet, "isLong"))
        {
          auto val = this->ReadScalar<long>(localMetaDataName);
          EncapsulateMetaData<long>(metaDict, name, val);
        }
        else if (doesAttrExist(metaDataSet, "isUnsignedLong"))
        {
          auto val = this->ReadScalar<unsigned long>(localMetaDataName);
          EncapsulateMetaData<unsigned long>(metaDict, name, val);
        }
        else
        {
          this->StoreMetaData<int>(&metaDict, localMetaDataName, name, metaDataDims[0]);
        }
      }
      else if (metaDataType == H5::PredType::NATIVE_CHAR)
      {
        this->StoreMetaData<char>(&metaDict, localMetaDataName, name, metaDataDims[0]);
      }
      else if (metaDataType == H5::PredType::NATIVE_UCHAR)
      {
        if (doesAttrExist(metaDataSet, "isBool"))
        {
          // itk::Array<bool> apparently can't
          // happen because vnl_vector<bool> isn't
          // instantiated
          bool val;
          auto tmpVal = this->ReadScalar<int>(localMetaDataName);
          val = tmpVal != 0;
          EncapsulateMetaData<bool>(metaDict, name, val);
        }
        else
        {
          this->StoreMetaData<unsigned char>(&metaDict, localMetaDataName, name, metaDataDims[0]);
        }
      }
      else if (metaDataType == H5::PredType::NATIVE_SHORT)
      {
        this->StoreMetaData<short>(&metaDict, localMetaDataName, name, metaDataDims[0]);
      }
      else if (metaDataType == H5::PredType::NATIVE_USHORT)
      {
        this->StoreMetaData<unsigned short>(&metaDict, localMetaDataName, name, metaDataDims[0]);
      }
      else if (metaDataType == H5::PredType::NATIVE_UINT)
      {
        if (doesAttrExist(metaDataSet, "isUnsignedLong"))
        {
          auto val = this->ReadScalar<unsigned long>(localMetaDataName);
          EncapsulateMetaData<unsigned long>(metaDict, name, val);
        }
        else
        {
          this->StoreMetaData<unsigned int>(&metaDict, localMetaDataName, name, metaDataDims[0]);
        }
      }
      else if (metaDataType == H5::PredType::NATIVE_LONG)
      {
        if (doesAttrExist(metaDataSet, "isLLong"))
        {
          auto val = this->ReadScalar<long long>(localMetaDataName);
          EncapsulateMetaData<long long>(metaDict, name, val);
        }
        else
        {
          this->StoreMetaData<long>(&metaDict, localMetaDataName, name, metaDataDims[0]);
        }
      }
      else if (metaDataType == H5::PredType::NATIVE_ULONG)
      {
        if (doesAttrExist(metaDataSet, "isULLong"))
        {
          auto val = this->ReadScalar<unsigned long long>(localMetaDataName);
          EncapsulateMetaData<unsigned long long>(metaDict, name, val);
        }
        else
        {
          this->StoreMetaData<unsigned long>(&metaDict, localMetaDataName, name, metaDataDims[0]);
        }
      }
      else if (metaDataType == H5::PredType::NATIVE_LLONG)
      {
        this->StoreMetaData<long long>(&metaDict, localMetaDataName, name, metaDataDims[0]);
      }
      else if (metaDataType == H5::PredType::NATIVE_ULLONG)
      {
        this->StoreMetaData<unsigned long long>(&metaDict, localMetaDataName, name, metaDataDims[0]);
      }
      else if (metaDataType == H5::PredType::NATIVE_FLOAT)
      {
        this->StoreMetaData<float>(&metaDict, localMetaDataName, name, metaDataDims[0]);
      }
      else if (metaDataType == H5::PredType::NATIVE_DOUBLE)
      {
        this->StoreMetaData<double>(&metaDict, localMetaDataName, name, metaDataDims[0]);
      }
      else
      {
        H5::StrType strType(H5::PredType::C_S1, H5T_VARIABLE);
        if (metaDataType == strType)
        {
          std::string val = this->ReadString(localMetaDataName);
          EncapsulateMetaData<std::string>(metaDict, name, val);
        }
      }
    }
    imageSet.close();
  }
  // catch failure caused by the H5File operations
  catch (const H5::AttributeIException & error)
  {
    itkExceptionMacro(<< error.getCDetailMsg());
  }
  catch (const H5::FileIException & error)
  {
    itkExceptionMacro(<< error.getCDetailMsg());
  }
  // catch failure caused by the DataSet operations
  catch (const H5::DataSetIException & error)
  {
    itkExceptionMacro(<< error.getCDetailMsg());
  }
  // catch failure caused by the DataSpace operations
  catch (const H5::DataSpaceIException & error)
  {
    itkExceptionMacro(<< error.getCDetailMsg());
  }
  // catch failure caused by the DataSpace operations
  catch (const H5::DataTypeIException & error)
  {
    itkExceptionMacro(<< error.getCDetailMsg());
  }
  catch (...)
  {
    itkExceptionMacro(<< "Unspecified error occured during ReadImageInformation " << this->GetFileName() << " with "
                      << this->GetNameOfClass());
  }
}

void
HDF5ImageIO::SetupStreaming(H5::DataSpace * imageSpace, H5::DataSpace * slabSpace)
{
  ImageIORegion            regionToRead = this->GetIORegion();
  ImageIORegion::SizeType  size = regionToRead.GetSize();
  ImageIORegion::IndexType start = regionToRead.GetIndex();
  //
  int numComponents = this->GetNumberOfComponents();

  const int HDFDim(this->GetNumberOfDimensions() + (numComponents > 1 ? 1 : 0));

  const auto offset = make_unique_for_overwrite<hsize_t[]>(HDFDim);
  const auto HDFSize = make_unique_for_overwrite<hsize_t[]>(HDFDim);
  const int  limit = regionToRead.GetImageDimension();
  //
  // fastest moving dimension is intra-voxel
  // index
  int i = 0;
  if (numComponents > 1)
  {
    offset[HDFDim - 1] = 0;
    HDFSize[HDFDim - 1] = numComponents;
    ++i;
  }

  for (int j = 0; j < limit && i < HDFDim; ++i, ++j)
  {
    offset[HDFDim - i - 1] = start[j];
    HDFSize[HDFDim - i - 1] = size[j];
  }

  while (i < HDFDim)
  {
    offset[HDFDim - i - 1] = 0;
    HDFSize[HDFDim - i - 1] = 1;
    ++i;
  }

  slabSpace->setExtentSimple(HDFDim, HDFSize.get());
  imageSpace->selectHyperslab(H5S_SELECT_SET, HDFSize.get(), offset.get());
}

void
HDF5ImageIO::Read(void * buffer)
{
  ImageIORegion            regionToRead = this->GetIORegion();
  ImageIORegion::SizeType  size = regionToRead.GetSize();
  ImageIORegion::IndexType start = regionToRead.GetIndex();

  H5::DataType  voxelType = this->m_VoxelDataSet->getDataType();
  H5::DataSpace imageSpace = this->m_VoxelDataSet->getSpace();

  H5::DataSpace dspace;
  this->SetupStreaming(&imageSpace, &dspace);
  this->m_VoxelDataSet->read(buffer, voxelType, dspace, imageSpace);
}

template <typename TType>
bool
HDF5ImageIO::WriteMeta(const std::string & name, MetaDataObjectBase * metaObjBase)
{
  auto * metaObj = dynamic_cast<MetaDataObject<TType> *>(metaObjBase);
  if (metaObj == nullptr)
  {
    return false;
  }
  TType val = metaObj->GetMetaDataObjectValue();
  this->WriteScalar(name, val);
  return true;
}

template <typename TType>
bool
HDF5ImageIO::WriteMetaArray(const std::string & name, MetaDataObjectBase * metaObjBase)
{
  using MetaDataArrayObject = MetaDataObject<Array<TType>>;
  auto * metaObj = dynamic_cast<MetaDataArrayObject *>(metaObjBase);
  if (metaObj == nullptr)
  {
    return false;
  }
  Array<TType>       val = metaObj->GetMetaDataObjectValue();
  std::vector<TType> vecVal(val.GetSize());
  for (unsigned int i = 0; i < val.size(); ++i)
  {
    vecVal[i] = val[i];
  }
  this->WriteVector(name, vecVal);
  return true;
}
/**
 * For HDF5 this does not write a file, it only fills in the
 * appropriate header information.
 */
void
HDF5ImageIO::WriteImageInformation()
{
  //
  // guard so that image information is only written once
  // if WriteImageInformation followed by Write
  if (this->m_ImageInformationWritten)
  {
    return;
  }

  try
  {
    this->ResetToInitialState();

    H5::FileAccPropList fapl;
#if (H5_VERS_MAJOR > 1) || (H5_VERS_MAJOR == 1) && (H5_VERS_MINOR > 10) || \
  (H5_VERS_MAJOR == 1) && (H5_VERS_MINOR == 10) && (H5_VERS_RELEASE >= 2)
    // File format which is backwards compatible with HDF5 version 1.8
    // Only HDF5 v1.10.2 has both setLibverBounds method and H5F_LIBVER_V18 constant
    fapl.setLibverBounds(H5F_LIBVER_V18, H5F_LIBVER_V18);
#elif (H5_VERS_MAJOR == 1) && (H5_VERS_MINOR == 10) && (H5_VERS_RELEASE < 2)
#  error The selected version of HDF5 library does not support setting backwards compatibility at run-time.\
  Please use a different version of HDF5, e.g. the one bundled with ITK (by setting ITK_USE_SYSTEM_HDF5 to OFF).
#endif
    this->m_H5File = new H5::H5File(this->GetFileName(), H5F_ACC_TRUNC, H5::FileCreatPropList::DEFAULT, fapl);
    this->m_VoxelDataSet = new H5::DataSet();

    this->WriteString(ItkVersion, Version::GetITKVersion());

    this->WriteString(HDFVersion, H5_VERS_INFO);
    std::string groupName(ImageGroup);
    this->m_H5File->createGroup(groupName);
    groupName += "/0";
    this->m_H5File->createGroup(groupName);
    std::string OriginName(groupName);
    OriginName += Origin;
    this->WriteVector(OriginName, this->m_Origin);
    std::string DirectionsName(groupName);
    DirectionsName += Directions;
    this->WriteDirections(DirectionsName, this->m_Direction);
    std::string SpacingName(groupName);
    SpacingName += Spacing;
    this->WriteVector(SpacingName, this->m_Spacing);

    std::string DimensionsName(groupName);
    DimensionsName += Dimensions;
    this->WriteVector(DimensionsName, this->m_Dimensions);

    std::string VoxelTypeName(groupName);
    VoxelTypeName += VoxelType;
    std::string typeVal(ComponentToString(this->GetComponentType()));
    this->WriteString(VoxelTypeName, typeVal);

    int numComponents = this->GetNumberOfComponents();
    int numDims = this->GetNumberOfDimensions();
    // HDF5 dimensions listed slowest moving first, ITK are fastest
    // moving first.
    auto dims = make_unique_for_overwrite<hsize_t[]>(numDims + (numComponents == 1 ? 0 : 1));

    for (int i(0), j(numDims - 1); i < numDims; i++, j--)
    {
      dims[j] = this->m_Dimensions[i];
    }
    if (numComponents > 1)
    {
      dims[numDims] = numComponents;
      ++numDims;
    }
    H5::DataSpace imageSpace(numDims, dims.get());
    H5::PredType  dataType = ComponentToPredType(this->GetComponentType());

    // set up properties for chunked, compressed writes.
    // in this case, set the chunk size to be the N-1 dimension
    // region
    H5::DSetCreatPropList plist;

    // we have implicit compression enabled here?
    plist.setDeflate(this->GetCompressionLevel());

    dims[0] = 1;
    plist.setChunk(numDims, dims.get());
    dims.reset();

    std::string VoxelDataName(ImageGroup);
    VoxelDataName += "/0";
    VoxelDataName += VoxelData;
    *(this->m_VoxelDataSet) = this->m_H5File->createDataSet(VoxelDataName, dataType, imageSpace, plist);
    std::string MetaDataGroupName(groupName);
    MetaDataGroupName += MetaDataName;
    this->m_H5File->createGroup(MetaDataGroupName);
    //
    // MetaData.
    MetaDataDictionary & metaDict = this->GetMetaDataDictionary();
    auto                 it = metaDict.Begin(), end = metaDict.End();
    for (; it != end; ++it)
    {
      MetaDataObjectBase * metaObj = it->second.GetPointer();
      std::string          objName(MetaDataGroupName);
      objName += "/";
      objName += it->first;
      // scalars
      if (this->WriteMeta<bool>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMeta<char>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMeta<unsigned char>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMeta<short>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMeta<unsigned short>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMeta<int>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMeta<unsigned int>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMeta<long>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMeta<unsigned long>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMeta<long long>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMeta<unsigned long long>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMeta<float>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMeta<double>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMetaArray<char>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMetaArray<unsigned char>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMetaArray<short>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMetaArray<unsigned short>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMetaArray<int>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMetaArray<unsigned int>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMetaArray<long>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMetaArray<unsigned long>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMetaArray<float>(objName, metaObj))
      {
        continue;
      }
      if (this->WriteMetaArray<double>(objName, metaObj))
      {
        continue;
      }
      //
      // C String Arrays
      {
        auto * cstringObj = dynamic_cast<MetaDataObject<char *> *>(metaObj);
        auto * constCstringObj = dynamic_cast<MetaDataObject<const char *> *>(metaObj);
        if (cstringObj != nullptr || constCstringObj != nullptr)
        {
          const char * val;
          if (cstringObj != nullptr)
          {
            val = cstringObj->GetMetaDataObjectValue();
          }
          else
          {
            val = constCstringObj->GetMetaDataObjectValue();
          }
          this->WriteString(objName, val);
          continue;
        }
      }
      //
      // std::string
      {
        auto * stdStringObj = dynamic_cast<MetaDataObject<std::string> *>(metaObj);
        if (stdStringObj != nullptr)
        {
          std::string val = stdStringObj->GetMetaDataObjectValue();
          this->WriteString(objName, val);
          continue;
        }
      }
    }
  }
  // catch failure caused by the H5File operations
  catch (const H5::FileIException & error)
  {
    itkExceptionMacro(<< error.getCDetailMsg());
  }
  // catch failure caused by the DataSet operations
  catch (const H5::DataSetIException & error)
  {
    itkExceptionMacro(<< error.getCDetailMsg());
  }
  // catch failure caused by the DataSpace operations
  catch (const H5::DataSpaceIException & error)
  {
    itkExceptionMacro(<< error.getCDetailMsg());
  }
  // catch failure caused by the DataSpace operations
  catch (const H5::DataTypeIException & error)
  {
    itkExceptionMacro(<< error.getCDetailMsg());
  }
  catch (...)
  {
    itkExceptionMacro(<< "Unspecified error occured during WriteImageInformation: " << this->GetFileName() << " with "
                      << this->GetNameOfClass());
  }
  //
  // only write image information once.
  this->m_ImageInformationWritten = true;
}

/**
 * Write the image Information before writing data
 */
void
HDF5ImageIO::Write(const void * buffer)
{
  this->WriteImageInformation();
  try
  {
    int numComponents = this->GetNumberOfComponents();
    int numDims = this->GetNumberOfDimensions();
    // HDF5 dimensions listed slowest moving first, ITK are fastest
    // moving first.
    const auto dims = make_unique_for_overwrite<hsize_t[]>(numDims + (numComponents == 1 ? 0 : 1));

    for (int i(0), j(numDims - 1); i < numDims; i++, j--)
    {
      dims[j] = this->m_Dimensions[i];
    }
    if (numComponents > 1)
    {
      dims[numDims] = numComponents;
      ++numDims;
    }
    H5::DataSpace imageSpace(numDims, dims.get());
    H5::PredType  dataType = ComponentToPredType(this->GetComponentType());
    H5::DataSpace dspace;
    this->SetupStreaming(&imageSpace, &dspace);
    this->m_VoxelDataSet->write(buffer, dataType, dspace, imageSpace);
  }
  // catch failure caused by the H5File operations
  catch (const H5::FileIException & error)
  {
    itkExceptionMacro(<< error.getCDetailMsg());
  }
  // catch failure caused by the DataSet operations
  catch (const H5::DataSetIException & error)
  {
    itkExceptionMacro(<< error.getCDetailMsg());
  }
  // catch failure caused by the DataSpace operations
  catch (const H5::DataSpaceIException & error)
  {
    itkExceptionMacro(<< error.getCDetailMsg());
  }
  // catch failure caused by the DataSpace operations
  catch (const H5::DataTypeIException & error)
  {
    itkExceptionMacro(<< error.getCDetailMsg());
  }
  catch (...)
  {
    itkExceptionMacro(<< "Unspecified error occured during Write: " << this->GetFileName() << " with "
                      << this->GetNameOfClass());
  }
  // TODO: including this line allows the IO object to be re-used multiple times for writing, but
  // but causes the streaming tests for HDF5ImageIO to fail.
  // this->ResetToInitialState();
}

//
// GetHeaderSize -- return 0
ImageIOBase::SizeType
HDF5ImageIO::GetHeaderSize() const
{
  return 0;
}

} // end namespace itk
