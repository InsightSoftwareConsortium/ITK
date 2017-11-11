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
#include "itkVersion.h"
#include "itkHDF5ImageIO.h"
#include "itkMetaDataObject.h"
#include "itkArray.h"
#include "itksys/SystemTools.hxx"
#include "itk_H5Cpp.h"

namespace itk
{

HDF5ImageIO::HDF5ImageIO() : m_H5File(ITK_NULLPTR),
                             m_VoxelDataSet(ITK_NULLPTR),
                             m_ImageInformationWritten(false)
{
}

HDF5ImageIO::~HDF5ImageIO()
{
  this->CloseDataSet();
  this->CloseH5File();
}

void
HDF5ImageIO
::PrintSelf(std::ostream & os, Indent indent) const
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
H5::PredType GetType()
{
  itkGenericExceptionMacro(<< "Type not handled "
                           << "in HDF5 File: "
                           << typeid(TScalar).name());

}
#define GetH5TypeSpecialize(CXXType,H5Type) \
  template <>                             \
  H5::PredType GetType<CXXType>()         \
  {                                       \
    return H5Type;                        \
  }

GetH5TypeSpecialize(float,                  H5::PredType::NATIVE_FLOAT)
GetH5TypeSpecialize(double,                 H5::PredType::NATIVE_DOUBLE)

GetH5TypeSpecialize(char,                   H5::PredType::NATIVE_CHAR)
GetH5TypeSpecialize(unsigned char,          H5::PredType::NATIVE_UCHAR)

GetH5TypeSpecialize(short int,              H5::PredType::NATIVE_SHORT)
GetH5TypeSpecialize(short unsigned int,     H5::PredType::NATIVE_USHORT)

GetH5TypeSpecialize(int,                    H5::PredType::NATIVE_INT)
GetH5TypeSpecialize(unsigned int,           H5::PredType::NATIVE_UINT)

GetH5TypeSpecialize(long int,               H5::PredType::NATIVE_LONG)
GetH5TypeSpecialize(long unsigned int,      H5::PredType::NATIVE_ULONG)

GetH5TypeSpecialize(long long int,          H5::PredType::NATIVE_LLONG)
GetH5TypeSpecialize(unsigned long long int, H5::PredType::NATIVE_ULLONG)

/* The following types are not implmented.  This comment serves
 * to indicate that the full complement of possible H5::PredType
 * types are not implemented int the ITK IO reader/writer
 * GetH5TypeSpecialize(bool,              H5::PredType::NATIVE_HBOOL)
*/

#undef GetH5TypeSpecialize

inline
ImageIOBase::IOComponentType
PredTypeToComponentType(H5::DataType &type)
{
  if(type ==  H5::PredType::NATIVE_UCHAR)
    {
    return ImageIOBase::UCHAR;
    }
  else if(type ==  H5::PredType::NATIVE_CHAR)
    {
    return ImageIOBase::CHAR;
    }
  else if(type ==  H5::PredType::NATIVE_USHORT)
    {
    return ImageIOBase::USHORT;
    }
  else if(type ==  H5::PredType::NATIVE_SHORT)
    {
    return ImageIOBase::SHORT;
    }
  else if(type ==  H5::PredType::NATIVE_UINT)
    {
    return ImageIOBase::UINT;
    }
  else if(type ==  H5::PredType::NATIVE_INT)
    {
    return ImageIOBase::INT;
    }
  else if(type ==  H5::PredType::NATIVE_ULONG)
    {
    return ImageIOBase::ULONG;
    }
  else if(type ==  H5::PredType::NATIVE_LONG)
    {
    return ImageIOBase::LONG;
    }
  else if(type ==  H5::PredType::NATIVE_LLONG)
    {
    return ImageIOBase::LONGLONG;
    }
  else if(type ==  H5::PredType::NATIVE_ULLONG)
    {
    return ImageIOBase::ULONGLONG;
    }
  else if(type ==  H5::PredType::NATIVE_FLOAT)
    {
    return ImageIOBase::FLOAT;
    }
  else if(type ==  H5::PredType::NATIVE_DOUBLE)
    {
    return ImageIOBase::DOUBLE;
    }
  itkGenericExceptionMacro(<< "unsupported HDF5 data type with id " << type.getId());
}

H5::PredType
ComponentToPredType(ImageIOBase::IOComponentType cType)
{
  switch ( cType )
    {
    case ImageIOBase::UCHAR:
      return H5::PredType::NATIVE_UCHAR;
    case ImageIOBase::CHAR:
      return H5::PredType::NATIVE_CHAR;
    case ImageIOBase::USHORT:
      return H5::PredType::NATIVE_USHORT;
    case ImageIOBase::SHORT:
      return H5::PredType::NATIVE_SHORT;
    case ImageIOBase::UINT:
      return H5::PredType::NATIVE_UINT;
    case ImageIOBase::INT:
      return H5::PredType::NATIVE_INT;
    case ImageIOBase::ULONG:
      return H5::PredType::NATIVE_ULONG;
    case ImageIOBase::LONG:
      return H5::PredType::NATIVE_LONG;
    case ImageIOBase::ULONGLONG:
      return H5::PredType::NATIVE_ULLONG;
    case ImageIOBase::LONGLONG:
      return H5::PredType::NATIVE_LLONG;
    case ImageIOBase::FLOAT:
      return H5::PredType::NATIVE_FLOAT;
    case ImageIOBase::DOUBLE:
      return H5::PredType::NATIVE_DOUBLE;
    case ImageIOBase::UNKNOWNCOMPONENTTYPE:
      itkGenericExceptionMacro(<< "unsupported IOComponentType"
                               << cType);
    }

    itkGenericExceptionMacro(<< "unsupported IOComponentType"
                             << cType);
}

std::string
ComponentToString(ImageIOBase::IOComponentType cType)
{
  std::string rval;
  switch ( cType )
    {
    case ImageIOBase::UCHAR:
      rval = "UCHAR";
      break;
    case ImageIOBase::CHAR:
      rval = "CHAR";
      break;
    case ImageIOBase::USHORT:
      rval = "USHORT";
      break;
    case ImageIOBase::SHORT:
      rval = "SHORT";
      break;
    case ImageIOBase::UINT:
      rval = "UINT";
      break;
    case ImageIOBase::INT:
      rval = "INT";
      break;
    case ImageIOBase::ULONG:
      rval = "ULONG";
      break;
    case ImageIOBase::LONG:
      rval = "LONG";
      break;
    case ImageIOBase::ULONGLONG:
      rval = "ULONGLONG";
      break;
    case ImageIOBase::LONGLONG:
      rval = "LONGLONG";
      break;
    case ImageIOBase::FLOAT:
      rval = "FLOAT";
      break;
    case ImageIOBase::DOUBLE:
      rval = "DOUBLE";
      break;
    default:
      itkGenericExceptionMacro(<< "unsupported IOComponentType"
                               << cType);
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
static bool doesAttrExist(const H5::H5Object &object, const char * const name )
{
  return( H5Aexists(object.getId(), name) > 0 ? true : false );
}

}

void
HDF5ImageIO
::WriteScalar(const std::string &path,
              const bool &value)
{
  hsize_t numScalars(1);
  H5::DataSpace scalarSpace(1,&numScalars);
  H5::PredType scalarType =
    H5::PredType::NATIVE_HBOOL;
  H5::DataSet scalarSet =
    this->m_H5File->createDataSet(path,
                          scalarType,
                          scalarSpace);
  //
  // HDF5 can't distinguish
  // between bool and int datasets
  // in a disk file. So add an attribute
  // labeling this as a bool
  const std::string isBoolName("isBool");
  H5::Attribute isBool =
    scalarSet.createAttribute(isBoolName,
                               scalarType,
                               scalarSpace);
  bool trueVal(true);
  isBool.write(scalarType,&trueVal);
  isBool.close();
  int tempVal = static_cast<int>(value);
  scalarSet.write(&tempVal,scalarType);
  scalarSet.close();
}

void
HDF5ImageIO
::WriteScalar(const std::string &path,
              const long &value)
{
  hsize_t numScalars(1);
  H5::DataSpace scalarSpace(1,&numScalars);
  H5::PredType scalarType =
    H5::PredType::NATIVE_INT;
  H5::PredType attrType =
    H5::PredType::NATIVE_HBOOL;
  H5::DataSet scalarSet =
    this->m_H5File->createDataSet(path,
                          scalarType,
                          scalarSpace);
  //
  // HDF5 can't distinguish
  // between long and int datasets
  // in a disk file. So add an attribute
  // labeling this as a long.
  const std::string isLongName("isLong");
  H5::Attribute isLong =
    scalarSet.createAttribute(isLongName,
                               attrType,
                               scalarSpace);
  bool trueVal(true);
  isLong.write(attrType,&trueVal);
  isLong.close();
  int tempVal = static_cast<int>(value);
  scalarSet.write(&tempVal,scalarType);
  scalarSet.close();
}

void
HDF5ImageIO
::WriteScalar(const std::string &path,
              const unsigned long &value)
{
  hsize_t numScalars(1);
  H5::DataSpace scalarSpace(1,&numScalars);
  H5::PredType scalarType =
    H5::PredType::NATIVE_UINT;
  H5::PredType attrType =
    H5::PredType::NATIVE_HBOOL;
  H5::DataSet scalarSet =
    this->m_H5File->createDataSet(path,
                          scalarType,
                          scalarSpace);
  //
  // HDF5 can't distinguish
  // between unsigned long and unsigned int datasets
  // in a disk file. So add an attribute
  // labeling this as an unsigned long.
  const std::string isUnsignedLongName("isUnsignedLong");
  H5::Attribute isUnsignedLong =
    scalarSet.createAttribute(isUnsignedLongName,
                               attrType,
                               scalarSpace);
  bool trueVal(true);
  isUnsignedLong.write(attrType,&trueVal);
  isUnsignedLong.close();
  int tempVal = static_cast<int>(value);
  scalarSet.write(&tempVal,scalarType);
  scalarSet.close();
}

void
HDF5ImageIO
::WriteScalar(const std::string &path,
              const long long &value)
{
  hsize_t numScalars(1);
  H5::DataSpace scalarSpace(1,&numScalars);
  H5::PredType scalarType =
    H5::PredType::STD_I64LE;
  H5::PredType attrType =
    H5::PredType::NATIVE_HBOOL;
  H5::DataSet scalarSet =
    this->m_H5File->createDataSet(path,
                          scalarType,
                          scalarSpace);
  //
  // HDF5 can't distinguish
  // between long and long long datasets
  // in a disk file. So add an attribute
  // labeling this as a long long
  const std::string isLLongName("isLLong");
  H5::Attribute isLLong =
    scalarSet.createAttribute(isLLongName,
                               attrType,
                               scalarSpace);
  bool trueVal(true);
  isLLong.write(attrType,&trueVal);
  isLLong.close();
  scalarSet.write(&value,scalarType);
  scalarSet.close();
}

void
HDF5ImageIO
::WriteScalar(const std::string &path,
              const unsigned long long &value)
{
    hsize_t numScalars(1);
    H5::DataSpace scalarSpace(1,&numScalars);
    H5::PredType scalarType =
      H5::PredType::STD_U64LE;
    H5::PredType attrType =
      H5::PredType::NATIVE_HBOOL;
    H5::DataSet scalarSet =
      this->m_H5File->createDataSet(path,
                            scalarType,
                            scalarSpace);
    //
    // HDF5 can't distinguish
    // between unsigned long and unsigned long long
    // datasets in a disk file. So add an attribute
    // labeling this as a unsigned long long
    const std::string isULLongName("isULLong");
    H5::Attribute isULLong =
      scalarSet.createAttribute(isULLongName,
                                 attrType,
                                 scalarSpace);
    bool trueVal(true);
    isULLong.write(attrType,&trueVal);
    isULLong.close();
    scalarSet.write(&value,scalarType);
    scalarSet.close();
}

template <typename TScalar>
void
HDF5ImageIO
::WriteScalar(const std::string &path,
              const TScalar &value)
{
  hsize_t numScalars(1);
  H5::DataSpace scalarSpace(1,&numScalars);
  H5::PredType scalarType =
    GetType<TScalar>();
  H5::DataSet scalarSet =
    this->m_H5File->createDataSet(path,
                          scalarType,
                          scalarSpace);
  scalarSet.write(&value,scalarType);
  scalarSet.close();
}

template <typename TScalar>
TScalar
HDF5ImageIO
::ReadScalar(const std::string &DataSetName)
{
  hsize_t dim[1];
  H5::DataSet scalarSet = this->m_H5File->openDataSet(DataSetName);
  H5::DataSpace Space = scalarSet.getSpace();

  if(Space.getSimpleExtentNdims() != 1)
    {
    itkExceptionMacro(<< "Wrong # of dims for TransformType "
                      << "in HDF5 File");
    }
  Space.getSimpleExtentDims(dim,ITK_NULLPTR);
  if(dim[0] != 1)
    {
    itkExceptionMacro(<< "Elements > 1 for scalar type "
                      << "in HDF5 File");
    }
  TScalar scalar;
  H5::PredType scalarType = GetType<TScalar>();
  scalarSet.read(&scalar,scalarType);
  scalarSet.close();
  return scalar;
}


void
HDF5ImageIO
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
HDF5ImageIO
::WriteString(const std::string &path,
                 const char *s)
{
  std::string _s(s);
  WriteString(path,_s);
}

std::string
HDF5ImageIO
::ReadString(const std::string &path)
{
  std::string rval;
  hsize_t numStrings(1);
  H5::DataSpace strSpace(1,&numStrings);
  H5::StrType strType(H5::PredType::C_S1,H5T_VARIABLE);
  H5::DataSet strSet = this->m_H5File->openDataSet(path);
  strSet.read(rval,strType,strSpace);
  strSet.close();
  return rval;
}

template <typename TScalar>
void
HDF5ImageIO
::WriteVector(const std::string &path,
              const std::vector<TScalar> &vec)
{
  hsize_t dim(vec.size());
  TScalar *buf(new TScalar[dim]);
  for(unsigned i = 0; i < dim; i++)
    {
    buf[i] = vec[i];
    }

  H5::DataSpace vecSpace(1,&dim);
  H5::PredType vecType =
    GetType<TScalar>();
  H5::DataSet vecSet = this->m_H5File->createDataSet(path, vecType, vecSpace);
  vecSet.write(buf,vecType);
  vecSet.close();
  delete[] buf;

}

template <typename TScalar>
std::vector<TScalar>
HDF5ImageIO
::ReadVector(const std::string &DataSetName)
{
  std::vector<TScalar> vec;
  hsize_t dim[1];
  H5::DataSet vecSet = this->m_H5File->openDataSet(DataSetName);
  H5::DataSpace Space = vecSet.getSpace();

  if(Space.getSimpleExtentNdims() != 1)
    {
    itkExceptionMacro(<< "Wrong # of dims for TransformType "
                      << "in HDF5 File");
    }
  Space.getSimpleExtentDims(dim,ITK_NULLPTR);
  vec.resize(dim[0]);
  TScalar *buf = new TScalar[dim[0]];
  H5::PredType vecType =
    GetType<TScalar>();
  vecSet.read(buf,vecType);
  for(unsigned i = 0; i < dim[0]; i++)
    {
    vec[i] = buf[i];
    }
  delete[] buf;
  vecSet.close();
  return vec;
}

void
HDF5ImageIO
::WriteDirections(const std::string &path,
                const std::vector<std::vector<double> > &dir)
{
  hsize_t dim[2];
  dim[1] = dir.size();
  dim[0] = dir[0].size();
  double *buf(new double[dim[0] * dim[1]]);
  unsigned k(0);
  for(unsigned i = 0; i < dim[1]; i++)
    {
    for(unsigned j = 0; j < dim[0]; j++)
      {
      buf[k] = dir[i][j];
      k++;
      }
    }

  H5::DataSpace dirSpace(2,dim);
  H5::DataSet dirSet = this->m_H5File->createDataSet(path,
                                                     H5::PredType::NATIVE_DOUBLE,
                                                     dirSpace);
  dirSet.write(buf,H5::PredType::NATIVE_DOUBLE);
  dirSet.close();
  delete[] buf;
}

std::vector<std::vector<double> >
HDF5ImageIO
::ReadDirections(const std::string &path)
{
  std::vector<std::vector<double> > rval;
  H5::DataSet dirSet = this->m_H5File->openDataSet(path);
  H5::DataSpace dirSpace = dirSet.getSpace();
  hsize_t dim[2];
  if(dirSpace.getSimpleExtentNdims() != 2)
    {
    itkExceptionMacro(<< " Wrong # of dims for Image Directions "
                      << "in HDF5 File");
    }
  dirSpace.getSimpleExtentDims(dim,ITK_NULLPTR);
  rval.resize(dim[1]);
  for(unsigned i = 0; i < dim[1]; i++)
    {
    rval[i].resize(dim[0]);
    }
  H5::FloatType dirType = dirSet.getFloatType();
  if(dirType.getSize() == sizeof(double))
    {
    double *buf = new double[dim[0] * dim[1]];
    dirSet.read(buf,H5::PredType::NATIVE_DOUBLE);
    int k = 0;
    for(unsigned i = 0; i < dim[1]; i++)
      {
      for(unsigned j = 0; j < dim[0]; j++)
        {
        rval[i][j] = buf[k];
        k++;
        }
      }
    delete[] buf;
    }
  else
    {
    float *buf = new float[dim[0] * dim[1]];
    dirSet.read(buf,H5::PredType::NATIVE_FLOAT);
    int k = 0;
    for(unsigned i = 0; i < dim[1]; i++)
      {
      for(unsigned j = 0; j < dim[0]; j++)
        {
        rval[i][j] = buf[k];
        k++;
        }
      }
    delete[] buf;
    }
  dirSet.close();
  return rval;
}

template <typename TType>
void
HDF5ImageIO
::StoreMetaData(MetaDataDictionary *metaDict,
                const std::string &HDFPath,
                const std::string &name,
                unsigned long numElements)
{
  if(numElements == 1)
    {
    TType val = this->ReadScalar<TType>(HDFPath);
    EncapsulateMetaData<TType>(*metaDict,name,val);
    }
  else
    {
    //
    // store as itk::Array -- consistent with how
    // metadatadictionary actually is used in ITK
    std::vector<TType> valVec = this->ReadVector<TType>(HDFPath);
    Array<TType> val(static_cast< typename Array<TType>::SizeValueType >( valVec.size()));
    for(unsigned int i = 0; i < val.GetSize(); i++)
      {
      val[i] = valVec[i];
      }
    EncapsulateMetaData<Array<TType> >(*metaDict,name,val);
    }
}

bool
HDF5ImageIO
::CanWriteFile(const char *FileNameToWrite)
{
  const char *extensions[] =
    {
      ".hdf",".h4",".hdf4",".h5",".hdf5",".he4",".he5",".hd5",ITK_NULLPTR,
    };
  std::string ext(itksys::SystemTools::GetFilenameLastExtension(FileNameToWrite));
  for(unsigned i = 0; extensions[i] != ITK_NULLPTR; i++)
    {
    if(ext == extensions[i])
      {
      return true;
      }
    }
  return false;
}

// This method will only test if the header looks like an
// HDF5 Header.  Some code is redundant with ReadImageInformation
// a StateMachine could provide a better implementation
bool
HDF5ImageIO
::CanReadFile(const char *FileNameToRead)
{
  //HDF5 is overly verbose in complaining that
  //     a file does not exist.
  if ( !itksys::SystemTools::FileExists(FileNameToRead) )
    {
    return false;
    }

  //Do not read if it is a MINC file.
  std::string filename(FileNameToRead);
  std::string::size_type mncPos = filename.rfind(".mnc");
  if ( (mncPos != std::string::npos)
       && (mncPos == filename.length() - 4) )
    {
    return false;
    }

  mncPos = filename.rfind(".MNC");
  if ( (mncPos != std::string::npos)
       && (mncPos == filename.length() - 4) )
    {
    return false;
    }

  mncPos = filename.rfind(".mnc2");
  if ( (mncPos != std::string::npos)
       && (mncPos == filename.length() - 5) )
    {
    return false;
    }

  mncPos = filename.rfind(".MNC2");
  if ( (mncPos != std::string::npos)
       && (mncPos == filename.length() - 5) )
    {
    return false;
    }

  // call standard method to determine HDF-ness
  bool rval;
  // HDF5 is so exception happy, we have to worry about
  // it throwing a wobbly here if the file doesn't exist
  // or has some other problem.
  try
    {
    rval = H5::H5File::isHdf5(FileNameToRead);
    }
  catch(...)
    {
    rval = false;
    }
  return rval;
}


void
HDF5ImageIO
::CloseH5File()
{
  if(this->m_H5File != ITK_NULLPTR)
    {
    this->m_H5File->close();
    delete this->m_H5File;
    this->m_H5File = ITK_NULLPTR;
    }
}

void
HDF5ImageIO
::CloseDataSet()
{
  if(this->m_VoxelDataSet != ITK_NULLPTR)
    {
    m_VoxelDataSet->close();
    delete m_VoxelDataSet;
    this->m_VoxelDataSet = ITK_NULLPTR;
    }
}

void
HDF5ImageIO
::ReadImageInformation()
{
  try
    {
    this->CloseH5File();
    this->CloseDataSet();
    this->m_H5File = new H5::H5File(this->GetFileName(),
                                    H5F_ACC_RDONLY);
    this->m_VoxelDataSet = new H5::DataSet();

    // not sure what to do with this initially
    //eventually it will be needed if the file versions change
    //std::string fileVersion = this->ReadString(ItkVersion);
    //std::string hdfVersion = this->ReadString(HDFVersion);

    std::string groupName(ImageGroup);
    //H5::Group imageGroup(this->m_H5File->openGroup(groupName));
    groupName += "/0";

    std::string DirectionsName(groupName);
    DirectionsName += Directions;

    std::vector<std::vector<double> > directions =
      this->ReadDirections(DirectionsName);

    int numDims = static_cast<int>( directions.size() );
    this->SetNumberOfDimensions(numDims);

    //H5::Group instanceGroup(this->m_H5File->openGroup(groupName));
    std::string OriginName(groupName);
    OriginName += Origin;
    this->m_Origin = this->ReadVector<double>(OriginName);

    for(int i = 0; i < numDims; i++)
      {
      this->SetDirection(i,directions[i]);
      }

    std::string SpacingName(groupName);
    SpacingName += Spacing;
    std::vector<double> spacing = this->ReadVector<double>(SpacingName);
    for(int i = 0; i < numDims; i++)
      {
      this->SetSpacing(i,spacing[i]);
      }

    std::string DimensionsName(groupName);
    DimensionsName += Dimensions;

    {
    std::vector<ImageIOBase::SizeValueType> Dims =
      this->ReadVector<ImageIOBase::SizeValueType>(DimensionsName);
    for(int i = 0; i < numDims; i++)
      {
      this->SetDimensions(i,Dims[i]);
      }
    }

    std::string VoxelDataName(groupName);
    VoxelDataName += VoxelData;
    *(this->m_VoxelDataSet) = this->m_H5File->openDataSet(VoxelDataName);
    H5::DataSet imageSet = *(this->m_VoxelDataSet);
    H5::DataSpace imageSpace = imageSet.getSpace();
    //
    // set the componentType
    H5::DataType imageVoxelType = imageSet.getDataType();
    this->m_ComponentType = PredTypeToComponentType(imageVoxelType);
    //
    // if this isn't a scalar image, deduce the # of components
    // by comparing the size of the Directions matrix with the
    // reported # of dimensions in the voxel dataset
    hsize_t nDims = imageSpace.getSimpleExtentNdims();
    hsize_t *Dims = new hsize_t[nDims];
    imageSpace.getSimpleExtentDims(Dims);
    if(nDims > this->GetNumberOfDimensions())
      {
      this->SetNumberOfComponents(Dims[nDims - 1]);
      }
    delete[] Dims;
    //
    // read out metadata
    MetaDataDictionary & metaDict = this->GetMetaDataDictionary();
    // Necessary to clear dict if ImageIO object is re-used
    metaDict.Clear();
    std::string MetaDataGroupName(groupName);
    MetaDataGroupName += MetaDataName;
    MetaDataGroupName += "/";
    H5::Group metaGroup(this->m_H5File->openGroup(MetaDataGroupName));
    for(unsigned int i = 0; i < metaGroup.getNumObjs(); i++)
      {
      H5std_string name = metaGroup.getObjnameByIdx(i);

      std::string localMetaDataName(MetaDataGroupName);
      localMetaDataName += name;
      H5::DataSet metaDataSet = this->m_H5File->openDataSet(localMetaDataName);
      H5::DataType metaDataType = metaDataSet.getDataType();
      H5::DataSpace metaDataSpace = metaDataSet.getSpace();
      if(metaDataSpace.getSimpleExtentNdims() != 1)
        {
        // ignore > 1D metadata
        continue;
        }
      hsize_t metaDataDims[1];
      metaDataSpace.getSimpleExtentDims(metaDataDims);
      //
      // work around bool/int confusion on disk.
      if(metaDataType == H5::PredType::NATIVE_INT)
        {
        if(doesAttrExist(metaDataSet,"isBool"))
          {
          // itk::Array<bool> apparently can't
          // happen because vnl_vector<bool> isn't
          // instantiated
          bool val;
          int tmpVal = this->ReadScalar<int>(localMetaDataName);
          val = tmpVal != 0;
          EncapsulateMetaData<bool>(metaDict,name,val);
          }
        else if(doesAttrExist(metaDataSet,"isLong"))
          {
          long val = this->ReadScalar<long>(localMetaDataName);
          EncapsulateMetaData<long>(metaDict,name,val);
          }
        else if(doesAttrExist(metaDataSet,"isUnsignedLong"))
          {
          unsigned long val = this->ReadScalar<unsigned long>(localMetaDataName);
          EncapsulateMetaData<unsigned long>(metaDict,name,val);
          }
        else
          {
          this->StoreMetaData<int>(&metaDict,
                              localMetaDataName,
                              name,
                              metaDataDims[0]);
          }
        }
      else if(metaDataType == H5::PredType::NATIVE_CHAR)
        {
        this->StoreMetaData<char>(&metaDict,
                                  localMetaDataName,
                                  name,
                                  metaDataDims[0]);
        }
      else if(metaDataType == H5::PredType::NATIVE_UCHAR)
        {
        if(doesAttrExist(metaDataSet,"isBool"))
          {
          // itk::Array<bool> apparently can't
          // happen because vnl_vector<bool> isn't
          // instantiated
          bool val;
          int tmpVal = this->ReadScalar<int>(localMetaDataName);
          val = tmpVal != 0;
          EncapsulateMetaData<bool>(metaDict,name,val);
          }
        else
          {
          this->StoreMetaData<unsigned char>(&metaDict,
                                             localMetaDataName,
                                             name,
                                             metaDataDims[0]);
          }
        }
      else if(metaDataType == H5::PredType::NATIVE_SHORT)
        {
        this->StoreMetaData<short>(&metaDict,
                                   localMetaDataName,
                                   name,
                                   metaDataDims[0]);
        }
      else if(metaDataType == H5::PredType::NATIVE_USHORT)
        {
        this->StoreMetaData<unsigned short>(&metaDict,
                                            localMetaDataName,
                                            name,
                                            metaDataDims[0]);
        }
      else if(metaDataType == H5::PredType::NATIVE_UINT)
        {
        if(doesAttrExist(metaDataSet,"isUnsignedLong"))
          {
          unsigned long val = this->ReadScalar<unsigned long>(localMetaDataName);
          EncapsulateMetaData<unsigned long>(metaDict,name,val);
          }
        else
          {
          this->StoreMetaData<unsigned int>(&metaDict,
                                            localMetaDataName,
                                            name,
                                            metaDataDims[0]);
          }
        }
      else if(metaDataType == H5::PredType::NATIVE_LONG)
        {
        if(doesAttrExist(metaDataSet,"isLLong"))
            {
            long long val = this->ReadScalar<long long>(localMetaDataName);
            EncapsulateMetaData<long long>(metaDict,name,val);
            }
        else
            {
            this->StoreMetaData<long>(&metaDict,
                                      localMetaDataName,
                                      name,
                                      metaDataDims[0]);
            }
        }
      else if(metaDataType == H5::PredType::NATIVE_ULONG)
        {
        if(doesAttrExist(metaDataSet,"isULLong"))
            {
            unsigned long long val = this->ReadScalar<unsigned long long>(localMetaDataName);
            EncapsulateMetaData<unsigned long long>(metaDict,name,val);
            }
        else
            {
            this->StoreMetaData<unsigned long>(&metaDict,
                                               localMetaDataName,
                                               name,
                                               metaDataDims[0]);
            }
        }
      else if(metaDataType == H5::PredType::NATIVE_LLONG)
        {
        this->StoreMetaData<long long int>(&metaDict,
                                           localMetaDataName,
                                           name,
                                           metaDataDims[0]);
        }
      else if(metaDataType == H5::PredType::NATIVE_ULLONG)
        {
        this->StoreMetaData<unsigned long long int>(&metaDict,
                                           localMetaDataName,
                                           name,
                                           metaDataDims[0]);
        }
      else if(metaDataType == H5::PredType::NATIVE_FLOAT)
        {
        this->StoreMetaData<float>(&metaDict,
                                   localMetaDataName,
                                   name,
                                   metaDataDims[0]);
        }
      else if(metaDataType == H5::PredType::NATIVE_DOUBLE)
        {
        this->StoreMetaData<double>(&metaDict,
                                    localMetaDataName,
                                    name,
                                    metaDataDims[0]);
        }
      else
        {
        H5::StrType strType(H5::PredType::C_S1,H5T_VARIABLE);
        if(metaDataType == strType)
          {
          std::string val = this->ReadString(localMetaDataName);
          EncapsulateMetaData<std::string>(metaDict,name,val);
          }
        }
      }
    imageSet.close();
    }
  // catch failure caused by the H5File operations
  catch( H5::AttributeIException & error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  catch( H5::FileIException & error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  // catch failure caused by the DataSet operations
  catch( H5::DataSetIException & error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  // catch failure caused by the DataSpace operations
  catch( H5::DataSpaceIException & error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  // catch failure caused by the DataSpace operations
  catch( H5::DataTypeIException & error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
}

void
HDF5ImageIO
::SetupStreaming(H5::DataSpace *imageSpace, H5::DataSpace *slabSpace)
{
  ImageIORegion            regionToRead = this->GetIORegion();
  ImageIORegion::SizeType  size = regionToRead.GetSize();
  ImageIORegion::IndexType start = regionToRead.GetIndex();
  //
  int numComponents = this->GetNumberOfComponents();

  const int HDFDim(this->GetNumberOfDimensions() +
                   (numComponents > 1 ? 1 : 0));

  hsize_t *offset = new hsize_t[HDFDim];
  hsize_t *HDFSize = new hsize_t[HDFDim];
  const int limit = regionToRead.GetImageDimension();
  //
  // fastest moving dimension is intra-voxel
  // index
  int i = 0;
  if(numComponents > 1)
    {
    offset[HDFDim - 1] = 0;
    HDFSize[HDFDim - 1] = numComponents;
    ++i;
    }

  for(int j = 0; j < limit && i < HDFDim; ++i, ++j )
    {
      offset[HDFDim - i - 1] = start[j];
      HDFSize[HDFDim - i - 1] = size[j];
    }

  while ( i < HDFDim )
    {
    offset[HDFDim - i - 1] = 0;
    HDFSize[HDFDim - i - 1] = 1;
    ++i;
    }

  slabSpace->setExtentSimple(HDFDim,HDFSize);
  imageSpace->selectHyperslab(H5S_SELECT_SET,HDFSize,offset);
  delete[] HDFSize;
  delete[] offset;
}

void
HDF5ImageIO
::Read(void *buffer)
{
  ImageIORegion            regionToRead = this->GetIORegion();
  ImageIORegion::SizeType  size = regionToRead.GetSize();
  ImageIORegion::IndexType start = regionToRead.GetIndex();

  H5::DataType voxelType = this->m_VoxelDataSet->getDataType();
  H5::DataSpace imageSpace = this->m_VoxelDataSet->getSpace();

  H5::DataSpace dspace;
  this->SetupStreaming(&imageSpace,&dspace);
  this->m_VoxelDataSet->read(buffer,voxelType,dspace,imageSpace);
}

template <typename TType>
bool
HDF5ImageIO
::WriteMeta(const std::string &name,MetaDataObjectBase *metaObjBase)
{
  MetaDataObject<TType> *metaObj =
    dynamic_cast<MetaDataObject<TType> *>(metaObjBase);
  if(metaObj == ITK_NULLPTR)
    {
    return false;
    }
  TType val = metaObj->GetMetaDataObjectValue();
  this->WriteScalar(name,val);
  return true;
}

template <typename TType>
bool
HDF5ImageIO
::WriteMetaArray(const std::string &name,MetaDataObjectBase *metaObjBase)
{
  typedef MetaDataObject< Array<TType> > MetaDataArrayObject;
    MetaDataArrayObject *metaObj =
    dynamic_cast<MetaDataArrayObject  *>(metaObjBase);
  if(metaObj == ITK_NULLPTR)
    {
    return false;
    }
  Array<TType> val = metaObj->GetMetaDataObjectValue();
  std::vector<TType> vecVal(val.GetSize());
  for(unsigned i = 0; i < val.size(); i++)
    {
    vecVal[i] = val[i];
    }
  this->WriteVector(name,vecVal);
  return true;
}
/**
 * For HDF5 this does not write a file, it only fills in the
 * appropriate header information.
 */
void
HDF5ImageIO
::WriteImageInformation(void)
{
  //
  // guard so that image information
  if(this->m_ImageInformationWritten)
    {
    return;
    }

  try
    {
    this->CloseH5File();
    this->CloseDataSet();
    this->m_H5File = new H5::H5File(this->GetFileName(),
                                    H5F_ACC_TRUNC);
    this->m_VoxelDataSet = new H5::DataSet();

    this->WriteString(ItkVersion,
                      Version::GetITKVersion());

    this->WriteString(HDFVersion,
                                H5_VERS_INFO);
    std::string groupName(ImageGroup);
    this->m_H5File->createGroup(groupName);
    groupName += "/0";
    this->m_H5File->createGroup(groupName);
    std::string OriginName(groupName);
    OriginName += Origin;
    this->WriteVector(OriginName,this->m_Origin);
    std::string DirectionsName(groupName);
    DirectionsName += Directions;
    this->WriteDirections(DirectionsName,this->m_Direction);
    std::string SpacingName(groupName);
    SpacingName += Spacing;
    this->WriteVector(SpacingName,
                      this->m_Spacing);

    std::string DimensionsName(groupName);
    DimensionsName += Dimensions;
    this->WriteVector(DimensionsName,
                      this->m_Dimensions);

    std::string VoxelTypeName(groupName);
    VoxelTypeName += VoxelType;
    std::string typeVal(ComponentToString(this->GetComponentType()));
    this->WriteString(VoxelTypeName,typeVal);

    int numComponents = this->GetNumberOfComponents();
    int numDims = this->GetNumberOfDimensions();
    // HDF5 dimensions listed slowest moving first, ITK are fastest
    // moving first.
    hsize_t *dims = new hsize_t[numDims + (numComponents == 1 ? 0 : 1)];

    for(int i(0), j(numDims-1); i < numDims; i++, j--)
      {
      dims[j] = this->m_Dimensions[i];
      }
    if(numComponents > 1)
      {
      dims[numDims] = numComponents;
      numDims++;
      }
    H5::DataSpace imageSpace(numDims,dims);
    H5::PredType dataType = ComponentToPredType(this->GetComponentType());

    // set up properties for chunked, compressed writes.
    // in this case, set the chunk size to be the N-1 dimension
    // region
    H5::DSetCreatPropList plist;
    plist.setDeflate(5);
    dims[0] = 1;
    plist.setChunk(numDims,dims);
    delete[] dims;

    std::string VoxelDataName(ImageGroup);
    VoxelDataName += "/0";
    VoxelDataName += VoxelData;
    *(this->m_VoxelDataSet) = this->m_H5File->createDataSet(VoxelDataName,
                                                            dataType,
                                                            imageSpace,plist);
    std::string MetaDataGroupName(groupName);
    MetaDataGroupName += MetaDataName;
    this->m_H5File->createGroup(MetaDataGroupName);
    //
    // MetaData.
    MetaDataDictionary & metaDict = this->GetMetaDataDictionary();
    MetaDataDictionary::Iterator it = metaDict.Begin(),
      end = metaDict.End();
    for(; it != end; it++)
      {
      MetaDataObjectBase *metaObj =
        it->second.GetPointer();
      std::string objName(MetaDataGroupName);
      objName += "/";
      objName += it->first;
      // scalars
      if(this->WriteMeta<bool>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMeta<char>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMeta<unsigned char>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMeta<short>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMeta<unsigned short>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMeta<int>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMeta<unsigned int>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMeta<long>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMeta<unsigned long>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMeta<long long int>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMeta<unsigned long long int>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMeta<float>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMeta<double>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMetaArray<char>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMetaArray<unsigned char>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMetaArray<short>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMetaArray<unsigned short>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMetaArray<int>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMetaArray<unsigned int>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMetaArray<long>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMetaArray<unsigned long>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMetaArray<float>(objName,metaObj))
        {
        continue;
        }
      if(this->WriteMetaArray<double>(objName,metaObj))
        {
        continue;
        }
      //
      // C String Arrays
      {
      MetaDataObject<char *> *cstringObj =
        dynamic_cast<MetaDataObject<char *> *>(metaObj);
      MetaDataObject<const char *> *constCstringObj =
        dynamic_cast<MetaDataObject<const char *> *>(metaObj);
      if(cstringObj != ITK_NULLPTR || constCstringObj != ITK_NULLPTR)
        {
        const char *val;
        if(cstringObj != ITK_NULLPTR)
          {
          val = cstringObj->GetMetaDataObjectValue();
          }
        else
          {
          val = constCstringObj->GetMetaDataObjectValue();
          }
        this->WriteString(objName,val);
        continue;
        }
      }
      //
      // std::string
      {
      MetaDataObject<std::string> *stdStringObj =
        dynamic_cast<MetaDataObject<std::string> *>(metaObj);
      if(stdStringObj != ITK_NULLPTR)
        {
        std::string val = stdStringObj->GetMetaDataObjectValue();
        this->WriteString(objName,val);
        continue;
        }
      }
      }
    }
  // catch failure caused by the H5File operations
  catch( H5::FileIException & error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  // catch failure caused by the DataSet operations
  catch( H5::DataSetIException & error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  // catch failure caused by the DataSpace operations
  catch( H5::DataSpaceIException & error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  // catch failure caused by the DataSpace operations
  catch( H5::DataTypeIException & error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  //
  // only write image information once.
  this->m_ImageInformationWritten = true;
}

/**
 * Write the image Information before writing data
 */
void
HDF5ImageIO
::Write(const void *buffer)
{
  this->WriteImageInformation();
  try
    {
    int numComponents = this->GetNumberOfComponents();
    int numDims = this->GetNumberOfDimensions();
    // HDF5 dimensions listed slowest moving first, ITK are fastest
    // moving first.
    hsize_t *dims = new hsize_t[numDims + (numComponents == 1 ? 0 : 1)];

    for(int i(0), j(numDims-1); i < numDims; i++, j--)
      {
      dims[j] = this->m_Dimensions[i];
      }
    if(numComponents > 1)
      {
      dims[numDims] = numComponents;
      numDims++;
      }
    H5::DataSpace imageSpace(numDims,dims);
    H5::PredType dataType = ComponentToPredType(this->GetComponentType());
    H5::DataSpace dspace;
    this->SetupStreaming(&imageSpace,&dspace);
    this->m_VoxelDataSet->write(buffer,dataType,dspace,imageSpace);
    delete[] dims;
    }
  // catch failure caused by the H5File operations
  catch( H5::FileIException & error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  // catch failure caused by the DataSet operations
  catch( H5::DataSetIException & error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  // catch failure caused by the DataSpace operations
  catch( H5::DataSpaceIException & error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
  // catch failure caused by the DataSpace operations
  catch( H5::DataTypeIException & error )
    {
    itkExceptionMacro(<< error.getCDetailMsg());
    }
}

//
// GetHeaderSize -- return 0
ImageIOBase::SizeType
HDF5ImageIO
::GetHeaderSize(void) const
{
  return 0;
}

} // end namespace itk
