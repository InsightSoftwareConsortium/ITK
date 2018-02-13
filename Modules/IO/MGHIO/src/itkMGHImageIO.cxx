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
#include "itkMGHImageIO.h"
#include "itkByteSwapper.h"
#include "itkMetaDataObject.h"
#include "itksys/SystemTools.hxx"

namespace itk
{

//VALID file extensions
static const std::string __MGH_EXT(".mgh");
static const std::string __MGZ_EXT(".mgz");
static const std::string __GZ_EXT(".gz");

using MatrixType = itk::Matrix<double,3,3>;
using VectorType = itk::Vector<double, 3>;

static MatrixType GetRAS2LPS()
  {
  MatrixType RAS2LAS;
  RAS2LAS.SetIdentity();
  RAS2LAS[0][0]=-1.0;
  RAS2LAS[1][1]=-1.0;
  RAS2LAS[2][2]= 1.0;
  return RAS2LAS;
  }

// -------------------------------
//
// Convert to BE
//
// -------------------------------

template <typename TDiskType, typename TOutType>
int
MGHImageIO
::TRead(TOutType & outValue)
{
  TDiskType onDiskValue;
  const int result = ::gzread(this->m_GZFile, &onDiskValue, sizeof(TDiskType) );
  itk::ByteSwapper<TDiskType>::SwapFromSystemToBigEndian(&onDiskValue);
  outValue = static_cast<TOutType>(onDiskValue);
  return result;
}

template <typename TInType, typename TDiskType>
int
MGHImageIO
::TWrite(const TInType inValue)
{
  auto onDiskValue = static_cast<TDiskType>(inValue);
  itk::ByteSwapper<TDiskType>::SwapFromSystemToBigEndian(&onDiskValue);
  if(this->m_IsCompressed)
    {
    return ::gzwrite(this->m_GZFile,&onDiskValue,sizeof(TDiskType));
    }
  else
    {
    this->m_Output.write(reinterpret_cast<char *>(&onDiskValue),sizeof(TDiskType));
    return this->m_Output.good() ? sizeof(TDiskType) : 0;
    }
}

int
MGHImageIO
::TWrite(const char *buf,const unsigned long count)
{
  if(this->m_IsCompressed)
    {
    const unsigned long int result = ::gzwrite( this->m_GZFile, buf, count );
    if( result != count )
      {
      itkExceptionMacro( << " Failed to write " << count << ", only wrote " << result);
      }
    return result;
    }
  else
    {
    this->m_Output.write(buf,count);
    return this->m_Output.good() ? count : 0;
    }
}
// --------------------------------------
//
// MGHImageIO
//


MGHImageIO
::MGHImageIO()
{
  this->SetNumberOfDimensions(3);
  std::fill(m_Dimensions.begin(), m_Dimensions.end(), 0U);
  m_ByteOrder = ( ByteSwapper<int>::SystemIsBigEndian() ) ? BigEndian : LittleEndian;
}

MGHImageIO
::~MGHImageIO()
{
  //Nothing to do in destructor
}

bool
MGHImageIO
::IsCompressedFilename(const std::string fname)
{
  //
  // Originally MGHImageIO allowed ".mgh", ".mgz",
  // "mgh.gz" and ".gz"
  //
  // The '.gz' extension is too ambiguous. It collides with NIfTI
  // (.nii.gz) and given that there's no consistent 'magic number' in
  // the header, the MGH library will blindly try and read any '.gz'
  // file until it crashes or detects invalid data.
  //
  // So the bare '.gz' extension is no longer recognized.
  const std::string lastExtension = itksys::SystemTools::GetFilenameLastExtension(fname.c_str());
  if(lastExtension == __MGZ_EXT)
    {
    return true;
    }
  if(lastExtension == __GZ_EXT)
    {
    const std::string fnameWithoutLastExtension =
      itksys::SystemTools::GetFilenameWithoutLastExtension(fname);
    const std::string penultimateExtension =
      itksys::SystemTools::GetFilenameLastExtension(fnameWithoutLastExtension);
    if(penultimateExtension == __MGH_EXT)
      {
      return true;
      }
    }
  return false;
}

bool
MGHImageIO
::CanReadFile(const char* FileNameToRead)
{
  const std::string filename(FileNameToRead);

  if( filename == "" )
    {
    itkExceptionMacro(<< "A FileName must be specified.");
    return false;
    }

  // check if the correct extension is given by the user
  const std::string extension = itksys::SystemTools::GetFilenameLastExtension(filename.c_str());
  if( extension == __MGH_EXT || this->IsCompressedFilename(filename) )
    {
    return true;
    }
  return false;
}

void
MGHImageIO
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  const std::string strSep = ", ";

  os << indent
     << "Data Dimensions: ("
     << m_Dimensions[0] << strSep
     << m_Dimensions[1] << strSep
     << m_Dimensions[2] << ")\n"
     << indent
     << "Data Spacing: ("
     << m_Spacing[0] << strSep
     << m_Spacing[1] << strSep
     << m_Spacing[2] << ")\n"
     << indent
     << "Scalar Type: " << m_ComponentType << std::endl
     << indent
     << "Number of Frames: " << m_NumberOfComponents << std::endl;

  os << indent << "RAS to IJK matrix: " << std::endl;
}

void
MGHImageIO
::ReadImageInformation()
{
  this->m_GZFile = gzopen( m_FileName.c_str(), "rb");
  if( !this->m_GZFile )
    {
    itkExceptionMacro(<< "Can't find/open file: " << m_FileName);
    return;
    }

  ReadVolumeHeader();
  gzclose(this->m_GZFile);
}

void
MGHImageIO
::ReadVolumeHeader()
{
  // check file reading
  if( !this->m_GZFile )
    {
    itkExceptionMacro(<< "Can't find/open file: " << this->m_FileName);
    return;
    }
  int   version;
  this->TRead<int,int>( version);
  this->TRead<int,itk::SizeValueType>( m_Dimensions[0] );
  this->TRead<int,itk::SizeValueType>( m_Dimensions[1] );
  this->TRead<int,itk::SizeValueType>( m_Dimensions[2] );
  // next is nframes
  this->TRead<int,unsigned int>( m_NumberOfComponents );
  int   type;
  this->TRead<int,int>( type);
  int   dof;
  this->TRead<int,int>( dof);   // what does this do?

  // Convert type to an ITK type
  switch( type )
    {
    case MRI_UCHAR:
      {
      m_ComponentType = UCHAR;
      }
      break;
    case MRI_INT:
      {
      m_ComponentType = INT;
      }
      break;
    case MRI_FLOAT:
      {
      m_ComponentType = FLOAT;
      }
      break;
    case MRI_SHORT:
      {
      m_ComponentType = SHORT;
      }
      break;
    case MRI_TENSOR:
      {
      m_ComponentType = FLOAT;
      m_NumberOfComponents = 9;
      }
      break;
    default:
      itkExceptionMacro(<< " Unknown data type " << type << " using float by default.");
      m_ComponentType = FLOAT;
    }

  // Next short says whether RAS registration information is good.
  // If so, read the voxel size and then the matrix
  short RASgood;
  this->TRead<short,short>( RASgood);
  if( RASgood )
    {
    for( size_t nSpacing = 0; nSpacing < 3; ++nSpacing )
      {
      this->TRead<float,double>( m_Spacing[nSpacing] );
      }
    /*
      From http://www.nmr.mgh.harvard.edu/~tosa/#coords:
      To go from freesurfer voxel coordinates to RAS coordinates, they use:
      translation:  t_r, t_a, t_s is defined using c_r, c_a, c_s centre voxel position in RAS
      rotation: direction cosines x_(r,a,s), y_(r,a,s), z_(r,a,s)
      voxel size for scale: s_x, s_y, s_z

      [ x_r y_r z_r t_r][s_x  0   0  0]
      [ x_a y_a z_a t_a][0   s_y  0  0]
      [ x_s y_s z_s t_s][0    0  s_z 0]
      [  0   0   0   1 ][0    0   0  1]
      Voxel center is a column matrix, multipled from the right
      [v_x]
      [v_y]
      [v_z]
      [ 1 ]

      In the MGH header, they hold:
      x_r x_a x_s
      y_r y_a y_s
      z_r z_a z_s
      c_r c_a c_s
    */
    MatrixType MGHdirMatrix;
    // reading in x_r x_a x_s y_r y_a y_s z_r z_a z_s and putting it into the
    // matrix as:
    // x_r y_r z_r
    // x_a y_a z_a
    // x_s y_s z_s
    for( unsigned int c = 0; c < 3; ++c )
      {
      for( unsigned int r = 0; r < 3; ++r ) //NOTE: Data stored row-major form, so traverse rows first
        {
        this->TRead<float,double>( MGHdirMatrix[r][c] );
        }
      }
    // getting orientation must be done before RAS->LAS conversion
    // but  not used const std::string orientation = GetOrientation( MGHdirMatrix );
    // convert the coordinates from RAS to LPS, as the ITK archetype assumes
    // LPS volumes. volume orientation not related to scan order, always convert
    MatrixType ITKdirMatrix=GetRAS2LPS()*MGHdirMatrix; // Pre-multipy by conversion
    for( size_t c = 0; c < 3; ++c )
      {
      // now take x_r, x_a, x_s out of the matrix and set it to the direction
      // vector 0, same for y_* and direction vector 1, z_* and vector 2
      std::vector<double> vDir;
      for( size_t r = 0; r < 3; ++r )
        {
        vDir.push_back( ITKdirMatrix[r][c] );
        }
      SetDirection( c, vDir );
      }

    VectorType MGHcenterVoxel; //c_r c_a c_s
    for( size_t ui = 0; ui < 3; ++ui )
      {
      this->TRead<float,double>( MGHcenterVoxel[ui]);
      }
    // convert C to from RAS to LPS
    VectorType ITKcenterVoxel=GetRAS2LPS()*MGHcenterVoxel;


    // MriDirCos(); // convert direction cosines
    // finally, store the origin of the image -> only works
    // if the image is properly oriented in the sequel
    //
    // computed in CORONAL orientation = ITK_COORDINATE_ORIENTATION_LIA
    VectorType fc;
    fc[0] = m_Dimensions[0] * 0.5;
    fc[1] = m_Dimensions[1] * 0.5;
    fc[2] = m_Dimensions[2] * 0.5;
    MatrixType spcing;
    spcing[0][0] = m_Spacing[0];
    spcing[1][1] = m_Spacing[1];
    spcing[2][2] = m_Spacing[2];

    VectorType ITKorigin = ITKcenterVoxel - ( ITKdirMatrix * spcing * fc );
    m_Origin[0] = ITKorigin[0];
    m_Origin[1] = ITKorigin[1];
    m_Origin[2] = ITKorigin[2];
    }

  // ==================
  // read tags at the end of file
  const size_t numValues = m_Dimensions[0] * m_Dimensions[1] * m_Dimensions[2];
  gzseek(this->m_GZFile, FS_WHOLE_HEADER_SIZE
         + ( m_NumberOfComponents * numValues * this->GetComponentSize() ),
         SEEK_SET);

  float fBufTR;
  // read TR, Flip, TE, FI, FOV
  if( this->TRead<float,float>( fBufTR) )
    {
    itk::MetaDataDictionary & thisDic = this->GetMetaDataDictionary();
    itk::EncapsulateMetaData<float>(thisDic,
                                    std::string("TR"),
                                    fBufTR);

    // try to read flipAngle
    float fBufFA;
    if( this->TRead<float,float>( fBufFA ) )
      {
      itk::EncapsulateMetaData<float>(thisDic,
                                      std::string("FlipAngle"),
                                      fBufFA);
      // TE
      float fBufTE;
      if( this->TRead<float,float>( fBufTE ) )
        {
        itk::EncapsulateMetaData<float>(thisDic,
                                        std::string("TE"),
                                        fBufTE);
        // TI
        float fBufTI;
        if( this->TRead<float,float>( fBufTI) )
          {
          itk::EncapsulateMetaData<float>(thisDic,
                                          std::string("TI"),
                                          fBufTI);
          // FOV
          float fBufFOV;
          if( this->TRead<float,float>( fBufFOV) )
            {
            itk::EncapsulateMetaData<float>(thisDic,
                                            std::string("FoV"),
                                            fBufFOV);
            }
          }
        }
      }
    }
}

void
MGHImageIO
::Read(void* pData)
{
  this->m_GZFile = gzopen( m_FileName.c_str(), "rb");
  if( !this->m_GZFile )
    {
    itkExceptionMacro(<< "Can't find/open file: " << m_FileName);
    return;
    }

  const unsigned long numPixels = m_Dimensions[0] * m_Dimensions[1] * m_Dimensions[2];

  const unsigned int componentSize( this->GetComponentSize() );

  // check that the offset is actually computed wrt. the beginning
  gzseek(this->m_GZFile, FS_WHOLE_HEADER_SIZE, SEEK_SET );

  const unsigned int frameSize = numPixels * componentSize;

  if( m_NumberOfComponents > 1  )
    {
    auto* pBuffer = new char[frameSize];

    const unsigned int pixelSize = componentSize * m_NumberOfComponents;
    for( unsigned int frameIndex = 0;
         frameIndex < m_NumberOfComponents;
         ++frameIndex )
      {
      // read current frame
      gzread( this->m_GZFile, pBuffer, frameSize );
      // copy memory location in the final buffer

      auto * pSrc = (char *)pBuffer;
      auto * pDst = (char *)pData;

      pDst += frameIndex * componentSize;
      for( unsigned int ui = 0;
           ui < numPixels;
           ++ui, pSrc += componentSize, pDst += pixelSize )
        {
        for( unsigned int byteCount = 0;
             byteCount < componentSize; ++byteCount )
          {
          *(pDst + byteCount) = *(pSrc + byteCount);
          }
        } // next ui
      }   // next frameIndex

    // clear resources
    delete[] pBuffer;
    }
  else
    {
    gzread( this->m_GZFile, pData, frameSize);
    }

  gzclose(this->m_GZFile);

  SwapBytesIfNecessary( pData, numPixels * m_NumberOfComponents );

}   // end Read function

void
MGHImageIO
::SwapBytesIfNecessary(void * const buffer,
                                 const unsigned long numberOfPixels)
{
  // NOTE: If machine order is little endian, and the data needs to be
  // swapped, the SwapFromBigEndianToSystem is equivalent to
  // SwapFromSystemToBigEndian.

  switch( m_ComponentType )
    {
    case UCHAR:
      {
      ByteSwapper<unsigned char>::SwapRangeFromSystemToBigEndian( (unsigned char *)buffer,
                                                                  numberOfPixels);
      }
      break;
    case SHORT:
      {
      ByteSwapper<short>::SwapRangeFromSystemToBigEndian( (short *)buffer,
                                                          numberOfPixels);
      }
      break;
    case INT:
      {
      ByteSwapper<int>::SwapRangeFromSystemToBigEndian( (int *)buffer,
                                                        numberOfPixels);
      }
      break;
    case FLOAT:
      {
      ByteSwapper<float>::SwapRangeFromSystemToBigEndian( (float *)buffer,
                                                          numberOfPixels);
      }
      break;
    default:
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Pixel Type Unknown");
      throw exception;
    }   // end switch
}

bool
MGHImageIO
::CanWriteFile(const char* name)
{
  const std::string filename(name);

  if( filename.empty() )
    {
    itkExceptionMacro(<< "A FileName must be specified.");
    return false;
    }

  //TODO:  Use ITK Extension extractor
  const std::string extension = itksys::SystemTools::GetFilenameExtension(filename.c_str());
  if( extension != __MGH_EXT && !this->IsCompressedFilename(filename))
    {
    return false;
    }
  return true;
}

void
MGHImageIO
::OpenFile()
{
  if(this->m_IsCompressed)
    {
    this->m_GZFile = gzopen(m_FileName.c_str(), "wb");
    if( this->m_GZFile == nullptr )
      {
      itkExceptionMacro(<< " Failed to open gzFile for writing");
      itkExceptionMacro(<< " File cannot be written");
      }
    }
  else
    {
    this->m_Output.open(m_FileName.c_str(), std::ios::out | std::ios::binary );
    if( this->m_Output.fail() )
      {
      itkExceptionMacro(<< " File cannot be written");
      }
    }
}
void
MGHImageIO
::CloseFile()
{
  if(this->m_IsCompressed)
    {
    gzclose(this->m_GZFile);
    }
  else
    {
    this->m_Output.close();
    }
}
void
MGHImageIO
::WriteImageInformation()
{
  this->m_IsCompressed = this->IsCompressedFilename(this->m_FileName);
  this->OpenFile();
  this->WriteHeader();
  this->CloseFile();
}

void
MGHImageIO
::Write(const void* buffer)
{
  this->m_IsCompressed = this->IsCompressedFilename(this->m_FileName);
  this->OpenFile();
  this->WriteHeader();
  this->WriteData(buffer);
  this->CloseFile();
}

void
MGHImageIO
::WriteHeader()
{
  // version
  constexpr int mghVersion = 1;
  this->TWrite<int,int>( mghVersion );
  // dimensions
  for( size_t ui = 0; ui < 3; ++ui )
    {
    this->TWrite<size_t, int>( m_Dimensions[ui] );
    }

  // nframes
  this->TWrite<size_t,int>( m_NumberOfComponents );

  // type
  switch( m_ComponentType )
    {
    case UCHAR:
      {
      this->TWrite<int,int>(MRI_UCHAR);
      }
      break;
    case INT:
      {
      this->TWrite<int,int>(MRI_INT);
      }
      break;
    case FLOAT:
      {
      this->TWrite<int,int>(MRI_FLOAT);
      }
      break;
    case SHORT:
      {
      this->TWrite<int,int>(MRI_SHORT);
      }
      break;
    default:
      itkExceptionMacro(
        << "MGHImageIO supports unsigned char, int, float and short");

    }

  // dof !?! -> default value = 1
  this->TWrite<int,int>(1);

  // write RAS and voxel size info
  // for now, RAS flag will be good
  // in the future, check if the m_Directions matrix is a permutation matrix
  this->TWrite<int,short>(1);
  // spacing
  for( unsigned int ui = 0; ui < 3; ++ui )
    {
    this->TWrite<double,float>(m_Spacing[ui]);
    }

  // get directions matrix
  MatrixType ITKdirMatrix;
  for( unsigned int c = 0; c < 3; ++c )
    {
    const std::vector<double> & dir_line=GetDirection(c);
    for( unsigned int r = 0; r < 3; ++r )
      {
      ITKdirMatrix[r][c] = dir_line[r];
      }
    }
  MatrixType MGHdirMatrix=GetRAS2LPS()*ITKdirMatrix; //Pre-multipy by RAS2LPS
  // writing in x_r x_a x_s y_r y_a y_s z_r z_a z_s and putting it into the
  // matrix as:
  // x_r y_r z_r
  // x_a y_a z_a
  // x_s y_s z_s
  for( unsigned int c = 0; c < 3; ++c )
    {
    for( unsigned int r = 0; r < 3; ++r ) //Write in row-major order
      {
      this->TWrite<double,float>(MGHdirMatrix[r][c]);
      }
    }

  VectorType fc;
  fc[0] = m_Dimensions[0] * 0.5;
  fc[1] = m_Dimensions[1] * 0.5;
  fc[2] = m_Dimensions[2] * 0.5;
  VectorType ITKorigin;
  ITKorigin[0]=m_Origin[0];
  ITKorigin[1]=m_Origin[1];
  ITKorigin[2]=m_Origin[2];
  MatrixType spcing;
  spcing.SetIdentity();
  spcing[0][0] = m_Spacing[0];
  spcing[1][1] = m_Spacing[1];
  spcing[2][2] = m_Spacing[2];
  const VectorType ITKcenterVoxel = ITKorigin + ( ITKdirMatrix * spcing * fc );
  const VectorType MGHcenterVoxel=GetRAS2LPS()*ITKcenterVoxel;
  for( size_t ui = 0; ui < 3; ++ui )
    {
    this->TWrite<double,float>(MGHcenterVoxel[ui]);
    }
  // fill the rest of the buffer with zeros
  const char zerobyte(0);
  for(size_t i = 0; i < FS_UNUSED_HEADER_SIZE; ++i)
    {
    this->TWrite<char,char>(zerobyte);
    }
}

void
MGHImageIO
::WriteData(const void* buffer)
{
  // swap bytes if necessary
  const unsigned int numPixels =  m_Dimensions[0]
    * m_Dimensions[1] * m_Dimensions[2];
  const unsigned long int numvalues = numPixels * m_NumberOfComponents;
  const unsigned long int numbytes = this->GetComponentSize() * numvalues;

  auto* tempmemory = new char[numbytes];

  // re-arrange data in frames
  if( m_NumberOfComponents > 1 )
    {
    PermuteFrameValues(buffer, tempmemory);
    }
  else
    {
    memcpy(tempmemory, buffer, numbytes);
    }

  this->SwapBytesIfNecessary(tempmemory, numvalues);

  this->TWrite(tempmemory,this->GetImageSizeInBytes());
  delete[] tempmemory;

  // if present, the scan parameters are present at the end of the file, so now's the time to write them
  itk::MetaDataDictionary & thisDic = this->GetMetaDataDictionary();

  float fScanBuffer = 0.0F;
  if( ExposeMetaData<float>(thisDic, "TR", fScanBuffer) )
    {
    this->TWrite<float,float>(fScanBuffer );
    } // end TR
  if( ExposeMetaData<float>(thisDic, "FlipAngle", fScanBuffer) )
    {
    this->TWrite<float,float>(fScanBuffer);
    } // end FlipAngle

  if( ExposeMetaData<float>(thisDic, "TE", fScanBuffer) )
    {
    this->TWrite<float,float>(fScanBuffer);
    } // end TE

  if( ExposeMetaData<float>(thisDic, "TI", fScanBuffer) )
    {
    this->TWrite<float,float>(fScanBuffer);
    } // end TI

  if( ExposeMetaData<float>(thisDic, "FoV", fScanBuffer) )
    {
    this->TWrite<float,float>(fScanBuffer);
    } // end FoV

  // no need to close the stream
}

void
MGHImageIO
::PermuteFrameValues(const void* buffer,
                               char* tempmemory)
{
  const unsigned int numPixels =  m_Dimensions[0]
    * m_Dimensions[1] * m_Dimensions[2];
  const unsigned int valueSize( this->GetComponentSize() );
  const unsigned int frameSize = numPixels * valueSize;

  const auto* pSrc = (const char *)buffer;
  auto*       pDst = (char *)tempmemory;

  for( unsigned int pixelIndex = 0;
       pixelIndex < numPixels; ++pixelIndex, pDst += valueSize )
    {
    for( unsigned int componentIndex = 0;
         componentIndex < m_NumberOfComponents;
         ++componentIndex, pSrc += valueSize )
      {
      std::copy( pSrc, pSrc + valueSize,
                 pDst + frameSize * componentIndex );
      } // next component index
    }   // next pixelIndex
}

unsigned int
MGHImageIO
::GetComponentSize() const
{
  unsigned int returnValue;
  switch( m_ComponentType )
    {
  case UCHAR:
      {
      returnValue = sizeof(unsigned char);
      }
    break;
  case SHORT:
      {
      returnValue = sizeof(short);
      }
    break;
  case INT:
      {
      returnValue = sizeof(int);
      }
    break;
  case FLOAT:
      {
      returnValue = sizeof(float);
      }
    break;
  default:
    itkExceptionMacro(
      << "MGHImageIO supports unsigned char, int, float and short");
    }
  return returnValue;
}

/**
 * Examines the direction cosines and creates an Orientation String.
 * The Orientation String is a three character string indicating the primary
 * direction of each axis in the 3d matrix. The characters can be L,R,A,P,I,S.
 **/
std::string
MGHImageIO
::GetOrientation( itk::Matrix<double> directions )
{
  std::string orientation("");
  for( int cAxes = 0; cAxes < 3; cAxes++ )
    {
    const double sag = directions( 0, cAxes );   // LR axis
    const double cor = directions( 1, cAxes );   // PA axis
    const double ax  = directions( 2, cAxes );   // IS axis
    if( fabs(sag) > fabs(cor) && fabs(sag) > fabs(ax) )
      {
      if( sag > 0 )
        {
        orientation += "R";
        }
      else
        {
        orientation += "L";
        }
      continue;
      }
    if( fabs(cor) > fabs(ax) )
      {
      if( cor > 0 )
        {
        orientation += "A";
        }
      else
        {
        orientation += "P";
        }
      continue;
      }
    if( ax > 0 )
      {
      orientation += "S";
      }
    else
      {
      orientation += "I";
      }
    }
  return orientation;
}
} // end namespace itk
