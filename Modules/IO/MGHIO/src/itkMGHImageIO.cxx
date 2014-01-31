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


// -------------------------------
//
// Convert to BE
//
// -------------------------------

template <class T>
int
MGHImageIO
::TRead(T & out)
{
  const int result = ::gzread(this->m_GZFile, &out, sizeof(T) );
  itk::ByteSwapper<T>::SwapFromSystemToBigEndian(&out);
  return result;
}

template <class T>
int
MGHImageIO
::TWrite(T out)
{
  itk::ByteSwapper<T>::SwapFromSystemToBigEndian(&out);
  if(this->m_IsCompressed)
    {
    return ::gzwrite(this->m_GZFile,&out,sizeof(T));
    }
  else
    {
    this->m_Output.write(reinterpret_cast<char *>(&out),sizeof(T));
    return this->m_Output.good() ? sizeof(T) : 0;
    }
}

int
MGHImageIO
::TWrite(const char *buf,unsigned long count)
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
  this->TRead( version);
  int   bufInt;        // buffer -> int type (most ITK types are unsigned)
  this->TRead( bufInt);
  m_Dimensions[0] = static_cast<unsigned int>(bufInt);
  this->TRead( bufInt);
  m_Dimensions[1] = static_cast<unsigned int>(bufInt);
  this->TRead( bufInt);
  m_Dimensions[2] = static_cast<unsigned int>(bufInt);
  // next is nframes
  this->TRead( bufInt);
  m_NumberOfComponents = static_cast<unsigned int>(bufInt);
  int   type;
  this->TRead( type);
  int   dof;
  this->TRead( dof);   // what does this do?

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
  this->TRead( RASgood);
  if( RASgood )
    {
    for( int nSpacing = 0; nSpacing < 3; ++nSpacing )
      {
      float spacing;
      this->TRead( spacing); // type is different
      m_Spacing[nSpacing] = spacing;
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
    typedef itk::Matrix<double> MatrixType;
    MatrixType matrix;
    // reading in x_r x_a x_s y_r y_a y_s z_r z_a z_s and putting it into the
    // matrix as:
    // x_r y_r z_r
    // x_a y_a z_a
    // x_s y_s z_s
    for( unsigned int uj = 0; uj < 3; ++uj )
      {
      for( unsigned int ui = 0; ui < 3; ++ui )
        {
        float fBuffer;
        this->TRead( fBuffer);
        matrix[ui][uj] = fBuffer;
//      std::cout << "itkMGHImageIO ReadVolumeHeader: matrix[" << ui << "][" << uj << "] = " << matrix[ui][uj] << "\n";
        }
      }
    float c[3];
    for( unsigned int ui = 0; ui < 3; ++ui )
      {
      this->TRead( c[ui]);
      }

    const std::string orientation = GetOrientation( matrix );
    // now take x_r, x_a, x_s out of the matrix and set it to the direction
    // vector 0, same for y_* and direction vector 1, z_* and vector 2
    for( unsigned int ui = 0; ui < 3; ++ui )
      {
      // convert the coordinates from RAS to LPS, as the ITK archetype assumes
      // LPS volumes
      // volume orientation not related to scan order, always convert
      matrix[0][ui] *= -1.0; // R -> L
      matrix[1][ui] *= -1.0; // A -> P
      std::vector<double> vDir;
      for( unsigned int uj = 0; uj < 3; ++uj )
        {
        vDir.push_back( matrix[uj][ui] );
        }
      // std::cout << "itkMGHImageIO ReadVolumeHeader: setting " << ui << " direction in LPS: " << vDir[0] << "," <<
      // vDir[1] << "," << vDir[2] << "\n";
      SetDirection( ui, vDir );
      }

    // MriDirCos(); // convert direction cosines

    // finally, store the origin of the image -> only works
    // if the image is properly orriented in the sequel
    //
    // computed in CORONAL orientation = ITK_COORDINATE_ORIENTATION_LIA
    // convert C to from RAS to LPS
    c[0] *= -1;
    c[1] *= -1;
    const float fcx = static_cast<float>(m_Dimensions[0]) / 2.0f;
    const float fcy = static_cast<float>(m_Dimensions[1]) / 2.0f;
    const float fcz = static_cast<float>(m_Dimensions[2]) / 2.0f;
    for( unsigned int ui = 0; ui < 3; ++ui )
      {
      m_Origin[ui] = c[ui]
        - ( matrix[ui][0] * m_Spacing[0] * fcx
            + matrix[ui][1] * m_Spacing[1] * fcy
            + matrix[ui][2] * m_Spacing[2] * fcz );
      }

    }

  // ==================
  // read tags at the end of file

  const unsigned long numValues = m_Dimensions[0] * m_Dimensions[1] * m_Dimensions[2];
  gzseek(this->m_GZFile, FS_WHOLE_HEADER_SIZE
         + ( m_NumberOfComponents * numValues * this->GetComponentSize() ),
         SEEK_SET);

  float fBuf;
  // read TR, Flip, TE, FI, FOV
  if( this->TRead( fBuf) )
    {
    itk::MetaDataDictionary & thisDic = this->GetMetaDataDictionary();
    itk::EncapsulateMetaData<float>(thisDic,
                                    std::string("TR"),
                                    fBuf);

    // try to read flipAngle
    if( this->TRead( fBuf ) )
      {
      itk::EncapsulateMetaData<float>(thisDic,
                                      std::string("FlipAngle"),
                                      fBuf);
      // TE
      if( this->TRead( fBuf ) )
        {
        itk::EncapsulateMetaData<float>(thisDic,
                                        std::string("TE"),
                                        fBuf);
        // TI
        if( this->TRead( fBuf) )
          {
          itk::EncapsulateMetaData<float>(thisDic,
                                          std::string("TI"),
                                          fBuf);
          // FOV
          if( this->TRead( fBuf) )
            {
            itk::EncapsulateMetaData<float>(thisDic,
                                            std::string("FoV"),
                                            fBuf);
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
    char* pBuffer = new char[frameSize];

    const unsigned int pixelSize = componentSize * m_NumberOfComponents;
    for( unsigned int frameIndex = 0;
         frameIndex < m_NumberOfComponents;
         ++frameIndex )
      {
      // read current frame
      gzread( this->m_GZFile, pBuffer, frameSize );
      // copy memory location in the final buffer

      char * pSrc = (char *)pBuffer;
      char * pDst = (char *)pData;

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
    if( this->m_GZFile == 0 )
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
  const int mghVersion = 1;
  this->TWrite(mghVersion );
  // dimensions
  for( unsigned int ui = 0; ui < 3; ++ui )
    {
    this->TWrite((int)m_Dimensions[ui] );
    }

  // nframes
  this->TWrite((int)m_NumberOfComponents );

  // type
  switch( m_ComponentType )
    {
    case UCHAR:
      {
      this->TWrite(MRI_UCHAR);
      }
      break;
    case INT:
      {
      this->TWrite(MRI_INT);
      }
      break;
    case FLOAT:
      {
      this->TWrite(MRI_FLOAT);
      }
      break;
    case SHORT:
      {
      this->TWrite(MRI_SHORT);
      }
      break;
    default:
      itkExceptionMacro(
        << "MGHImageIO supports unsigned char, int, float and short");

    }

  // dof !?! -> default value = 1
  this->TWrite(1);

  // write RAS and voxel size info
  // for now, RAS flag will be good
  // in the future, check if the m_Directions matrix is a permutation matrix
  this->TWrite((short)1);
  // spacing
  for( unsigned int ui = 0; ui < 3; ++ui )
    {
    this->TWrite((float)m_Spacing[ui]);
    }

  // get directions matrix
  std::vector<std::vector<double> > vvRas;
  for( unsigned int ui = 0; ui < 3; ++ui )
    {
    vvRas.push_back( GetDirection(ui) );
    }
  // transpose data before writing it
  std::vector<float> vBufRas;
  // transpose the matrix
  for( unsigned int ui(0); ui < 3; ++ui )
    {
    for( unsigned int uj(0); uj < 3; ++uj )
      {
      if( uj == 0 || uj == 1 )
        {
        // convert the coordinates from LPS to RAS
        vBufRas.push_back(-1.0 * (float)vvRas[uj][ui] );
        }
      else
        {
        vBufRas.push_back( (float)vvRas[uj][ui] );
        }
      }
    }
  for( std::vector<float>::const_iterator cit = vBufRas.begin(); cit != vBufRas.end(); ++cit )
    {
    this->TWrite(*cit);
    }

  const float fcx = static_cast<float>(m_Dimensions[0]) / 2.0f;
  const float fcy = static_cast<float>(m_Dimensions[1]) / 2.0f;
  const float fcz = static_cast<float>(m_Dimensions[2]) / 2.0f;
  float c[3];
  for( unsigned int ui = 0; ui < 3; ++ui )
    {
    c[ui] = m_Origin[ui]
      + ( vvRas[ui][0] * m_Spacing[0] * fcx
          + vvRas[ui][1] * m_Spacing[1] * fcy
          + vvRas[ui][2] * m_Spacing[2] * fcz );
    }
  c[0] *= -1.0;
  c[1] *= -1.0;
  for( unsigned int ui = 0; ui < 3; ++ui )
    {
    this->TWrite(c[ui]);
    }
  // fill the rest of the buffer with zeros
  const char zerobyte(0);
  for(unsigned int i = 0; i < FS_UNUSED_HEADER_SIZE; ++i)
    {
    this->TWrite(zerobyte);
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

  char* tempmemory = new char[numbytes];

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
    this->TWrite(fScanBuffer );
    } // end TR
  if( ExposeMetaData<float>(thisDic, "FlipAngle", fScanBuffer) )
    {
    this->TWrite(fScanBuffer);
    } // end FlipAngle

  if( ExposeMetaData<float>(thisDic, "TE", fScanBuffer) )
    {
    this->TWrite(fScanBuffer);
    } // end TE

  if( ExposeMetaData<float>(thisDic, "TI", fScanBuffer) )
    {
    this->TWrite(fScanBuffer);
    } // end TI

  if( ExposeMetaData<float>(thisDic, "FoV", fScanBuffer) )
    {
    this->TWrite(fScanBuffer);
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

  const char* pSrc = (const char *)buffer;
  char*       pDst = (char *)tempmemory;

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
  // std::cout << "GetOrientation returning " << orientation.c_str() << std::endl;
  return orientation;
}
} // end namespace itk
