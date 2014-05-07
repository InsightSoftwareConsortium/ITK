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
#include "itkMRCHeaderObject.h"
#include "itkMRCImageIOPrivate.h"
#include "itkByteSwapper.h"


namespace itk
{
void MRCHeaderObject::DeepCopy(ConstPointer h)
{
  memcpy( &this->m_Header, &h->m_Header, sizeof( Header ) );

  this->m_ExtendedHeaderSize = h->m_ExtendedHeaderSize;
  if ( this->m_ExtendedHeaderSize )
    {
    delete[] static_cast< char * >( this->m_ExtendedHeader );

    this->m_ExtendedHeader = new char[this->m_ExtendedHeaderSize];
    memcpy(this->m_ExtendedHeader, h->m_ExtendedHeader, this->m_ExtendedHeaderSize);
    }

  this->m_BigEndianHeader = h->m_BigEndianHeader;

  if ( h->m_ExtendedFeiHeader )
    {
    this->m_ExtendedFeiHeader = static_cast< FeiExtendedHeader * >( this->m_ExtendedHeader );
    }
  else
    {
    this->m_ExtendedFeiHeader = ITK_NULLPTR;
    }
}

const MRCHeaderObject::Header & MRCHeaderObject::GetHeader() const
{
  return this->m_Header;
}

bool MRCHeaderObject::SetHeader(const Header *buffer)
{
  if ( !buffer )
    {
    return false;
    }

  const Header *_header = static_cast< const Header * >( buffer );

  memcpy( &this->m_Header, buffer, sizeof( Header ) );

  // the cmap field must should either be the magic field or 0
  if ( strncmp(_header->cmap, magicMAP, 4) != 0
       && memcmp(_header->cmap, "\0\0\0\0", 4) != 0 )
    {
    itkWarningMacro(<< "The header's cmap field does not have expected values");
    return false;
    }

  // the stamp may help up tell the endian of the header
  // or it's an older header
  if ( _header->stamp[0] == 17 )
    {
    this->m_BigEndianHeader = true;
    this->swapHeader(this->m_BigEndianHeader);
    }
  else if ( _header->stamp[0] == 68 )
    {
    this->m_BigEndianHeader = false;
    this->swapHeader(this->m_BigEndianHeader);
    }
  else if ( _header->stamp[0] == 0
            && _header->stamp[1] == 0
            && _header->stamp[2] == 0
            && _header->stamp[3] == 0 )
    {
    this->m_BigEndianHeader = ByteSwapper< void * >::SystemIsBigEndian();

    // do a check to see which byte order make sense
    if ( this->m_Header.mapc < 1 || this->m_Header.mapc > 3
         || this->m_Header.mapr < 1 || this->m_Header.mapr > 3
         || this->m_Header.maps < 1 || this->m_Header.maps > 3 )
      {
      this->m_BigEndianHeader = !this->m_BigEndianHeader;
      this->swapHeader(this->m_BigEndianHeader);
      }
    }
  else
    {
    // the stamp is not expected
    itkWarningMacro(<< "The header's stamp field does not have expected values");
    return false;
    }

  // clean up
  delete[] static_cast< char * >( this->m_ExtendedHeader );

  this->m_ExtendedHeader = ITK_NULLPTR;
  this->m_ExtendedFeiHeader = ITK_NULLPTR;

  SizeValueType extendedHeaderBytes = 0;
  if ( this->m_Header.nreal & 1   ) { extendedHeaderBytes += 2; }
  if ( this->m_Header.nreal & 2   ) { extendedHeaderBytes += 6; }
  if ( this->m_Header.nreal & 4   ) { extendedHeaderBytes += 3; }
  if ( this->m_Header.nreal & 8   ) { extendedHeaderBytes += 2; }
  if ( this->m_Header.nreal & 16  ) { extendedHeaderBytes += 2; }
  if ( this->m_Header.nreal & 32  ) { extendedHeaderBytes += 4; }
  if ( this->m_Header.nreal & 64  ) { extendedHeaderBytes += 2; }
  if ( this->m_Header.nreal & 128 ) { extendedHeaderBytes += 4; }
  if ( this->m_Header.nreal & 256 ) { extendedHeaderBytes += 2; }
  if ( this->m_Header.nreal & 512 ) { extendedHeaderBytes += 4; }
  if ( this->m_Header.nreal & 1024 ) { extendedHeaderBytes += 2; }

  this->m_ExtendedHeaderSize = this->m_Header.next;

  // check to make sure the data makes sense
  if ( this->m_Header.nx <= 0 || this->m_Header.ny <= 0 || this->m_Header.nz <= 0
       || ( this->m_Header.nx > 65535 || this->m_Header.ny > 65535 || this->m_Header.nz > 65535 )
       || this->m_Header.mapc < MRCHEADER_MAP_X || this->m_Header.mapc > MRCHEADER_MAP_Z
       || this->m_Header.mapr < MRCHEADER_MAP_X || this->m_Header.mapr > MRCHEADER_MAP_Z
       || this->m_Header.maps < MRCHEADER_MAP_X || this->m_Header.maps > MRCHEADER_MAP_Z
       || this->m_Header.nxstart >= this->m_Header.nx
       || this->m_Header.nystart >= this->m_Header.ny
       || this->m_Header.nzstart >= this->m_Header.nz )
    {
    itkWarningMacro(<< "Some header data does not have sensable values");
    return false;
    }

  if ( this->m_Header.nxstart != 0
       || this->m_Header.nystart != 0
       || this->m_Header.nzstart != 0 )
    {
    itkWarningMacro(<< "The header's nxstart, nystart and nzstart fields are not supported correctly");
    }

  return true;
}

SizeValueType MRCHeaderObject::GetExtendedHeaderSize(void) const
{
  return this->m_ExtendedHeaderSize;
}

bool MRCHeaderObject::SetExtendedHeader(const void *buffer)
{
  if ( !this->m_ExtendedHeaderSize )
    {
    return false;
    }

  delete[] static_cast< char * >( this->m_ExtendedHeader );

  this->m_ExtendedHeader = new char[this->m_ExtendedHeaderSize];
  memcpy(this->m_ExtendedHeader, buffer, this->m_ExtendedHeaderSize);

  this->m_ExtendedFeiHeader = ITK_NULLPTR;
  if ( this->m_ExtendedHeaderSize == 128 * 1024 && this->m_Header.nint == 0 && this->m_Header.nreal == 32 )
    {
    this->m_ExtendedFeiHeader = static_cast< FeiExtendedHeader * >( this->m_ExtendedHeader );

    if ( this->m_BigEndianHeader )
      {
      ByteSwapper< float >::SwapRangeFromSystemToBigEndian( (float *)this->m_ExtendedHeader,
                                                            this->m_ExtendedHeaderSize );
      }
    else
      {
      ByteSwapper< float >::SwapRangeFromSystemToLittleEndian( (float *)this->m_ExtendedHeader,
                                                               this->m_ExtendedHeaderSize );
      }
    }
  return true;
}

bool MRCHeaderObject::IsOriginalHeaderBigEndian(void) const
{
  return this->m_BigEndianHeader;
}

MRCHeaderObject::MRCHeaderObject(void)
  : m_ExtendedHeaderSize(0),
    m_ExtendedHeader(ITK_NULLPTR),
    m_ExtendedFeiHeader(ITK_NULLPTR)
{
  memset( &this->m_Header, 0, sizeof( Header ) );
  this->m_BigEndianHeader = ByteSwapper< void * >::SystemIsBE();
}

MRCHeaderObject::~MRCHeaderObject(void)
{
  delete[] static_cast< char * >( this->m_ExtendedHeader );
}

void MRCHeaderObject::swapHeader(bool bigEndian)
{
  typedef itk::ByteSwapper< float >   FloatSwapper;
  typedef itk::ByteSwapper< int32_t > Int32Swapper;
  typedef itk::ByteSwapper< int16_t > Int16Swapper;

  if ( FloatSwapper::SystemIsBigEndian() )
    {
    this->m_Header.stamp[0] = 17;
    }
  else
    {
    this->m_Header.stamp[0] = 68;
    }

  if ( bigEndian )
    {
    Int32Swapper::SwapFromSystemToBigEndian(&this->m_Header.nx);
    Int32Swapper::SwapFromSystemToBigEndian(&this->m_Header.ny);
    Int32Swapper::SwapFromSystemToBigEndian(&this->m_Header.nz);

    Int32Swapper::SwapFromSystemToBigEndian(&this->m_Header.mode);

    Int32Swapper::SwapFromSystemToBigEndian(&this->m_Header.nxstart);
    Int32Swapper::SwapFromSystemToBigEndian(&this->m_Header.nystart);
    Int32Swapper::SwapFromSystemToBigEndian(&this->m_Header.nzstart);

    Int32Swapper::SwapFromSystemToBigEndian(&this->m_Header.mx);
    Int32Swapper::SwapFromSystemToBigEndian(&this->m_Header.my);
    Int32Swapper::SwapFromSystemToBigEndian(&this->m_Header.mz);

    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.xlen);
    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.ylen);
    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.zlen);

    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.alpha);
    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.beta);
    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.gamma);

    Int32Swapper::SwapFromSystemToBigEndian(&this->m_Header.mapc);
    Int32Swapper::SwapFromSystemToBigEndian(&this->m_Header.mapr);
    Int32Swapper::SwapFromSystemToBigEndian(&this->m_Header.maps);

    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.amin);
    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.amax);
    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.amean);

    Int16Swapper::SwapFromSystemToBigEndian(&this->m_Header.ispg);
    Int16Swapper::SwapFromSystemToBigEndian(&this->m_Header.nsymbt);
    Int32Swapper::SwapFromSystemToBigEndian(&this->m_Header.next);
    Int16Swapper::SwapFromSystemToBigEndian(&this->m_Header.creatid);

    Int16Swapper::SwapFromSystemToBigEndian(&this->m_Header.nint);
    Int16Swapper::SwapFromSystemToBigEndian(&this->m_Header.nreal);

    Int16Swapper::SwapFromSystemToBigEndian(&this->m_Header.idtype);
    Int16Swapper::SwapFromSystemToBigEndian(&this->m_Header.lens);
    Int16Swapper::SwapFromSystemToBigEndian(&this->m_Header.nd1);
    Int16Swapper::SwapFromSystemToBigEndian(&this->m_Header.nd2);
    Int16Swapper::SwapFromSystemToBigEndian(&this->m_Header.vd1);
    Int16Swapper::SwapFromSystemToBigEndian(&this->m_Header.vd2);

    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.tiltangles[0]);
    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.tiltangles[1]);
    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.tiltangles[2]);
    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.tiltangles[3]);
    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.tiltangles[4]);
    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.tiltangles[5]);

    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.xorg);
    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.yorg);
    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.zorg);

    FloatSwapper::SwapFromSystemToBigEndian(&this->m_Header.rms);

    Int32Swapper::SwapFromSystemToBigEndian(&this->m_Header.nlabl);
    }
  else
    {
    Int32Swapper::SwapFromSystemToLittleEndian(&this->m_Header.nx);
    Int32Swapper::SwapFromSystemToLittleEndian(&this->m_Header.ny);
    Int32Swapper::SwapFromSystemToLittleEndian(&this->m_Header.nz);

    Int32Swapper::SwapFromSystemToLittleEndian(&this->m_Header.mode);

    Int32Swapper::SwapFromSystemToLittleEndian(&this->m_Header.nxstart);
    Int32Swapper::SwapFromSystemToLittleEndian(&this->m_Header.nystart);
    Int32Swapper::SwapFromSystemToLittleEndian(&this->m_Header.nzstart);

    Int32Swapper::SwapFromSystemToLittleEndian(&this->m_Header.mx);
    Int32Swapper::SwapFromSystemToLittleEndian(&this->m_Header.my);
    Int32Swapper::SwapFromSystemToLittleEndian(&this->m_Header.mz);

    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.xlen);
    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.ylen);
    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.zlen);

    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.alpha);
    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.beta);
    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.gamma);

    Int32Swapper::SwapFromSystemToLittleEndian(&this->m_Header.mapc);
    Int32Swapper::SwapFromSystemToLittleEndian(&this->m_Header.mapr);
    Int32Swapper::SwapFromSystemToLittleEndian(&this->m_Header.maps);

    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.amin);
    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.amax);
    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.amean);

    Int16Swapper::SwapFromSystemToLittleEndian(&this->m_Header.ispg);
    Int16Swapper::SwapFromSystemToLittleEndian(&this->m_Header.nsymbt);
    Int32Swapper::SwapFromSystemToLittleEndian(&this->m_Header.next);
    Int16Swapper::SwapFromSystemToLittleEndian(&this->m_Header.creatid);

    Int16Swapper::SwapFromSystemToLittleEndian(&this->m_Header.nint);
    Int16Swapper::SwapFromSystemToLittleEndian(&this->m_Header.nreal);

    Int16Swapper::SwapFromSystemToLittleEndian(&this->m_Header.idtype);
    Int16Swapper::SwapFromSystemToLittleEndian(&this->m_Header.lens);
    Int16Swapper::SwapFromSystemToLittleEndian(&this->m_Header.nd1);
    Int16Swapper::SwapFromSystemToLittleEndian(&this->m_Header.nd2);
    Int16Swapper::SwapFromSystemToLittleEndian(&this->m_Header.vd1);
    Int16Swapper::SwapFromSystemToLittleEndian(&this->m_Header.vd2);

    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.tiltangles[0]);
    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.tiltangles[1]);
    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.tiltangles[2]);
    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.tiltangles[3]);
    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.tiltangles[4]);
    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.tiltangles[5]);

    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.xorg);
    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.yorg);
    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.zorg);

    FloatSwapper::SwapFromSystemToLittleEndian(&this->m_Header.rms);

    Int32Swapper::SwapFromSystemToLittleEndian(&this->m_Header.nlabl);
    }
}

void MRCHeaderObject::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "number: " << this->m_Header.nx << " " << this->m_Header.ny << " " << this->m_Header.nz << std::endl;
  os << indent << "mode: " << this->m_Header.mode << std::endl;
  os << indent << "start: " << this->m_Header.nxstart << " " << this->m_Header.nystart << " "
     << this->m_Header.nzstart << std::endl;
  os << indent << "grid: " << this->m_Header.mx  << " " << this->m_Header.my << " " << this->m_Header.mz << std::endl;
  os << indent << "len: " << this->m_Header.xlen << " " << this->m_Header.ylen << " " << this->m_Header.zlen
     << std::endl;
  os << indent << "abg angles: " << this->m_Header.alpha << " " << this->m_Header.beta << " "
     << this->m_Header.gamma << std::endl;
  os << indent << "map: " << this->m_Header.mapc << " " << this->m_Header.mapr << " " << this->m_Header.maps
     << std::endl;
  os << indent << "mmm: " << this->m_Header.amin << " " << this->m_Header.amax << " " << this->m_Header.amean
     << std::endl;
  os << indent << "ispg: " << this->m_Header.ispg << std::endl;
  os << indent << "nsymbt: " <<  this->m_Header.nsymbt << std::endl;
  os << indent << "next: " << this->m_Header.next << std::endl;
  os << indent << "creatid: " << this->m_Header.creatid << std::endl;
  os << indent << "nint: " << this->m_Header.nint << std::endl;
  os << indent << "nreal: " << this->m_Header.nreal << std::endl;
  os << indent << "idtype: " << this->m_Header.idtype << std::endl;
  os << indent << "lens: " << this->m_Header.lens << std::endl;
  os << indent << "nd: " << this->m_Header.nd1 << " " << this->m_Header.nd2 << std::endl;
  os << indent << "vd: " << this->m_Header.vd1 << " " << this->m_Header.vd2 << std::endl;
  os << indent << "tiltangles: (" << this->m_Header.tiltangles[0] << ", " << this->m_Header.tiltangles[1] << ", "
     << this->m_Header.tiltangles[2]
     << ") (" << this->m_Header.tiltangles[3] << ", " << this->m_Header.tiltangles[4] << ", "
     << this->m_Header.tiltangles[5] << ")" << std::endl;
  os << indent << "org: " << this->m_Header.xorg << " " << this->m_Header.yorg << " " << this->m_Header.zorg
     << std::endl;
  os << indent << "cmap: \"" << this->m_Header.cmap[0] << this->m_Header.cmap[1] << this->m_Header.cmap[2]
     << this->m_Header.cmap[3] << "\"" << std::endl;
  os << indent << "stamp: "
     << int(this->m_Header.stamp[0]) << " "
     << int(this->m_Header.stamp[1]) << " "
     << int(this->m_Header.stamp[2]) << " "
     << int(this->m_Header.stamp[3])
     << std::endl;
  os << indent << "rms: " << this->m_Header.rms << std::endl;
  os << indent << "nlabl: " << this->m_Header.nlabl << std::endl;

  for ( int32_t i = 0; i < this->m_Header.nlabl && i < 10; ++i )
    {
    os.write(this->m_Header.label[i], 80);
    os << indent << std::endl;
    }

  if ( this->m_ExtendedFeiHeader )
    {
    os << indent << "Extended Header: " << std::endl;
    os << indent
       <<
      "( atilt, btilt, xstage, ystage, zstage, xshift, yshift, defocus, exptime, meanint, tiltaxis, pixelsize, magnification)"
       << std::endl;
    for ( int32_t z = 0; z < this->m_Header.nz && z < 1024; ++z )
      {
      os << indent << "("
         << this->m_ExtendedFeiHeader[z].atilt << ", "
         << this->m_ExtendedFeiHeader[z].btilt << ", "
         << this->m_ExtendedFeiHeader[z].xstage << ", "
         << this->m_ExtendedFeiHeader[z].ystage << ", "
         << this->m_ExtendedFeiHeader[z].zstage << ", "
         << this->m_ExtendedFeiHeader[z].xshift << ", "
         << this->m_ExtendedFeiHeader[z].yshift << ", "
         << this->m_ExtendedFeiHeader[z].defocus << ", "
         << this->m_ExtendedFeiHeader[z].exptime << ", "
         << this->m_ExtendedFeiHeader[z].meanint << ", "
         << this->m_ExtendedFeiHeader[z].tiltaxis << ", "
         << this->m_ExtendedFeiHeader[z].pixelsize << ", "
         << this->m_ExtendedFeiHeader[z].magnification << ")" << std::endl;
      }
    }
}

} // namespace itk
