/*=========================================================================


  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTIFFImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifdef _MSC_VER
#pragma warning( disable : 4611 )
#endif 

#include "itkTIFFImageIO.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include <stdio.h>

#include <sys/stat.h>

extern "C" {
#include "tiffio.h"
}

namespace itk
{

class TIFFReaderInternal
{
public:
  TIFFReaderInternal();
  int Initialize();
  void Clean();
  int CanRead();
  int Open( const char *filename );
  TIFF *Image;
  bool IsOpen;
  unsigned int Width;
  unsigned int Height;
  unsigned short NumberOfPages;
  unsigned short CurrentPage;
  unsigned short SamplesPerPixel;
  unsigned short Compression;
  unsigned short BitsPerSample;
  unsigned short Photometrics;
  bool HasValidPhotometricInterpretation;
  unsigned short PlanarConfig;
  unsigned short Orientation;
  unsigned long int TileDepth;
  unsigned int TileRows;
  unsigned int TileColumns;
  unsigned int TileWidth;
  unsigned int TileHeight;
  unsigned short NumberOfTiles;
  unsigned int SubFiles;
};

int TIFFReaderInternal::Open( const char *filename )
{
  this->Clean();
  struct stat fs;
  if ( stat(filename, &fs) )
    {
    return 0;
    }
  
  this->Image = TIFFOpen(filename, "r");
  if ( !this->Image)
    {
    this->Clean();
    return 0;
    }
  if ( !this->Initialize() )
    {
    this->Clean();
    return 0;
    }

  this->IsOpen = true;
  return 1;
}

void TIFFReaderInternal::Clean()
{
  if ( this->Image )
    {
    TIFFClose(this->Image);
    }
  this->Image=NULL;
  this->Width = 0;
  this->Height = 0;
  this->SamplesPerPixel = 0;
  this->Compression = 0;
  this->BitsPerSample = 0;
  this->Photometrics = 0;
  this->HasValidPhotometricInterpretation = false;
  this->PlanarConfig = 0;
  this->TileDepth = 0;
  this->CurrentPage = 0;
  this->NumberOfPages = 0;
  this->NumberOfTiles = 0;
  this->TileRows = 0;
  this->TileColumns = 0;
  this->TileWidth = 0;
  this->TileHeight = 0;
  this->SubFiles = 0;
  this->IsOpen = false;
}

TIFFReaderInternal::TIFFReaderInternal()
{
  this->Image           = NULL;
  this->Clean();
}

int TIFFReaderInternal::Initialize()
{
  if ( this->Image )
    {
    if ( !TIFFGetField(this->Image, TIFFTAG_IMAGEWIDTH, &this->Width) ||
         !TIFFGetField(this->Image, TIFFTAG_IMAGELENGTH, &this->Height) )
      {
      return 0;
      }
   
    // Check the number of pages. First by looking at the number of directories 
    this->NumberOfPages=TIFFNumberOfDirectories(this->Image);
            
    if(this->NumberOfPages == 0)
      {
      if ( !TIFFGetField(this->Image,TIFFTAG_PAGENUMBER,&this->CurrentPage, &this->NumberOfPages))
        {
        // Check the Image Description tag to know the number of images
        // This is used by ImageJ
        char** description = new char*[255];
        if (TIFFGetField(this->Image,TIFFTAG_IMAGEDESCRIPTION,description))
          {
          // look for the number of images
          std::string desc = description[0];
          int pos = desc.find("images=");
          int pos2 = desc.find("\n");
          if( (pos != -1) && (pos2 != -1))
            {
            this->NumberOfPages = atoi(desc.substr(pos+7,pos2-pos-7).c_str());
            }
          }
        }
      }

    // If the number of pages is still zero we look if the image is tiled
    if(this->NumberOfPages == 0 && TIFFIsTiled(this->Image))
      {
      this->NumberOfTiles = TIFFNumberOfTiles(this->Image);
      
      if ( !TIFFGetField(this->Image,TIFFTAG_TILEWIDTH,&this->TileWidth)
        || !TIFFGetField(this->Image,TIFFTAG_TILELENGTH,&this->TileHeight)
        )
        {
        itkGenericExceptionMacro( << "Cannot read tile width and tile length from file" );
        }
      else
        {
        TileRows = this->Height/this->TileHeight;
        TileColumns = this->Width/this->TileWidth;
        }
      }

    // Checking if the TIFF contains subfiles
    if(this->NumberOfPages > 1)
      {
      this->SubFiles = 0;

      for(unsigned int page = 0;page<this->NumberOfPages;page++)
        {
        long subfiletype = 6;
        if(TIFFGetField(this->Image, TIFFTAG_SUBFILETYPE, &subfiletype))
          {
          if(subfiletype == 0)
            {
            this->SubFiles+=1;
            }
          }
        TIFFReadDirectory(this->Image);
        }

      // Set the directory to the first image
      TIFFSetDirectory(this->Image,0);
      }

 
    if(this->SubFiles>0)
      {
      itkGenericOutputMacro( << "This TIFF image contains subfiles: " << this->SubFiles );
      }

    TIFFGetFieldDefaulted(this->Image, TIFFTAG_ORIENTATION,
                          &this->Orientation);
    TIFFGetFieldDefaulted(this->Image, TIFFTAG_SAMPLESPERPIXEL, 
                          &this->SamplesPerPixel);
    TIFFGetFieldDefaulted(this->Image, TIFFTAG_COMPRESSION, &this->Compression);
    TIFFGetFieldDefaulted(this->Image, TIFFTAG_BITSPERSAMPLE, 
                          &this->BitsPerSample);
    TIFFGetFieldDefaulted(this->Image, TIFFTAG_PLANARCONFIG, &this->PlanarConfig);
    // If TIFFGetField returns false, there's no Photometric Interpretation 
    // set for this image, but that's a required field so we set a warning flag.
    // (Because the "Photometrics" field is an enum, we can't rely on setting 
    // this->Photometrics to some signal value.)
    if (TIFFGetField(this->Image, TIFFTAG_PHOTOMETRIC, &this->Photometrics))
      {
      this->HasValidPhotometricInterpretation = true;
      }
    else
      {
      this->HasValidPhotometricInterpretation = false;
      }
    if ( !TIFFGetField(this->Image, TIFFTAG_TILEDEPTH, &this->TileDepth) )
      {
      this->TileDepth = 0;
      }
    }

  return 1;
}

int TIFFReaderInternal::CanRead()
{
  return ( this->Image && ( this->Width > 0 ) && ( this->Height > 0 ) &&
           ( this->SamplesPerPixel > 0 ) && 
           ( this->Compression == COMPRESSION_NONE || this->Compression == COMPRESSION_PACKBITS) &&
           ( this->HasValidPhotometricInterpretation ) &&
           ( this->Photometrics == PHOTOMETRIC_RGB ||
             this->Photometrics == PHOTOMETRIC_MINISWHITE ||
             this->Photometrics == PHOTOMETRIC_MINISBLACK ||
             this->Photometrics == PHOTOMETRIC_PALETTE ) &&
           ( this->PlanarConfig == PLANARCONFIG_CONTIG ) &&
           ( !this->TileDepth ) &&
           ( this->BitsPerSample == 8 || this->BitsPerSample == 16 ) );
}




bool TIFFImageIO::CanReadFile(const char* file) 
{ 
  // First check the extension
  std::string filename = file;
  if(  filename == "" )
    {
    itkDebugMacro(<<"No filename specified.");
    return false;
    }   

  // Now check if this is a valid TIFF image
  TIFFErrorHandler save = TIFFSetErrorHandler(0);
  int res = m_InternalImage->Open(file);
  if (res)
    {
    TIFFSetErrorHandler(save);
    return true;
    }
  m_InternalImage->Clean();
  TIFFSetErrorHandler(save);
  return false;
}



/** To Support Zeiss images that contains only 2 samples per pixel but are actually
 *  RGB images */
void TIFFImageIO::ReadTwoSamplesPerPixelImage( void *out, 
                                               unsigned int width, 
                                               unsigned int height )
{
  unsigned int isize = TIFFScanlineSize(m_InternalImage->Image);
  unsigned int cc;
  int row;
  tdata_t buf = _TIFFmalloc(isize);

  int inc = 1;

  if(m_ComponentType == UCHAR)
    {
    unsigned char* image;
     if (m_InternalImage->PlanarConfig == PLANARCONFIG_CONTIG) 
      {
      for ( row = 0; row < (int)height; row ++ )
        {
        if (TIFFReadScanline(m_InternalImage->Image, buf, row, 0) <= 0)
          {
          itkExceptionMacro( << "Problem reading the row: " << row );
          break;
          }
          
        if (m_InternalImage->Orientation == ORIENTATION_TOPLEFT)
          {
          image = reinterpret_cast<unsigned char*>(out) + row * width * inc;
          }
        else
          {
          image = reinterpret_cast<unsigned char*>(out) + width * inc * (height - (row + 1));
          }

        for (cc = 0; cc < isize; 
             cc += m_InternalImage->SamplesPerPixel )
          {
          inc = this->EvaluateImageAt( image, 
                                       static_cast<unsigned char *>(buf) +
                                       cc );      
          image += inc;
          }
        }
      }
    else if(m_InternalImage->PlanarConfig == PLANARCONFIG_SEPARATE)
      {
      unsigned long s;
      unsigned long nsamples;
      TIFFGetField(m_InternalImage->Image, TIFFTAG_SAMPLESPERPIXEL, &nsamples);
      for (s = 0; s < nsamples; s++)
        {
        for ( row = 0; row < (int)height; row ++ )
          {
          if (TIFFReadScanline(m_InternalImage->Image, buf, row, s) <= 0)
            {
            itkExceptionMacro( << "Problem reading the row: " << row );
            break;
            }
          
          inc = 3;

          if (m_InternalImage->Orientation == ORIENTATION_TOPLEFT)
            {
            image = reinterpret_cast<unsigned char*>(out) + row * width * inc;
            }
          else
            {
            image = reinterpret_cast<unsigned char*>(out) + width * inc * (height - (row + 1));
            }

          // We translate the output pixel to be on the right RGB
          image += s;
          for (cc = 0; cc < isize; 
               cc += 1)
            {
            (*image) = *(static_cast<unsigned char *>(buf) + cc);
            inc = 3;
            image += inc;
            }
          }
        }
      }
    }
  else if(m_ComponentType == USHORT)
    {
    isize /= 2;
    unsigned short* image;
    if (m_InternalImage->PlanarConfig == PLANARCONFIG_CONTIG) 
      {
      for ( row = 0; row < (int)height; row ++ )
        {
        if (TIFFReadScanline(m_InternalImage->Image, buf, row, 0) <= 0)
          {
          itkExceptionMacro( << "Problem reading the row: " << row );
          break;
          }
          
        if (m_InternalImage->Orientation == ORIENTATION_TOPLEFT)
          {
          image = reinterpret_cast<unsigned short*>(out) + row * width * inc;
          }
        else
          {
          image = reinterpret_cast<unsigned short*>(out) + width * inc * (height - (row + 1));
          }

        for (cc = 0; cc < isize; 
             cc += m_InternalImage->SamplesPerPixel )
          {
          inc = this->EvaluateImageAt( image, 
                                       static_cast<unsigned short *>(buf) +
                                       cc );      
          image += inc;
          }
        }
      }
    else if(m_InternalImage->PlanarConfig == PLANARCONFIG_SEPARATE)
      {
      unsigned long s, nsamples;
      TIFFGetField(m_InternalImage->Image, TIFFTAG_SAMPLESPERPIXEL, &nsamples);
      for (s = 0; s < nsamples; s++)
        {
        for ( row = 0; row < (int)height; row ++ )
          {
          if (TIFFReadScanline(m_InternalImage->Image, buf, row, s) <= 0)
            {
            itkExceptionMacro( << "Problem reading the row: " << row );
            break;
            }
          
          if (m_InternalImage->Orientation == ORIENTATION_TOPLEFT)
            {
            image = reinterpret_cast<unsigned short*>(out) + row * width * inc;
            }
          else
            {
            image = reinterpret_cast<unsigned short*>(out) + width * inc * (height - (row + 1));
            }
          // We translate the output pixel to be on the right RGB
          image += s;
          for (cc = 0; cc < isize; 
               cc += 1)
            {
            (*image) = *(static_cast<unsigned short *>(buf) + cc);
            inc = 3;
            image += inc;
            }
          }
        }
      }
    }
  _TIFFfree(buf); 
}


void TIFFImageIO::ReadGenericImage( void *out, 
                                    unsigned int width, 
                                    unsigned int height )
{
  unsigned int isize = TIFFScanlineSize(m_InternalImage->Image);
  unsigned int cc;
  int row, inc;
  tdata_t buf = _TIFFmalloc(isize);

  if ( m_InternalImage->PlanarConfig != PLANARCONFIG_CONTIG )
    {
    itkExceptionMacro( << "This reader can only do PLANARCONFIG_CONTIG" );
    return;
    }

  switch ( this->GetFormat() )
    {
    default:
    case TIFFImageIO::GRAYSCALE:
    case TIFFImageIO::PALETTE_GRAYSCALE:
      inc = 1;
      break;
    case TIFFImageIO::RGB_: 
      inc = m_InternalImage->SamplesPerPixel;
      break;
    case TIFFImageIO::PALETTE_RGB:
      inc = 3;
      break;
    }

  if(m_ComponentType == UCHAR)
    {
    unsigned char* image;
     if (m_InternalImage->PlanarConfig == PLANARCONFIG_CONTIG) 
      {
      for ( row = 0; row < (int)height; row ++ )
        {
        if (TIFFReadScanline(m_InternalImage->Image, buf, row, 0) <= 0)
          {
          itkExceptionMacro( << "Problem reading the row: " << row );
          break;
          }
          
        if (m_InternalImage->Orientation == ORIENTATION_TOPLEFT)
          {
          image = reinterpret_cast<unsigned char*>(out) + row * width * inc;
          }
        else
          {
          image = reinterpret_cast<unsigned char*>(out) + width * inc * (height - (row + 1));
          }

        for (cc = 0; cc < isize; 
             cc += m_InternalImage->SamplesPerPixel )
          {
          inc = this->EvaluateImageAt( image, 
                                       static_cast<unsigned char *>(buf) +
                                       cc );      
          image += inc;
          }
        }
      }
    else if(m_InternalImage->PlanarConfig == PLANARCONFIG_SEPARATE)
      {
      unsigned long s;
      unsigned long nsamples;
      TIFFGetField(m_InternalImage->Image, TIFFTAG_SAMPLESPERPIXEL, &nsamples);
      for (s = 0; s < nsamples; s++)
        {
        for ( row = 0; row < (int)height; row ++ )
          {
          if (TIFFReadScanline(m_InternalImage->Image, buf, row, s) <= 0)
            {
            itkExceptionMacro( << "Problem reading the row: " << row );
            break;
            }
          
          inc = 3;
          if (m_InternalImage->Orientation == ORIENTATION_TOPLEFT)
            {
            image = reinterpret_cast<unsigned char*>(out) + row * width * inc;
            }
          else
            {
            image = reinterpret_cast<unsigned char*>(out) + width * inc * (height - (row + 1));
            }

           for (cc = 0; cc < isize; 
               cc += m_InternalImage->SamplesPerPixel )
            {
            inc = this->EvaluateImageAt( image, 
                                         static_cast<unsigned char *>(buf) +
                                         cc );
 
            image += inc;
            }
          }
        }
      }
    }
  else if(m_ComponentType == USHORT)
    {
    isize /= 2;
    unsigned short* image;
    if (m_InternalImage->PlanarConfig == PLANARCONFIG_CONTIG) 
      {
      for ( row = 0; row < (int)height; row ++ )
        {
        if (TIFFReadScanline(m_InternalImage->Image, buf, row, 0) <= 0)
          {
          itkExceptionMacro( << "Problem reading the row: " << row );
          break;
          }
          
        if (m_InternalImage->Orientation == ORIENTATION_TOPLEFT)
          {
          image = reinterpret_cast<unsigned short*>(out) + row * width * inc;
          }
        else
          {
          image = reinterpret_cast<unsigned short*>(out) + width * inc * (height - (row + 1));
          }

        for (cc = 0; cc < isize; 
             cc += m_InternalImage->SamplesPerPixel )
          {
          inc = this->EvaluateImageAt( image, 
                                       static_cast<unsigned short *>(buf) +
                                       cc );      
          image += inc;
          }
        }
      }
    else if(m_InternalImage->PlanarConfig == PLANARCONFIG_SEPARATE)
      {
      unsigned long s, nsamples;
      TIFFGetField(m_InternalImage->Image, TIFFTAG_SAMPLESPERPIXEL, &nsamples);
      for (s = 0; s < nsamples; s++)
        {
        for ( row = 0; row < (int)height; row ++ )
          {
          if (TIFFReadScanline(m_InternalImage->Image, buf, row, s) <= 0)
            {
            itkExceptionMacro( << "Problem reading the row: " << row );
            break;
            }
          
          if (m_InternalImage->Orientation == ORIENTATION_TOPLEFT)
            {
            image = reinterpret_cast<unsigned short*>(out) + row * width * inc;
            }
          else
            {
            image = reinterpret_cast<unsigned short*>(out) + width * inc * (height - (row + 1));
            }
          for (cc = 0; cc < isize; 
               cc += m_InternalImage->SamplesPerPixel )
            {
            inc = this->EvaluateImageAt( image, 
                                         static_cast<unsigned short *>(buf) +
                                         cc );      
            image += inc;
            }
          }
        }
      }
    }
  _TIFFfree(buf); 
}


int TIFFImageIO::EvaluateImageAt( void* out, void* in )
{
  unsigned char *image = (unsigned char*)out;
  unsigned char *source = (unsigned char*)in;

  int increment;
  unsigned short red, green, blue, alpha;
  switch ( this->GetFormat() )
    {
    case TIFFImageIO::GRAYSCALE:
      if ( m_InternalImage->Photometrics == 
           PHOTOMETRIC_MINISBLACK )
        {
        if(m_ComponentType == USHORT)
          {
          unsigned short *image = (unsigned short*)out;
          unsigned short *source = (unsigned short*)in;
          *image = *source;
          }
        else
          {
          *image = *source;
          }
        }
      else
        {
        *image = ~( *source );
        }
      increment = 1;
      break;
    case TIFFImageIO::PALETTE_GRAYSCALE:
      this->GetColor(*source, &red, &green, &blue);
      *image = static_cast<unsigned char>(red >> 8);
      increment = 1;
      break;
    case TIFFImageIO::RGB_: 
      red   = *(source);
      green = *(source+1);
      blue  = *(source+2);
      *(image)   = red;
      *(image+1) = green;
      *(image+2) = blue;
      if ( m_InternalImage->SamplesPerPixel == 4 )
        {
        alpha = *(source+3);
        *(image+3) = 255-alpha;       
        }
      increment = m_InternalImage->SamplesPerPixel;
      break;
    case TIFFImageIO::PALETTE_RGB:
      this->GetColor(*source, &red, &green, &blue);     
      *(image)   = static_cast<unsigned char>(red >> 8);
      *(image+1) = static_cast<unsigned char>(green >> 8);
      *(image+2) = static_cast<unsigned char>(blue >> 8);
      increment = 3;
      break;
    default:
      return 0;
    }
  
  return increment;
}


void TIFFImageIO::GetColor( int index, unsigned short *red, 
                                 unsigned short *green, unsigned short *blue )
{
  *red   = 0;
  *green = 0;
  *blue  = 0;
  if ( index < 0 ) 
    {
    itkExceptionMacro( << "Color index has to be greater than 0" );
    return;
    }
  if ( this->TotalColors > 0 && 
       this->ColorRed && this->ColorGreen && this->ColorBlue )
    {
    if ( index >= this->TotalColors )
      {
       itkExceptionMacro( << "Color index has to be less than number of colors ("
                    << this->TotalColors << ")" );
      return;
      }
    *red   = *(this->ColorRed   + index);
    *green = *(this->ColorGreen + index);
    *blue  = *(this->ColorBlue  + index);
    return;
    }

  unsigned short photometric;
  
  if (!TIFFGetField(m_InternalImage->Image, TIFFTAG_PHOTOMETRIC, &photometric)) 
    {
    if ( m_InternalImage->Photometrics != PHOTOMETRIC_PALETTE )
      {
      itkExceptionMacro( << "You can only access colors for palette images" );
      return;
      }
    }
  
  unsigned short *red_orig, *green_orig, *blue_orig;
  
  switch (m_InternalImage->BitsPerSample) 
    {
    case 1: case 2: case 4:
    case 8: case 16:
        break;
    default:
      itkExceptionMacro( <<  "Sorry, can not image with " 
                     << m_InternalImage->BitsPerSample
                     << "-bit samples" );
        return;
    }
  if (!TIFFGetField(m_InternalImage->Image, TIFFTAG_COLORMAP,
                    &red_orig, &green_orig, &blue_orig)) 
    {
    itkExceptionMacro( << "Missing required \"Colormap\" tag" );
    return;
    }
  this->TotalColors = (1L << m_InternalImage->BitsPerSample);

  if ( index >= this->TotalColors )
    {
    itkExceptionMacro( << "Color index has to be less than number of colors ("
      << this->TotalColors << ")" );
    return;
    }
  this->ColorRed   =   red_orig;
  this->ColorGreen = green_orig;
  this->ColorBlue  =  blue_orig;
  
  *red   = *(red_orig   + index);
  *green = *(green_orig + index);
  *blue  = *(blue_orig  + index);
}


unsigned int TIFFImageIO::GetFormat( )
{
  unsigned int cc;  

  if ( this->ImageFormat != TIFFImageIO::NOFORMAT )
    {
    return this->ImageFormat;
    }


  switch ( m_InternalImage->Photometrics )
    {
    case PHOTOMETRIC_RGB: 
    case PHOTOMETRIC_YCBCR: 
      this->ImageFormat = TIFFImageIO::RGB_;
      return this->ImageFormat;
    case PHOTOMETRIC_MINISWHITE:
    case PHOTOMETRIC_MINISBLACK:
      this->ImageFormat = TIFFImageIO::GRAYSCALE;
      return this->ImageFormat;
    case PHOTOMETRIC_PALETTE:
      for( cc=0; cc<256; cc++ ) 
        {
        unsigned short red, green, blue;
        this->GetColor( cc, &red, &green, &blue );
        if ( red != green || red != blue )
          {
          this->ImageFormat = TIFFImageIO::PALETTE_RGB;
          return this->ImageFormat;
          }
        }
      this->ImageFormat = TIFFImageIO::PALETTE_GRAYSCALE;
      return this->ImageFormat;
    }
  this->ImageFormat = TIFFImageIO::OTHER;
  return this->ImageFormat;
}

/** Read a tiled tiff */  
void TIFFImageIO::ReadTiles(void* buffer)
{
  unsigned char* volume = reinterpret_cast<unsigned char*>(buffer);

  for(unsigned int col = 0;col<m_InternalImage->Width;col+=m_InternalImage->TileWidth)
    {
    for(unsigned int row = 0;row<m_InternalImage->Height;row+=m_InternalImage->TileHeight)
      {
      unsigned char *tempImage;
      tempImage = new unsigned char[ m_InternalImage->TileWidth * m_InternalImage->TileHeight * m_InternalImage->SamplesPerPixel];

      if(TIFFReadTile(m_InternalImage->Image,tempImage, col,row,0,0)<0)
        {
        itkExceptionMacro( << "Cannot read tile : "<< row << "," << col << " from file" );
        if ( tempImage != buffer )
          {
          delete [] tempImage;
          }
      
        return;
        }

      unsigned int xx, yy;
      for ( yy = 0; yy < m_InternalImage->TileHeight; yy++ )
        {
        for ( xx = 0; xx <  m_InternalImage->TileWidth; xx++ )
          {
          for(unsigned int i=0;i< m_InternalImage->SamplesPerPixel;i++)
            {
            *volume = *(tempImage++);
            volume++;
            }
          }
        }
      }
    }
}

/** Read a multipage tiff */  
void TIFFImageIO::ReadVolume(void* buffer)
{
  int width  = m_InternalImage->Width;
  int height = m_InternalImage->Height;

  for(unsigned int page = 0;page<m_InternalImage->NumberOfPages;page++)
    {
    if(m_InternalImage->SubFiles>0)
      {
      long subfiletype = 6;
      if(TIFFGetField(m_InternalImage->Image, TIFFTAG_SUBFILETYPE, &subfiletype))
        {
        if(subfiletype != 0)
          {
          TIFFReadDirectory(m_InternalImage->Image);
          continue;
          }
        }
      }

    // if we have a Zeiss image meaning that the SamplesPerPixel is 2
    if(m_InternalImage->SamplesPerPixel == 2)
      {
      if(m_ComponentType == USHORT)
        {
        unsigned short* volume = reinterpret_cast<unsigned short*>(buffer);
        volume += width*height*m_InternalImage->SamplesPerPixel*page;
        this->ReadTwoSamplesPerPixelImage( volume, width, height );
        }
      else
        {
        unsigned char* volume = reinterpret_cast<unsigned char*>(buffer);
        volume += width*height*m_InternalImage->SamplesPerPixel*page;
        this->ReadTwoSamplesPerPixelImage( volume, width, height );
        }
      break;
      }
    else if ( !m_InternalImage->CanRead() )
      {
      uint32 *tempImage;
      tempImage = new uint32[ width * height ];

      if ( !TIFFReadRGBAImage(m_InternalImage->Image, 
                              width, height, 
                              tempImage, 1 ) )
        {
        itkExceptionMacro( << "Cannot read TIFF image or as a TIFF RGBA image" );
        if ( tempImage != buffer )
          {
          delete [] tempImage;
          } 
        return;
        }
      int xx, yy;
      uint32* ssimage;
      
      if(m_ComponentType == USHORT)
        {
        unsigned short *fimage = (unsigned short *)buffer;
        fimage += width*height*4*page;
        for ( yy = 0; yy < height; yy ++ )
          {
          ssimage = tempImage + (height - yy - 1) * width;
          for ( xx = 0; xx < width; xx++ )
            {
            unsigned short red   = static_cast<unsigned short>(TIFFGetR(*ssimage));
            unsigned short green = static_cast<unsigned short>(TIFFGetG(*ssimage));
            unsigned short blue  = static_cast<unsigned short>(TIFFGetB(*ssimage));
            unsigned short alpha = static_cast<unsigned short>(TIFFGetA(*ssimage));
         
            *(fimage  ) = red;
            *(fimage+1) = green;
            *(fimage+2) = blue;
            *(fimage+3) = alpha;
            fimage += 4;
            ssimage ++;
            }
          }
        }
      else
        {
        unsigned char *fimage = (unsigned char *)buffer;
        fimage += width*height*4*page/2;
        for ( yy = 0; yy < height; yy ++ )
          {
          ssimage = tempImage + (height - yy - 1) * width;
          for ( xx = 0; xx < width; xx++ )
            {
            unsigned char red   = static_cast<unsigned char>(TIFFGetR(*ssimage));
            unsigned char green = static_cast<unsigned char>(TIFFGetG(*ssimage));
            unsigned char blue  = static_cast<unsigned char>(TIFFGetB(*ssimage));
            unsigned char alpha = static_cast<unsigned char>(TIFFGetA(*ssimage));
      
            *(fimage  ) = red;
            *(fimage+1) = green;
            *(fimage+2) = blue;
            *(fimage+3) = alpha;
            fimage += 4;
            ssimage ++;
            }
          }
        }
      if ( tempImage != 0 && tempImage != buffer )
        {
        delete [] tempImage;
        }
      }
    else
      {
      unsigned int format = this->GetFormat();  
    
      switch ( format )
        {
        case TIFFImageIO::GRAYSCALE:
        case TIFFImageIO::RGB_: 
        case TIFFImageIO::PALETTE_RGB:
        case TIFFImageIO::PALETTE_GRAYSCALE:
          if(m_ComponentType == USHORT)
            {
            unsigned short* volume = reinterpret_cast<unsigned short*>(buffer);
            volume += width*height*m_InternalImage->SamplesPerPixel*page;
            this->ReadGenericImage( volume, width, height );
            }
          else
            {
            unsigned char* volume = reinterpret_cast<unsigned char*>(buffer);
            volume += width*height*m_InternalImage->SamplesPerPixel*page;
            this->ReadGenericImage( volume, width, height );
            }
          break;
        default:
          return;
        }
      }
    TIFFReadDirectory(m_InternalImage->Image);
    }
}

  
void TIFFImageIO::Read(void* buffer)
{

  if ( m_InternalImage->Compression == COMPRESSION_OJPEG )
    {
    itkExceptionMacro( << "This reader cannot read old JPEG compression" );
    return;
    }

  // The IO region should be of dimensions 3 otherwise we read only the first page
  if(m_InternalImage->NumberOfPages>0 && this->GetIORegion().GetImageDimension()>2)
    {
    this->ReadVolume(buffer);
    return;
    }

  if(m_InternalImage->NumberOfTiles>0 && this->GetIORegion().GetImageDimension()>2)
    {
    this->ReadTiles(buffer);
    return;
    }


  int width  = m_InternalImage->Width;
  int height = m_InternalImage->Height;


  if ( !m_InternalImage->CanRead() )
    {
    uint32 *tempImage ;
    tempImage = new uint32[ width * height ];

    if ( !TIFFReadRGBAImage(m_InternalImage->Image, 
                            width, height, 
                            tempImage, 1 ) )
      {
      itkExceptionMacro( << "Cannot read TIFF image or as a TIFF RGBA image" );
      if ( tempImage != buffer )
        {
        delete [] tempImage;
        }
      
      return;
      }
    int xx, yy;
    uint32* ssimage;
    unsigned char *fimage = (unsigned char *)buffer;

    for ( yy = 0; yy < height; yy ++ )
      {
      ssimage = tempImage + (height - yy - 1) * width;
      for ( xx = 0; xx < width; xx++ )
        {
        unsigned char red   = static_cast<unsigned char>(TIFFGetR(*ssimage));
        unsigned char green = static_cast<unsigned char>(TIFFGetG(*ssimage));
        unsigned char blue  = static_cast<unsigned char>(TIFFGetB(*ssimage));
        unsigned char alpha = static_cast<unsigned char>(TIFFGetA(*ssimage));
          
        *(fimage  ) = red;
        *(fimage+1) = green;
        *(fimage+2) = blue;
        *(fimage+3) = alpha;
        fimage += 4;

        ssimage ++;
        }
      }
    
    if ( tempImage != 0 && tempImage != buffer )
      {
      delete [] tempImage;
      }
    return;
    }

  unsigned int format = this->GetFormat();  

  switch ( format )
    {
    case TIFFImageIO::GRAYSCALE:
    case TIFFImageIO::RGB_: 
    case TIFFImageIO::PALETTE_RGB:
    case TIFFImageIO::PALETTE_GRAYSCALE:
      this->ReadGenericImage( buffer, width, height );
      break;
    default:
      return;
    }
}


TIFFImageIO::TIFFImageIO()
{
  this->SetNumberOfDimensions(2);
  m_PixelType = SCALAR;
  m_ComponentType = UCHAR;
  
  this->InitializeColors();
  m_InternalImage = new TIFFReaderInternal;

  m_Spacing[0] = 1.0;
  m_Spacing[1] = 1.0;
  
  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;

  m_Compression = TIFFImageIO::PackBits;
}

TIFFImageIO::~TIFFImageIO()
{
  m_InternalImage->Clean();
  delete this->m_InternalImage;
}

void TIFFImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Compression: " << m_Compression << "\n";
}

void TIFFImageIO::InitializeColors()
{
  this->ColorRed    = 0;
  this->ColorGreen  = 0;
  this->ColorBlue   = 0;
  this->TotalColors = -1;  
  this->ImageFormat = TIFFImageIO::NOFORMAT;
}
  
void TIFFImageIO::ReadImageInformation()
{
  // If the internal image was not open we open it.
  // This is usually done when the user sets the ImageIO manually
  if(!m_InternalImage->IsOpen)
    {
    if(!this->CanReadFile(this->m_FileName.c_str()))
      {
      itkExceptionMacro(<<"Cannot open the file!");
      return;
      }
    }
 
  m_Spacing[0] = 1.0;  // We'll look for TIFF pixel size information later,
  m_Spacing[1] = 1.0;  // but set the defaults now

  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;


  int width  = m_InternalImage->Width;
  int height = m_InternalImage->Height;

  m_Dimensions[0] = width;
  m_Dimensions[1] = height;

  switch ( this->GetFormat() )
    {
    case TIFFImageIO::GRAYSCALE:
    case TIFFImageIO::PALETTE_GRAYSCALE:
      this->SetNumberOfComponents( 1 );
      this->SetPixelType(SCALAR);
      break;
    case TIFFImageIO::RGB_:      
      this->SetNumberOfComponents( m_InternalImage->SamplesPerPixel );
      this->SetPixelType(RGB);
      break;
    case TIFFImageIO::PALETTE_RGB:      
      this->SetNumberOfComponents( 3 );
      this->SetPixelType(RGB);
      break;
    default:
      this->SetNumberOfComponents( 4 );
      this->SetPixelType(RGBA);
    }

  if ( !m_InternalImage->CanRead() )
    {
    this->SetNumberOfComponents( 4 );
    this->SetPixelType(RGBA);
    }

  if (m_InternalImage->BitsPerSample <= 8)
    {
    m_ComponentType = UCHAR;
    }
  else
    {
    m_ComponentType = USHORT;
    }

  // We check if we have a Zeiss image. 
  // Meaning that the SamplesPerPixel is 2 but the image should be treated as
  // an RGB image.
  if(m_InternalImage->SamplesPerPixel == 2)
    {
    this->SetNumberOfComponents(3);
    this->SetPixelType(RGB);
    }

   // if the tiff file is multi-pages
   if(m_InternalImage->NumberOfPages>0)
     {
     this->SetNumberOfDimensions(3);
     if(m_InternalImage->SubFiles>0)
       {
       m_Dimensions[2] = m_InternalImage->SubFiles;
       }
     else
       {
       m_Dimensions[2] = m_InternalImage->NumberOfPages;
       }
     m_Spacing[2] = 1.0;
     m_Origin[2] = 0.0;
     }

   // if the tiff is tiled
   if(m_InternalImage->NumberOfTiles>0)
     {
     this->SetNumberOfDimensions(3);
     m_Dimensions[0] = m_InternalImage->TileWidth;
     m_Dimensions[1] = m_InternalImage->TileHeight;
     m_Dimensions[2] = m_InternalImage->NumberOfTiles;
     m_Spacing[2] = 1.0;
     m_Origin[2] = 0.0;
     }

  return;
}

bool TIFFImageIO::CanWriteFile( const char * name )
{
  std::string filename = name;

  if (filename == "")
    {
    return false;
    }
  
  std::string::size_type TIFFPos = filename.rfind(".TIFF");
  if ( (TIFFPos != std::string::npos)
       && (TIFFPos == filename.length() - 5) )
    {
    return true;
    }

  TIFFPos = filename.rfind(".tiff");
  if ( (TIFFPos != std::string::npos)
       && (TIFFPos == filename.length() - 5) )
    {
    return true;
    }

  TIFFPos = filename.rfind(".tif");
  if ( (TIFFPos != std::string::npos)
       && (TIFFPos == filename.length() - 4) )
    {
    return true;
    }


  TIFFPos = filename.rfind(".TIF");
  if ( (TIFFPos != std::string::npos)
       && (TIFFPos == filename.length() - 4) )
    {
    return true;
    }


  return false;
}


void TIFFImageIO::WriteImageInformation()
{
}

void TIFFImageIO::Write(const void* buffer)
{
  if( m_NumberOfDimensions == 2 || m_NumberOfDimensions == 3 )
    {
    this->InternalWrite(buffer);
    }
  else
    {
    itkExceptionMacro(<<"TIFF Writer can only write 2-d or 3-d images");
    }
}



class TIFFWriterIO
{
public:
  // Writing file no reading
  static tsize_t TIFFRead(thandle_t, tdata_t, tsize_t) { return 0; }

  // Write data
  static tsize_t TIFFWrite(thandle_t fd, tdata_t buf, tsize_t size) 
    {
    std::ostream *out = reinterpret_cast<std::ostream *>(fd);
    out->write(static_cast<char *>(buf), size);
    return out->fail() ? static_cast<tsize_t>(0) : size;
    }

  static toff_t TIFFSeek(thandle_t fd, toff_t off, int whence) 
    {
#ifdef __sgi
    int need;
#endif
    std::ostream *out = reinterpret_cast<std::ostream *>(fd);
    switch (whence) 
      {
      case SEEK_SET:
#ifdef __sgi
        out->seekp(0, std::ios::end);
        need = static_cast<int>(off) - static_cast<int>(out->tellp());
        if (need > 0)
          {
          char buf[1] = {'\0'};
          for (int ii = 0; ii < need; ii++) out->write(buf,1);
          }
#endif
        out->seekp(off, std::ios::beg);
        break;
      case SEEK_END:
        out->seekp(off, std::ios::end);
        break;
      case SEEK_CUR:
        out->seekp(off, std::ios::cur);
        break;
      default:
        return out->tellp();
      }
    return out->tellp();
    }

  // File will be closed by the superclass
  static int TIFFClose(thandle_t) { return 1; }

  static toff_t TIFFSize(thandle_t fd) 
    {
    std::ostream *out = reinterpret_cast<std::ostream *>(fd);
    out->seekp(0, std::ios::end);
    return out->tellp();
    }

  static int TIFFMapFile(thandle_t, tdata_t*, toff_t*) { return (0); }
  static void TIFFUnmapFile(thandle_t, tdata_t, toff_t) {}
};



void TIFFImageIO::InternalWrite(const void* buffer)
{
  unsigned char *outPtr = (unsigned char *) buffer;

  unsigned int width, height, page, pages = 1;
  width =  m_Dimensions[0];
  height = m_Dimensions[1];  
  if( m_NumberOfDimensions == 3 )
    {
    pages = m_Dimensions[2];
    }

  int scomponents = this->GetNumberOfComponents();
  double resolution = -1;
  uint32 rowsperstrip = (uint32) -1;
  int bps;

  switch (this->GetComponentType())
    {
  case UCHAR:
    bps = 8;
    break;

  case USHORT:
    bps = 16;
    break;

  default:
    itkExceptionMacro(<<"TIFF supports unsigned char and unsigned short");
    }

  int predictor;

  TIFF *tif = TIFFOpen(m_FileName.c_str(), "w");
  if ( !tif )
    {
    itkDebugMacro( << "Returning" );
    return;
    }

  uint32 w = width;
  uint32 h = height;

  if( m_NumberOfDimensions == 3)
    {
    TIFFCreateDirectory( tif );
    }
  for (page = 0; page < pages; page++)
    {
    TIFFSetDirectory( tif, page );
    TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, w);
    TIFFSetField(tif, TIFFTAG_IMAGELENGTH, h);
    TIFFSetField(tif, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
    TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, scomponents);
    TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, bps); // Fix for stype
    TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
    TIFFSetField(tif, TIFFTAG_SOFTWARE, "InsightToolkit");

    if ( scomponents > 3 )
      {
      // if number of scalar components is greater than 3, that means we assume
      // there is alpha.
      uint16 extra_samples = scomponents-3;
      uint16 *sample_info = new uint16[scomponents-3];
      sample_info[0]=EXTRASAMPLE_ASSOCALPHA;
      int cc;
      for ( cc = 1; cc < scomponents-3; cc ++ )
        {
        sample_info[cc] = EXTRASAMPLE_UNSPECIFIED;
        }
      TIFFSetField(tif,TIFFTAG_EXTRASAMPLES,extra_samples,
        sample_info);
      delete [] sample_info;
      }

    int compression;

    if(m_UseCompression)
      {
      switch ( m_Compression )
        {
      case TIFFImageIO::PackBits: compression = COMPRESSION_PACKBITS; break;
      case TIFFImageIO::JPEG:     compression = COMPRESSION_JPEG; break;
      case TIFFImageIO::Deflate:  compression = COMPRESSION_DEFLATE; break;
      case TIFFImageIO::LZW:      compression = COMPRESSION_LZW; break;
      default: compression = COMPRESSION_NONE;
        }
      }
    else
      {
      compression = COMPRESSION_NONE;
      }

    TIFFSetField(tif, TIFFTAG_COMPRESSION, compression); // Fix for compression

    uint16 photometric = (scomponents ==1) ? PHOTOMETRIC_MINISBLACK : PHOTOMETRIC_RGB;

    if ( compression == COMPRESSION_JPEG )
      {
      TIFFSetField(tif, TIFFTAG_JPEGQUALITY, 75); // Parameter
      TIFFSetField(tif, TIFFTAG_JPEGCOLORMODE, JPEGCOLORMODE_RGB);
      photometric = PHOTOMETRIC_YCBCR;
      }
    else if ( compression == COMPRESSION_LZW )
      {
      predictor = 2;
      TIFFSetField(tif, TIFFTAG_PREDICTOR, predictor);
      itkDebugMacro( << "LZW compression is patented outside US so it is disabled" );
      }
    else if ( compression == COMPRESSION_DEFLATE )
      {
      predictor = 2;
      TIFFSetField(tif, TIFFTAG_PREDICTOR, predictor);
      }

    TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, photometric); // Fix for scomponents

    TIFFSetField(tif,
      TIFFTAG_ROWSPERSTRIP,
      TIFFDefaultStripSize(tif, rowsperstrip));
    if (resolution > 0)
      {
      TIFFSetField(tif, TIFFTAG_XRESOLUTION, resolution);
      TIFFSetField(tif, TIFFTAG_YRESOLUTION, resolution);
      TIFFSetField(tif, TIFFTAG_RESOLUTIONUNIT, RESUNIT_INCH);
      }

    if( m_NumberOfDimensions == 3)
      {
      // We are writing single page of the multipage file
      TIFFSetField(tif, TIFFTAG_SUBFILETYPE, FILETYPE_PAGE);
      // Set the page number
      TIFFSetField(tif, TIFFTAG_PAGENUMBER, page, pages);
      }
    int rowLength; // in bytes

    switch (this->GetComponentType())
      {
    case UCHAR:
      rowLength = sizeof(unsigned char); 
      break;
    case USHORT:
      rowLength = sizeof(unsigned short);
      break;
    default:
      itkExceptionMacro(<<"TIFF supports unsigned char and unsigned short");
      }

    rowLength *= this->GetNumberOfComponents();
    rowLength *= width;

    int row = 0;
    for (unsigned int idx2 = 0; idx2 < height; idx2++)
      {
      if ( TIFFWriteScanline(tif, const_cast<unsigned char*>(outPtr), row, 0) < 0)
        {
        itkExceptionMacro( << "TIFFImageIO: error out of disk space" );
        break;
        }
      outPtr += rowLength;
      row ++;
      }

    if(m_NumberOfDimensions == 3 )
      {
      TIFFWriteDirectory(tif);
      }
    }
  TIFFClose(tif);
}

bool TIFFImageIO::CanFindTIFFTag( unsigned int t )
{
  // m_InternalImage needs to be valid
  if( !m_InternalImage )
    {
    itkExceptionMacro( << "Need to call CanReadFile before" );
    return false;
    }

  ttag_t tag = t; // 32bits integer
  const TIFFFieldInfo *fld = TIFFFieldWithTag( m_InternalImage->Image, tag );
  if( fld == NULL )
    {
    return false;
    }
  return true;
}

void *TIFFImageIO::ReadRawByteFromTag( unsigned int t )
{
  // m_InternalImage needs to be valid
  if( !m_InternalImage )
    {
    itkExceptionMacro( << "Need to call CanReadFile before" );
    return NULL;
    }
  ttag_t tag = t;
  void *raw_data = NULL;
  const TIFFFieldInfo *fld = TIFFFieldWithTag( m_InternalImage->Image, tag );
  if( fld == NULL )
    {
    itkExceptionMacro( << "fld is NULL" );
    return NULL;
    }
  else
    {
    if( fld->field_passcount )
      {
      short value_count;

      if( TIFFGetField( m_InternalImage->Image, tag, &value_count, &raw_data ) != 1 )
        {
        itkExceptionMacro( << "Tag cannot be found" );
        return NULL;
        }
      else
        {
        if( fld->field_type != TIFF_BYTE )
          {
          itkExceptionMacro( << "Tag is not of type TIFF_BYTE" );
          return NULL;
          }
        }
      }
    }
  return raw_data;
}

} // end namespace itk

