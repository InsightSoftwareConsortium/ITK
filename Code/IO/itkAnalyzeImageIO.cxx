/*=========================================================================
Program:   Insight Segmentation & Registration Toolkit
Module:    itkAnalyzeImageIO.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkAnalyzeImageIO.h"
#include "itkIOCommon.h"
#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include "itkMetaDataObject.h"

#include "dbh.h"
#include <zlib.h>
#include <stdio.h>
#include <stdlib.h>

//An array of the Analyze v7.5 known DataTypes
const char DataTypes[12][10]=  {
  "UNKNOWN","BINARY","CHAR","SHORT", "INT","FLOAT",
  "COMPLEX", "DOUBLE","RGB","ALL","USHORT","UINT"
};

//An array with the corresponding number of bits for each image type.
//NOTE: the following two line should be equivalent.
const short int DataTypeSizes[12]={0,1,8,16,32,32,64,64,24,0,16,32};

//An array with Data type key sizes
const short int DataTypeKey[12]={
  ANALYZE_DT_UNKNOWN,
  ANALYZE_DT_BINARY,
  ANALYZE_DT_UNSIGNED_CHAR,
  ANALYZE_DT_SIGNED_SHORT,
  ANALYZE_DT_SIGNED_INT,
  ANALYZE_DT_FLOAT,
  ANALYZE_DT_COMPLEX,
  ANALYZE_DT_DOUBLE,
  ANALYZE_DT_RGB,
  ANALYZE_DT_ALL,
  ANALYZE_DT_UNSIGNED_SHORT,
  ANALYZE_DT_UNSIGNED_INT
};


//The following was inserted based on Bill Hoffman's CMake
//implementation.
#if defined(_WIN32) && (defined(_MSC_VER) || defined(__BORLANDC__))
#include <stdlib.h>
#define _unlink unlink
#else
#include <unistd.h>
#endif
/**
 * \author Kent Williams <Bill Hoffman>
 * Remove file with name fname from the file system
 * \param fname  The name of the file to remove
 * \return true if successful, false if not successful.
 */
//NOTE: After further testing, this should probably be added to the
//Directory manipulation section of ITK.
static inline bool RemoveFile(const char *fname)
{
  return (unlink(fname) != 0)? false: true;
}

//GetExtension from uiig library.
static std::string
GetExtension( const std::string& filename ) {

  // This assumes that the final '.' in a file name is the delimiter
  // for the file's extension type
  const std::string::size_type it = filename.find_last_of( "." );

  // This determines the file's type by creating a new string
  // who's value is the extension of the input filename
  // eg. "myimage.gif" has an extension of "gif"
  std::string fileExt( filename, it+1, filename.length() );

  return( fileExt );
}


//GetRootName from uiig library.
static std::string
GetRootName( const std::string& filename )
{
  const std::string fileExt = GetExtension(filename);

  // Create a base filename
  // i.e Image.hdr --> Image
  if( fileExt.length() > 0 )
  {
    const std::string::size_type it = filename.find_last_of( fileExt );
    std::string baseName( filename, 0, it-fileExt.length() );
    return( baseName );
  }
  else
  {
    // Case when the extension is nothing (Analyze)
    return( filename );
  }
}


static std::string
GetHeaderFileName( const std::string & filename )
{
  std::string ImageFileName = GetRootName(filename);
  std::string fileExt = GetExtension(filename);
  //If file was named xxx.img.gz then remove both the gz and the img endings.
  if(!fileExt.compare("gz"))
  {
    ImageFileName=GetRootName(GetRootName(filename));
  }
  ImageFileName += ".hdr";
  return( ImageFileName );
}

//Returns the base image filename.
static std::string GetImageFileName( const std::string& filename )
{
  // Why do we add ".img" here?  Look in fileutils.h
  std::string fileExt = GetExtension(filename);
  std::string ImageFileName = GetRootName(filename);
  if(!fileExt.compare("gz"))
  {
    //First strip both extensions off
    ImageFileName=GetRootName(GetRootName(filename));
    ImageFileName += ".img.gz";
  }
  else if(!fileExt.compare("img") || !fileExt.compare("hdr") )
  {
    ImageFileName += ".img";
  }
  else
  {
    //uiig::Reporter* reporter = uiig::Reporter::getReporter();
    //std::string temp="Error, Can not determine compressed file image name. ";
    //temp+=filename;
    //reporter->setMessage( temp );
    return ("");
  }
  return( ImageFileName );
}


namespace itk
{
  void
    AnalyzeImageIO::SwapBytesIfNecessary( void* buffer,
        unsigned long numberOfPixels )
    {
      if ( m_ByteOrder == LittleEndian )
      {
        switch(m_PixelType)
        {
          case CHAR:
            ByteSwapper<char>::SwapRangeFromSystemToLittleEndian((char*)buffer,
                numberOfPixels );
            break;
          case UCHAR:
            ByteSwapper<unsigned char>::SwapRangeFromSystemToLittleEndian
              ((unsigned char*)buffer, numberOfPixels );
            break;
          case SHORT:
            ByteSwapper<short>::SwapRangeFromSystemToLittleEndian
              ((short*)buffer, numberOfPixels );
            break;
          case USHORT:
            ByteSwapper<unsigned short>::SwapRangeFromSystemToLittleEndian
              ((unsigned short*)buffer, numberOfPixels );
            break;
          case INT:
            ByteSwapper<int>::SwapRangeFromSystemToLittleEndian
              ((int*)buffer, numberOfPixels );
            break;
          case UINT:
            ByteSwapper<unsigned int>::SwapRangeFromSystemToLittleEndian
              ((unsigned int*)buffer, numberOfPixels );
            break;
          case LONG:
            ByteSwapper<long>::SwapRangeFromSystemToLittleEndian
              ((long*)buffer, numberOfPixels );
            break;
          case ULONG:
            ByteSwapper<unsigned long>::SwapRangeFromSystemToLittleEndian
              ((unsigned long*)buffer, numberOfPixels );
            break;
          case FLOAT:
            ByteSwapper<float>::SwapRangeFromSystemToLittleEndian((float*)buffer,
                numberOfPixels );
            break;
          case DOUBLE:
            ByteSwapper<double>::SwapRangeFromSystemToLittleEndian
              ((double*)buffer, numberOfPixels );
            break;
          default:
            ExceptionObject exception(__FILE__, __LINE__);
            exception.SetDescription("Pixel Type Unknown");
            throw exception;
        }
      }
      else
      {
        switch(m_PixelType)
        {
          case CHAR:
            ByteSwapper<char>::SwapRangeFromSystemToBigEndian((char *)buffer,
                numberOfPixels );
            break;
          case UCHAR:
            ByteSwapper<unsigned char>::SwapRangeFromSystemToBigEndian
              ((unsigned char *)buffer, numberOfPixels );
            break;
          case SHORT:
            ByteSwapper<short>::SwapRangeFromSystemToBigEndian
              ((short *)buffer, numberOfPixels );
            break;
          case USHORT:
            ByteSwapper<unsigned short>::SwapRangeFromSystemToBigEndian
              ((unsigned short *)buffer, numberOfPixels );
            break;
          case INT:
            ByteSwapper<int>::SwapRangeFromSystemToBigEndian
              ((int *)buffer, numberOfPixels );
            break;
          case UINT:
            ByteSwapper<unsigned int>::SwapRangeFromSystemToBigEndian
              ((unsigned int *)buffer, numberOfPixels );
            break;
          case LONG:
            ByteSwapper<long>::SwapRangeFromSystemToBigEndian
              ((long *)buffer, numberOfPixels );
            break;
          case ULONG:
            ByteSwapper<unsigned long>::SwapRangeFromSystemToBigEndian
              ((unsigned long *)buffer, numberOfPixels );
            break;
          case FLOAT:
            ByteSwapper<float>::SwapRangeFromSystemToBigEndian
              ((float *)buffer, numberOfPixels );
            break;
          case DOUBLE:
            ByteSwapper<double>::SwapRangeFromSystemToBigEndian
              ((double *)buffer, numberOfPixels );
            break;
          default:
            ExceptionObject exception(__FILE__, __LINE__);
            exception.SetDescription("Pixel Type Unknown");
            throw exception;
        }
      }
    }

  ImageIOBase::ByteOrder
    AnalyzeImageIO::CheckAnalyzeEndian(const struct dsr &temphdr)
    {
      ImageIOBase::ByteOrder returnvalue;
      // Machine and header endianess is same

      //checking hk.extents only is NOT a good idea. Many programs do not set
      //hk.extents correctly. Doing an additional check on hk.sizeof_hdr
      //increases chance of correct result. --Juerg Tschirrin Univeristy of Iowa
      //All properly constructed analyze images should have the extents feild
      //set.  It is part of the file format standard.  While most headers of
      //analyze images are 348 bytes long, The Analyze file format allows the
      //header to have other lengths.
      //This code will fail in the unlikely event that the extents feild is
      //not set (invalid anlyze file anyway) and the header is not the normal
      //size.  Other peices of code have used a heuristic on the image
      //dimensions.  If the Image dimensions is greater
      //than 16000 then the image is almost certainly byte-swapped-- Hans

      const ImageIOBase::ByteOrder systemOrder= (ByteSwapper<int>::SystemIsBigEndian()) ? BigEndian : LittleEndian;

      if((temphdr.hk.extents == 16384) || (temphdr.hk.sizeof_hdr == 348))
      {
        returnvalue = systemOrder;
      }
      else
      {
        // File does not match machine
        returnvalue = (systemOrder == BigEndian ) ? LittleEndian : BigEndian;
      }
      return returnvalue;
    }

  void
    AnalyzeImageIO::SwapHeaderBytesIfNecessary( struct dsr * const imageheader )
    {
      if ( m_ByteOrder == LittleEndian )
      {
        // NOTE: If machine order is little endian, and the data needs to be
        // swapped, the SwapFromBigEndianToSystem is equivalent to
        // SwapFromSystemToBigEndian.
        ByteSwapper<int>::SwapFromSystemToLittleEndian(&imageheader->hk.sizeof_hdr);
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToLittleEndian( (&imageheader->hk.data_type[0]),10 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToLittleEndian( &(imageheader->hk.db_name[0]),18 );
        ByteSwapper<int  >::SwapFromSystemToLittleEndian( &imageheader->hk.extents );
        ByteSwapper<short int>::SwapFromSystemToLittleEndian( &imageheader->hk.session_error );
        //Here for completeness ByteSwapper<char >::SwapFromSystemToLittleEndian( &imageheader->hk.regular );
        //Here for completeness ByteSwapper<char >::SwapFromSystemToLittleEndian( &imageheader->hk.hkey_un0 );

        ByteSwapper<short int>::SwapRangeFromSystemToLittleEndian( &imageheader->dime.dim[0], 8 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToLittleEndian( &(imageheader->hk.vox_units[0]),4 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToLittleEndian( &(imageheader->hk.cal_units[0]),8 );
        ByteSwapper<short int>::SwapFromSystemToLittleEndian( &imageheader->dime.unused1 );
        ByteSwapper<short int>::SwapFromSystemToLittleEndian( &imageheader->dime.datatype );
        ByteSwapper<short int>::SwapFromSystemToLittleEndian( &imageheader->dime.bitpix );
        ByteSwapper<short int>::SwapFromSystemToLittleEndian( &imageheader->dime.dim_un0 );

        ByteSwapper<float>::SwapRangeFromSystemToLittleEndian( &imageheader->dime.pixdim[0],8 );
        ByteSwapper<float>::SwapFromSystemToLittleEndian( &imageheader->dime.vox_offset );
        ByteSwapper<float>::SwapFromSystemToLittleEndian( &imageheader->dime.roi_scale );
        ByteSwapper<float>::SwapFromSystemToLittleEndian( &imageheader->dime.funused1 );
        ByteSwapper<float>::SwapFromSystemToLittleEndian( &imageheader->dime.funused2 );
        ByteSwapper<float>::SwapFromSystemToLittleEndian( &imageheader->dime.cal_max );
        ByteSwapper<float>::SwapFromSystemToLittleEndian( &imageheader->dime.cal_min );
        ByteSwapper<int>::SwapFromSystemToLittleEndian( &imageheader->dime.compressed );
        ByteSwapper<int>::SwapFromSystemToLittleEndian( &imageheader->dime.verified );
        ByteSwapper<int>::SwapFromSystemToLittleEndian( &imageheader->dime.glmax );
        ByteSwapper<int>::SwapFromSystemToLittleEndian( &imageheader->dime.glmin );

        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToLittleEndian( &(imageheader->hk.descrip[0]),80 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToLittleEndian( &(imageheader->hk.aux_file[0]),24 );
        //Here for completeness ByteSwapper<char >::SwapFromSystemToLittleEndian( &(imageheader->hk.orient );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToLittleEndian( &(imageheader->hk.originator[0]),24 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToLittleEndian( &(imageheader->hk.generated[0]),24 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToLittleEndian( &(imageheader->hk.scannum[0]),24 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToLittleEndian( &(imageheader->hk.patient_id[0]),24 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToLittleEndian( &(imageheader->hk.exp_date[0]),24 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToLittleEndian( &(imageheader->hk.exp_time[0]),24 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToLittleEndian( &(imageheader->hk.hist_un0[0]),24 );
        ByteSwapper<int>::SwapFromSystemToLittleEndian( &imageheader->hist.views );
        ByteSwapper<int>::SwapFromSystemToLittleEndian( &imageheader->hist.vols_added );
        ByteSwapper<int>::SwapFromSystemToLittleEndian( &imageheader->hist.start_field );
        ByteSwapper<int>::SwapFromSystemToLittleEndian( &imageheader->hist.field_skip );
        ByteSwapper<int>::SwapFromSystemToLittleEndian( &imageheader->hist.omax );
        ByteSwapper<int>::SwapFromSystemToLittleEndian( &imageheader->hist.omin );
        ByteSwapper<int>::SwapFromSystemToLittleEndian( &imageheader->hist.smax );
        ByteSwapper<int>::SwapFromSystemToLittleEndian( &imageheader->hist.smin );
      }
      else if ( m_ByteOrder == BigEndian )
      {
        //NOTE: If machine order is little endian, and the data needs to be
        // swapped, the SwapFromBigEndianToSystem is equivalent to
        // SwapFromSystemToLittleEndian.
        ByteSwapper<int  >::SwapFromSystemToBigEndian( &imageheader->hk.sizeof_hdr );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToBigEndian( (&imageheader->hk.data_type[0]),10 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToBigEndian( &(imageheader->hk.db_name[0]),18 );
        ByteSwapper<int  >::SwapFromSystemToBigEndian( &imageheader->hk.extents );
        ByteSwapper<short int>::SwapFromSystemToBigEndian( &imageheader->hk.session_error );
        //Here for completeness ByteSwapper<char >::SwapFromSystemToBigEndian( &imageheader->hk.regular );
        //Here for completeness ByteSwapper<char >::SwapFromSystemToBigEndian( &imageheader->hk.hkey_un0 );

        ByteSwapper<short int>::SwapRangeFromSystemToBigEndian( &imageheader->dime.dim[0], 8 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToBigEndian( &(imageheader->hk.vox_units[0]),4 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToBigEndian( &(imageheader->hk.cal_units[0]),8 );
        ByteSwapper<short int>::SwapFromSystemToBigEndian( &imageheader->dime.unused1 );
        ByteSwapper<short int>::SwapFromSystemToBigEndian( &imageheader->dime.datatype );
        ByteSwapper<short int>::SwapFromSystemToBigEndian( &imageheader->dime.bitpix );
        ByteSwapper<short int>::SwapFromSystemToBigEndian( &imageheader->dime.dim_un0 );

        ByteSwapper<float>::SwapRangeFromSystemToBigEndian( &imageheader->dime.pixdim[0],8 );
        ByteSwapper<float>::SwapFromSystemToBigEndian( &imageheader->dime.vox_offset );
        ByteSwapper<float>::SwapFromSystemToBigEndian( &imageheader->dime.roi_scale );
        ByteSwapper<float>::SwapFromSystemToBigEndian( &imageheader->dime.funused1 );
        ByteSwapper<float>::SwapFromSystemToBigEndian( &imageheader->dime.funused2 );
        ByteSwapper<float>::SwapFromSystemToBigEndian( &imageheader->dime.cal_max );
        ByteSwapper<float>::SwapFromSystemToBigEndian( &imageheader->dime.cal_min );
        ByteSwapper<int>::SwapFromSystemToBigEndian( &imageheader->dime.compressed );
        ByteSwapper<int>::SwapFromSystemToBigEndian( &imageheader->dime.verified );
        ByteSwapper<int>::SwapFromSystemToBigEndian( &imageheader->dime.glmax );
        ByteSwapper<int>::SwapFromSystemToBigEndian( &imageheader->dime.glmin );

        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToBigEndian( &(imageheader->hk.descrip[0]),80 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToBigEndian( &(imageheader->hk.aux_file[0]),24 );
        //Here for completeness ByteSwapper<char >::SwapFromSystemToBigEndian( &(imageheader->hk.orient );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToBigEndian( &(imageheader->hk.originator[0]),24 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToBigEndian( &(imageheader->hk.generated[0]),24 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToBigEndian( &(imageheader->hk.scannum[0]),24 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToBigEndian( &(imageheader->hk.patient_id[0]),24 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToBigEndian( &(imageheader->hk.exp_date[0]),24 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToBigEndian( &(imageheader->hk.exp_time[0]),24 );
        //Here for completeness ByteSwapper<char >::SwapRangeFromSystemToBigEndian( &(imageheader->hk.hist_un0[0]),24 );
        ByteSwapper<int>::SwapFromSystemToBigEndian( &imageheader->hist.views );
        ByteSwapper<int>::SwapFromSystemToBigEndian( &imageheader->hist.vols_added );
        ByteSwapper<int>::SwapFromSystemToBigEndian( &imageheader->hist.start_field );
        ByteSwapper<int>::SwapFromSystemToBigEndian( &imageheader->hist.field_skip );
        ByteSwapper<int>::SwapFromSystemToBigEndian( &imageheader->hist.omax );
        ByteSwapper<int>::SwapFromSystemToBigEndian( &imageheader->hist.omin );
        ByteSwapper<int>::SwapFromSystemToBigEndian( &imageheader->hist.smax );
        ByteSwapper<int>::SwapFromSystemToBigEndian( &imageheader->hist.smin );
      }
      else
      {
        ExceptionObject exception(__FILE__, __LINE__);
        exception.SetDescription("Machine Endian Type Unknown");
        throw exception;
      }
    }


  AnalyzeImageIO::AnalyzeImageIO()
  {
    //by default, only have 3 dimensions
    this->SetNumberOfDimensions(3);
    m_PixelType         = UCHAR;
    // Set m_MachineByteOrder to the ByteOrder of the machine
    // Start out with file byte order == system byte order
    // this will be changed if we're reading a file to whatever
    // the file actually contains.
    if(ByteSwapper<int>::SystemIsBigEndian())
      m_MachineByteOrder = m_ByteOrder = BigEndian;
    else
      m_MachineByteOrder = m_ByteOrder = LittleEndian;

    // Set all values to a default value
    // Must check again -- memset!!!!

    //Analyze stuff
    //  memset sets the first n bytes in memory area s to the value of c
    //  (cothis->m_hdr.dime.dim[4]erted to an unsigned char).  It returns s.
    //  void *memset (void *s, int c, size_t n);
    memset(&(this->m_hdr),0, sizeof(struct dsr));

    //strcpy(this->m_hdr.hk.data_type,DataTypes[DT_INDEX_UNKNOWN]);
    /*Acceptable this->m_hdr.hk.data_type values are */
    /*"UNKNOWN","BINARY","CHAR","SHORT","INT","FLOAT","COMPLEX","DOUBLE","RGB" */
    this->m_hdr.hk.sizeof_hdr=sizeof(struct dsr);
    this->m_hdr.hk.db_name[0]='\0';
    this->m_hdr.hk.extents=16384;
    this->m_hdr.hk.session_error=0;
    this->m_hdr.hk.regular='r';
    this->m_hdr.hk.hkey_un0='\0';

    /*HeaderObj_dimension information*/
    this->m_hdr.dime.dim[0]=4;     //Usually 4 x,y,z,time
    this->m_hdr.dime.dim[1]=1;     //size_x;//number of columns
    this->m_hdr.dime.dim[2]=1;     //size_y;//number of rows
    this->m_hdr.dime.dim[3]=1;     //size_z;//number of slices
    this->m_hdr.dime.dim[4]=1;     //size_t;//number of volumes
    this->m_hdr.dime.dim[5]=1;
    this->m_hdr.dime.dim[6]=1;
    this->m_hdr.dime.dim[7]=1;

    /*labels voxel spatial unit */
    this->m_hdr.dime.vox_units[0]='\0';
    /*labels voxel calibration unit */
    this->m_hdr.dime.cal_units[0]='\0';

    this->m_hdr.dime.unused1=0;
    // Acceptable data values are DT_NONE, DT_UNKOWN, DT_BINARY,
    // DT_UNSIGNED_CHAR
    // DT_SIGNED_SHORT, DT_SIGNED_INT, DT_FLOAT, DT_COMPLEX, DT_DOUBLE,
    // DT_RGB, DT_ALL
    //this->m_hdr.dime.datatype=DataTypeKey[DT_INDEX_UNKNOWN];

    //this->m_hdr.dime.bitpix=DataTypeSizes[DT_INDEX_UNKNOWN];/*bits per pixel*/
    this->m_hdr.dime.dim_un0=0;

    //Set the voxel dimension fields:
    //A value of 0.0 for these fields implies that the value is unknown.
    //Change these values to what is appropriate for your data
    //or pass additional commathis->m_hdr.dime.dim[0] line arguments
    this->m_hdr.dime.pixdim[0]=0.0;//Unused field
    this->m_hdr.dime.pixdim[1]=1.0;//x_dimension
    this->m_hdr.dime.pixdim[2]=1.0;//y_dimension
    this->m_hdr.dime.pixdim[3]=1.0;//z_dimension
    this->m_hdr.dime.pixdim[4]=1.0;//t_dimension
    this->m_hdr.dime.pixdim[5]=1.0;
    this->m_hdr.dime.pixdim[6]=1.0;
    this->m_hdr.dime.pixdim[7]=1.0;
    // Assume zero offset in .img file, byte at which pixel data starts in
    // the HeaderObj file
    //byte offset in the HeaderObj file which voxels start
    this->m_hdr.dime.vox_offset=0.0;

    this->m_hdr.dime.roi_scale=0.0;
    this->m_hdr.dime.funused1=0.0;
    this->m_hdr.dime.funused2=0.0;
    this->m_hdr.dime.cal_max=0.0;  /*specify range of calibration values*/
    this->m_hdr.dime.cal_min=0.0;  /*specify range of calibration values*/
    this->m_hdr.dime.compressed=0; /*specify that the data file with extension .img is not compressed*/
    this->m_hdr.dime.verified=0;
    this->m_hdr.dime.glmax=0;      /*max value for all of the data set*/
    this->m_hdr.dime.glmin=0;      /*min value for all of the data set*/

    /*data_history*/
    this->m_hdr.hist.descrip[0]='\0';
    this->m_hdr.hist.aux_file[0]='\0';
    /*Acceptable values are*/
    /*0-transverse unflipped*/
    /*1-coronal unflipped*/
    /*2-sagittal unfipped*/
    /*3-transverse flipped*/
    /*4-coronal flipped*/
    /*5-sagittal flipped*/
    this->m_hdr.hist.orient=itk::IOCommon::ITK_ORIENTATION_IRP_TRANSVERSE; //default orientation is ITK_ANALYZE_TRANSVERSE

    this->m_hdr.hist.originator[0]='\0';
    this->m_hdr.hist.generated[0]='\0';
    this->m_hdr.hist.scannum[0]='\0';
    this->m_hdr.hist.patient_id[0]='\0';
    this->m_hdr.hist.exp_date[0]='\0';
    this->m_hdr.hist.exp_time[0]='\0';
    this->m_hdr.hist.hist_un0[0]='\0';
    this->m_hdr.hist.views=0;
    this->m_hdr.hist.vols_added=0;
    this->m_hdr.hist.start_field=0;
    this->m_hdr.hist.field_skip=0;
    this->m_hdr.hist.omax=0;
    this->m_hdr.hist.omin=0;
    this->m_hdr.hist.smax=0;
    this->m_hdr.hist.smin=0;
  }

  AnalyzeImageIO::~AnalyzeImageIO()
  {
    //Purposefully left blank
  }

  void AnalyzeImageIO::PrintSelf(std::ostream& os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "PixelType " << m_PixelType << "\n";
  }

  bool AnalyzeImageIO::CanWriteFile(const char * FileNameToWrite)
  {
    m_FileName=FileNameToWrite;
    if(m_FileName != "" &&
        // DataFile Name Given*/
        (m_FileName.find(".img") < m_FileName.length() ||
         // HeaderFile Name Given
         m_FileName.find(".hdr") < m_FileName.length() ||
         //Compressed Images
         m_FileName.find(".img.gz") < m_FileName.length()))
    {
      return true;
    }
    return false;
  }

  const std::type_info& AnalyzeImageIO::GetPixelType() const
  {
    switch(m_PixelType)
    {
      case CHAR:
        return typeid(char);
      case UCHAR:
        return typeid(unsigned char);
      case SHORT:
        return typeid(short);
      case USHORT:
        return typeid(unsigned short);
      case INT:
        return typeid(int);
      case UINT:
        return typeid(unsigned int);
      case LONG:
        return typeid(long);
      case ULONG:
        return typeid(unsigned long);
      case FLOAT:
        return typeid(float);
      case DOUBLE:
        return typeid(double);
      default:
        ExceptionObject exception(__FILE__, __LINE__);
        exception.SetDescription("Pixel Type Unknown");
        throw exception;
    }
  }

  unsigned int AnalyzeImageIO::GetComponentSize() const
  {
    switch(m_PixelType)
    {
      case CHAR:
        return sizeof(char);
      case UCHAR:
        return sizeof(unsigned char);
      case SHORT:
        return sizeof(short);
      case USHORT:
        return sizeof(unsigned short);
      case INT:
        return sizeof(int);
      case UINT:
        return sizeof(unsigned int);
      case LONG:
        return sizeof(long);
      case ULONG:
        return sizeof(unsigned long);
      case FLOAT:
        return sizeof(float);
      case DOUBLE:
        return sizeof(double);
      default:
        ExceptionObject exception(__FILE__, __LINE__);
        exception.SetDescription("Pixel Type Unknown");
        throw exception;
    }
    return 1;
  }

  //Set Data Type Values and min/max values
  //////////////////////////////////////////////////////////////////////////
  // Programmer: Hans J. Johnson
  //       Date: 10/29/98
  //   Function: DefineHeaderObjDataType
  //  Algorithm: Set DataType Values appropriatly
  // Func. Ret.:
  //     Output:
  //      Input: DataTypeIndex - Is one of the following
  //              DT_INDEX_UNSIGNED_CHAR
  //              DT_INDEX_SIGNED_SHORT   DT_INDEX_SIGNED_INT
  //              DT_INDEX_FLOAT          DT_INDEX_DOUBLE
  //              DT_INDEX_COMPLEX        DT_INDEX_RGB
  //              DT_INDEX_BINARY         DT_INDEX_UNKNOWN
  //////////////////////////////////////////////////////////////////////////
  void  AnalyzeImageIO::DefineHeaderObjectDataType()
  {
    enum DataTypeIndex eNewType;
    switch(m_PixelType)
    {
      case CHAR:
      case UCHAR:
        eNewType=ANALYZE_DT_INDEX_UNSIGNED_CHAR;
        break;
      case SHORT:
        eNewType=ANALYZE_DT_INDEX_SIGNED_SHORT;
        break;
      case USHORT:
        eNewType = ANALYZE_DT_INDEX_UNSIGNED_SHORT;
        break;
      case INT:
        eNewType=ANALYZE_DT_INDEX_SIGNED_INT;
        break;
      case FLOAT:
        eNewType=ANALYZE_DT_INDEX_FLOAT;
        break;
      case DOUBLE:
        eNewType=ANALYZE_DT_INDEX_DOUBLE;
        break;
        //case DATA_COMPLEX_FLOAT:
        //  eNewType=ANALYZE_DT_INDEX_COMPLEX;
        //  break;
        //case DATA_RGBTRIPLE:
        //  eNewType=ANALYZE_DT_INDEX_RGB;
        //  break;
        //case DATA_BINARY:
        //  eNewType=ANALYZE_DT_INDEX_BINARY;
        //  break;
        //  case
        //       DATA_UNKNOWN:
        //        eNewType=ANALYZE_DT_INDEX_UNKNOWN;
        //  break;
      default:
        eNewType=ANALYZE_DT_INDEX_UNKNOWN;
        ExceptionObject exception(__FILE__, __LINE__);
        exception.SetDescription("Pixel Type Unknown");
        throw exception;
    }
    m_hdr.dime.datatype=DataTypeKey[eNewType];
    m_hdr.dime.bitpix=DataTypeSizes[eNewType];
    strcpy(m_hdr.hk.data_type,DataTypes[eNewType]);
    switch(m_hdr.dime.datatype)
    {
      case ANALYZE_DT_INDEX_BINARY:
        m_hdr.dime.glmax=1;  /*max value for all of the data set*/
        m_hdr.dime.glmin=0;  /*min value for all of the data set*/
        break;
      case ANALYZE_DT_INDEX_UNSIGNED_CHAR:
        m_hdr.dime.glmax=255;/*max value for all of the data set*/
        m_hdr.dime.glmin=0;  /*min value for all of the data set*/
        break;
      case ANALYZE_DT_INDEX_SIGNED_SHORT:
        //m_hdr.dime.glmax=0;/*max value for all of the data set*/
        //m_hdr.dime.glmin=0;/*min value for all of the data set*/
        break;
      case ANALYZE_DT_INDEX_FLOAT:
        //m_hdr.dime.glmax=0;/*max value for all of the data set*/
        //m_hdr.dime.glmin=0;/*min value for all of the data set*/
        break;
      case ANALYZE_DT_INDEX_DOUBLE:
        //m_hdr.dime.glmax=0;/*max value for all of the data set*/
        //m_hdr.dime.glmin=0;/*min value for all of the data set*/
        break;
      case ANALYZE_DT_INDEX_RGB:
        m_hdr.dime.glmax=255;/*max value for all of the data set*/
        m_hdr.dime.glmin=0;/*min value for all of the data set*/
        break;
      default:
        m_hdr.dime.glmax=0;  /*max value for all of the
                               data set*/
        m_hdr.dime.glmin=0;  /*min value for all of
                               the data set*/
        break;
    }
  }

  void AnalyzeImageIO::Read(void* buffer)
  {
    unsigned int dim;
    const unsigned int dimensions = this->GetNumberOfDimensions();
    unsigned int numberOfPixels = 1;
    for(dim=0; dim< dimensions; dim++ )
    {
      numberOfPixels *= m_Dimensions[ dim ];
    }

    char * const p = static_cast<char *>(buffer);
    //4 cases to handle
    //1: given .hdr and image is .img
    //2: given .img
    //3: given .img.gz
    //4: given .hdr and image is .img.gz
    //   Special processing needed for this case onl
    // NOT NEEDED const std::string fileExt = GetExtension(m_FileName);

    /* Returns proper name for cases 1,2,3 */
    std::string ImageFileName = GetImageFileName( m_FileName );
    //NOTE: gzFile operations act just like FILE * operations when the files
    // are not in gzip fromat.
    // This greatly simplifies the following code, and gzFile types are used
    // everywhere.
    // In addition, it has the added benifit of reading gzip compressed image
    // files that do not have a .gz ending.
    gzFile file_p = ::gzopen( ImageFileName.c_str(), "rb" );
    if( file_p == NULL )
    {
      /* Do a separate check to take care of case #4 */
      ImageFileName += ".gz";
      file_p = ::gzopen( ImageFileName.c_str(), "rb" );
      if( file_p == NULL )
      {
        ExceptionObject exception(__FILE__, __LINE__);
        std::string message="Analyze Data File can not be read: The following files were attempted:\n ";
        message += GetImageFileName( m_FileName );
        message += '\n';
        message += ImageFileName;
        message += '\n';
        exception.SetDescription(message.c_str());
        throw exception;
      }
    }

    // Seek through the file to the correct position, This is only necessary
    // when readin in sub-volumes
    // const long int total_offset = static_cast<long int>(tempX * tempY *
    //                                start_slice * m_dataSize)
    //    + static_cast<long int>(tempX * tempY * total_z * start_time *
    //          m_dataSize);
    // ::gzseek( file_p, total_offset, SEEK_SET );

    // read image in
    ::gzread( file_p, p, this->GetImageSizeInBytes());
    gzclose( file_p );
    SwapBytesIfNecessary( buffer, numberOfPixels );
  }


  // This method will only test if the header looks like an
  // Analyze Header.  Some code is redundant with ReadImageInformation
  // a StateMachine could provide a better implementation
  bool AnalyzeImageIO::CanReadFile( const char* FileNameToRead )
  {
    m_FileName=FileNameToRead;
    const std::string HeaderFileName = GetHeaderFileName( m_FileName );
    //
    // only try to read HDR files
    std::string ext = GetExtension(HeaderFileName);
    if(ext == std::string("gz"))
    {
      ext = GetExtension(GetRootName(HeaderFileName));
    }
    if(ext != std::string("hdr") && ext != std::string("img"))
    {
      return false;
    }

    std::ifstream   local_InputStream;
    local_InputStream.open( HeaderFileName.c_str(), 
                            std::ios::in | std::ios::binary );
    if( local_InputStream.fail() )
    {
      return false;
    }
    local_InputStream.read( (char *)&(this->m_hdr), sizeof(struct dsr) );
    if( local_InputStream.fail() )
    {
      return false;
    }
    local_InputStream.close();

    // if the machine and file endianess are different
    // perform the byte swapping on it
    this->m_ByteOrder = this->CheckAnalyzeEndian(this->m_hdr);
    this->SwapHeaderBytesIfNecessary( &(this->m_hdr) );
    if(this->m_hdr.dime.compressed==1)
    {
      return false;
    //    ExceptionObject exception(__FILE__, __LINE__);
    //    exception.SetDescription("Unix compress file is not supported.");
    //    throw exception;
    }
    return true;
  }

  void AnalyzeImageIO::ReadImageInformation()
  {
    unsigned int dim;
    const std::string HeaderFileName = GetHeaderFileName( m_FileName );
    std::ifstream   local_InputStream;
    local_InputStream.open(HeaderFileName.c_str(),
        std::ios::in | std::ios::binary);
    if( local_InputStream.fail())
    {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("File cannot be read");
      throw exception;
    }
    local_InputStream.read( (char *)(&(this->m_hdr)), sizeof(struct dsr) );
    if( local_InputStream.eof() )
    {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Unexpected end of file");
      throw exception;
    }
    local_InputStream.close();

    // if the machine and file endianess are different
    // perform the byte swapping on it
    this->m_ByteOrder=this->CheckAnalyzeEndian(this->m_hdr);
    if( this->m_MachineByteOrder != this->m_ByteOrder  )
    {
      this->SwapHeaderBytesIfNecessary( &(this->m_hdr) );
    }

    this->SetNumberOfDimensions(this->m_hdr.dime.dim[0]);
    switch( this->m_hdr.dime.datatype )
    {
      case ANALYZE_DT_BINARY:
        m_ComponentType = CHAR;
        m_PixelType = CHAR;
        break;
      case ANALYZE_DT_UNSIGNED_CHAR:
        m_ComponentType = CHAR;
        m_PixelType = CHAR;
        break;
      case ANALYZE_DT_SIGNED_SHORT:
        m_ComponentType = SHORT;
        m_PixelType = SHORT;
        break;
      case ANALYZE_DT_UNSIGNED_SHORT:
        m_ComponentType = USHORT;
        m_PixelType = USHORT;
        break;
      case ANALYZE_DT_SIGNED_INT:
        m_ComponentType = INT;
        m_PixelType = INT;
        break;
      case ANALYZE_DT_FLOAT:
        m_ComponentType = FLOAT;
        m_PixelType = FLOAT;
        break;
      case ANALYZE_DT_DOUBLE:
        m_ComponentType = DOUBLE;
        m_PixelType = DOUBLE;
        break;
      case ANALYZE_DT_RGB:
        // DEBUG -- Assuming this is a triple, not quad
        //image.setDataType( uiig::DATA_RGBQUAD );
        break;
      default:
        break;
    }
    //
    // set up the dimension stuff
    for(dim = 0; dim < this->GetNumberOfDimensions(); dim++)
    {
      this->SetDimensions(dim,this->m_hdr.dime.dim[dim+1]);
      this->SetSpacing(dim,this->m_hdr.dime.pixdim[dim+1]);
    }
    //
    // figure out re-orientation required if not in Coronal
    this->ComputeStrides();
    //Get Dictionary Information
    //Insert Orientation.
    {
      char temp[348];
      //Important hk fields.
      strncpy(temp,this->m_hdr.hk.data_type,10);//Note this is necessary because the array is not necessarily null terminated.
      this->GetMetaDataDictionary()["ITK_OnDiskStorageTypeName"]=
        new itk::MetaDataObject<std::string>(temp);
      strncpy(temp,this->m_hdr.hk.db_name,18);//Note this is necessary because the array is not necessarily null terminated.
      this->GetMetaDataDictionary()["ITK_ImageFileBaseName"]=
        new itk::MetaDataObject<std::string>(temp);
      //Important dime fields
      strncpy(temp,this->m_hdr.dime.vox_units,4);//Note this is necessary because the array is not necessarily null terminated.
      this->GetMetaDataDictionary()["ITK_VoxelUnits"]=
        new itk::MetaDataObject<std::string>(temp);
      strncpy(temp,this->m_hdr.dime.cal_units,8);//Note this is necessary because the array is not necessarily null terminated.
      this->GetMetaDataDictionary()["ITK_CalibrationUnits"]=
        new itk::MetaDataObject<std::string>(temp);
      //this->GetMetaDataDictionary()["ITK_OnDiskStorageType"]=
      //  new itk::MetaDataObject<>(temp);
      this->GetMetaDataDictionary()["ITK_OnDiskBitPerPixel"]=
        new itk::MetaDataObject<short int>(this->m_hdr.dime.bitpix);
      this->GetMetaDataDictionary()["SPM_ROI_SCALE"]=
        new itk::MetaDataObject<float>(this->m_hdr.dime.roi_scale);
      this->GetMetaDataDictionary()["ANALYZE_CAL_MAX"]=
        new itk::MetaDataObject<float>(this->m_hdr.dime.cal_max);
      this->GetMetaDataDictionary()["ANALYZE_CAL_MIN"]=
        new itk::MetaDataObject<float>(this->m_hdr.dime.cal_min);
      this->GetMetaDataDictionary()["ANALYZE_GLMAX"]=
        new itk::MetaDataObject<float>(this->m_hdr.dime.glmax);
      this->GetMetaDataDictionary()["ANALYZE_GLMIN"]=
        new itk::MetaDataObject<float>(this->m_hdr.dime.glmin);
      //Important hist fields
      strncpy(temp,this->m_hdr.hist.descrip,80);//Note this is necessary because the array is not necessarily null terminated.
      this->GetMetaDataDictionary()["ITK_FileNotes"]=
        new itk::MetaDataObject<std::string>(temp);
      strncpy(temp,this->m_hdr.hist.aux_file,24);//Note this is necessary because the array is not necessarily null terminated.
      this->GetMetaDataDictionary()["ANALYZE_AUX_FILE_NAME"]=
        new itk::MetaDataObject<std::string>(temp);
      this->GetMetaDataDictionary()["ITK_Orientation"]=
        new itk::MetaDataObject<itk::IOCommon::ValidOrientationFlags>(static_cast<itk::IOCommon::ValidOrientationFlags>(this->m_hdr.hist.orient));
      strncpy(temp,this->m_hdr.hist.originator,10);//Note this is necessary because the array is not necessarily null terminated.
      this->GetMetaDataDictionary()["ITK_FileOriginator"]=
        new itk::MetaDataObject<std::string>(temp);
      strncpy(temp,this->m_hdr.hist.generated,10);//Note this is necessary because the array is not necessarily null terminated.
      this->GetMetaDataDictionary()["ITK_OriginationData"]=
        new itk::MetaDataObject<std::string>(temp);
      strncpy(temp,this->m_hdr.hist.scannum,10);//Note this is necessary because the array is not necessarily null terminated.
      this->GetMetaDataDictionary()["ANALYZE_ScanNumber"]=
        new itk::MetaDataObject<std::string>(temp);
      strncpy(temp,this->m_hdr.hist.patient_id,10);//Note this is necessary because the array is not necessarily null terminated.
      this->GetMetaDataDictionary()["ANALYZE_PatientID"]=
        new itk::MetaDataObject<std::string>(temp);
      strncpy(temp,this->m_hdr.hist.exp_date,10);//Note this is necessary because the array is not necessarily null terminated.
      this->GetMetaDataDictionary()["ANALYZE_ExperimentDate"]=
        new itk::MetaDataObject<std::string>(temp);
      strncpy(temp,this->m_hdr.hist.exp_date,10);//Note this is necessary because the array is not necessarily null terminated.
      this->GetMetaDataDictionary()["ANALYZE_ExperimentTime"]=
        new itk::MetaDataObject<std::string>(temp);
      this->GetMetaDataDictionary()["ANALYZE_O_MAX"]=
        new itk::MetaDataObject<int>(this->m_hdr.hist.omax);
      this->GetMetaDataDictionary()["ANALYZE_O_MIN"]=
        new itk::MetaDataObject<int>(this->m_hdr.hist.omin);
      this->GetMetaDataDictionary()["ANALYZE_S_MAX"]=
        new itk::MetaDataObject<int>(this->m_hdr.hist.smax);
      this->GetMetaDataDictionary()["ANALYZE_S_MIN"]=
        new itk::MetaDataObject<int>(this->m_hdr.hist.smin);

    }
    return;
  }

  /**
   *
   */
  void
    AnalyzeImageIO
    ::WriteImageInformation(void)
    {
      unsigned int dim;

      const std::string HeaderFileName = GetHeaderFileName( m_FileName );
      std::ofstream   local_OutputStream;
      local_OutputStream.open( HeaderFileName.c_str(),
          std::ios::out | std::ios::binary );
      if( local_OutputStream.fail() )
      {
        ExceptionObject exception(__FILE__, __LINE__);
        std::string ErrorMessage="File cannot be written";
        ErrorMessage+=HeaderFileName;
        exception.SetDescription(ErrorMessage.c_str());
        throw exception;
      }
      for( dim=0; dim< this->GetNumberOfDimensions(); dim++ )
      {
        //NOTE: Analyze dim[0] are the number of dims, and dim[1..7] are the actual dims.
        this->m_hdr.dime.dim[dim+1]  = m_Dimensions[ dim ];
      }
      //DEBUG--HACK It seems that analyze 7.5 requires 4 dimensions.
      this->m_hdr.dime.dim[0]= 4;
      for( dim=this->GetNumberOfDimensions();(int)dim < this->m_hdr.dime.dim[0];
          dim++ )
      {
        //NOTE: Analyze dim[0] are the number of dims, and dim[1..7] are the actual dims.
        this->m_hdr.dime.dim[dim+1]  = 1; //Hardcoded to be 1;
      }
      for( dim=0; dim< this->GetNumberOfDimensions(); dim++ )
      {
        //NOTE: Analyze pixdim[0] is ignored, and the number of dims are taken from dims[0], and pixdim[1..7] are the actual pixdims.
        this->m_hdr.dime.pixdim[dim+1]= m_Spacing[ dim ];
      }
      //The next funciton sets bitpix, and datatype, and data_type fields
      //Along with gl_min and gl_max feilds.
      this->DefineHeaderObjectDataType();

      local_OutputStream.write( (const char *)&(this->m_hdr), sizeof(struct dsr) );
      if( local_OutputStream.eof() )
      {
        ExceptionObject exception(__FILE__, __LINE__);
        exception.SetDescription("Unexpected end of file");
        throw exception;
      }
      local_OutputStream.close();
      return;
    }


  /**
   *
   */
  void
    AnalyzeImageIO
    ::Write( const void* buffer)
    {
      unsigned int dim;

      //Write the image Information before writing data
      this->WriteImageInformation();
      const unsigned int dimensions = this->GetNumberOfDimensions();
      unsigned int numberOfPixels = 1;
      for( dim=0; dim< dimensions; dim++ )
      {
        numberOfPixels *= m_Dimensions[ dim ];
      }

      //NOTE: voidp is defined by zlib.h
      //NOTE: Need const_cast because voidp is "void*", so
      //      "const voidp" is "void* const", not "const void*".
      voidp p = const_cast<voidp>(buffer);
      const std::string ImageFileName = GetImageFileName( m_FileName );
      const std::string fileExt=GetExtension( m_FileName );
      // Check case where image is acually a compressed image
      if(!fileExt.compare( "gz" ))
      {
        // Open the *.img.gz file for writing.
        gzFile  file_p = ::gzopen( ImageFileName.c_str(), "wb" );
        if( file_p==NULL )
        {
          ExceptionObject exception(__FILE__, __LINE__);
          std::string ErrorMessage="Error, Can not write compressed image file for ";
          ErrorMessage+=m_FileName;
          exception.SetDescription(ErrorMessage.c_str());
          throw exception;
        }

#ifdef __OMIT_UNTIL_RGB_IS_NEEEDED
        if ( image.getDataType() == uiig::DATA_RGBTRIPLE )
        {
          // Analyze RGB images are stored in channels, where all the red components are stored
          // first, followed by the green and blue components for each plane of the volume.
          // This is stored in an image of RGBTRIPLE data structures, which are in memory
          // stored as (red,green,blue).  The triples need to be converted to channels for
          // each plane when writing out the image.

          // NOTE: Do NOT change this.  The math here is necessary for CImageStrided to
          // read files correctly
          for( register unsigned int l=0; l<tempT; l++ )
          {
            unsigned int volumeOffset = l*m_uiVolumeOffset;
            for( register unsigned int k=0; k<tempZ; k++ )
            {
              unsigned int planeVolOffset = k*m_uiPlaneOffset + volumeOffset;

              // Reading the red channel
              {
                for( register unsigned int j=0; j<tempY; j++ )
                {
                  unsigned int rowOffset =    j*m_uiRowOffset;
                  for ( register unsigned int i=0; i<tempX; i++ )
                  {
                    //NOTE: unsigned char * is used to do byte wise offsets The offsets are computed
                    //in bytes
                    ::gzwrite( file_p, &(static_cast<unsigned char *>(data)[(m_uiInitialOffset+planeVolOffset+rowOffset)*m_dataSize]) + (i*3), sizeof(unsigned char) );
                  }
                }
              }

              // Reading the green channel
              {
                for( register unsigned int j=0; j<tempY; j++ )
                {
                  unsigned int rowOffset =    j*m_uiRowOffset;
                  for ( register unsigned int i=0; i<tempX; i++ )
                  {
                    //NOTE: unsigned char * is used to do byte wise offsets The offsets are computed
                    //in bytes
                    ::gzwrite( file_p, &(static_cast<unsigned char *>(data)[(m_uiInitialOffset+planeVolOffset+rowOffset)*m_dataSize]) + (i*3) + 1, sizeof(unsigned char) );
                  }
                }
              }

              // Reading the blue channel
              {
                for( register unsigned int j=0; j<tempY; j++ )
                {
                  unsigned int rowOffset =    j*m_uiRowOffset;
                  for ( register unsigned int i=0; i<tempX; i++ )
                  {
                    //NOTE: unsigned char * is used to do byte wise offsets The offsets are computed
                    //in bytes
                    ::gzwrite( file_p, &(static_cast<unsigned char *>(data)[(m_uiInitialOffset+planeVolOffset+rowOffset)*m_dataSize]) + (i*3) + 2, sizeof(unsigned char) );
                  }
                }
              }

            }
          }
        }
        else
#endif
        {
          ::gzwrite( file_p,p,this->GetImageSizeInBytes());
        }
        ::gzclose( file_p );
        //RemoveFile FileNameToRead.img so that it does not get confused with
        //FileNameToRead.img.gz
        //The following is a hack that can be used to remove ambiguity when an
        //uncompressed image is read, and then written as compressed.
        //This results in one *.hdr file being assosiated with a *.img and a
        // *.img.gz image file.
        //DEBUG -- Will this work under windows?
        std::string unusedbaseimgname= GetRootName(GetHeaderFileName(m_FileName));
        unusedbaseimgname+=".img";
        RemoveFile(unusedbaseimgname.c_str());
      }
      else
      {
        //No compression
        std::ofstream   local_OutputStream;
        local_OutputStream.open( ImageFileName.c_str(), std::ios::out | std::ios::binary );
        if( !local_OutputStream )
        {
          ExceptionObject exception(__FILE__, __LINE__);
          std::string ErrorMessage="Error opening image data file for writing.";
          ErrorMessage+=m_FileName;
          exception.SetDescription(ErrorMessage.c_str());
          throw exception;
        }
        local_OutputStream.write((const char *)p, this->GetImageSizeInBytes() );
        bool success = !local_OutputStream.bad();
        local_OutputStream.close();
        if( !success )
        {
          ExceptionObject exception(__FILE__, __LINE__);
          std::string ErrorMessage="Error writing image data.";
          ErrorMessage+=m_FileName;
          exception.SetDescription(ErrorMessage.c_str());
          throw exception;
        }
        //RemoveFile FileNameToRead.img.gz so that it does not get confused with FileNameToRead.img
        //The following is a hack that can be used to remove ambiguity when an
        //uncompressed image is read, and then written as compressed.
        //This results in one *.hdr file being assosiated with a *.img and a *.img.gz image file.
        //DEBUG -- Will this work under windows?
        std::string unusedbaseimgname= GetRootName(GetHeaderFileName(m_FileName));
        unusedbaseimgname+=".img.gz";
        RemoveFile(unusedbaseimgname.c_str());
      }
    }
} // end namespace itk
