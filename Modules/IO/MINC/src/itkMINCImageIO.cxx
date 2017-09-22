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
#include "itkMINCImageIO.h"

#include <cstdio>
#include <cctype>
#include "vnl/vnl_vector.h"
#include "itkMetaDataObject.h"
#include "itkArray.h"

#include <itk_minc2.h>


namespace itk
{

struct ITKIOMINC_HIDDEN MINCImageIOPImpl
{
  int  m_NDims; /*Number of dimensions*/

  // dimension size and start and step, in FILE ORDER!

  char         **m_DimensionName;
  misize_t      *m_DimensionSize;
  double        *m_DimensionStart;
  double        *m_DimensionStep;
  int            m_DimensionIndices[5];

  midimhandle_t *m_MincFileDims;
  midimhandle_t *m_MincApparentDims;
  mitype_t       m_Volume_type;
  miclass_t      m_Volume_class;
  int            m_CompressionLevel;

  // MINC2 volume handle , currently opened
  mihandle_t     m_Volume;
};


bool MINCImageIO::CanReadFile(const char *file)
{
  if ( *file == 0 )
    {
    itkDebugMacro( << "No filename specified.");
    return false;
    }
  std::string filename(file);

  std::string::size_type mncPos = filename.rfind(".mnc");
  if ( (mncPos != std::string::npos)
       && (mncPos == filename.length() - 4) )
    {
    return true;
    }

  mncPos = filename.rfind(".MNC");
  if ( (mncPos != std::string::npos)
       && (mncPos == filename.length() - 4) )
    {
    return true;
    }
#ifdef HAVE_MINC1
  mncPos = filename.rfind(".mnc.gz");
  if ( (mncPos != std::string::npos)
       && (mncPos == filename.length() - 7) )
    {
    return true;
    }

  mncPos = filename.rfind(".MNC.GZ");
  if ( (mncPos != std::string::npos)
       && (mncPos == filename.length() - 7) )
    {
    return true;
    }
#endif //HAVE_MINC1
  mncPos = filename.rfind(".mnc2");
  if ( (mncPos != std::string::npos)
       && (mncPos == filename.length() - 5) )
    {
    return true;
    }

  mncPos = filename.rfind(".MNC2");
  if ( (mncPos != std::string::npos)
       && (mncPos == filename.length() - 5) )
    {
    return true;
    }

  return false;
}

void MINCImageIO::Read(void *buffer)
{
  const unsigned int nDims = this->GetNumberOfDimensions();
  const unsigned int nComp = this->GetNumberOfComponents();

  misize_t *start=new misize_t[nDims+(nComp>1 ? 1 : 0)];
  misize_t *count=new misize_t[nDims+(nComp>1 ? 1 : 0)];

  for ( unsigned int i = 0; i < nDims; i++ )
    {
    if ( i < m_IORegion.GetImageDimension() )
      {
      start[nDims-i-1] = m_IORegion.GetIndex()[i];
      count[nDims-i-1] = m_IORegion.GetSize()[i];
      }
    else
      {
      start[nDims-i-1] = 0;
      count[nDims-i-1] = 1;
      }
    }
  if(nComp>1)
    {
    start[nDims]=0;
    count[nDims]=nComp;
    }
  mitype_t volume_data_type=MI_TYPE_UBYTE;

  switch(this->GetComponentType() )
    {
    case UCHAR:
      volume_data_type=MI_TYPE_UBYTE;
      break;
    case CHAR:
      volume_data_type=MI_TYPE_BYTE;
      break;
    case USHORT:
      volume_data_type=MI_TYPE_USHORT;
      break;
    case SHORT:
      volume_data_type=MI_TYPE_SHORT;
      break;
    case UINT:
      volume_data_type=MI_TYPE_UINT;
      break;
    case INT:
      volume_data_type=MI_TYPE_INT;
      break;
    case ULONG: //TODO: make sure we are cross-platform here!
      volume_data_type=MI_TYPE_UINT;
      break;
    case LONG: //TODO: make sure we are cross-platform here!
      volume_data_type=MI_TYPE_INT;
      break;
    case FLOAT:
      volume_data_type=MI_TYPE_FLOAT;
      break;
    case DOUBLE:
      volume_data_type=MI_TYPE_DOUBLE;
      break;
    default:
      itkDebugMacro(<<"Could read datatype " << this->GetComponentType() );
      delete[] start;
      delete[] count;
      return;
    }

  if ( miget_real_value_hyperslab(this->m_MINCPImpl->m_Volume, volume_data_type, start, count, buffer) < 0 )
    {
    delete[] start;
    delete[] count;
    itkExceptionMacro( << " Can not get real value hyperslab!!\n");
    }
  delete[] start;
  delete[] count;
}

void MINCImageIO::CleanupDimensions(void)
{
  if( this->m_MINCPImpl->m_DimensionName )
    {
    for ( int i = 0; i < this->m_MINCPImpl->m_NDims; i++ )
      {
      mifree_name( this->m_MINCPImpl->m_DimensionName[i] );
      this->m_MINCPImpl->m_DimensionName[i]=ITK_NULLPTR;
      }
    }

  delete[] this->m_MINCPImpl->m_DimensionName;
  delete[] this->m_MINCPImpl->m_DimensionSize;
  delete[] this->m_MINCPImpl->m_DimensionStart;
  delete[] this->m_MINCPImpl->m_DimensionStep;
  delete[] this->m_MINCPImpl->m_MincFileDims;
  delete[] this->m_MINCPImpl->m_MincApparentDims;

  this->m_MINCPImpl->m_DimensionName    = ITK_NULLPTR;
  this->m_MINCPImpl->m_DimensionSize    = ITK_NULLPTR;
  this->m_MINCPImpl->m_DimensionStart   = ITK_NULLPTR;
  this->m_MINCPImpl->m_DimensionStep    = ITK_NULLPTR;
  this->m_MINCPImpl->m_MincFileDims     = ITK_NULLPTR;
  this->m_MINCPImpl->m_MincApparentDims = ITK_NULLPTR;
}

void MINCImageIO::AllocateDimensions(int nDims)
{
  this->CleanupDimensions();

  this->m_MINCPImpl->m_NDims=nDims;

  this->m_MINCPImpl->m_DimensionName  = new char*[this->m_MINCPImpl->m_NDims];
  this->m_MINCPImpl->m_DimensionSize  = new misize_t[this->m_MINCPImpl->m_NDims];
  this->m_MINCPImpl->m_DimensionStart = new double[this->m_MINCPImpl->m_NDims];
  this->m_MINCPImpl->m_DimensionStep  = new double[this->m_MINCPImpl->m_NDims];
  this->m_MINCPImpl->m_MincFileDims   = new midimhandle_t[this->m_MINCPImpl->m_NDims];
  this->m_MINCPImpl->m_MincApparentDims = new midimhandle_t[this->m_MINCPImpl->m_NDims];

  for ( int i = 0; i < this->m_MINCPImpl->m_NDims; i++ )
    {
    this->m_MINCPImpl->m_DimensionName[i]  = ITK_NULLPTR;
    this->m_MINCPImpl->m_DimensionSize[i]  = 0;
    this->m_MINCPImpl->m_DimensionStart[i] = 0.0;
    this->m_MINCPImpl->m_DimensionStep[i]  = 0.0;
    }

  for ( int i = 0; i < 5; i++ )
    {
    this->m_MINCPImpl->m_DimensionIndices[i] = -1;
    }

}

// close existing volume, cleanup internal structures
void MINCImageIO::CloseVolume(void)
{
  this->CleanupDimensions();

  if( this->m_MINCPImpl->m_Volume )
    {
    miclose_volume( this->m_MINCPImpl->m_Volume );
    }
  this->m_MINCPImpl->m_Volume = ITK_NULLPTR;
}

MINCImageIO::MINCImageIO()
  : m_MINCPImpl(new MINCImageIOPImpl)
{
  this->m_MINCPImpl->m_NDims = 0;
  this->m_MINCPImpl->m_DimensionName  = ITK_NULLPTR;
  this->m_MINCPImpl->m_DimensionSize  = ITK_NULLPTR;
  this->m_MINCPImpl->m_DimensionStart = ITK_NULLPTR;
  this->m_MINCPImpl->m_DimensionStep  = ITK_NULLPTR;
  this->m_MINCPImpl->m_MincFileDims   = ITK_NULLPTR;
  this->m_MINCPImpl->m_MincApparentDims = ITK_NULLPTR;
  this->m_MINCPImpl->m_Volume = ITK_NULLPTR;

  for ( int i = 0; i < 5; i++ )
    {
    this->m_MINCPImpl->m_DimensionIndices[i] = -1;
    }

  this->AddSupportedWriteExtension(".mnc");
  this->AddSupportedWriteExtension(".MNC");

  this->AddSupportedReadExtension(".mnc");
  this->AddSupportedReadExtension(".MNC");

  this->m_UseCompression = false;
  this->m_MINCPImpl->m_CompressionLevel = 4; // Range 0-9; 0 = no file compression, 9 =
                                // maximum file compression
  this->m_MINCPImpl->m_Volume_type = MI_TYPE_FLOAT;
  this->m_MINCPImpl->m_Volume_class = MI_CLASS_REAL;

}

MINCImageIO::~MINCImageIO()
{
  this->CloseVolume();
  delete m_MINCPImpl;
}

void MINCImageIO::SetCompressionLevel(int level)
{
   itkDebugMacro("setting CompressionLevel to " << level);

   if ( this->m_MINCPImpl->m_CompressionLevel != level )
     {
     this->m_MINCPImpl->m_CompressionLevel = level;
     this->Modified();
     }
}

int MINCImageIO::GetCompressionLevel() const
{
  return this->m_MINCPImpl->m_CompressionLevel;
}


void MINCImageIO::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NDims: " << this->m_MINCPImpl->m_NDims << std::endl;
}

void MINCImageIO::ReadImageInformation()
{
  std::string dimension_order;

  this->CloseVolume();
  // call to minc2.0 function to open the file
  if ( miopen_volume(m_FileName.c_str(), MI2_OPEN_READ, &this->m_MINCPImpl->m_Volume) < 0 )
    {
    // Error opening the volume
    itkExceptionMacro(<< "Could not open file \"" << m_FileName.c_str() << "\".");
  }

  // find out how many dimensions are there regularly sampled
  // dimensions only
  int ndims;
  if ( miget_volume_dimension_count(this->m_MINCPImpl->m_Volume, MI_DIMCLASS_ANY, MI_DIMATTR_ALL, &ndims) < 0 )
    {
    itkDebugMacro("Could not get the number of dimensions in the volume!");
    return;
    }
  this->AllocateDimensions(ndims);

  // get dimension handles in FILE ORDER (i.e, the order as they are
  // submitted to file)
  if ( miget_volume_dimensions(this->m_MINCPImpl->m_Volume, MI_DIMCLASS_ANY, MI_DIMATTR_ALL, MI_DIMORDER_FILE, this->m_MINCPImpl->m_NDims,
                               this->m_MINCPImpl->m_MincFileDims) < 0 )
    {
    itkExceptionMacro(<< "Could not get dimension handles!");
  }

  for (int i = 0; i < this->m_MINCPImpl->m_NDims; i++ )
    {
    char *      name;
    double      _sep;
    const char *_sign="+";
    if ( miget_dimension_name(this->m_MINCPImpl->m_MincFileDims[i], &name) < 0 )
      {
      // Error getting dimension name
      itkExceptionMacro( << "Could not get dimension name!");
    }

    if( miget_dimension_separation(this->m_MINCPImpl->m_MincFileDims[i],MI_ORDER_FILE, &_sep) == MI_NOERROR && _sep < 0 )
      _sign = "-";

    this->m_MINCPImpl->m_DimensionName[i] = name;
    if(!strcmp(name,MIxspace) || !strcmp(name,MIxfrequency) ) //this is X space
      {
      this->m_MINCPImpl->m_DimensionIndices[1]=i;
      dimension_order += _sign;
      dimension_order += "X";
      }
    else if(!strcmp(name,MIyspace) || !strcmp(name,MIyfrequency) ) //this is Y
                                                                   // space
      {
      this->m_MINCPImpl->m_DimensionIndices[2]=i;
      dimension_order += _sign;
      dimension_order += "Y";
      }
    else if(!strcmp(name,MIzspace) || !strcmp(name,MIzfrequency) ) //this is Z
                                                                   // space
      {
      this->m_MINCPImpl->m_DimensionIndices[3]=i;
      dimension_order += _sign;
      dimension_order += "Z";
      }
    else if(!strcmp(name,MIvector_dimension) ) //this is vector space
      {
      this->m_MINCPImpl->m_DimensionIndices[0]=i;
      dimension_order += "+"; // vector dimension is always positive
      dimension_order += "V";
      }
    else if(!strcmp(name,MItime) || !strcmp(name,MItfrequency) ) //this is time
                                                                 // space
      {
      this->m_MINCPImpl->m_DimensionIndices[4]=i;
      dimension_order += _sign;
      dimension_order += "T";
      }
    else
      {
      itkExceptionMacro( << "Unsupported MINC dimension:"<<name);
    }
    }

  // fill the DimensionSize by calling the following MINC2.0 function
  if ( miget_dimension_sizes(this->m_MINCPImpl->m_MincFileDims, this->m_MINCPImpl->m_NDims, this->m_MINCPImpl->m_DimensionSize) < 0 )
    {
    // Error getting dimension sizes
    itkExceptionMacro( << "Could not get dimension sizes!");
  }

  if ( miget_dimension_separations(this->m_MINCPImpl->m_MincFileDims, MI_ORDER_FILE, this->m_MINCPImpl->m_NDims, this->m_MINCPImpl->m_DimensionStep) < 0 )
    {
    itkExceptionMacro( << " Could not dimension sizes");
  }

  if ( miget_dimension_starts(this->m_MINCPImpl->m_MincFileDims, MI_ORDER_FILE, this->m_MINCPImpl->m_NDims, this->m_MINCPImpl->m_DimensionStart) < 0 )
    {
    itkExceptionMacro( << " Could not dimension sizes");
  }

  mitype_t volume_data_type;
  if ( miget_data_type(this->m_MINCPImpl->m_Volume, &volume_data_type) < 0 )
    {
    itkExceptionMacro( << " Can not get volume data type!!\n");
    }

  // find out whether the data has slice scaling
  miboolean_t slice_scaling_flag=0;
  miboolean_t global_scaling_flag=0;

  if ( miget_slice_scaling_flag(this->m_MINCPImpl->m_Volume, &slice_scaling_flag) < 0 )
    {
    itkExceptionMacro( << " Can not get slice scaling flag!!\n");
    }

  //voxel valid range
  double valid_min,valid_max;
  //get the voxel valid range
  if(miget_volume_valid_range(this->m_MINCPImpl->m_Volume,&valid_max,&valid_min) < 0 )
    {
    itkExceptionMacro( << " Can not get volume valid range!!\n");
    }

  //real volume range, only awailable when slice scaling is off
  double volume_min=0.0,volume_max=1.0;
  if( !slice_scaling_flag )
    {
    if( miget_volume_range(this->m_MINCPImpl->m_Volume,&volume_max,&volume_min) < 0 )
      {
      itkExceptionMacro( << " Can not get volume range!!\n");
      }


    global_scaling_flag=!(volume_min == valid_min && volume_max == valid_max);
    }

  int spatial_dimension_count=0;

  // extract direction cosines
  for(int i=1; i<4; i++)
    {
    if(this->m_MINCPImpl->m_DimensionIndices[i]!=-1) //this dimension is present
      {
      spatial_dimension_count++;
      }
    }

  if ( spatial_dimension_count == 0 ) // sorry, this is metaphysical question
    {
    itkExceptionMacro( <<  " minc files without spatial dimensions are not supported!");
    }

  if ( this->m_MINCPImpl->m_DimensionIndices[0]!=-1 && this->m_MINCPImpl->m_DimensionIndices[4]!=-1 )
    {
    itkExceptionMacro( <<  " 4D minc files vector dimension are not supported currently");
    }

  this->SetNumberOfDimensions(spatial_dimension_count);

  int numberOfComponents=1;
  int usable_dimensions=0;

  Matrix< double, 3,3 > dir_cos;
  dir_cos.Fill(0.0);
  dir_cos.SetIdentity();

  Vector< double,3> origin,sep;
  Vector< double,3> o_origin;
  origin.Fill(0.0);
  o_origin.Fill(0.0);

  int spatial_dimension=0;
  //minc api uses inverse order of dimensions , fastest varying are last
  for(int i=3; i>0; i--)
    {
    if(this->m_MINCPImpl->m_DimensionIndices[i]!=-1)
      {
      //MINC2: bad design!
      //micopy_dimension(hdim[this->m_MINCPImpl->m_DimensionIndices[i]],&apparent_dimension_order[usable_dimensions]);
      this->m_MINCPImpl->m_MincApparentDims[usable_dimensions]=this->m_MINCPImpl->m_MincFileDims[this->m_MINCPImpl->m_DimensionIndices[i]];
      //always use positive
      miset_dimension_apparent_voxel_order(this->m_MINCPImpl->m_MincApparentDims[usable_dimensions],MI_POSITIVE);
      misize_t _sz;
      miget_dimension_size(this->m_MINCPImpl->m_MincApparentDims[usable_dimensions],&_sz);

      std::vector< double > _dir(3);
      double                _sep,_start;

      miget_dimension_separation(this->m_MINCPImpl->m_MincApparentDims[usable_dimensions],MI_ORDER_APPARENT,&_sep);
      miget_dimension_cosines(this->m_MINCPImpl->m_MincApparentDims[usable_dimensions],&_dir[0]);
      miget_dimension_start(this->m_MINCPImpl->m_MincApparentDims[usable_dimensions],MI_ORDER_APPARENT,&_start);

      for(int j=0; j<3; j++)
        dir_cos[j][i-1]=_dir[j];

      origin[i-1]=_start;
      sep[i-1]=_sep;

      this->SetDimensions(i-1,static_cast<unsigned int>(_sz) );
      this->SetDirection(i-1,_dir);
      this->SetSpacing(i-1,_sep);

      spatial_dimension++;
      usable_dimensions++;
      }
    }

  if(this->m_MINCPImpl->m_DimensionIndices[0]!=-1) // have vector dimension
    {
    //micopy_dimension(this->m_MINCPImpl->m_MincFileDims[this->m_MINCPImpl->m_DimensionIndices[0]],&apparent_dimension_order[usable_dimensions]);
    this->m_MINCPImpl->m_MincApparentDims[usable_dimensions]=this->m_MINCPImpl->m_MincFileDims[this->m_MINCPImpl->m_DimensionIndices[0]];
    //always use positive, vector dimension does not supposed to have notion of positive step size, so leaving as is
    //miset_dimension_apparent_voxel_order(this->m_MINCPImpl->m_MincApparentDims[usable_dimensions],MI_POSITIVE);
    misize_t _sz;
    miget_dimension_size(this->m_MINCPImpl->m_MincApparentDims[usable_dimensions],&_sz);
    numberOfComponents=_sz;
    usable_dimensions++;
    }

  if(this->m_MINCPImpl->m_DimensionIndices[4]!=-1) // have time dimension
    {
    //micopy_dimension(hdim[this->m_MINCPImpl->m_DimensionIndices[4]],&apparent_dimension_order[usable_dimensions]);
    this->m_MINCPImpl->m_MincApparentDims[usable_dimensions]=this->m_MINCPImpl->m_MincFileDims[this->m_MINCPImpl->m_DimensionIndices[4]];
    //always use positive
    miset_dimension_apparent_voxel_order(this->m_MINCPImpl->m_MincApparentDims[usable_dimensions],MI_POSITIVE);
    misize_t _sz;
    miget_dimension_size(this->m_MINCPImpl->m_MincApparentDims[usable_dimensions],&_sz);
    numberOfComponents=_sz;
    usable_dimensions++;
    }

  //Set apparent dimension order to the MINC2 api
  if(miset_apparent_dimension_order(this->m_MINCPImpl->m_Volume,usable_dimensions,this->m_MINCPImpl->m_MincApparentDims)<0)
    {
    itkExceptionMacro( << " Can't set apparent dimension order!");
    }

  o_origin=dir_cos*origin;

  for(int i=0; i<spatial_dimension_count; i++)
    this->SetOrigin(i,o_origin[i]);

  miclass_t volume_data_class;

  if ( miget_data_class(this->m_MINCPImpl->m_Volume, &volume_data_class) < 0 )
    {
    itkExceptionMacro( << " Could not get data class");
    }

  // set the file data type
  if(slice_scaling_flag || global_scaling_flag)
    {
    switch ( volume_data_type )
      {
      case MI_TYPE_FLOAT:
        this->SetComponentType(FLOAT);
        break;
      case MI_TYPE_DOUBLE:
        this->SetComponentType(DOUBLE);
        break;
      case MI_TYPE_FCOMPLEX:
        this->SetComponentType(FLOAT);
        break;
      case MI_TYPE_DCOMPLEX:
        this->SetComponentType(DOUBLE);
        break;
      default:
        this->SetComponentType(FLOAT);
        break;
      } //end of switch
        //file will have do
    }
  else
    {
    switch ( volume_data_type )
      {
      case MI_TYPE_BYTE:
        this->SetComponentType(CHAR);
        break;
      case MI_TYPE_UBYTE:
        this->SetComponentType(UCHAR);
        break;
      case MI_TYPE_SHORT:
        this->SetComponentType(SHORT);
        break;
      case MI_TYPE_USHORT:
        this->SetComponentType(USHORT);
        break;
      case MI_TYPE_INT:
        this->SetComponentType(INT);
        break;
      case MI_TYPE_UINT:
        this->SetComponentType(UINT);
        break;
      case MI_TYPE_FLOAT:
        this->SetComponentType(FLOAT);
        break;
      case MI_TYPE_DOUBLE:
        this->SetComponentType(DOUBLE);
        break;
      case MI_TYPE_SCOMPLEX:
        this->SetComponentType(SHORT);
        break;
      case MI_TYPE_ICOMPLEX:
        this->SetComponentType(INT);
        break;
      case MI_TYPE_FCOMPLEX:
        this->SetComponentType(FLOAT);
        break;
      case MI_TYPE_DCOMPLEX:
        this->SetComponentType(DOUBLE);
        break;
      default:
        itkExceptionMacro( << "Bad data type ");
      } //end of switch
    }

  switch ( volume_data_class )
    {
    case MI_CLASS_REAL:
      if(numberOfComponents == 1)
        {
        this->SetPixelType(SCALAR);
        }
      else
        {
        this->SetPixelType(VECTOR); //TODO: handle more types (i.e matrix,
        }                           // tensor etc)
      break;
    case MI_CLASS_INT:
      if(numberOfComponents == 1)
        {
        this->SetPixelType(SCALAR);
        }
      else
        {
        this->SetPixelType(VECTOR); //TODO: handle more types (i.e matrix,
        }                           // tensor etc)
      break;
    case MI_CLASS_LABEL:
      if(numberOfComponents == 1)
        {
        this->SetPixelType(SCALAR);
        }
      else
        {
        this->SetPixelType(VECTOR);
        }
      // create an array of label names and values
      // not sure how to pass this to itk yet!
      break;
    case MI_CLASS_COMPLEX:
      //m_Complex = 1;
      this->SetPixelType(COMPLEX);
      numberOfComponents *= 2;
      break;
    default:
      itkExceptionMacro("Bad data class ");
    } //end of switch

  this->SetNumberOfComponents(numberOfComponents);
  this->ComputeStrides();

  // create metadata object to store usefull additional information
  MetaDataDictionary &thisDic=GetMetaDataDictionary();
  thisDic.Clear();

  std::string classname(GetNameOfClass() );
  //  EncapsulateMetaData<std::string>(thisDic,ITK_InputFilterName,
  // classname);

  // store history
  size_t minc_history_length=0;
  if(miget_attr_length(this->m_MINCPImpl->m_Volume,"","history",&minc_history_length) == MI_NOERROR)
    {
    char *minc_history=new char[minc_history_length+1];
    if( miget_attr_values(this->m_MINCPImpl->m_Volume,MI_TYPE_STRING,"","history",minc_history_length+1,minc_history) == MI_NOERROR)
      {
      EncapsulateMetaData<std::string>(thisDic,"history", std::string(minc_history) );
      }
    delete[] minc_history;
    }

  if(this->m_MINCPImpl->m_DimensionIndices[4]!=-1) //have time dimension
    {
    //store time dimension start and step in metadata for preservation
    double _sep,_start;
    miget_dimension_separation(this->m_MINCPImpl->m_MincFileDims[this->m_MINCPImpl->m_DimensionIndices[4]],MI_ORDER_APPARENT,&_sep);
    miget_dimension_start(this->m_MINCPImpl->m_MincFileDims[this->m_MINCPImpl->m_DimensionIndices[4]],MI_ORDER_APPARENT,&_start);
    EncapsulateMetaData<double>(thisDic,"tstart",_start);
    EncapsulateMetaData<double>(thisDic,"tstep",_sep);
    }

  EncapsulateMetaData< std::string >(thisDic,"dimension_order",dimension_order);

  // preserve original volume storage data type
  switch ( volume_data_type )
    {
    case MI_TYPE_BYTE:
      EncapsulateMetaData< std::string >(thisDic,"storage_data_type",typeid(char).name());
      break;
    case MI_TYPE_UBYTE:
      EncapsulateMetaData< std::string >(thisDic,"storage_data_type",typeid(unsigned char).name());
      break;
    case MI_TYPE_SHORT:
      EncapsulateMetaData< std::string >(thisDic,"storage_data_type",typeid(short).name());
      break;
    case MI_TYPE_USHORT:
      EncapsulateMetaData< std::string >(thisDic,"storage_data_type",typeid(unsigned short).name());
      break;
    case MI_TYPE_INT:
      EncapsulateMetaData< std::string >(thisDic,"storage_data_type",typeid(int).name());
      break;
    case MI_TYPE_UINT:
      EncapsulateMetaData< std::string >(thisDic,"storage_data_type",typeid(unsigned int).name());
      break;
    case MI_TYPE_FLOAT:
      EncapsulateMetaData< std::string >(thisDic,"storage_data_type",typeid(float).name());
      break;
    case MI_TYPE_DOUBLE:
      EncapsulateMetaData< std::string >(thisDic,"storage_data_type",typeid(double).name());
      break;
    case MI_TYPE_SCOMPLEX:
      EncapsulateMetaData< std::string >(thisDic,"storage_data_type",typeid(short).name());
      break;
    case MI_TYPE_ICOMPLEX:
      EncapsulateMetaData< std::string >(thisDic,"storage_data_type",typeid(int).name());
      break;
    case MI_TYPE_FCOMPLEX:
      EncapsulateMetaData< std::string >(thisDic,"storage_data_type",typeid(float).name());
      break;
    case MI_TYPE_DCOMPLEX:
      EncapsulateMetaData< std::string >(thisDic,"storage_data_type",typeid(double).name());
      break;
    default:
      break;
      // don't store this storage data type
    } //end of switch

  //iterate over all root level groups , and extract all underlying attributes
  //unfortunately more complicated attribute structure of MINC2 is not supported
  //at least it is not used anywhere
  milisthandle_t grplist;

  if ( (milist_start(this->m_MINCPImpl->m_Volume, "", 0, &grplist) ) == MI_NOERROR )
    {
    char           group_name[256];
    milisthandle_t attlist;

    while( milist_grp_next(grplist, group_name, sizeof(group_name) ) == MI_NOERROR )
      {
      if( (milist_start(this->m_MINCPImpl->m_Volume, group_name, 1 , &attlist) ) == MI_NOERROR)
        {
        char attribute[256];

        while( milist_attr_next(this->m_MINCPImpl->m_Volume,attlist,group_name,sizeof(group_name),attribute,
                                sizeof(attribute) ) == MI_NOERROR )
          {
          mitype_t    att_data_type;
          size_t      att_length;
          std::string entry_key;

          entry_key=group_name;
          entry_key += ":";
          entry_key += attribute;

          if(miget_attr_type(this->m_MINCPImpl->m_Volume,group_name,attribute,&att_data_type) == MI_NOERROR &&
             miget_attr_length(this->m_MINCPImpl->m_Volume,group_name,attribute,&att_length) == MI_NOERROR )
            {
            switch(att_data_type)
              {
              case MI_TYPE_STRING:
                {
                char *tmp=new char[att_length+1];
                if(miget_attr_values(this->m_MINCPImpl->m_Volume,att_data_type,group_name,attribute,att_length+1,tmp) == MI_NOERROR )
                  {
                  EncapsulateMetaData< std::string >( thisDic, entry_key, std::string(tmp) );
                  }
                delete[] tmp;
                }
                break;
              case MI_TYPE_FLOAT:
                {
                Array<float> tmp(att_length);
                if(miget_attr_values(this->m_MINCPImpl->m_Volume,att_data_type,group_name,attribute,att_length,
                                     tmp.data_block() ) == MI_NOERROR )
                  {
                  if(att_length==1)
                    {
                    EncapsulateMetaData<float>(thisDic,entry_key,tmp[0]);
                    }
                  else
                    {
                    EncapsulateMetaData<Array<float> >(thisDic,entry_key,tmp);
                    }
                  } else {
                   itkExceptionMacro( <<  " Error getting float attribute! ");
                  }
                }
                break;
              case MI_TYPE_DOUBLE:
                {
                Array<double> tmp(att_length);
                if(miget_attr_values(this->m_MINCPImpl->m_Volume,att_data_type,group_name,attribute,att_length,
                                     tmp.data_block() ) == MI_NOERROR )
                  {
                  if(att_length==1)
                    {
                    EncapsulateMetaData<double>(thisDic,entry_key,tmp[0]);
                    }
                  else
                    {
                    EncapsulateMetaData<Array<double> >(thisDic,entry_key,tmp);
                    }
                  }
                }
                break;
              case MI_TYPE_INT:
                {
                Array<int> tmp(att_length);
                if(miget_attr_values(this->m_MINCPImpl->m_Volume,att_data_type,group_name,attribute,att_length,
                                     tmp.data_block() ) == MI_NOERROR )
                  {
                  if(att_length==1)
                    {
                    EncapsulateMetaData<int>(thisDic,entry_key,tmp[0]);
                    }
                  else
                    {
                    EncapsulateMetaData<Array<int> >(thisDic,entry_key,tmp);
                    }
                  }
                }
                break;
              default:
                itkExceptionMacro( << "Unsupported Attribute data type ");
              }
            }
          else
            {
            itkExceptionMacro( <<  "Problem reading attribute info ");
            }
          }
        milist_finish(attlist);
        }
      }
    milist_finish(grplist);
    }
}

bool MINCImageIO::CanWriteFile(const char *file)
{
  std::string filename = file;

#ifdef _MSC_VER
  // transform filename to lower case to make checks case-insensitive
  std::transform(filename.begin(), filename.end(), filename.begin(), ( int ( * )(int) )tolower);
#else
  // transform filename to lower case to make checks case-insensitive
  std::transform(filename.begin(), filename.end(), filename.begin(), ( int ( * )(int) ) std::tolower);
#endif //_MSC_VER
  if (  filename == "" )
    {
    itkDebugMacro( << "No filename specified.");
    return false;
    }

  std::string::size_type mncPos = filename.rfind(".mnc");
  if ( ( mncPos != std::string::npos )
       && ( mncPos > 0 )
       && ( mncPos == filename.length() - 4 ) )
    {
    return true;
    }

  mncPos = filename.rfind(".mnc2");
  if ( ( mncPos != std::string::npos )
       && ( mncPos > 0 )
       && ( mncPos == filename.length() - 5 ) )
    {
    return true;
    }

  return false;
}

/*
 * fill out the appropriate header information
*/
void MINCImageIO::WriteImageInformation(void)
{
  const unsigned int nDims = this->GetNumberOfDimensions();
  const unsigned int nComp = this->GetNumberOfComponents();

  this->CloseVolume();
  this->AllocateDimensions(nDims+(nComp>1 ? 1 : 0) );

  MetaDataDictionary &thisDic=GetMetaDataDictionary();

  unsigned int    minc_dimensions=0;
  double tstart=0.0;
  double tstep=1.0;

  if(nComp>3) //last dimension will be either vector or time
    {
    micreate_dimension(MItime, MI_DIMCLASS_TIME, MI_DIMATTR_REGULARLY_SAMPLED, nComp,
                       &this->m_MINCPImpl->m_MincApparentDims[this->m_MINCPImpl->m_NDims-minc_dimensions-1] );

    if(!ExposeMetaData< double >(thisDic,"tstart",tstart))
      {
      tstart=0.0;
      }

    miset_dimension_start(this->m_MINCPImpl->m_MincApparentDims[this->m_MINCPImpl->m_NDims-minc_dimensions-1],tstart);

    if(!ExposeMetaData< double >(thisDic,"tstep",tstep))
      {
      tstep=1.0;
      }

    miset_dimension_separation(this->m_MINCPImpl->m_MincApparentDims[this->m_MINCPImpl->m_NDims-minc_dimensions-1],tstep);

    minc_dimensions++;
    }
  else if(nComp>1)
    {
    micreate_dimension(MIvector_dimension,MI_DIMCLASS_RECORD, MI_DIMATTR_REGULARLY_SAMPLED, nComp,
                       &this->m_MINCPImpl->m_MincApparentDims[this->m_MINCPImpl->m_NDims-minc_dimensions-1] );
    minc_dimensions++;
    }

  micreate_dimension(MIxspace,MI_DIMCLASS_SPATIAL, MI_DIMATTR_REGULARLY_SAMPLED,
                     this->GetDimensions(0), &this->m_MINCPImpl->m_MincApparentDims[this->m_MINCPImpl->m_NDims-minc_dimensions-1] );
  minc_dimensions++;

  if(nDims > 1)
    {
    micreate_dimension(MIyspace,MI_DIMCLASS_SPATIAL, MI_DIMATTR_REGULARLY_SAMPLED,
                       this->GetDimensions(1), &this->m_MINCPImpl->m_MincApparentDims[this->m_MINCPImpl->m_NDims-minc_dimensions-1] );
    minc_dimensions++;
    }

  if(nDims > 2)
    {
    micreate_dimension(MIzspace,MI_DIMCLASS_SPATIAL, MI_DIMATTR_REGULARLY_SAMPLED,
                       this->GetDimensions(2), &this->m_MINCPImpl->m_MincApparentDims[this->m_MINCPImpl->m_NDims-minc_dimensions-1] );
    minc_dimensions++;
    }

  if(nDims > 3)
    {
    itkExceptionMacro( <<  "Unfortunately, only up to 3D volume are supported now.");
    }

  //allocating dimensions
  vnl_matrix< double > dircosmatrix(nDims, nDims);
  dircosmatrix.set_identity();
  vnl_vector<double> origin(nDims);

  for (unsigned int i = 0; i < nDims; i++ )
    {
    for (unsigned int j = 0; j < nDims; j++ )
      {
      dircosmatrix[i][j] = this->GetDirection(i)[j];
      }
    origin[i] = this->GetOrigin(i);
    }

  vnl_matrix< double > inverseDirectionCosines = vnl_matrix_inverse< double >(dircosmatrix);
  origin *= inverseDirectionCosines; //transform to minc convention

  for (unsigned int i = 0; i < nDims; i++ )
    {
    unsigned int j=i+(nComp>1 ? 1 : 0);
    double dir_cos[3];
    for(unsigned int k=0; k<3; k++)
      {
      if(k<nDims)
        {
        dir_cos[k]=dircosmatrix[i][k];
        }
      else
        {
        dir_cos[k]=0.0;
        }
      }
    miset_dimension_separation(this->m_MINCPImpl->m_MincApparentDims[minc_dimensions-j-1],this->GetSpacing(i) );
    miset_dimension_start(this->m_MINCPImpl->m_MincApparentDims[minc_dimensions-j-1],origin[i]);
    miset_dimension_cosines(this->m_MINCPImpl->m_MincApparentDims[minc_dimensions-j-1],dir_cos);
    }

  //TODO: fix this to appropriate
  this->m_MINCPImpl->m_Volume_type=MI_TYPE_FLOAT;
  this->m_MINCPImpl->m_Volume_class=MI_CLASS_REAL;

  switch(this->GetComponentType() )
    {
    case UCHAR:
      this->m_MINCPImpl->m_Volume_type=MI_TYPE_UBYTE;
      //this->m_MINCPImpl->m_Volume_class=MI_CLASS_INT;
      break;
    case CHAR:
      this->m_MINCPImpl->m_Volume_type=MI_TYPE_BYTE;
      //this->m_MINCPImpl->m_Volume_class=MI_CLASS_INT;
      break;
    case USHORT:
      this->m_MINCPImpl->m_Volume_type=MI_TYPE_USHORT;
      //this->m_MINCPImpl->m_Volume_class=MI_CLASS_INT;
      break;
    case SHORT:
      this->m_MINCPImpl->m_Volume_type=MI_TYPE_SHORT;
      //this->m_MINCPImpl->m_Volume_class=MI_CLASS_INT;
      break;
    case UINT:
      this->m_MINCPImpl->m_Volume_type=MI_TYPE_UINT;
      //this->m_MINCPImpl->m_Volume_class=MI_CLASS_INT;
      break;
    case INT:
      this->m_MINCPImpl->m_Volume_type=MI_TYPE_INT;
      //this->m_MINCPImpl->m_Volume_class=MI_CLASS_INT;
      break;
//     case ULONG://TODO: make sure we are cross-platform here!
//       volume_data_type=MI_TYPE_ULONG;
//       break;
//     case LONG://TODO: make sure we are cross-platform here!
//       volume_data_type=MI_TYPE_LONG;
//       break;
    case FLOAT: //TODO: make sure we are cross-platform here!
      this->m_MINCPImpl->m_Volume_type=MI_TYPE_FLOAT;
      break;
    case DOUBLE: //TODO: make sure we are cross-platform here!
      this->m_MINCPImpl->m_Volume_type=MI_TYPE_DOUBLE;
      break;
    default:
      itkExceptionMacro(<<"Could read datatype " << this->GetComponentType() );
    }

  std::string storage_data_type;

  //perform storage of floating point data using fixed point arithmetics
  if( ( this->GetComponentType() == FLOAT || this->GetComponentType() == DOUBLE ) &&
      ExposeMetaData< std::string >(thisDic,"storage_data_type",storage_data_type) )
    {
      if(storage_data_type==typeid(char).name())
        this->m_MINCPImpl->m_Volume_type=MI_TYPE_BYTE;
      else if(storage_data_type==typeid(unsigned char).name())
        this->m_MINCPImpl->m_Volume_type=MI_TYPE_UBYTE;
      else if(storage_data_type==typeid(short).name())
        this->m_MINCPImpl->m_Volume_type=MI_TYPE_SHORT;
      else if(storage_data_type==typeid(unsigned short).name())
        this->m_MINCPImpl->m_Volume_type=MI_TYPE_USHORT;
      else if(storage_data_type==typeid(int).name() )
        this->m_MINCPImpl->m_Volume_type=MI_TYPE_INT;
      else if(storage_data_type==typeid(unsigned int).name())
        this->m_MINCPImpl->m_Volume_type=MI_TYPE_UINT;
      else if(storage_data_type==typeid(float).name())
        this->m_MINCPImpl->m_Volume_type=MI_TYPE_FLOAT;
      else if(storage_data_type==typeid(double).name())
        this->m_MINCPImpl->m_Volume_type=MI_TYPE_DOUBLE;
    }
  //now let's create the same dimension order and positive/negative step size as
  // in original image
  std::string dimension_order;
  bool        dimorder_good=false;
  if(ExposeMetaData< std::string >(thisDic,"dimension_order",dimension_order) )
    {
    //the format should be ((+|-)(X|Y|Z|V|T))*
    //std::cout<<"Restoring original dimension order:"<<dimension_order.c_str()<<std::endl;
    if(dimension_order.length() == (minc_dimensions*2) )
      {
      dimorder_good=true;
      for(unsigned int i=0; i<minc_dimensions && dimorder_good; i++)
        {
        bool positive=(dimension_order[i*2] == '+');
        int  j=0;
        switch(dimension_order[i*2+1])
          {
          case 'v':
          case 'V':
            if(nComp<=1)
              {
              itkDebugMacro( << "Dimension order is incorrect " << dimension_order.c_str() );
              dimorder_good=false;
              }
            else
              {
              j=this->m_MINCPImpl->m_NDims-1;
              }
            break;
          case 't':
          case 'T':
            if(nComp<=1)
              {
              itkDebugMacro( << "Dimension order is incorrect " << dimension_order.c_str() );
              dimorder_good=false;
              }
            else
              {
              j=this->m_MINCPImpl->m_NDims-1;
              }
            break;
          case 'x':
          case 'X':
            j=this->m_MINCPImpl->m_NDims-1-((nComp>1 ? 1 : 0));
            break;
          case 'y':
          case 'Y':
            j=this->m_MINCPImpl->m_NDims-1-((nComp>1 ? 1 : 0)+1);
            break;
          case 'z':
          case 'Z':
            j=this->m_MINCPImpl->m_NDims-1-((nComp>1 ? 1 : 0)+2);
            break;
          default:
            itkDebugMacro( << "Dimension order is incorrect " << dimension_order.c_str() );
            dimorder_good=false;
            j=0;
            break;
          }

        if(dimorder_good)
          {
          //flip the sign
          if(!positive && dimension_order[i*2+1]!='V' && dimension_order[i*2+1]!='v' ) //Vector dimension is always positive
            {
            double   _sep,_start;
            misize_t _sz;

            miget_dimension_separation(this->m_MINCPImpl->m_MincApparentDims[j],MI_ORDER_FILE,&_sep);
            miget_dimension_start(this->m_MINCPImpl->m_MincApparentDims[j],MI_ORDER_FILE,&_start);
            miget_dimension_size(this->m_MINCPImpl->m_MincApparentDims[j],&_sz);

            _start=_start+(_sz-1)*_sep;
            _sep=-_sep;

            miset_dimension_separation(this->m_MINCPImpl->m_MincApparentDims[j],_sep);
            miset_dimension_start(this->m_MINCPImpl->m_MincApparentDims[j],_start);

            miset_dimension_apparent_voxel_order(this->m_MINCPImpl->m_MincApparentDims[j],MI_POSITIVE);
            }
          //Hmmm.... what are we going to get in the end?
          this->m_MINCPImpl->m_MincFileDims[i]=this->m_MINCPImpl->m_MincApparentDims[j];
          }
        }
      }
    else
      {
      itkDebugMacro( << "Dimension order is incorrect " << dimension_order.c_str() );
      }
    }

  if(!dimorder_good) //use default order!
    {
    for(unsigned int i=0; i<minc_dimensions; i++)
      this->m_MINCPImpl->m_MincFileDims[i]=this->m_MINCPImpl->m_MincApparentDims[i];
    }

  mivolumeprops_t hprops;
  if( minew_volume_props(&hprops) < 0)
    {
    itkExceptionMacro( << "Could not allocate MINC properties");
    }

  if(  this->m_UseCompression )
    {
    if(miset_props_compression_type(hprops, MI_COMPRESS_ZLIB)<0)
      {
      itkExceptionMacro( << "Could not set MINC compression");
      }

    if(miset_props_zlib_compression(hprops,this->m_MINCPImpl->m_CompressionLevel)<0)
      {
      itkExceptionMacro( << "Could not set MINC compression level");
      }
    }
  else
    {
    if(miset_props_compression_type(hprops, MI_COMPRESS_NONE)<0)
      {
      itkExceptionMacro( << "Could not set MINC compression");
      }
    }

  if ( micreate_volume ( m_FileName.c_str(), minc_dimensions, this->m_MINCPImpl->m_MincFileDims, this->m_MINCPImpl->m_Volume_type,
                         this->m_MINCPImpl->m_Volume_class, hprops, &this->m_MINCPImpl->m_Volume )<0 )
    {
    // Error opening the volume
    itkExceptionMacro( << "Could not open file \"" << m_FileName.c_str() << "\".");
    }

  if (  micreate_volume_image ( this->m_MINCPImpl->m_Volume ) <0 )
    {
    // Error opening the volume
    itkExceptionMacro( << "Could not create image in  file \"" << m_FileName.c_str() << "\".");
    }

  if ( miset_apparent_dimension_order(this->m_MINCPImpl->m_Volume,minc_dimensions,this->m_MINCPImpl->m_MincApparentDims)<0)
    {
    itkExceptionMacro( << " Can't set apparent dimension order!");
    }

  if ( miset_slice_scaling_flag(this->m_MINCPImpl->m_Volume, 0 )<0)
    {
    itkExceptionMacro( << "Could not set slice scaling flag");
    }

  double valid_min,valid_max;
  miget_volume_valid_range(this->m_MINCPImpl->m_Volume,&valid_max,&valid_min);

  //by default valid range will be equal to range, to avoid scaling
  miset_volume_range(this->m_MINCPImpl->m_Volume,valid_max,valid_min);

  for(MetaDataDictionary::ConstIterator it=thisDic.Begin();
      it != thisDic.End(); ++it)
    {
    //don't store some internal ITK junk
    if(
      (*it).first == "ITK_InputFilterName" ||
      (*it).first == "NRRD_content"  ||
      (*it).first == "NRRD_centerings[0]" ||
      (*it).first == "NRRD_centerings[1]" ||
      (*it).first == "NRRD_centerings[2]" ||
      (*it).first == "NRRD_centerings[3]" ||
      (*it).first == "NRRD_kinds[0]" ||
      (*it).first == "NRRD_kinds[1]" ||
      (*it).first == "NRRD_kinds[2]" ||
      (*it).first == "NRRD_kinds[3]" ||
      (*it).first == "NRRD_space"
      ) continue;

    const char *        d=strchr( (*it).first.c_str(),':');
    MetaDataObjectBase *bs=(*it).second;
    const char *        tname=bs->GetMetaDataObjectTypeName();

    if(d)
      {
      std::string var( (*it).first.c_str(),d-(*it).first.c_str() );
      std::string att(d+1);

      //VF:THIS is not good OO style at all :(
      if(!strcmp(tname,typeid(std::string).name() ) )
        {
        const std::string &tmp=dynamic_cast<MetaDataObject<std::string> *>(bs )->GetMetaDataObjectValue();
        miset_attr_values(this->m_MINCPImpl->m_Volume,MI_TYPE_STRING,var.c_str(),att.c_str(),tmp.length()+1,tmp.c_str() );
        }
      else if(!strcmp(tname,typeid(Array<double>).name() ) )
        {
        const Array<double> &tmp=dynamic_cast<MetaDataObject<Array<double> > * >(bs)->GetMetaDataObjectValue();
        miset_attr_values(this->m_MINCPImpl->m_Volume,MI_TYPE_DOUBLE,var.c_str(),att.c_str(),tmp.Size(),tmp.data_block() );
        }
      else if(!strcmp(tname,typeid(Array<float>).name() ) )
        {
        const Array<float> &tmp=dynamic_cast<MetaDataObject<Array<float> > * >(bs)->GetMetaDataObjectValue();
        miset_attr_values(this->m_MINCPImpl->m_Volume,MI_TYPE_FLOAT,var.c_str(),att.c_str(),tmp.Size(),tmp.data_block() );
        }
      else if(!strcmp(tname,typeid(Array<int>).name() ) )
        {
        const Array<int> &tmp=dynamic_cast<MetaDataObject<Array<int> > * >(bs)->GetMetaDataObjectValue();
        miset_attr_values(this->m_MINCPImpl->m_Volume,MI_TYPE_INT,var.c_str(),att.c_str(),tmp.Size(),tmp.data_block() );
        }
      else if(!strcmp(tname,typeid(double).name() ) )
        {
        double tmp=dynamic_cast<MetaDataObject<double > * >(bs)->GetMetaDataObjectValue();
        miset_attr_values(this->m_MINCPImpl->m_Volume,MI_TYPE_DOUBLE,var.c_str(),att.c_str(),1,&tmp);
        }
      else if(!strcmp(tname,typeid(float).name() ) )
        {
        float tmp=dynamic_cast<MetaDataObject<float > * >(bs)->GetMetaDataObjectValue();
        miset_attr_values(this->m_MINCPImpl->m_Volume,MI_TYPE_FLOAT,var.c_str(),att.c_str(),1,&tmp);
        }
      else if(!strcmp(tname,typeid(int).name() ) )
        {
        int tmp=dynamic_cast<MetaDataObject<int > * >(bs)->GetMetaDataObjectValue();
        miset_attr_values(this->m_MINCPImpl->m_Volume,MI_TYPE_INT,var.c_str(),att.c_str(),1,&tmp);
        }
      else
        {
        itkDebugMacro( << "Unsupported metadata type:" << tname);
        }
      }
    else if( (*it).first == "history")
      {
      const std::string &tmp=dynamic_cast<MetaDataObject<std::string> *>(bs )->GetMetaDataObjectValue();
      miset_attr_values(this->m_MINCPImpl->m_Volume,MI_TYPE_STRING,"","history",tmp.length()+1,tmp.c_str() );
      }
    else
      { // else we have an attribute without variable name, most likely
        // it comes from another file type
        //TODO: figure out what to do with it
      }
    }
    mifree_volume_props( hprops );
}

template<typename T> void get_buffer_min_max(const void* _buffer,size_t len,double &buf_min,double &buf_max)
{
  const T* buf=static_cast<const T*>(_buffer);

  buf_min=buf_max=buf[0];
  for(size_t i=0; i<len; i++)
    {
    if(buf[i]<(double)buf_min) buf_min=(double)buf[i];
    if(buf[i]>(double)buf_max) buf_max=(double)buf[i];
    }
}

void MINCImageIO::Write(const void *buffer)
{
  const unsigned int nDims = this->GetNumberOfDimensions();
  const unsigned int nComp = this->GetNumberOfComponents();
  size_t             buffer_length=1;

  misize_t *start=new misize_t[nDims+(nComp>1 ? 1 : 0)];
  misize_t *count=new misize_t[nDims+(nComp>1 ? 1 : 0)];

  for ( unsigned int i = 0; i < nDims; i++ )
    {
    if ( i < m_IORegion.GetImageDimension() )
      {
      start[nDims-i-1] = m_IORegion.GetIndex()[i];
      count[nDims-i-1] = m_IORegion.GetSize()[i];
      buffer_length *= m_IORegion.GetSize()[i];
      }
    else
      {
      start[nDims-i-1] = 0;
      count[nDims-i-1] = 1;
      }
    }

  if(nComp>1)
    {
    start[nDims]=0;
    count[nDims]=nComp;
    buffer_length *= nComp;
    }

  double   buffer_min,buffer_max;
  mitype_t volume_data_type=MI_TYPE_UBYTE;

  switch(this->GetComponentType() )
    {
    case UCHAR:
      volume_data_type=MI_TYPE_UBYTE;
      get_buffer_min_max<unsigned char>(buffer,buffer_length,buffer_min,buffer_max);
      break;
    case CHAR:
      volume_data_type=MI_TYPE_BYTE;
      get_buffer_min_max<char>(buffer,buffer_length,buffer_min,buffer_max);
      break;
    case USHORT:
      volume_data_type=MI_TYPE_USHORT;
      get_buffer_min_max<unsigned short>(buffer,buffer_length,buffer_min,buffer_max);
      break;
    case SHORT:
      volume_data_type=MI_TYPE_SHORT;
      get_buffer_min_max<short>(buffer,buffer_length,buffer_min,buffer_max);
      break;
    case UINT:
      volume_data_type=MI_TYPE_UINT;
      get_buffer_min_max<unsigned int>(buffer,buffer_length,buffer_min,buffer_max);
      break;
    case INT:
      volume_data_type=MI_TYPE_INT;
      get_buffer_min_max<int>(buffer,buffer_length,buffer_min,buffer_max);
      break;
//     case ULONG://TODO: make sure we are cross-platform here!
//       volume_data_type=MI_TYPE_ULONG;
//       break;
//     case LONG://TODO: make sure we are cross-platform here!
//       volume_data_type=MI_TYPE_LONG;
//       break;
    case FLOAT: //TODO: make sure we are cross-platform here!
      volume_data_type=MI_TYPE_FLOAT;
      get_buffer_min_max<float>(buffer,buffer_length,buffer_min,buffer_max);
      break;
    case DOUBLE: //TODO: make sure we are cross-platform here!
      volume_data_type=MI_TYPE_DOUBLE;
      get_buffer_min_max<double>(buffer,buffer_length,buffer_min,buffer_max);
      break;
    default:
      delete[] start;
      delete[] count;
      itkExceptionMacro(<<"Could not read datatype " << this->GetComponentType() );
    }
  this->WriteImageInformation();

  //by default valid range will be equal to range, to avoid scaling
  if( volume_data_type == this->m_MINCPImpl->m_Volume_type )
    {
    miset_volume_valid_range(this->m_MINCPImpl->m_Volume,buffer_max,buffer_min);
    miset_volume_range(this->m_MINCPImpl->m_Volume,buffer_max,buffer_min);
    }
  else // we are using scaling
    {
      //Special hack to deal with rounding errors
      //unfortunately the dynamic range will be smaller
      //but it's ok since it matches float
      if( this->GetComponentType() == FLOAT )
        {
          if( this->m_MINCPImpl->m_Volume_type == MI_TYPE_INT )
            {
              miset_volume_valid_range(this->m_MINCPImpl->m_Volume,INT_MAX/2,INT_MIN/2);
            }
          else if( this->m_MINCPImpl->m_Volume_type == MI_TYPE_UINT )
            {
              miset_volume_valid_range(this->m_MINCPImpl->m_Volume,UINT_MAX/2,0);
            }
        }
      miset_volume_range(this->m_MINCPImpl->m_Volume,buffer_max,buffer_min);
    }

  if ( miset_real_value_hyperslab(this->m_MINCPImpl->m_Volume, volume_data_type, start, count, const_cast<void*>( buffer) ) < 0 )
    {
    delete[] start;
    delete[] count;
    itkExceptionMacro( << " Can not set real value hyperslab!!\n");
    }
  //TODO: determine what to do if we are streming
  this->CloseVolume();

  delete[] start;
  delete[] count;
}

} // end namespace itk
