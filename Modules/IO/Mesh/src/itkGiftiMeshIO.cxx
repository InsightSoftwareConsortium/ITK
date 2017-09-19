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

#include "itkGiftiMeshIO.h"
#include "itkMetaDataObject.h"

#include <itksys/SystemTools.hxx>
#include "gifti_io.h"

namespace itk
{
//This internal proxy class provides a pointer-like interface to a gifti_image*, by supporting
//conversions between proxy and gifti_image pointer and arrow syntax (e.g., m_GiftiImage->data).
class GiftiMeshIO::GiftiImageProxy
{
  gifti_image* m_ptr;
public:
  GiftiImageProxy(gifti_image* ptr) :
    m_ptr(ptr)
  {
  }

  operator gifti_image*()
  {
    return m_ptr;
  }

  gifti_image* operator->()
  {
    return m_ptr;
  }
};


GiftiMeshIO
::GiftiMeshIO() :
  m_GiftiImageHolder(new GiftiImageProxy(ITK_NULLPTR), true),
  m_GiftiImage(*m_GiftiImageHolder.GetPointer())
{
  this->AddSupportedWriteExtension(".gii");
  m_ReadPointData = true;
  m_Direction.SetIdentity();
  this->m_FileType = BINARY;
  this->m_ByteOrder = BigEndian;
  this->m_UseCompression = true;
}

GiftiMeshIO
::~GiftiMeshIO()
{
}

bool
GiftiMeshIO
::CanReadFile(const char *fileName)
{
  if ( !itksys::SystemTools::FileExists(fileName, true) )
    {
    return false;
    }

  if ( itksys::SystemTools::GetFilenameLastExtension(fileName) != ".gii" )
    {
    return false;
    }

  return true;
}

bool
GiftiMeshIO
::CanWriteFile(const char *fileName)
{
  if ( itksys::SystemTools::GetFilenameLastExtension(fileName) != ".gii" )
    {
    return false;
    }

  return true;
}

void
GiftiMeshIO
::SetDirection(const DirectionType & direction)
{
  for ( unsigned int rr = 0; rr < 4; rr++ )
    {
    for ( unsigned int cc = 0; cc < 4; cc++ )
      {
      m_Direction[rr][cc] = direction[rr][cc];
      }
    }

  this->Modified();
}

GiftiMeshIO::LabelColorContainerPointer
GiftiMeshIO
::GetLabelColorTable()
{
  LabelColorContainerPointer colorMap;
  if(ExposeMetaData< LabelColorContainerPointer >(this->GetMetaDataDictionary(), "colorContainer", colorMap))
    {
    return colorMap;
    }
    else
    {
    return ITK_NULLPTR;
    }
}

GiftiMeshIO::LabelNameContainerPointer
GiftiMeshIO
::GetLabelNameTable()
{
  LabelNameContainerPointer labelMap;
  if(ExposeMetaData< LabelNameContainerPointer >(this->GetMetaDataDictionary(), "labelContainer", labelMap))
    {
    return labelMap;
    }
    else
    {
    return ITK_NULLPTR;
    }
}

void
GiftiMeshIO
::SetLabelColorTable(const LabelColorContainer * colorMap)
{
  EncapsulateMetaData< LabelColorContainerPointer >(this->GetMetaDataDictionary(), "colorContainer", const_cast<LabelColorContainer *>(colorMap));
  this->Modified();
}

void
GiftiMeshIO
::SetLabelNameTable(const LabelNameContainer * labelMap)
{
  EncapsulateMetaData< LabelNameContainerPointer >(this->GetMetaDataDictionary(), "labelContainer", const_cast<LabelNameContainer *>(labelMap));
  this->Modified();
}

void
GiftiMeshIO
::ReadMeshInformation()
{
  // Get gifti image pointer
  m_GiftiImage = gifti_read_image(this->GetFileName(), false);

  // Whether reading is successful
  if ( m_GiftiImage == ITK_NULLPTR )
    {
    itkExceptionMacro(<< this->GetFileName() << " is not recognized as a GIFTI file");
    }

  // Number of data array
  for ( int ii = 0; ii < m_GiftiImage->numDA; ++ii )
    {
    if ( m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_POINTSET )
      {
      if ( m_GiftiImage->darray[ii]->num_dim > 0 )
        {
        this->m_NumberOfPoints = m_GiftiImage->darray[ii]->dims[0];
        }

      if ( m_GiftiImage->darray[ii]->num_dim > 1 )
        {
        this->m_PointDimension = m_GiftiImage->darray[ii]->dims[1];
        }
      this->m_UpdatePoints = true;

      switch ( m_GiftiImage->darray[ii]->datatype )
        {
        case NIFTI_TYPE_INT8:
          this->m_PointComponentType = CHAR;
          break;
        case NIFTI_TYPE_UINT8:
          this->m_PointComponentType = UCHAR;
          break;
        case NIFTI_TYPE_INT16:
          this->m_PointComponentType = SHORT;
          break;
        case NIFTI_TYPE_UINT16:
          this->m_PointComponentType = USHORT;
          break;
        case NIFTI_TYPE_INT32:
          this->m_PointComponentType = INT;
          break;
        case NIFTI_TYPE_UINT32:
          this->m_PointComponentType = UINT;
          break;
        case NIFTI_TYPE_INT64:
          this->m_PointComponentType = LONGLONG;
          break;
        case NIFTI_TYPE_UINT64:
          this->m_PointComponentType = ULONGLONG;
          break;
        case NIFTI_TYPE_FLOAT32:
          this->m_PointComponentType = FLOAT;
          break;
        case NIFTI_TYPE_FLOAT64:
          this->m_PointComponentType = DOUBLE;
          break;
        case NIFTI_TYPE_FLOAT128:
          this->m_PointComponentType = LDOUBLE;
          break;
        default:
          itkExceptionMacro(<< "Unknown point component type");
        }

      // get coord system
      if ( m_GiftiImage->darray[ii]->numCS )
        {
        for ( unsigned int rr = 0; rr < 4; rr++ )
          {
          for ( unsigned int cc = 0; cc < 4; cc++ )
            {
            m_Direction[rr][cc] = m_GiftiImage->darray[ii]->coordsys[0]->xform[rr][cc];
            }
          }
        }
      }
    else if ( m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_TRIANGLE )
      {
      if ( m_GiftiImage->darray[ii]->num_dim > 0 )
        {
        this->m_NumberOfCells = m_GiftiImage->darray[ii]->dims[0];
        }

      if ( m_GiftiImage->darray[ii]->num_dim > 1 )
        {
        if ( m_GiftiImage->darray[ii]->dims[1] != 3 )
          {
          gifti_free_image( m_GiftiImage );
          itkExceptionMacro(<< "Input mesh is not triangle mesh");
          }
        }
      this->m_CellBufferSize = static_cast< SizeValueType >( m_GiftiImage->darray[ii]->nvals + 2 * this->m_NumberOfCells );
      this->m_UpdateCells = true;

      switch ( m_GiftiImage->darray[ii]->datatype )
        {
        case NIFTI_TYPE_INT8:
          this->m_CellComponentType = CHAR;
          break;
        case NIFTI_TYPE_UINT8:
          this->m_CellComponentType = UCHAR;
          break;
        case NIFTI_TYPE_INT16:
          this->m_CellComponentType = SHORT;
          break;
        case NIFTI_TYPE_UINT16:
          this->m_CellComponentType = USHORT;
          break;
        case NIFTI_TYPE_INT32:
          this->m_CellComponentType = INT;
          break;
        case NIFTI_TYPE_UINT32:
          this->m_CellComponentType = UINT;
          break;
        case NIFTI_TYPE_INT64:
          this->m_CellComponentType = LONGLONG;
          break;
        case NIFTI_TYPE_UINT64:
          this->m_CellComponentType = ULONGLONG;
          break;
        case NIFTI_TYPE_FLOAT32:
          this->m_CellComponentType = FLOAT;
          break;
        case NIFTI_TYPE_FLOAT64:
          this->m_CellComponentType = DOUBLE;
          break;
        case NIFTI_TYPE_FLOAT128:
          this->m_CellComponentType = LDOUBLE;
          break;
        default:
          gifti_free_image( m_GiftiImage );
          itkExceptionMacro(<< "Unknown cell component type");
        }
      }
    else if ( m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_SHAPE )
      {
      if ( m_GiftiImage->darray[ii]->num_dim > 0 )
        {
        // we assume that the data is point data
        if ( this->m_NumberOfPoints != static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) && this->m_NumberOfCells !=
            static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) )
          {
          if ( this->m_NumberOfPoints == 0 && m_ReadPointData )
            {
            this->m_NumberOfPointPixels = m_GiftiImage->darray[ii]->dims[0];
            }
          else if ( this->m_NumberOfCells == 0 && !m_ReadPointData )
            {
            this->m_NumberOfCellPixels = m_GiftiImage->darray[ii]->dims[0];
            }
          else
            {
            gifti_free_image( m_GiftiImage );
            itkExceptionMacro(
              << "Could not read input gifti image because inconsistency of number of point data or number of cell data "
              << this->m_FileName);
            }
          }
        else if ( this->m_NumberOfPoints == static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) )
          {
          this->m_NumberOfPointPixels = m_GiftiImage->darray[ii]->dims[0];
          }
        else if ( this->m_NumberOfCells == static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) )
          {
          this->m_NumberOfCellPixels = m_GiftiImage->darray[ii]->dims[0];
          }

        if ( static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) == this->m_NumberOfPointPixels )
          {
          this->m_UpdatePointData = true;
          this->m_NumberOfPointPixelComponents = 1;
          switch ( m_GiftiImage->darray[ii]->datatype )
            {
            case NIFTI_TYPE_INT8:
              this->m_PointPixelComponentType = CHAR;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_UINT8:
              this->m_PointPixelComponentType = UCHAR;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_INT16:
              this->m_PointPixelComponentType = SHORT;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_UINT16:
              this->m_PointPixelComponentType = USHORT;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_INT32:
              this->m_PointPixelComponentType = INT;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_UINT32:
              this->m_PointPixelComponentType = UINT;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_INT64:
              this->m_PointPixelComponentType = LONGLONG;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_UINT64:
              this->m_PointPixelComponentType = ULONGLONG;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_FLOAT32:
              this->m_PointPixelComponentType = FLOAT;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_FLOAT64:
              this->m_PointPixelComponentType = DOUBLE;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_COMPLEX64:
              this->m_PointPixelComponentType = FLOAT;
              this->m_PointPixelType = COMPLEX;
              this->SetNumberOfPointPixelComponents(2);
              break;
            case NIFTI_TYPE_COMPLEX128:
              this->m_PointPixelComponentType = DOUBLE;
              this->m_PointPixelType = COMPLEX;
              this->SetNumberOfPointPixelComponents(2);
              break;
            case NIFTI_TYPE_RGB24:
              this->m_PointPixelComponentType = UCHAR;
              this->m_PointPixelType = RGB;
              this->SetNumberOfPointPixelComponents(3);
              // TODO:  Need to be able to read/write RGB images into ITK.
              //    case DT_RGB:
              // DEBUG -- Assuming this is a triple, not quad
              // image.setDataType( uiig::DATA_RGBQUAD );
              break;
            case NIFTI_TYPE_RGBA32:
              this->m_PointPixelComponentType = UCHAR;
              this->m_PointPixelType = RGBA;
              this->SetNumberOfPointPixelComponents(4);
              break;
            default:
              gifti_free_image( m_GiftiImage );
              itkExceptionMacro(<< "Unknown data attribute component type");
            }
          }
        else if ( this->m_NumberOfCellPixels == static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) )
          {
          this->m_UpdateCellData = true;
          this->m_NumberOfCellPixelComponents = 1;
          switch ( m_GiftiImage->darray[ii]->datatype )
            {
            case NIFTI_TYPE_INT8:
              this->m_CellPixelComponentType = CHAR;
              this->m_CellPixelType = SCALAR;
              break;
            case NIFTI_TYPE_UINT8:
              this->m_CellPixelComponentType = UCHAR;
              this->m_CellPixelType = SCALAR;
              break;
            case NIFTI_TYPE_INT16:
              this->m_CellPixelComponentType = SHORT;
              this->m_CellPixelType = SCALAR;
              break;
            case NIFTI_TYPE_UINT16:
              this->m_CellPixelComponentType = USHORT;
              this->m_CellPixelType = SCALAR;
              break;
            case NIFTI_TYPE_INT32:
              this->m_CellPixelComponentType = INT;
              this->m_CellPixelType = SCALAR;
              break;
            case NIFTI_TYPE_UINT32:
              this->m_CellPixelComponentType = UINT;
              this->m_CellPixelType = SCALAR;
              break;
            case NIFTI_TYPE_FLOAT32:
              this->m_CellPixelComponentType = FLOAT;
              this->m_CellPixelType = SCALAR;
              break;
            case NIFTI_TYPE_FLOAT64:
              this->m_CellPixelComponentType = DOUBLE;
              this->m_CellPixelType = SCALAR;
              break;
            case NIFTI_TYPE_COMPLEX64:
              this->m_CellPixelComponentType = FLOAT;
              this->m_CellPixelType = COMPLEX;
              this->SetNumberOfCellPixelComponents(2);
              break;
            case NIFTI_TYPE_COMPLEX128:
              this->m_CellPixelComponentType = DOUBLE;
              this->m_CellPixelType = COMPLEX;
              this->SetNumberOfCellPixelComponents(2);
              break;
            case NIFTI_TYPE_RGB24:
              this->m_CellPixelComponentType = UCHAR;
              this->m_CellPixelType = RGB;
              this->SetNumberOfCellPixelComponents(3);
              // TODO:  Need to be able to read/write RGB images into ITK.
              //    case DT_RGB:
              // DEBUG -- Assuming this is a triple, not quad
              // image.setDataType( uiig::DATA_RGBQUAD );
              break;
            case NIFTI_TYPE_RGBA32:
              this->m_CellPixelComponentType = UCHAR;
              this->m_CellPixelType = RGBA;
              this->SetNumberOfCellPixelComponents(4);
              break;
            default:
              break;
            }
          }
        }
      }
    else if ( m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_VECTOR )
      {
      if ( m_GiftiImage->darray[ii]->num_dim > 0 )
        {
        if ( this->m_NumberOfPoints != static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) && this->m_NumberOfCells !=
            static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) )
          {
          if ( this->m_NumberOfPoints == 0 && m_ReadPointData )
            {
            this->m_NumberOfPointPixels = m_GiftiImage->darray[ii]->dims[0];
            }
          else if ( this->m_NumberOfCells == 0 && !m_ReadPointData )
            {
            this->m_NumberOfCellPixels = m_GiftiImage->darray[ii]->dims[0];
            }
          else
            {
            gifti_free_image( m_GiftiImage );
            itkExceptionMacro(
              << "Could not read input gifti image because inconsistency of number of point data or number of cell data "
              << this->m_FileName);
            }
          }
        else if ( this->m_NumberOfPoints == static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) )
          {
          this->m_NumberOfPointPixels = m_GiftiImage->darray[ii]->dims[0];
          }
        else if ( this->m_NumberOfCells == static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) )
          {
          this->m_NumberOfCellPixels = m_GiftiImage->darray[ii]->dims[0];
          }

        if ( static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) == this->m_NumberOfPointPixels )
          {
          this->m_UpdatePointData = true;
          if ( m_GiftiImage->darray[ii]->num_dim > 1 )
            {
            this->m_NumberOfPointPixelComponents = m_GiftiImage->darray[ii]->dims[1];

            switch ( m_GiftiImage->darray[ii]->datatype )
              {
              case NIFTI_TYPE_INT8:
                this->m_PointPixelComponentType = CHAR;
                this->m_PointPixelType = VECTOR;
                break;
              case NIFTI_TYPE_UINT8:
                this->m_PointPixelComponentType = UCHAR;
                this->m_PointPixelType = VECTOR;
                break;
              case NIFTI_TYPE_INT16:
                this->m_PointPixelComponentType = SHORT;
                this->m_PointPixelType = VECTOR;
                break;
              case NIFTI_TYPE_UINT16:
                this->m_PointPixelComponentType = USHORT;
                this->m_PointPixelType = VECTOR;
                break;
              case NIFTI_TYPE_INT32:
                this->m_PointPixelComponentType = INT;
                this->m_PointPixelType = VECTOR;
                break;
              case NIFTI_TYPE_UINT32:
                this->m_PointPixelComponentType = UINT;
                this->m_PointPixelType = VECTOR;
                break;
              case NIFTI_TYPE_INT64:
                this->m_PointPixelComponentType = LONGLONG;
                this->m_PointPixelType = VECTOR;
                break;
              case NIFTI_TYPE_UINT64:
                this->m_PointPixelComponentType = ULONGLONG;
                this->m_PointPixelType = VECTOR;
                break;
              case NIFTI_TYPE_FLOAT32:
                this->m_PointPixelComponentType = FLOAT;
                this->m_PointPixelType = VECTOR;
                break;
              case NIFTI_TYPE_FLOAT64:
                this->m_PointPixelComponentType = DOUBLE;
                this->m_PointPixelType = VECTOR;
                break;
              case NIFTI_TYPE_COMPLEX64:
                this->m_PointPixelComponentType = FLOAT;
                this->m_PointPixelType = COMPLEX;
                this->SetNumberOfPointPixelComponents(2);
                break;
              case NIFTI_TYPE_COMPLEX128:
                this->m_PointPixelComponentType = DOUBLE;
                this->m_PointPixelType = COMPLEX;
                this->SetNumberOfPointPixelComponents(2);
                break;
              case NIFTI_TYPE_RGB24:
                this->m_PointPixelComponentType = UCHAR;
                this->m_PointPixelType = RGB;
                this->SetNumberOfPointPixelComponents(3);
                // TODO:  Need to be able to read/write RGB images into ITK.
                //    case DT_RGB:
                // DEBUG -- Assuming this is a triple, not quad
                // image.setDataType( uiig::DATA_RGBQUAD );
                break;
              case NIFTI_TYPE_RGBA32:
                this->m_PointPixelComponentType = UCHAR;
                this->m_PointPixelType = RGBA;
                this->SetNumberOfPointPixelComponents(4);
                break;
              default:
                gifti_free_image( m_GiftiImage );
                itkExceptionMacro(<< "Unknown data attribute component type");
              }
            }
          }
        else if ( this->m_NumberOfCellPixels == static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) )
          {
          this->m_UpdateCellData = true;
          if ( m_GiftiImage->darray[ii]->num_dim > 1 )
            {
            this->m_NumberOfCellPixelComponents = m_GiftiImage->darray[ii]->dims[1];

            switch ( m_GiftiImage->darray[ii]->datatype )
              {
              case NIFTI_TYPE_INT8:
                this->m_CellPixelComponentType = CHAR;
                this->m_CellPixelType = VECTOR;
                break;
              case NIFTI_TYPE_UINT8:
                this->m_CellPixelComponentType = UCHAR;
                this->m_CellPixelType = VECTOR;
                break;
              case NIFTI_TYPE_INT16:
                this->m_CellPixelComponentType = SHORT;
                this->m_CellPixelType = VECTOR;
                break;
              case NIFTI_TYPE_UINT16:
                this->m_CellPixelComponentType = USHORT;
                this->m_CellPixelType = VECTOR;
                break;
              case NIFTI_TYPE_INT32:
                this->m_CellPixelComponentType = INT;
                this->m_CellPixelType = VECTOR;
                break;
              case NIFTI_TYPE_UINT32:
                this->m_CellPixelComponentType = UINT;
                this->m_CellPixelType = VECTOR;
                break;
              case NIFTI_TYPE_FLOAT32:
                this->m_CellPixelComponentType = FLOAT;
                this->m_CellPixelType = VECTOR;
                break;
              case NIFTI_TYPE_FLOAT64:
                this->m_CellPixelComponentType = DOUBLE;
                this->m_CellPixelType = VECTOR;
                break;
              case NIFTI_TYPE_COMPLEX64:
                this->m_CellPixelComponentType = FLOAT;
                this->m_CellPixelType = COMPLEX;
                this->SetNumberOfCellPixelComponents(2);
                break;
              case NIFTI_TYPE_COMPLEX128:
                this->m_CellPixelComponentType = DOUBLE;
                this->m_CellPixelType = COMPLEX;
                this->SetNumberOfCellPixelComponents(2);
                break;
              case NIFTI_TYPE_RGB24:
                this->m_CellPixelComponentType = UCHAR;
                this->m_CellPixelType = RGB;
                this->SetNumberOfCellPixelComponents(3);
                // TODO:  Need to be able to read/write RGB images into ITK.
                //    case DT_RGB:
                // DEBUG -- Assuming this is a triple, not quad
                // image.setDataType( uiig::DATA_RGBQUAD );
                break;
              case NIFTI_TYPE_RGBA32:
                this->m_CellPixelComponentType = UCHAR;
                this->m_CellPixelType = RGBA;
                this->SetNumberOfCellPixelComponents(4);
                break;
              default:
                break;
              }
            }
          }
        }
      }
    else if ( m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_LABEL )
      {
      MetaDataDictionary & metaDic = this->GetMetaDataDictionary();
      if ( m_GiftiImage->labeltable.rgba )
        {
        LabelColorContainerPointer colorMap = LabelColorContainer::New();
        for ( int mm = 0; mm < m_GiftiImage->labeltable.length; ++mm )
          {
          RGBAPixelType pp;
          for ( int nn = 0; nn < 4; ++nn )
            {
            pp.SetNthComponent(nn, m_GiftiImage->labeltable.rgba[mm * 4 + nn]);
            }
          colorMap->InsertElement(m_GiftiImage->labeltable.key[mm], pp);
          }

        EncapsulateMetaData< LabelColorContainerPointer >(metaDic, "colorContainer", colorMap);
        }

      if ( m_GiftiImage->labeltable.label )
        {
        LabelNameContainerPointer labelMap = LabelNameContainer::New();
        for ( int mm = 0; mm < m_GiftiImage->labeltable.length; ++mm )
          {
          if ( m_GiftiImage->labeltable.label[mm] )
            {
            labelMap->InsertElement(m_GiftiImage->labeltable.key[mm], m_GiftiImage->labeltable.label[mm]);
            }
          else
            {
            labelMap->InsertElement(m_GiftiImage->labeltable.key[mm], "");
            }
          }

        EncapsulateMetaData< LabelNameContainerPointer >(metaDic, "labelContainer", labelMap);
        }

      if ( m_GiftiImage->darray[ii]->num_dim > 0 )
        {
        if ( this->m_NumberOfPoints != static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) && this->m_NumberOfCells !=
            static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) )
          {
          if ( this->m_NumberOfPoints == 0 && m_ReadPointData )
            {
            this->m_NumberOfPointPixels = m_GiftiImage->darray[ii]->dims[0];
            }
          else if ( this->m_NumberOfCells == 0 && !m_ReadPointData )
            {
            this->m_NumberOfCellPixels = m_GiftiImage->darray[ii]->dims[0];
            }
          else
            {
            gifti_free_image( m_GiftiImage );
            itkExceptionMacro(
              << "Could not read input gifti image because inconsistency of number of point data or number of cell data "
              << this->m_FileName);
            }
          }
        else if ( this->m_NumberOfPoints == static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) )
          {
          this->m_NumberOfPointPixels = m_GiftiImage->darray[ii]->dims[0];
          }
        else if ( this->m_NumberOfCells == static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) )
          {
          this->m_NumberOfCellPixels = m_GiftiImage->darray[ii]->dims[0];
          }

        if ( static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) == this->m_NumberOfPointPixels )
          {
          this->m_UpdatePointData = true;
          this->m_NumberOfPointPixelComponents = 1;
          switch ( m_GiftiImage->darray[ii]->datatype )
            {
            case NIFTI_TYPE_INT8:
              this->m_PointPixelComponentType = CHAR;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_UINT8:
              this->m_PointPixelComponentType = UCHAR;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_INT16:
              this->m_PointPixelComponentType = SHORT;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_UINT16:
              this->m_PointPixelComponentType = USHORT;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_INT32:
              this->m_PointPixelComponentType = INT;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_UINT32:
              this->m_PointPixelComponentType = UINT;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_INT64:
              this->m_PointPixelComponentType = LONGLONG;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_UINT64:
              this->m_PointPixelComponentType = ULONGLONG;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_FLOAT32:
              this->m_PointPixelComponentType = FLOAT;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_FLOAT64:
              this->m_PointPixelComponentType = DOUBLE;
              this->m_PointPixelType = SCALAR;
              break;
            case NIFTI_TYPE_COMPLEX64:
              this->m_PointPixelComponentType = FLOAT;
              this->m_PointPixelType = COMPLEX;
              this->SetNumberOfPointPixelComponents(2);
              break;
            case NIFTI_TYPE_COMPLEX128:
              this->m_PointPixelComponentType = DOUBLE;
              this->m_PointPixelType = COMPLEX;
              this->SetNumberOfPointPixelComponents(2);
              break;
            case NIFTI_TYPE_RGB24:
              this->m_PointPixelComponentType = UCHAR;
              this->m_PointPixelType = RGB;
              this->SetNumberOfPointPixelComponents(3);
              // TODO:  Need to be able to read/write RGB images into ITK.
              //    case DT_RGB:
              // DEBUG -- Assuming this is a triple, not quad
              // image.setDataType( uiig::DATA_RGBQUAD );
              break;
            case NIFTI_TYPE_RGBA32:
              this->m_PointPixelComponentType = UCHAR;
              this->m_PointPixelType = RGBA;
              this->SetNumberOfPointPixelComponents(4);
              break;
            default:
              gifti_free_image( m_GiftiImage );
              itkExceptionMacro(<< "Unknown data attribute component type");
            }
          }
        else if ( this->m_NumberOfCellPixels == static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) )
          {
          this->m_UpdateCellData = true;
          this->m_NumberOfCellPixelComponents = 1;
          switch ( m_GiftiImage->darray[ii]->datatype )
            {
            case NIFTI_TYPE_INT8:
              this->m_CellPixelComponentType = CHAR;
              this->m_CellPixelType = SCALAR;
              break;
            case NIFTI_TYPE_UINT8:
              this->m_CellPixelComponentType = UCHAR;
              this->m_CellPixelType = SCALAR;
              break;
            case NIFTI_TYPE_INT16:
              this->m_CellPixelComponentType = SHORT;
              this->m_CellPixelType = SCALAR;
              break;
            case NIFTI_TYPE_UINT16:
              this->m_CellPixelComponentType = USHORT;
              this->m_CellPixelType = SCALAR;
              break;
            case NIFTI_TYPE_INT32:
              this->m_CellPixelComponentType = INT;
              this->m_CellPixelType = SCALAR;
              break;
            case NIFTI_TYPE_UINT32:
              this->m_CellPixelComponentType = UINT;
              this->m_CellPixelType = SCALAR;
              break;
            case NIFTI_TYPE_FLOAT32:
              this->m_CellPixelComponentType = FLOAT;
              this->m_CellPixelType = SCALAR;
              break;
            case NIFTI_TYPE_FLOAT64:
              this->m_CellPixelComponentType = DOUBLE;
              this->m_CellPixelType = SCALAR;
              break;
            case NIFTI_TYPE_COMPLEX64:
              this->m_CellPixelComponentType = FLOAT;
              this->m_CellPixelType = COMPLEX;
              this->SetNumberOfCellPixelComponents(2);
              break;
            case NIFTI_TYPE_COMPLEX128:
              this->m_CellPixelComponentType = DOUBLE;
              this->m_CellPixelType = COMPLEX;
              this->SetNumberOfCellPixelComponents(2);
              break;
            case NIFTI_TYPE_RGB24:
              this->m_CellPixelComponentType = UCHAR;
              this->m_CellPixelType = RGB;
              this->SetNumberOfCellPixelComponents(3);
              // TODO:  Need to be able to read/write RGB images into ITK.
              //    case DT_RGB:
              // DEBUG -- Assuming this is a triple, not quad
              // image.setDataType( uiig::DATA_RGBQUAD );
              break;
            case NIFTI_TYPE_RGBA32:
              this->m_CellPixelComponentType = UCHAR;
              this->m_CellPixelType = RGBA;
              this->SetNumberOfCellPixelComponents(4);
              break;
            default:
              break;
            }
          }
        }
      }
    }
  gifti_free_image( m_GiftiImage );
}

void
GiftiMeshIO
::ReadPoints(void *buffer)
{
  // Get gifti image pointer
  m_GiftiImage = gifti_read_image(this->GetFileName(), true);

  // Whter reading is successful
  if ( m_GiftiImage == ITK_NULLPTR )
    {
    itkExceptionMacro(<< this->GetFileName() << " is not recognized as a GIFTI file");
    }

  // Number of data array
  const SizeValueType pointsBufferSize = this->m_NumberOfPoints * this->m_PointDimension;

  for ( int ii = 0; ii < m_GiftiImage->numDA; ++ii )
    {
    if ( m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_POINTSET )
      {
      memcpy(buffer, m_GiftiImage->darray[ii]->data, pointsBufferSize * m_GiftiImage->darray[ii]->nbyper);
      }
    }

  gifti_free_image( m_GiftiImage );
}

void
GiftiMeshIO
::ReadCells(void *buffer)
{
  // Get gifti image pointer
  m_GiftiImage = gifti_read_image(this->GetFileName(), true);

  // Whter reading is successful
  if ( m_GiftiImage == ITK_NULLPTR )
    {
    itkExceptionMacro(<< this->GetFileName() << " is not recognized as a GIFTI file");
    }

  // Number of data array
  for ( int ii = 0; ii < m_GiftiImage->numDA; ++ii )
    {
    if ( m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_TRIANGLE )
      {
      switch ( this->m_CellComponentType )
        {
        case CHAR:
          {
          this->WriteCellsBuffer(static_cast< char * >( m_GiftiImage->darray[ii]->data ),
                                 static_cast< char * >( buffer ),
                                 TRIANGLE_CELL,
                                 3,
                                 this->m_NumberOfCells);
          break;
          }
        case UCHAR:
          {
          this->WriteCellsBuffer(static_cast< unsigned char * >( m_GiftiImage->darray[ii]->data ),
                                 static_cast< unsigned char * >( buffer ),
                                 TRIANGLE_CELL,
                                 3,
                                 this->m_NumberOfCells);
          break;
          }
        case USHORT:
          {
          this->WriteCellsBuffer(static_cast< unsigned short * >( m_GiftiImage->darray[ii]->data ),
                                 static_cast< unsigned short * >( buffer ),
                                 TRIANGLE_CELL,
                                 3,
                                 this->m_NumberOfCells);
          break;
          }
        case SHORT:
          {
          this->WriteCellsBuffer(static_cast< short * >( m_GiftiImage->darray[ii]->data ),
                                 static_cast< short * >( buffer ),
                                 TRIANGLE_CELL,
                                 3,
                                 this->m_NumberOfCells);
          break;
          }
        case UINT:
          {
          this->WriteCellsBuffer(static_cast< unsigned int * >( m_GiftiImage->darray[ii]->data ),
                                 static_cast< unsigned int * >( buffer ),
                                 TRIANGLE_CELL,
                                 3,
                                 this->m_NumberOfCells);
          break;
          }
        case INT:
          {
          this->WriteCellsBuffer(static_cast< int * >( m_GiftiImage->darray[ii]->data ),
                                 static_cast< int * >( buffer ),
                                 TRIANGLE_CELL,
                                 3,
                                 this->m_NumberOfCells);
          break;
          }
        case ULONG:
          {
          this->WriteCellsBuffer(static_cast< unsigned long * >( m_GiftiImage->darray[ii]->data ),
                                 static_cast< unsigned long * >( buffer ),
                                 TRIANGLE_CELL,
                                 3,
                                 this->m_NumberOfCells);
          break;
          }
        case LONG:
          {
          this->WriteCellsBuffer(static_cast< long * >( m_GiftiImage->darray[ii]->data ),
                                 static_cast< long * >( buffer ),
                                 TRIANGLE_CELL,
                                 3,
                                 this->m_NumberOfCells);
          break;
          }
        case LONGLONG:
          {
          this->WriteCellsBuffer(static_cast< long long * >( m_GiftiImage->darray[ii]->data ),
                                 static_cast< long long * >( buffer ),
                                 TRIANGLE_CELL,
                                 3,
                                 this->m_NumberOfCells);
          break;
          }
        case ULONGLONG:
          {
          this->WriteCellsBuffer(static_cast< unsigned long long * >( m_GiftiImage->darray[ii]->data ),
                                 static_cast< unsigned long long * >( buffer ), TRIANGLE_CELL, 3, this->m_NumberOfCells);
          break;
          }
        case FLOAT:
          {
          this->WriteCellsBuffer(static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                                 static_cast< float * >( buffer ),
                                 TRIANGLE_CELL,
                                 3,
                                 this->m_NumberOfCells);
          break;
          }
        case DOUBLE:
          {
          this->WriteCellsBuffer(static_cast< double * >( m_GiftiImage->darray[ii]->data ),
                                 static_cast< double * >( buffer ),
                                 TRIANGLE_CELL,
                                 3,
                                 this->m_NumberOfCells);
          break;
          }
        case LDOUBLE:
          {
          this->WriteCellsBuffer(static_cast< long double * >( m_GiftiImage->darray[ii]->data ),
                                 static_cast< long double * >( buffer ),
                                 TRIANGLE_CELL,
                                 3,
                                 this->m_NumberOfCells);
          break;
          }
        default:
          {
          gifti_free_image( m_GiftiImage );
          itkExceptionMacro(<< "Unknown cell data pixel component type" << std::endl);
          }
        }
      }
    }

  gifti_free_image( m_GiftiImage );
}

void
GiftiMeshIO
::ReadPointData(void *buffer)
{
  // Get gifti image pointer
  m_GiftiImage = gifti_read_image(this->GetFileName(), true);

  // Whter reading is successful
  if ( m_GiftiImage == ITK_NULLPTR )
    {
    itkExceptionMacro(<< this->GetFileName() << " is not recognized as a GIFTI file");
    }

  // Read point or cell Data
  for ( int ii = 0; ii < m_GiftiImage->numDA; ++ii )
    {
    if ( m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_SHAPE  || m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_VECTOR
         || m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_LABEL )
      {
      if ( static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) == this->m_NumberOfPointPixels )
        {
        const SizeValueType pointDataBufferSize = this->m_NumberOfPointPixels * this->m_NumberOfPointPixelComponents;
        memcpy(buffer, m_GiftiImage->darray[ii]->data, pointDataBufferSize * m_GiftiImage->darray[ii]->nbyper);
        }
      }
    }

  gifti_free_image( m_GiftiImage );
}

void
GiftiMeshIO
::ReadCellData(void *buffer)
{
  // Get gifti image pointer
  m_GiftiImage = gifti_read_image(this->GetFileName(), true);

  // Whter reading is successful
  if ( m_GiftiImage == ITK_NULLPTR )
    {
    itkExceptionMacro(<< this->GetFileName() << " is not recognized as a GIFTI file");
    }

  // Read point or cell Data
  for ( int ii = 0; ii < m_GiftiImage->numDA; ++ii )
    {
    if ( m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_SHAPE  || m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_VECTOR
         || m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_LABEL )
      {
      if ( static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) == this->m_NumberOfCellPixels )
        {
        const SizeValueType cellDataBufferSize = this->m_NumberOfCellPixels * this->m_NumberOfCellPixelComponents;
        memcpy(buffer, m_GiftiImage->darray[ii]->data, cellDataBufferSize * m_GiftiImage->darray[ii]->nbyper);
        }
      }
    }

  gifti_free_image( m_GiftiImage );
}

void
GiftiMeshIO
::WriteMeshInformation()
{
  // Define number of data arrays
  int nda = 0;

  if ( this->m_UpdatePoints )
    {
    nda++;
    }

  if ( this->m_UpdateCells )
    {
    nda++;
    }

  if ( this->m_UpdatePointData )
    {
    nda++;
    }

  if ( this->m_UpdateCellData )
    {
    nda++;
    }

  // Create a new gifti image
  int dims[6] = {0};
  m_GiftiImage = gifti_create_image(nda, NIFTI_INTENT_POINTSET, NIFTI_TYPE_UINT32, 0, dims, 0);

  // Whter reading is successful
  if ( m_GiftiImage == ITK_NULLPTR )
    {
    itkExceptionMacro(<< "Could not create a new gifti image");
    }

  // write labelTable using labelMap and colorMap
  MetaDataDictionary &      metaDic = this->GetMetaDataDictionary();
  LabelNameContainerPointer labelMap;
  if ( ExposeMetaData< LabelNameContainerPointer >(metaDic, "labelContainer", labelMap) )
    {
    gifti_clear_LabelTable(&m_GiftiImage->labeltable);
    m_GiftiImage->labeltable.length = labelMap->Size();

    m_GiftiImage->labeltable.key = (int *)malloc( labelMap->Size() * sizeof( int ) );
    m_GiftiImage->labeltable.label = (char **)malloc( labelMap->Size() * sizeof( char * ) );

    unsigned int mm = 0;
    for ( LabelNameContainer::ConstIterator lt = labelMap->Begin(); lt != labelMap->End(); ++lt )
      {
      m_GiftiImage->labeltable.key[mm] = lt->Index();
      m_GiftiImage->labeltable.label[mm] = gifti_strdup( lt->Value().c_str() );
      mm++;
      }

    LabelColorContainerPointer colorMap;
    if ( ExposeMetaData< LabelColorContainerPointer >(metaDic, "colorContainer", colorMap) )
      {
      m_GiftiImage->labeltable.rgba = (float *)malloc( colorMap->Size() * 4 * sizeof( float ) );
      unsigned int kk = 0;
      for ( LabelColorContainer::ConstIterator lt = colorMap->Begin(); lt != colorMap->End(); ++lt )
        {
        for ( int nn = 0; nn < 4; ++nn )
          {
          m_GiftiImage->labeltable.rgba[kk * 4 + nn] = lt->Value().GetNthComponent(nn);
          }
        kk++;
        }
      }
    }

  nda = 0;
  int dalist[1];

  // Update points dataarray information
  if ( this->m_UpdatePoints )
    {
    // used data array list for points
    dalist[0] = nda++;

    // define dimensions
    int dimensions[6] = {0};
    dimensions[0] = this->m_NumberOfPoints;
    dimensions[1] = this->m_PointDimension;
    m_GiftiImage->darray[dalist[0]]->num_dim = 2;

    long long nvals = 1;
    for ( int ii = 0; ii < m_GiftiImage->darray[dalist[0]]->num_dim; ii++ )
      {
      m_GiftiImage->darray[dalist[0]]->dims[ii] = dimensions[ii];
      nvals *= dimensions[ii];
      }

    m_GiftiImage->darray[dalist[0]]->nvals = nvals;
    int dtype = NIFTI_TYPE_FLOAT32;

    // Set intent of data array
    gifti_set_atr_in_DAs(m_GiftiImage, "Intent", gifti_intent_to_string(NIFTI_INTENT_POINTSET), dalist, 1);

    // Set data type of data array
    gifti_set_atr_in_DAs(m_GiftiImage, "DataType", gifti_datatype2str(dtype), dalist, 1);

    // Set data encoding type
    if ( this->m_FileType == ASCII )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Encoding", "ASCII", dalist, 1);
      }
    else if ( this->m_FileType == BINARY && !this->m_UseCompression )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Encoding", "Base64Binary", dalist, 1);
      }
    else
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Encoding", "GZipBase64Binary", dalist, 1);
      }

    // set endian type
    if ( this->m_ByteOrder == LittleEndian )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Endian", "LittleEndian", dalist, 1);
      }
    else if ( this->m_ByteOrder == BigEndian )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Endian", "BigEndian", dalist, 1);
      }

    // Write coord system matrix
    m_GiftiImage->darray[dalist[0]]->numCS = 0;
    gifti_add_empty_CS(m_GiftiImage->darray[dalist[0]]);
    if ( m_GiftiImage->darray[dalist[0]]->numCS )
      {
      for ( unsigned int rr = 0; rr < 4; rr++ )
        {
        for ( unsigned int cc = 0; cc < 4; cc++ )
          {
          m_GiftiImage->darray[dalist[0]]->coordsys[0]->xform[rr][cc] = m_Direction[rr][cc];
          }
        }
      }

    gifti_update_nbyper(m_GiftiImage);

    // Allocate memory
    gifti_alloc_DA_data(m_GiftiImage, dalist, 1);
    }

  // Update cells
  if ( this->m_UpdateCells )
    {
    // used data array list for points
    dalist[0] = nda++;

    // define dimensions
    int dimensions[6] = {0};
    dimensions[0] = this->m_NumberOfCells;
    dimensions[1] = 3;
    m_GiftiImage->darray[dalist[0]]->num_dim = 2;

    long long nvals = 1;
    for ( int ii = 0; ii < m_GiftiImage->darray[dalist[0]]->num_dim; ii++ )
      {
      m_GiftiImage->darray[dalist[0]]->dims[ii] = dimensions[ii];
      nvals *= dimensions[ii];
      }

    m_GiftiImage->darray[dalist[0]]->nvals = nvals;
    int dtype = NIFTI_TYPE_INT32;

    // Set intent of data array
    gifti_set_atr_in_DAs(m_GiftiImage, "Intent", gifti_intent_to_string(NIFTI_INTENT_TRIANGLE), dalist, 1);

    // Set data type of data array
    gifti_set_atr_in_DAs(m_GiftiImage, "DataType", gifti_datatype2str(dtype), dalist, 1);

    // Set data encoding type
    if ( this->m_FileType == ASCII )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Encoding", "ASCII", dalist, 1);
      }
    else if ( this->m_FileType == BINARY && !this->m_UseCompression )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Encoding", "Base64Binary", dalist, 1);
      }
    else
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Encoding", "GZipBase64Binary", dalist, 1);
      }

    // set endian type
    if ( this->m_ByteOrder == LittleEndian )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Endian", "LittleEndian", dalist, 1);
      }
    else if ( this->m_ByteOrder == BigEndian )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Endian", "BigEndian", dalist, 1);
      }

    gifti_update_nbyper(m_GiftiImage);

    // Allocate memory
    gifti_alloc_DA_data(m_GiftiImage, dalist, 1);
    }

  // Update point data
  if ( this->m_UpdatePointData )
    {
    // used data array list for points
    dalist[0] = nda++;

    // define dimensions
    int dimensions[6] = {0};
    dimensions[0] = this->m_NumberOfPointPixels;
    dimensions[1] = this->m_NumberOfPointPixelComponents;
    if ( this->m_NumberOfPointPixelComponents == 1 )
      {
      m_GiftiImage->darray[dalist[0]]->num_dim = 1;
      }
    else
      {
      m_GiftiImage->darray[dalist[0]]->num_dim = 2;
      }

    long long nvals = 1;
    for ( int ii = 0; ii < m_GiftiImage->darray[dalist[0]]->num_dim; ii++ )
      {
      m_GiftiImage->darray[dalist[0]]->dims[ii] = dimensions[ii];
      nvals *= dimensions[ii];
      }

    m_GiftiImage->darray[dalist[0]]->nvals = nvals;
    int dtype = NIFTI_TYPE_FLOAT32;

    // Set intent of data array
    if ( this->m_NumberOfPointPixelComponents == 1 )
      {
      if ( m_GiftiImage->labeltable.length )
        {
        dtype = NIFTI_TYPE_INT32;
        gifti_set_atr_in_DAs(m_GiftiImage, "Intent", gifti_intent_to_string(NIFTI_INTENT_LABEL), dalist, 1);
        }
      else
        {
        gifti_set_atr_in_DAs(m_GiftiImage, "Intent", gifti_intent_to_string(NIFTI_INTENT_SHAPE), dalist, 1);
        }
      }
    else if ( this->m_NumberOfPointPixelComponents == 3 )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Intent", gifti_intent_to_string(NIFTI_INTENT_VECTOR), dalist, 1);
      }
    else
      {
      gifti_free_image( m_GiftiImage );
      itkExceptionMacro("Unsupported number of components in point data pixel : " << this->m_NumberOfPointPixelComponents);
      }

    // Set data type of data array
    gifti_set_atr_in_DAs(m_GiftiImage, "DataType", gifti_datatype2str(dtype), dalist, 1);

    // Set data encoding type
    if ( this->m_FileType == ASCII )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Encoding", "ASCII", dalist, 1);
      }
    else if ( this->m_FileType == BINARY && !this->m_UseCompression )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Encoding", "Base64Binary", dalist, 1);
      }
    else
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Encoding", "GZipBase64Binary", dalist, 1);
      }

    // set endian type
    if ( this->m_ByteOrder == LittleEndian )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Endian", "LittleEndian", dalist, 1);
      }
    else if ( this->m_ByteOrder == BigEndian )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Endian", "BigEndian", dalist, 1);
      }

    gifti_update_nbyper(m_GiftiImage);

    // Allocate memory
    gifti_alloc_DA_data(m_GiftiImage, dalist, 1);
    }

  // Update cell data
  if ( this->m_UpdateCellData )
    {
    // used data array list for points
    dalist[0] = nda++;

    // define dimensions
    int dimensions[6] = {0};
    dimensions[0] = this->m_NumberOfCellPixels;
    dimensions[1] = this->m_NumberOfCellPixelComponents;
    if ( this->m_NumberOfCellPixelComponents == 1 )
      {
      m_GiftiImage->darray[dalist[0]]->num_dim = 1;
      }
    else
      {
      m_GiftiImage->darray[dalist[0]]->num_dim = 2;
      }

    long long nvals = 1;
    for ( int ii = 0; ii < m_GiftiImage->darray[dalist[0]]->num_dim; ii++ )
      {
      m_GiftiImage->darray[dalist[0]]->dims[ii] = dimensions[ii];
      nvals *= dimensions[ii];
      }

    m_GiftiImage->darray[dalist[0]]->nvals = nvals;
    int dtype = NIFTI_TYPE_FLOAT32;

    // Set intent of data array
    if ( this->m_NumberOfCellPixelComponents == 1 )
      {
      if ( m_GiftiImage->labeltable.length )
        {
        dtype = NIFTI_TYPE_INT32;
        gifti_set_atr_in_DAs(m_GiftiImage, "Intent", gifti_intent_to_string(NIFTI_INTENT_LABEL), dalist, 1);
        }
      else
        {
        gifti_set_atr_in_DAs(m_GiftiImage, "Intent", gifti_intent_to_string(NIFTI_INTENT_SHAPE), dalist, 1);
        }
      }
    else if ( this->m_NumberOfCellPixelComponents == 3 )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Intent", gifti_intent_to_string(NIFTI_INTENT_VECTOR), dalist, 1);
      }
    else
      {
      gifti_free_image( m_GiftiImage );
      itkExceptionMacro("Unsupported number of components in cell data pixel : " << this->m_NumberOfCellPixelComponents);
      }

    // Set data type of data array
    gifti_set_atr_in_DAs(m_GiftiImage, "DataType", gifti_datatype2str(dtype), dalist, 1);

    // Set data encoding type
    if ( this->m_FileType == ASCII )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Encoding", "ASCII", dalist, 1);
      }
    else if ( this->m_FileType == BINARY && !this->m_UseCompression )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Encoding", "Base64Binary", dalist, 1);
      }
    else
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Encoding", "GZipBase64Binary", dalist, 1);
      }

    // set endian type
    if ( this->m_ByteOrder == LittleEndian )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Endian", "LittleEndian", dalist, 1);
      }
    else if ( this->m_ByteOrder == BigEndian )
      {
      gifti_set_atr_in_DAs(m_GiftiImage, "Endian", "BigEndian", dalist, 1);
      }

    gifti_update_nbyper(m_GiftiImage);

    // Allocate memory
    gifti_alloc_DA_data(m_GiftiImage, dalist, 1);
    }
}

void
GiftiMeshIO
::WritePoints(void *buffer)
{
  const SizeValueType pointsBufferSize = this->m_NumberOfPoints * this->m_PointDimension;

  for ( int ii = 0; ii < m_GiftiImage->numDA; ++ii )
    {
    if ( m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_POINTSET )
      {
      switch ( this->m_PointComponentType )
        {
        case UCHAR:
          {
          ConvertBuffer(static_cast< unsigned char * >( buffer ),
                        static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                        pointsBufferSize);
          break;
          }
        case CHAR:
          {
          ConvertBuffer(static_cast< char * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), pointsBufferSize);
          break;
          }
        case USHORT:
          {
          ConvertBuffer(static_cast< unsigned short * >( buffer ),
                        static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                        pointsBufferSize);
          break;
          }
        case SHORT:
          {
          ConvertBuffer(static_cast< short * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), pointsBufferSize);
          break;
          }
        case UINT:
          {
          ConvertBuffer(static_cast< unsigned int * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), pointsBufferSize);
          break;
          }
        case INT:
          {
          ConvertBuffer(static_cast< int * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), pointsBufferSize);
          break;
          }
        case ULONG:
          {
          ConvertBuffer(static_cast< unsigned long * >( buffer ),
                        static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                        pointsBufferSize);
          break;
          }
        case LONG:
          {
          ConvertBuffer(static_cast< long * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), pointsBufferSize);
          break;
          }
        case ULONGLONG:
          {
          ConvertBuffer(static_cast< unsigned long long * >( buffer ),
                        static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                        pointsBufferSize);
          break;
          }
        case LONGLONG:
          {
          ConvertBuffer(static_cast< long long * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), pointsBufferSize);
          break;
          }
        case FLOAT:
          {
          ConvertBuffer(static_cast< float * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), pointsBufferSize);
          break;
          }
        case DOUBLE:
          {
          ConvertBuffer(static_cast< double * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), pointsBufferSize);
          break;
          }
        case LDOUBLE:
          {
          ConvertBuffer(static_cast< long double * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), pointsBufferSize);
          break;
          }
        default:
          {
          gifti_free_image( m_GiftiImage );
          itkExceptionMacro(<< "Unknown point component type" << std::endl);
          }
        }
      }
    }
}

void
GiftiMeshIO
::WriteCells(void *buffer)
{
  // Get data array contain intent of NIFTI_INTENT_TRIANGLE
  for ( int ii = 0; ii < m_GiftiImage->numDA; ++ii )
    {
    if ( m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_TRIANGLE )
      {
      switch ( this->m_CellComponentType )
        {
        case UCHAR:
          {
          this->ReadCellsBuffer( static_cast< unsigned char * >( buffer ), static_cast< int32_t * >( m_GiftiImage->darray[ii]->data ) );
          break;
          }
        case CHAR:
          {
          this->ReadCellsBuffer( static_cast< char * >( buffer ), static_cast< int32_t * >( m_GiftiImage->darray[ii]->data ) );
          break;
          }
        case USHORT:
          {
          this->ReadCellsBuffer( static_cast< unsigned short * >( buffer ), static_cast< int32_t * >( m_GiftiImage->darray[ii]->data ) );
          break;
          }
        case SHORT:
          {
          this->ReadCellsBuffer( static_cast< short * >( buffer ), static_cast< int32_t * >( m_GiftiImage->darray[ii]->data ) );
          break;
          }
        case UINT:
          {
          this->ReadCellsBuffer( static_cast< unsigned int * >( buffer ), static_cast< int32_t * >( m_GiftiImage->darray[ii]->data ) );
          break;
          }
        case INT:
          {
          this->ReadCellsBuffer( static_cast< int * >( buffer ), static_cast< int32_t * >( m_GiftiImage->darray[ii]->data ) );
          break;
          }
        case ULONG:
          {
          this->ReadCellsBuffer( static_cast< unsigned long * >( buffer ), static_cast< int32_t * >( m_GiftiImage->darray[ii]->data ) );
          break;
          }
        case LONG:
          {
          this->ReadCellsBuffer( static_cast< long * >( buffer ), static_cast< int32_t * >( m_GiftiImage->darray[ii]->data ) );
          break;
          }
        case ULONGLONG:
          {
          this->ReadCellsBuffer( static_cast< unsigned long long * >( buffer ),
                                static_cast< int32_t * >( m_GiftiImage->darray[ii]->data ) );
          break;
          }
        case LONGLONG:
          {
          this->ReadCellsBuffer( static_cast< long long * >( buffer ), static_cast< int32_t * >( m_GiftiImage->darray[ii]->data ) );
          break;
          }
        case FLOAT:
          {
          this->ReadCellsBuffer( static_cast< float * >( buffer ), static_cast< int32_t * >( m_GiftiImage->darray[ii]->data ) );
          break;
          }
        case DOUBLE:
          {
          this->ReadCellsBuffer( static_cast< double * >( buffer ), static_cast< int32_t * >( m_GiftiImage->darray[ii]->data ) );
          break;
          }
        case LDOUBLE:
          {
          this->ReadCellsBuffer( static_cast< long double * >( buffer ), static_cast< int32_t * >( m_GiftiImage->darray[ii]->data ) );
          break;
          }
        default:
          {
          gifti_free_image( m_GiftiImage );
          itkExceptionMacro(<< "Unknown cell component type" << std::endl);
          }
        }
      }
    }
}

void
GiftiMeshIO
::WritePointData(void *buffer)
{
  // Get data array contain intent of NIFTI_INTENT_SHAPE
  for ( int ii = 0; ii < m_GiftiImage->numDA; ++ii )
    {
    if ( m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_SHAPE || m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_VECTOR )
      {
      if ( static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) == this->m_NumberOfPointPixels )
        {
        const SizeValueType pointDataBufferSize = this->m_NumberOfPointPixels * this->m_NumberOfPointPixelComponents;
        switch ( this->m_PointPixelComponentType )
          {
          case UCHAR:
            {
            ConvertBuffer(static_cast< unsigned char * >( buffer ),
                          static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                          pointDataBufferSize);
            break;
            }
          case CHAR:
            {
            ConvertBuffer(static_cast< char * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), pointDataBufferSize);
            break;
            }
          case USHORT:
            {
            ConvertBuffer(static_cast< unsigned short * >( buffer ),
                          static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                          pointDataBufferSize);
            break;
            }
          case SHORT:
            {
            ConvertBuffer(static_cast< short * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), pointDataBufferSize);
            break;
            }
          case UINT:
            {
            ConvertBuffer(static_cast< unsigned int * >( buffer ),
                          static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                          pointDataBufferSize);
            break;
            }
          case INT:
            {
            ConvertBuffer(static_cast< int * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), pointDataBufferSize);
            break;
            }
          case ULONG:
            {
            ConvertBuffer(static_cast< unsigned long * >( buffer ),
                          static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                          pointDataBufferSize);
            break;
            }
          case LONG:
            {
            ConvertBuffer(static_cast< long * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), pointDataBufferSize);
            break;
            }
          case ULONGLONG:
            {
            ConvertBuffer(static_cast< unsigned long long * >( buffer ),
                          static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                          pointDataBufferSize);
            break;
            }
          case LONGLONG:
            {
            ConvertBuffer(static_cast< long long * >( buffer ),
                          static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                          pointDataBufferSize);
            break;
            }
          case FLOAT:
            {
            ConvertBuffer(static_cast< float * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), pointDataBufferSize);
            break;
            }
          case DOUBLE:
            {
            ConvertBuffer(static_cast< double * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), pointDataBufferSize);
            break;
            }
          case LDOUBLE:
            {
            ConvertBuffer(static_cast< long double * >( buffer ),
                          static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                          pointDataBufferSize);
            break;
            }
          default:
            {
            gifti_free_image( m_GiftiImage );
            itkExceptionMacro(<< "Unknown point data pixel component type" << std::endl);
            }
          }
        }
      }
    else if ( m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_LABEL )
      {
      if ( static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) == this->m_NumberOfPointPixels )
        {
        const SizeValueType pointDataBufferSize = this->m_NumberOfPointPixels * this->m_NumberOfPointPixelComponents;

        switch ( this->m_PointPixelComponentType )
          {
          case UCHAR:
            {
            ConvertBuffer(static_cast< unsigned char * >( buffer ),
                          static_cast< int * >( m_GiftiImage->darray[ii]->data ),
                          pointDataBufferSize);
            break;
            }
          case CHAR:
            {
            ConvertBuffer(static_cast< char * >( buffer ), static_cast< int * >( m_GiftiImage->darray[ii]->data ), pointDataBufferSize);
            break;
            }
          case USHORT:
            {
            ConvertBuffer(static_cast< unsigned short * >( buffer ),
                          static_cast< int * >( m_GiftiImage->darray[ii]->data ),
                          pointDataBufferSize);
            break;
            }
          case SHORT:
            {
            ConvertBuffer(static_cast< short * >( buffer ), static_cast< int * >( m_GiftiImage->darray[ii]->data ), pointDataBufferSize);
            break;
            }
          case UINT:
            {
            ConvertBuffer(static_cast< unsigned int * >( buffer ),
                          static_cast< int * >( m_GiftiImage->darray[ii]->data ),
                          pointDataBufferSize);
            break;
            }
          case INT:
            {
            ConvertBuffer(static_cast< int * >( buffer ), static_cast< int * >( m_GiftiImage->darray[ii]->data ), pointDataBufferSize);
            break;
            }
          case ULONG:
            {
            ConvertBuffer(static_cast< unsigned long * >( buffer ),
                          static_cast< int * >( m_GiftiImage->darray[ii]->data ),
                          pointDataBufferSize);
            break;
            }
          case LONG:
            {
            ConvertBuffer(static_cast< long * >( buffer ), static_cast< int * >( m_GiftiImage->darray[ii]->data ), pointDataBufferSize);
            break;
            }
          case ULONGLONG:
            {
            ConvertBuffer(static_cast< unsigned long long * >( buffer ),
                          static_cast< int * >( m_GiftiImage->darray[ii]->data ),
                          pointDataBufferSize);
            break;
            }
          case LONGLONG:
            {
            ConvertBuffer(static_cast< long long * >( buffer ), static_cast< int * >( m_GiftiImage->darray[ii]->data ), pointDataBufferSize);
            break;
            }
          case FLOAT:
            {
            ConvertBuffer(static_cast< float * >( buffer ), static_cast< int * >( m_GiftiImage->darray[ii]->data ), pointDataBufferSize);
            break;
            }
          case DOUBLE:
            {
            ConvertBuffer(static_cast< double * >( buffer ), static_cast< int * >( m_GiftiImage->darray[ii]->data ), pointDataBufferSize);
            break;
            }
          case LDOUBLE:
            {
            ConvertBuffer(static_cast< long double * >( buffer ),
                          static_cast< int * >( m_GiftiImage->darray[ii]->data ),
                          pointDataBufferSize);
            break;
            }
          default:
            {
            gifti_free_image( m_GiftiImage );
            itkExceptionMacro(<< "Unknown point data pixel component type" << std::endl);
            }
          }
        }
      }
    }
}

void
GiftiMeshIO
::WriteCellData(void *buffer)
{
  // Get data array contain intent of NIFTI_INTENT_SHAPE
  for ( int ii = 0; ii < m_GiftiImage->numDA; ++ii )
    {
    if ( m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_SHAPE || m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_VECTOR )
      {
      if ( static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) == this->m_NumberOfCellPixels )
        {
        const SizeValueType cellDataBufferSize = this->m_NumberOfCellPixels * this->m_NumberOfCellPixelComponents;
        switch ( this->m_CellPixelComponentType )
          {
          case UCHAR:
            {
            ConvertBuffer(static_cast< unsigned char * >( buffer ),
                          static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                          cellDataBufferSize);
            break;
            }
          case CHAR:
            {
            ConvertBuffer(static_cast< char * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), cellDataBufferSize);
            break;
            }
          case USHORT:
            {
            ConvertBuffer(static_cast< unsigned short * >( buffer ),
                          static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                          cellDataBufferSize);
            break;
            }
          case SHORT:
            {
            ConvertBuffer(static_cast< short * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), cellDataBufferSize);
            break;
            }
          case UINT:
            {
            ConvertBuffer(static_cast< unsigned int * >( buffer ),
                          static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                          cellDataBufferSize);
            break;
            }
          case INT:
            {
            ConvertBuffer(static_cast< int * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), cellDataBufferSize);
            break;
            }
          case ULONG:
            {
            ConvertBuffer(static_cast< unsigned long * >( buffer ),
                          static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                          cellDataBufferSize);
            break;
            }
          case LONG:
            {
            ConvertBuffer(static_cast< long * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), cellDataBufferSize);
            break;
            }
          case ULONGLONG:
            {
            ConvertBuffer(static_cast< unsigned long long * >( buffer ),
                          static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                          cellDataBufferSize);
            break;
            }
          case LONGLONG:
            {
            ConvertBuffer(static_cast< long long * >( buffer ),
                          static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                          cellDataBufferSize);
            break;
            }
          case FLOAT:
            {
            ConvertBuffer(static_cast< float * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), cellDataBufferSize);
            break;
            }
          case DOUBLE:
            {
            ConvertBuffer(static_cast< double * >( buffer ), static_cast< float * >( m_GiftiImage->darray[ii]->data ), cellDataBufferSize);
            break;
            }
          case LDOUBLE:
            {
            ConvertBuffer(static_cast< long double * >( buffer ),
                          static_cast< float * >( m_GiftiImage->darray[ii]->data ),
                          cellDataBufferSize);
            break;
            }
          default:
            {
            gifti_free_image( m_GiftiImage );
            itkExceptionMacro(<< "Unknown cell data pixel component type" << std::endl);
            }
          }
        }
      }
    else if ( m_GiftiImage->darray[ii]->intent == NIFTI_INTENT_LABEL )
      {
      if ( static_cast< SizeValueType >( m_GiftiImage->darray[ii]->dims[0] ) == this->m_NumberOfCellPixels )
        {
        const SizeValueType cellDataBufferSize = this->m_NumberOfCellPixels * this->m_NumberOfCellPixelComponents;

        switch ( this->m_CellPixelComponentType )
          {
          case UCHAR:
            {
            ConvertBuffer(static_cast< unsigned char * >( buffer ),
                          static_cast< int * >( m_GiftiImage->darray[ii]->data ),
                          cellDataBufferSize);
            break;
            }
          case CHAR:
            {
            ConvertBuffer(static_cast< char * >( buffer ), static_cast< int * >( m_GiftiImage->darray[ii]->data ), cellDataBufferSize);
            break;
            }
          case USHORT:
            {
            ConvertBuffer(static_cast< unsigned short * >( buffer ),
                          static_cast< int * >( m_GiftiImage->darray[ii]->data ),
                          cellDataBufferSize);
            break;
            }
          case SHORT:
            {
            ConvertBuffer(static_cast< short * >( buffer ), static_cast< int * >( m_GiftiImage->darray[ii]->data ), cellDataBufferSize);
            break;
            }
          case UINT:
            {
            ConvertBuffer(static_cast< unsigned int * >( buffer ),
                          static_cast< int * >( m_GiftiImage->darray[ii]->data ),
                          cellDataBufferSize);
            break;
            }
          case INT:
            {
            ConvertBuffer(static_cast< int * >( buffer ), static_cast< int * >( m_GiftiImage->darray[ii]->data ), cellDataBufferSize);
            break;
            }
          case ULONG:
            {
            ConvertBuffer(static_cast< unsigned long * >( buffer ),
                          static_cast< int * >( m_GiftiImage->darray[ii]->data ),
                          cellDataBufferSize);
            break;
            }
          case LONG:
            {
            ConvertBuffer(static_cast< long * >( buffer ), static_cast< int * >( m_GiftiImage->darray[ii]->data ), cellDataBufferSize);
            break;
            }
          case ULONGLONG:
            {
            ConvertBuffer(static_cast< unsigned long long * >( buffer ),
                          static_cast< int * >( m_GiftiImage->darray[ii]->data ),
                          cellDataBufferSize);
            break;
            }
          case LONGLONG:
            {
            ConvertBuffer(static_cast< long long * >( buffer ), static_cast< int * >( m_GiftiImage->darray[ii]->data ), cellDataBufferSize);
            break;
            }
          case FLOAT:
            {
            ConvertBuffer(static_cast< float * >( buffer ), static_cast< int * >( m_GiftiImage->darray[ii]->data ), cellDataBufferSize);
            break;
            }
          case DOUBLE:
            {
            ConvertBuffer(static_cast< double * >( buffer ), static_cast< int * >( m_GiftiImage->darray[ii]->data ), cellDataBufferSize);
            break;
            }
          case LDOUBLE:
            {
            ConvertBuffer(static_cast< long double * >( buffer ),
                          static_cast< int * >( m_GiftiImage->darray[ii]->data ),
                          cellDataBufferSize);
            break;
            }
          default:
            {
            gifti_free_image( m_GiftiImage );
            itkExceptionMacro(<< "Unknown cell data pixel component type" << std::endl);
            }
          }
        }
      }
    }
}

void
GiftiMeshIO
::Write()
{
  gifti_write_image(m_GiftiImage, this->m_FileName.c_str(), 1);
  gifti_free_image( m_GiftiImage );
}

void
GiftiMeshIO
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "gii version : " << std::endl;
  os << indent << gifticlib_version() << std::endl;
  os << indent << "Direction : " << std::endl;
  os << indent << m_Direction << std::endl;
}
} // namespace itk end
