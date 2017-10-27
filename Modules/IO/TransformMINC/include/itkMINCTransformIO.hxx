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
#ifndef itkMINCTransformIO_hxx
#define itkMINCTransformIO_hxx

#include "itksys/SystemTools.hxx"
#include "itksys/SystemInformation.hxx"
#include "itkCompositeTransform.h"
#include "itkCompositeTransformIOHelper.h"
#include "itkVersion.h"
#include "itkMINCTransformIO.h"
#include "itkMINCImageIO.h"
#include "itkMINCImageIOFactory.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkAffineTransform.h"
#include "itkImage.h"
#include "itkVector.h"
#include "itkDisplacementFieldTransform.h"
#include "itkMetaDataObject.h"

namespace itk
{

template<typename TParametersValueType>
MINCTransformIOTemplate<TParametersValueType>
::MINCTransformIOTemplate()
{
  m_XFM_initialized=false;
}

template<typename TParametersValueType>
MINCTransformIOTemplate<TParametersValueType>
::~MINCTransformIOTemplate()
{
  _cleanup();
}

template<typename TParametersValueType>
void
MINCTransformIOTemplate<TParametersValueType>
::_cleanup(void)
{
  if(m_XFM_initialized)
      delete_general_transform(&m_XFM);
  m_XFM_initialized=false;
}

template<typename TParametersValueType>
bool
MINCTransformIOTemplate<TParametersValueType>
::CanReadFile(const char *fileName)
{
  std::string ext(itksys::SystemTools::GetFilenameLastExtension(fileName));
  return (ext == ".xfm" || ext==".XFM");
}

template<typename TParametersValueType>
bool
MINCTransformIOTemplate<TParametersValueType>
::CanWriteFile(const char *fileName)
{
  std::string ext(itksys::SystemTools::GetFilenameLastExtension(fileName));
  return (ext == ".xfm" || ext==".XFM");
}

template<typename TParametersValueType>
void
MINCTransformIOTemplate<TParametersValueType>
::ReadOneTransform(VIO_General_transform *xfm)
{
  const std::string typeNameString = Superclass::GetTypeNameString();
  switch(get_transform_type(xfm))
  {
    case LINEAR:
      {
      VIO_Transform *lin=get_linear_transform_ptr(xfm);

      TransformPointer transform;
      std::string transformTypeName = "AffineTransform_";
      transformTypeName += typeNameString;
      transformTypeName += "_3_3";
      this->CreateTransform(transform, transformTypeName);
      ParametersType parameterArray;
      parameterArray.SetSize(12);

      for(int j = 0; j < 3; ++j)
        {
        for(int i = 0; i < 3; ++i)
          {
          parameterArray.SetElement(i+j*3, Transform_elem(*lin,j,i));
          }
        parameterArray.SetElement(j+9, Transform_elem(*lin,j,3));
        }

      if(xfm->inverse_flag)
        {
        typedef AffineTransform<TParametersValueType, 3> AffineTransformType;
        typename AffineTransformType::Pointer tmp = AffineTransformType::New();
        tmp->SetParametersByValue(parameterArray);
        tmp->GetInverse(static_cast< AffineTransformType* >( transform.GetPointer()) );
        }
      else
        {
        transform->SetParametersByValue(parameterArray);
        }
      this->GetReadTransformList().push_back(transform);

      break;
      }
    case CONCATENATED_TRANSFORM:
      {
      for(int i = 0; i < get_n_concated_transforms(xfm); ++i)
        {
        this->ReadOneTransform(get_nth_general_transform(xfm, i));
        }
      break;
      }
    case THIN_PLATE_SPLINE:
      itkExceptionMacro( << "Reading THIN_PLATE_SPLINE transform is not supported yet" );
      break;
    case USER_TRANSFORM:
      itkExceptionMacro( << "Reading USER_TRANSFORM transform is not supported yet" );
      break;
    case GRID_TRANSFORM :
    {
      if(xfm->displacement_volume_file)
        {
        typedef DisplacementFieldTransform<TParametersValueType, 3> DisplacementFieldTransformType;
        typedef typename DisplacementFieldTransformType::DisplacementFieldType GridImageType;
        typedef ImageFileReader< GridImageType >                               MincReaderType;

        MINCImageIO::Pointer mincIO             = MINCImageIO::New();
        typename MincReaderType::Pointer reader = MincReaderType::New();
        reader->SetImageIO( mincIO );
        reader->SetFileName( xfm->displacement_volume_file );
        reader->Update();

        typename GridImageType::Pointer grid = reader->GetOutput();

        TransformPointer transform;
        std::string transformTypeName = "DisplacementFieldTransform_";
        transformTypeName += typeNameString;
        transformTypeName += "_3_3";
        this->CreateTransform(transform, transformTypeName);
        DisplacementFieldTransformType * gridTransform = static_cast< DisplacementFieldTransformType* >( transform.GetPointer());
        if( xfm->inverse_flag ) //TODO: invert grid transform?
          {
          gridTransform->SetInverseDisplacementField( grid );
          }
        else
          {
          gridTransform->SetDisplacementField( grid );
          }

        this->GetReadTransformList().push_back( transform );

        break;
        }
      else
        {
        itkExceptionMacro( << "Got grid transform without file name !" );
        }
    }
    default:
      itkExceptionMacro( << "Reading Unknown transform is not supported!" );
      break;
  }
}

template<typename TParametersValueType>
void
MINCTransformIOTemplate<TParametersValueType>
::Read()
{
  if(input_transform_file(this->GetFileName(), &m_XFM) != VIO_OK)
    {
    itkExceptionMacro( << "Error reading XFM:" << this->GetFileName() );
    }
  this->m_XFM_initialized=true;

  this->ReadOneTransform(&m_XFM);

  _cleanup();
}

template<typename TParametersValueType>
void
MINCTransformIOTemplate<TParametersValueType>
::WriteOneTransform(const int transformIndex,
                    const TransformType *curTransform,
                    std::vector<VIO_General_transform> &xfm,
                    const char * xfm_file_base,int & serial )
{
  const std::string transformType = curTransform->GetTransformTypeAsString();

  const MatrixOffsetTransformBaseType * matrixOffsetTransform =
        dynamic_cast<const MatrixOffsetTransformBaseType *>( curTransform );

  //
  // write out transform type.
  //
  // composite transform doesn't store own parameters
  if(transformType.find("CompositeTransform") != std::string::npos)
    {
      if(transformIndex != 0)
        {
        itkExceptionMacro(<< "Composite Transform can only be 1st transform in a file");
        }
    }
  else
    {
    if(matrixOffsetTransform)
      {
      VIO_Transform lin;
      memset(&lin, 0, sizeof(VIO_Transform));

      MatrixType matrix = matrixOffsetTransform->GetMatrix();
      OffsetType offset = matrixOffsetTransform->GetOffset();

      for(int j=0; j < 3; ++j)
        {
        for(int i=0; i < 3; ++i)
          {
          Transform_elem(lin,j,i)=matrix(j,i);
          }
        Transform_elem(lin,j,3)=offset[j];
        }
      //add 4th normalization row (not stored)
      Transform_elem(lin,3,3)=1.0;

      xfm.push_back(VIO_General_transform());
      memset(&xfm[xfm.size()-1], 0, sizeof(VIO_General_transform));
      create_linear_transform(&xfm[xfm.size()-1], &lin);
      }
    else if( transformType.find("DisplacementFieldTransform_") != std::string::npos
             && transformType.find("_3_3") != std::string::npos
             && curTransform->GetFixedParameters().Size() == 18 )
      {
      bool _inverse_grid=false;
      typedef DisplacementFieldTransform<TParametersValueType, 3> DisplacementFieldTransformType;
      typedef typename DisplacementFieldTransformType::DisplacementFieldType GridImageType;
      typedef ImageFileWriter< GridImageType > MincWriterType;
      DisplacementFieldTransformType* _grid_transform = static_cast< DisplacementFieldTransformType* >( const_cast< TransformType* >( curTransform ));
      char tmp[1024];
      sprintf(tmp,"%s_grid_%d.mnc",xfm_file_base,serial);
      ++serial;

      MINCImageIO::Pointer mincIO = MINCImageIO::New();
      typename MincWriterType::Pointer writer = MincWriterType::New();
      writer->SetImageIO( mincIO );
      writer->SetFileName( tmp );

      if( _grid_transform->GetDisplacementField() )
        {
        writer->SetInput( _grid_transform->GetModifiableDisplacementField() );
        }
      else if( _grid_transform->GetInverseDisplacementField() )
        {
        writer->SetInput( _grid_transform->GetModifiableInverseDisplacementField() );
        _inverse_grid=true;
        }
      else
        {
        itkExceptionMacro(<< "Trying to write-out displacement transform without displacement field");
        }
      writer->Update();

      xfm.push_back( VIO_General_transform() );
      create_grid_transform_no_copy( &xfm[xfm.size()-1], ITK_NULLPTR, ITK_NULLPTR ); //relying on volume_io using the same name
      if(_inverse_grid)
        {
        xfm[xfm.size()-1].inverse_flag=TRUE;
        }
      }
    else
      {
      itkExceptionMacro(<< "Transform type:" << transformType.c_str() << "is Unsupported");
      }
  }
}

template<typename TParametersValueType>
void
MINCTransformIOTemplate<TParametersValueType>
::Write()
{
  std::string xfm_filename = this->GetFileName();

  std::string::size_type xfmPos = xfm_filename.rfind(".xfm");

  if (xfmPos == std::string::npos)
    {
    xfmPos = xfm_filename.rfind(".XFM");
    }
  std::string xfm_file_base(xfm_filename,0,xfmPos);

  std::vector<VIO_General_transform> xfm;

  ConstTransformListType &transformList = this->GetWriteTransformList();

  std::string compositeTransformType = transformList.front()->GetTransformTypeAsString();

  CompositeTransformIOHelperTemplate<TParametersValueType> helper;

  // if the first transform in the list is a
  // composite transform, use its internal list
  // instead of the IO
  if( compositeTransformType.find("CompositeTransform") != std::string::npos )
    {
    transformList = helper.GetTransformList(transformList.front().GetPointer());
    }

  typename ConstTransformListType::const_iterator end = transformList.end();

  int count = 0;
  int serial = 0;
  for( typename ConstTransformListType::const_iterator it = transformList.begin();
      it != end; ++it,++count )
    {
    this->WriteOneTransform(count, (*it).GetPointer(), xfm, xfm_file_base.c_str(), serial);
    }

  VIO_General_transform transform = xfm[0];

  for( size_t i=1; i < xfm.size(); ++i )
    {
    VIO_General_transform concated;
    concat_general_transforms( &transform, &xfm[i], &concated );
    delete_general_transform( &transform );
    delete_general_transform( &xfm[i] );
    transform = concated;
    }

  VIO_Status wrt = output_transform_file(xfm_filename.c_str(),"ITK-XFM writer",&transform);

  delete_general_transform(&transform);

  if(wrt!=VIO_OK)
    {
    itkExceptionMacro( << "Error writing XFM:" << xfm_filename.c_str() );
    }
}

} // end namespace itk

#endif
