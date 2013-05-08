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

MINCTransformIO::MINCTransformIO()
{
  m_XFM_initialized=false;
}

MINCTransformIO::~MINCTransformIO()
{
  _cleanup();
}

void MINCTransformIO::_cleanup(void)
{
  if(m_XFM_initialized)
      delete_general_transform(&m_XFM);
  m_XFM_initialized=false;
}

bool MINCTransformIO::CanReadFile(const char *fileName)
{
  std::string ext(itksys::SystemTools::GetFilenameLastExtension(fileName));
  return (ext == ".xfm" || ext==".XFM");
}

bool MINCTransformIO::CanWriteFile(const char *fileName)
{
  std::string ext(itksys::SystemTools::GetFilenameLastExtension(fileName));
  return (ext == ".xfm" || ext==".XFM");
}

void MINCTransformIO::ReadOneTransform(VIO_General_transform *xfm)
{
  switch(get_transform_type(xfm))
  {
    case LINEAR:
      {
      VIO_Transform *lin=get_linear_transform_ptr(xfm);

      TransformPointer transform;
      this->CreateTransform(transform,"AffineTransform_double_3_3");
      ParametersType ParameterArray;
      ParameterArray.SetSize(12);

      for(int j=0;j<3;j++)
        {
        for(int i=0;i<4;i++)
          {
          ParameterArray.SetElement(i+j*4,Transform_elem(*lin,j,i));
          }
        }

      if(xfm->inverse_flag)
        {
        AffineTransform<double,3>::Pointer tmp=AffineTransform<double,3>::New();
        tmp->SetParametersByValue(ParameterArray);
        tmp->GetInverse(static_cast<AffineTransform<double,3>*>(transform.GetPointer()));
        }
      else
        {
        transform->SetParametersByValue(ParameterArray);
        }
      this->GetReadTransformList().push_back(transform);

      break;
      }
    case CONCATENATED_TRANSFORM:
      {
      for(int i=0;i<get_n_concated_transforms(xfm);i++)
        {
        this->ReadOneTransform(get_nth_general_transform(xfm,i));
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
        typedef DisplacementFieldTransform<double, 3 > DoubleDisplacementFieldTransformType;
        typedef DoubleDisplacementFieldTransformType::DisplacementFieldType DoubleGridImageType;
        typedef ImageFileReader< DoubleGridImageType > DoubleMincReaderType;

        MINCImageIO::Pointer mincIO = MINCImageIO::New();
        DoubleMincReaderType::Pointer reader = DoubleMincReaderType::New();
        reader->SetImageIO( mincIO );
        reader->SetFileName( xfm->displacement_volume_file );
        reader->Update();

        DoubleGridImageType::Pointer _grid=reader->GetOutput();

        TransformPointer transform;
        this->CreateTransform(transform,"DisplacementFieldTransform_double_3_3");
        DoubleDisplacementFieldTransformType *_grid_transform=static_cast<DoubleDisplacementFieldTransformType*>(transform.GetPointer());
        if(xfm->inverse_flag) //TODO: invert grid transform?
          {
          _grid_transform->SetInverseDisplacementField(_grid);
          }
        else
          {
          _grid_transform->SetDisplacementField(_grid);
          }

        this->GetReadTransformList().push_back(transform);

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

void MINCTransformIO::Read()
{
  if(input_transform_file((char*)this->GetFileName(), &m_XFM) != VIO_OK)
    itkExceptionMacro( << "Error reading XFM:" << this->GetFileName() );
  this->m_XFM_initialized=true;

  this->ReadOneTransform(&m_XFM);

  _cleanup();
}

void MINCTransformIO::WriteOneTransform(const int transformIndex,
                    const TransformBase *curTransform,
                    std::vector<VIO_General_transform> &xfm,
                    const char * xfm_file_base,int & serial )
{
  const std::string transformType = curTransform->GetTransformTypeAsString();

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
  } else {

    if(transformType.find("AffineTransform") != std::string::npos && curTransform->GetNumberOfParameters() == 12)
    {
      VIO_Transform lin;
      for(int j=0;j<3;j++)
        for(int i=0;i<4;i++)
          Transform_elem(lin,j,i)=curTransform->GetParameters()[i+j*4];
      xfm.push_back(VIO_General_transform());
      create_linear_transform(&xfm[xfm.size()-1],&lin );
    } else if(transformType.find("DisplacementFieldTransform_double_3_3") != std::string::npos && curTransform->GetFixedParameters().Size() == 18) {
      bool _inverse_grid=false;
      typedef DisplacementFieldTransform<double, 3 > DoubleDisplacementFieldTransformType;
      DoubleDisplacementFieldTransformType* _grid_transform=static_cast<DoubleDisplacementFieldTransformType*>(const_cast<TransformType*>(curTransform));
      char tmp[1024];
      sprintf(tmp,"%s_grid_%d.mnc",xfm_file_base,serial);
      serial++;

      MINCImageIO::Pointer mincIO = MINCImageIO::New();
      typedef DoubleDisplacementFieldTransformType::DisplacementFieldType DoubleGridImageType;
      typedef ImageFileWriter< DoubleGridImageType > DoubleMincWriterType;
      DoubleMincWriterType::Pointer writer = DoubleMincWriterType::New();
      writer->SetImageIO( mincIO );
      writer->SetFileName( tmp );
      if( _grid_transform->GetDisplacementField() )
        {
        EncapsulateMetaData<std::string>(_grid_transform->GetModifiableDisplacementField()->GetMetaDataDictionary(),std::string("storage_data_type"),std::string(typeid(float).name()));
        writer->SetInput( _grid_transform->GetDisplacementField() );
        }
      else if( _grid_transform->GetInverseDisplacementField() )
        {
        EncapsulateMetaData<std::string>(_grid_transform->GetModifiableInverseDisplacementField()->GetMetaDataDictionary(),std::string("storage_data_type"),std::string(typeid(float).name()));
        writer->SetInput( _grid_transform->GetInverseDisplacementField() );
        _inverse_grid=false;
        }
      else
        {
        itkExceptionMacro(<< "Trying to write-out displacement transform without displacement field");
        }
      writer->Update();

      xfm.push_back( VIO_General_transform() );
      create_grid_transform_no_copy( &xfm[xfm.size()-1], NULL, NULL );//relying on volume_io using the same name
      if(_inverse_grid)
        {
          xfm[xfm.size()-1].inverse_flag=TRUE;
        }

    }
#if !defined(__clang__) && !defined(_MSC_VER) //Seem to cause problems on MacOSX and Windows 
    else if(transformType.find("DisplacementFieldTransform_float_3_3") != std::string::npos && curTransform->GetFixedParameters().Size() == 18 ) {
      bool _inverse_grid=false;
      typedef DisplacementFieldTransform<float, 3 > FloatDisplacementFieldTransformType;
      typedef FloatDisplacementFieldTransformType::DisplacementFieldType FloatGridImageType;
      typedef ImageFileWriter< FloatGridImageType > FloatMincWriterType;
      FloatDisplacementFieldTransformType* _grid_transform=static_cast<FloatDisplacementFieldTransformType*>(const_cast<TransformType*>(curTransform));
      char tmp[1024];
      sprintf(tmp,"%s_grid_%d.mnc",xfm_file_base,serial);
      serial++;

      itk::MINCImageIO::Pointer mincIO = itk::MINCImageIO::New();
      FloatMincWriterType::Pointer writer = FloatMincWriterType::New();
      writer->SetImageIO( mincIO );
      writer->SetFileName( tmp );

      if( _grid_transform->GetDisplacementField() )
        {
        writer->SetInput( _grid_transform->GetModifiableDisplacementField() );
        }
      else if( _grid_transform->GetInverseDisplacementField() )
        {
          writer->SetInput( _grid_transform->GetModifiableInverseDisplacementField() );
          _inverse_grid=false;
        }
      else
        {
          itkExceptionMacro(<< "Trying to write-out displacement transform without displacement field");
        }
      writer->Update();

      xfm.push_back( VIO_General_transform() );
      create_grid_transform_no_copy( &xfm[xfm.size()-1],NULL,NULL ); //relying on volume_io using the same name
      if(_inverse_grid)
        {
          xfm[xfm.size()-1].inverse_flag=TRUE;
        }
    }
#endif
    else {
      itkExceptionMacro(<< "Transform type:"<<transformType.c_str()<<"is Unsupported");
    }
  }
}

void MINCTransformIO::Write()
{
  std::string xfm_filename=this->GetFileName();

  std::string::size_type xfmPos = xfm_filename.rfind(".xfm");

  if (xfmPos == std::string::npos)
    {
    xfmPos = xfm_filename.rfind(".XFM");
    }
  std::string xfm_file_base(xfm_filename,0,xfmPos);

  std::vector<VIO_General_transform> xfm;

  ConstTransformListType &transformList = this->GetWriteTransformList();

  std::string compositeTransformType = transformList.front()->GetTransformTypeAsString();

  CompositeTransformIOHelper helper;

  // if the first transform in the list is a
  // composite transform, use its internal list
  // instead of the IO
  if( compositeTransformType.find("CompositeTransform") != std::string::npos )
    {
    transformList = helper.GetTransformList(transformList.front().GetPointer());
    }

  ConstTransformListType::const_iterator end = transformList.end();

  int count = 0;
  int serial = 0;
  for( ConstTransformListType::const_iterator it = transformList.begin();
      it != end; ++it,++count )
    {
    this->WriteOneTransform(count,(*it).GetPointer(),xfm,xfm_file_base.c_str(),serial);
    }

  VIO_General_transform transform=xfm[0];

  for( size_t i=1;i<xfm.size();i++ )
    {
    VIO_General_transform concated;
    concat_general_transforms( &transform, &xfm[i], &concated );
    delete_general_transform( &transform );
    delete_general_transform( &xfm[i] );
    transform = concated;
    }

  VIO_Status wrt=output_transform_file((char*)(xfm_filename.c_str()),(char*)"ITK-XFM writer",&transform);

  if(xfm.size()>1)
    {
    delete_general_transform(&transform);
    }
  delete_general_transform(&xfm[0]);

  if(wrt!=VIO_OK)
    {
    itkExceptionMacro( << "Error writing XFM:" << xfm_filename.c_str() );
    }
}

} // end namespace itk
