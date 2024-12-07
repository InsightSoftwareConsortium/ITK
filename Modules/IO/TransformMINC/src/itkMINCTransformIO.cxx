/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#define ITK_TEMPLATE_EXPLICIT_MINCTransformIO
#include "itkMINCTransformIO.h"
#include "itksys/SystemTools.hxx"
#include "itkCompositeTransform.h"
#include "itkCompositeTransformIOHelper.h"
#include "itkVersion.h"
#include "itkMINCImageIO.h"
#include "itkMINCImageIOFactory.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkAffineTransform.h"
#include "itkImage.h"
#include "itkVector.h"
#include "itkDisplacementFieldTransform.h"
#include "itkMetaDataObject.h"
#include "itkImageRegionIterator.h"
#include "vnl/vnl_vector_fixed.h"

#include "itkMINCImageIOConfigurePrivate.h"


namespace itk
{

template <typename TParametersValueType>
MINCTransformIOTemplate<TParametersValueType>::MINCTransformIOTemplate()
  : m_XFM_initialized(false)
  , m_RAStoLPS(ITK_MINC_IO_RAS_TO_LPS)
{}

template <typename TParametersValueType>
MINCTransformIOTemplate<TParametersValueType>::~MINCTransformIOTemplate()
{
  _cleanup();
}

template <typename TParametersValueType>
void
MINCTransformIOTemplate<TParametersValueType>::_cleanup()
{
  if (m_XFM_initialized)
  {
    delete_general_transform(&m_XFM);
  }
  m_XFM_initialized = false;
}

template <typename TParametersValueType>
bool
MINCTransformIOTemplate<TParametersValueType>::CanReadFile(const char * fileName)
{
  std::string ext(itksys::SystemTools::GetFilenameLastExtension(fileName));
  return (ext == ".xfm" || ext == ".XFM");
}

template <typename TParametersValueType>
bool
MINCTransformIOTemplate<TParametersValueType>::CanWriteFile(const char * fileName)
{
  std::string ext(itksys::SystemTools::GetFilenameLastExtension(fileName));
  return (ext == ".xfm" || ext == ".XFM");
}

template <typename TParametersValueType>
void
MINCTransformIOTemplate<TParametersValueType>::ReadOneTransform(VIO_General_transform * xfm)
{
  const std::string typeNameString = Superclass::GetTypeNameString();
  switch (get_transform_type(xfm))
  {
    case LINEAR:
    {
      VIO_Transform * lin = get_linear_transform_ptr(xfm);

      TransformPointer transform;
      std::string      transformTypeName = "AffineTransform_";
      transformTypeName += typeNameString;
      transformTypeName += "_3_3";
      this->CreateTransform(transform, transformTypeName);
      ParametersType parameterArray;
      parameterArray.SetSize(12);


      Matrix<double, 4, 4> _affine_transform;

      _affine_transform.SetIdentity();

      // MINC stores transforms in PositiveCoordinateOrientation RAS
      // need to convert to PositiveCoordinateOrientation LPS for ITK
      Matrix<double, 4, 4> RAS_tofrom_LPS;
      RAS_tofrom_LPS.SetIdentity();
      RAS_tofrom_LPS(0, 0) = -1.0;
      RAS_tofrom_LPS(1, 1) = -1.0;

      for (int j = 0; j < 3; ++j)
      {
        for (int i = 0; i < 3; ++i)
        {
          _affine_transform(i, j) = Transform_elem(*lin, j, i);
        }
        // shifts
        _affine_transform(3, j) = Transform_elem(*lin, j, 3);
      }

      if (this->m_RAStoLPS) // flip RAS PositiveCoordinateOrientation to LPS PositiveCoordinateOrientation
        _affine_transform = RAS_tofrom_LPS * _affine_transform * RAS_tofrom_LPS;

      for (int j = 0; j < 3; ++j)
      {
        for (int i = 0; i < 3; ++i)
        {
          parameterArray.SetElement(i + j * 3, _affine_transform(i, j));
        }
        parameterArray.SetElement(j + 9, _affine_transform(3, j));
      }

      if (xfm->inverse_flag)
      {
        using AffineTransformType = AffineTransform<TParametersValueType, 3>;
        auto tmp = AffineTransformType::New();
        tmp->SetParametersByValue(parameterArray);
        tmp->GetInverse(static_cast<AffineTransformType *>(transform.GetPointer()));
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
      for (int i = get_n_concated_transforms(xfm) - 1; i >= 0; --i)
      {
        this->ReadOneTransform(get_nth_general_transform(xfm, i));
      }
      break;
    }
    case THIN_PLATE_SPLINE:
      itkExceptionMacro("Reading THIN_PLATE_SPLINE transform is not supported yet");
      break;
    case USER_TRANSFORM:
      itkExceptionMacro("Reading USER_TRANSFORM transform is not supported yet");
      break;
    case GRID_TRANSFORM:
    {
      if (xfm->displacement_volume_file)
      {
        using DisplacementFieldTransformType = DisplacementFieldTransform<TParametersValueType, 3>;
        using GridImageType = typename DisplacementFieldTransformType::DisplacementFieldType;
        using MincReaderType = ImageFileReader<GridImageType>;
        using OutputPixelType = vnl_vector_fixed<TParametersValueType, 3>;
        const OutputPixelType RAS_tofrom_LPS_vector = { -1, -1, 1 };

        auto mincIO = MINCImageIO::New();
        mincIO->SetRAStoLPS(this->m_RAStoLPS);

        auto reader = MincReaderType::New();
        reader->SetImageIO(mincIO);
        reader->SetFileName(xfm->displacement_volume_file);
        reader->Update();
        typename GridImageType::Pointer grid = reader->GetOutput();
        typename GridImageType::Pointer LPSgrid = GridImageType::New();

        if (this->m_RAStoLPS)
        {
          LPSgrid->CopyInformation(grid);
          LPSgrid->SetRegions(grid->GetBufferedRegion());
          LPSgrid->Allocate(true);

          itk::MultiThreaderBase::Pointer mt = itk::MultiThreaderBase::New();
          mt->ParallelizeImageRegion<3>(
            grid->GetBufferedRegion(),
            [grid, LPSgrid, RAS_tofrom_LPS_vector](const typename GridImageType::RegionType & region) {
              itk::Vector<TParametersValueType, 3>         p;
              itk::ImageRegionConstIterator<GridImageType> iIt(grid, region);
              itk::ImageRegionIterator<GridImageType>      oIt(LPSgrid, region);
              for (; !iIt.IsAtEnd(); ++iIt, ++oIt)
              {
                p.SetVnlVector(element_product(iIt.Get().GetVnlVector(), RAS_tofrom_LPS_vector));
                oIt.Set(p);
              }
            },
            nullptr);
        }

        TransformPointer transform;
        std::string      transformTypeName = "DisplacementFieldTransform_";
        transformTypeName += typeNameString;
        transformTypeName += "_3_3";
        this->CreateTransform(transform, transformTypeName);
        auto * gridTransform = static_cast<DisplacementFieldTransformType *>(transform.GetPointer());
        if (xfm->inverse_flag) // TODO: invert grid transform?
        {
          if (this->m_RAStoLPS)
            gridTransform->SetInverseDisplacementField(LPSgrid);
          else
            gridTransform->SetInverseDisplacementField(grid);
        }
        else
        {
          if (this->m_RAStoLPS)
            gridTransform->SetDisplacementField(LPSgrid);
          else
            gridTransform->SetDisplacementField(grid);
        }

        this->GetReadTransformList().push_back(transform);

        break;
      }
      else
      {
        itkExceptionMacro("Got grid transform without file name !");
      }
    }
    default:
      itkExceptionMacro("Reading Unknown transform is not supported!");
      break;
  }
}

template <typename TParametersValueType>
void
MINCTransformIOTemplate<TParametersValueType>::Read()
{
  if (input_transform_file(this->GetFileName(), &m_XFM) != VIO_OK)
  {
    itkExceptionMacro("Error reading XFM:" << this->GetFileName());
  }
  this->m_XFM_initialized = true;

  this->ReadOneTransform(&m_XFM);

  _cleanup();
}

template <typename TParametersValueType>
void
MINCTransformIOTemplate<TParametersValueType>::WriteOneTransform(const int                            transformIndex,
                                                                 const TransformType *                curTransform,
                                                                 std::vector<VIO_General_transform> & xfm,
                                                                 const char *                         xfm_file_base,
                                                                 int &                                serial)
{
  const std::string transformType = curTransform->GetTransformTypeAsString();

  const auto * matrixOffsetTransform = dynamic_cast<const MatrixOffsetTransformBaseType *>(curTransform);

  //
  // write out transform type.
  //
  // composite transform doesn't store own parameters
  if (transformType.find("CompositeTransform") != std::string::npos)
  {
    if (transformIndex != 0)
    {
      itkExceptionMacro("Composite Transform can only be 1st transform in a file");
    }
  }
  else
  {
    if (matrixOffsetTransform)
    {
      VIO_Transform lin{};

      MatrixType matrix = matrixOffsetTransform->GetMatrix();
      OffsetType offset = matrixOffsetTransform->GetOffset();

      // MINC stores everything in PositiveCoordinateOrientation RAS
      // need to convert from PositiveCoordinateOrientation LPS
      Matrix<double, 4, 4> RAS_tofrom_LPS;
      Matrix<double, 4, 4> _affine_transform;

      RAS_tofrom_LPS.SetIdentity();
      RAS_tofrom_LPS(0, 0) = -1.0;
      RAS_tofrom_LPS(1, 1) = -1.0;
      _affine_transform.SetIdentity();


      for (int j = 0; j < 3; ++j)
      {
        for (int i = 0; i < 3; ++i)
        {
          _affine_transform(j, i) = matrix(j, i);
        }
        _affine_transform(3, j) = offset[j];
      }

      if (this->m_RAStoLPS)
        _affine_transform = RAS_tofrom_LPS * _affine_transform * RAS_tofrom_LPS;

      for (int j = 0; j < 3; ++j)
      {
        for (int i = 0; i < 3; ++i)
        {
          Transform_elem(lin, j, i) = _affine_transform(j, i);
        }
        Transform_elem(lin, j, 3) = _affine_transform(3, j);
      }
      // add 4th normalization row (not stored)
      Transform_elem(lin, 3, 3) = 1.0;

      xfm.emplace_back();
      create_linear_transform(&xfm.back(), &lin);
    }
    else if (transformType.find("DisplacementFieldTransform_") != std::string::npos &&
             transformType.find("_3_3") != std::string::npos && curTransform->GetFixedParameters().Size() == 18)
    {
      bool _inverse_grid = false;
      using DisplacementFieldTransformType = DisplacementFieldTransform<TParametersValueType, 3>;
      using GridImageType = typename DisplacementFieldTransformType::DisplacementFieldType;
      using MincWriterType = ImageFileWriter<GridImageType>;
      typename GridImageType::Pointer grid = GridImageType::New();
      using OutputPixelType = vnl_vector_fixed<TParametersValueType, 3>;
      const OutputPixelType RAS_tofrom_LPS_vector = { -1, -1, 1 };
      auto * _grid_transform = static_cast<DisplacementFieldTransformType *>(const_cast<TransformType *>(curTransform));
      char   tmp[1024];
      snprintf(tmp, sizeof(tmp), "%s_grid_%d.mnc", xfm_file_base, serial);
      ++serial;

      auto mincIO = MINCImageIO::New();
      // writer should follow the same notation, in case we manually switched the coordinate system
      mincIO->SetRAStoLPS(this->m_RAStoLPS);

      auto writer = MincWriterType::New();

      writer->SetImageIO(mincIO);
      writer->SetFileName(tmp);

      if (_grid_transform->GetDisplacementField())
      {
        grid = _grid_transform->GetModifiableDisplacementField();
      }
      else if (_grid_transform->GetInverseDisplacementField())
      {
        grid = _grid_transform->GetModifiableInverseDisplacementField();
        _inverse_grid = true;
      }
      else
      {
        itkExceptionMacro("Trying to write-out displacement transform without displacement field");
      }

      if (this->m_RAStoLPS)
      {
        typename GridImageType::Pointer ras_grid = GridImageType::New(); // flipped grid for RAS->LPS conversion
        ras_grid->CopyInformation(grid);
        ras_grid->SetRegions(grid->GetBufferedRegion());
        ras_grid->Allocate(true);

        itk::MultiThreaderBase::Pointer mt = itk::MultiThreaderBase::New();
        mt->ParallelizeImageRegion<3>(
          grid->GetBufferedRegion(),
          [grid, ras_grid, RAS_tofrom_LPS_vector](const typename GridImageType::RegionType & region) {
            itk::Vector<TParametersValueType, 3>         p;
            itk::ImageRegionConstIterator<GridImageType> iIt(grid, region);
            itk::ImageRegionIterator<GridImageType>      oIt(ras_grid, region);
            for (; !iIt.IsAtEnd(); ++iIt, ++oIt)
            {
              p.SetVnlVector(element_product(iIt.Get().GetVnlVector(), RAS_tofrom_LPS_vector));
              oIt.Set(p);
            }
          },
          nullptr);

        writer->SetInput(ras_grid);
      }
      else
      {
        writer->SetInput(grid);
      }
      writer->Update();

      xfm.emplace_back();
      create_grid_transform_no_copy(&xfm.back(), nullptr, nullptr); // relying on volume_io using the same name
      if (_inverse_grid)
      {
        xfm.back().inverse_flag = TRUE;
      }
    }
    else
    {
      itkExceptionMacro("Transform type:" << transformType.c_str() << "is Unsupported");
    }
  }
}

template <typename TParametersValueType>
void
MINCTransformIOTemplate<TParametersValueType>::Write()
{
  std::string xfm_filename = this->GetFileName();

  std::string::size_type xfmPos = xfm_filename.rfind(".xfm");

  if (xfmPos == std::string::npos)
  {
    xfmPos = xfm_filename.rfind(".XFM");
  }
  std::string xfm_file_base(xfm_filename, 0, xfmPos);

  std::vector<VIO_General_transform> xfm;

  ConstTransformListType & transformList = this->GetWriteTransformList();

  std::string compositeTransformType = transformList.front()->GetTransformTypeAsString();

  CompositeTransformIOHelperTemplate<TParametersValueType> helper;

  // if the first transform in the list is a
  // composite transform, use its internal list
  // instead of the IO
  if (compositeTransformType.find("CompositeTransform") != std::string::npos)
  {
    transformList = helper.GetTransformList(transformList.front().GetPointer());
  }

  typename ConstTransformListType::const_iterator end = transformList.end();

  int count = 0;
  int serial = 0;
  for (typename ConstTransformListType::const_iterator it = transformList.begin(); it != end; ++it, ++count)
  {
    this->WriteOneTransform(count, it->GetPointer(), xfm, xfm_file_base.c_str(), serial);
  }

  VIO_General_transform transform = xfm.back();

  for (int i = xfm.size() - 2; i >= 0; --i)
  {
    VIO_General_transform concated;
    concat_general_transforms(&transform, &xfm[i], &concated);
    delete_general_transform(&transform);
    delete_general_transform(&xfm[i]);
    transform = concated;
  }

  VIO_Status wrt = output_transform_file(xfm_filename.c_str(), "ITK-XFM writer", &transform);

  delete_general_transform(&transform);

  if (wrt != VIO_OK)
  {
    itkExceptionMacro("Error writing XFM:" << xfm_filename.c_str());
  }
}

ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

template class ITKIOTransformMINC_EXPORT MINCTransformIOTemplate<double>;
template class ITKIOTransformMINC_EXPORT MINCTransformIOTemplate<float>;

ITK_GCC_PRAGMA_DIAG_POP()

} // end namespace itk
