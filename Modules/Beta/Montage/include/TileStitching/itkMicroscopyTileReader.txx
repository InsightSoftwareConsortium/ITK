/*=========================================================================
 *
 *  Copyright Kitware Inc.
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

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageSeriesReader.h"

namespace itk {

//----------------------------------------------------------------------------
template <class TImageType>
MicroscopyTileReader<TImageType>::MicroscopyTileReader()
{
  this->m_OutputImage = NULL;
}

//----------------------------------------------------------------------------
template <class TImageType>
MicroscopyTileReader<TImageType>::~MicroscopyTileReader()
{
}

//----------------------------------------------------------------------------
template <class TImageType>
void MicroscopyTileReader<TImageType>::
SetFileNames(const StringVectorType & filenames)
{
  this->m_FileNames = filenames;
}

//----------------------------------------------------------------------------
template <class TImageType>
typename MicroscopyTileReader<TImageType>::ImagePointerType
MicroscopyTileReader<TImageType>
::ReadImage(const std::string & filename)
{
  typedef itk::ImageFileReader<ImageType>     ImageReaderType;
  typedef typename ImageReaderType::Pointer   ImageReaderPointerType;
  ImageReaderPointerType reader = ImageReaderType::New();
  reader->SetFileName(filename);
  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << err << std::endl;
    return NULL;
    }
  return reader->GetOutput();
}

//----------------------------------------------------------------------------
template <class TImageType>
typename MicroscopyTileReader<TImageType>::ImagePointerType
MicroscopyTileReader<TImageType>
::ReadImageSeries(const StringVectorType & filenames)
{
  typedef itk::ImageSeriesReader<ImageType>       ImageSeriesReaderType;
  typedef typename ImageSeriesReaderType::Pointer ImageSeriesReaderPointerType;
  ImageSeriesReaderPointerType reader = ImageSeriesReaderType::New();
  reader->SetFileNames(filenames);
  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << err << std::endl;
    return NULL;
    }
  return reader->GetOutput();
}

//----------------------------------------------------------------------------
template <class TImageType>
void MicroscopyTileReader<TImageType>::Update()
{
  if (this->m_FileNames.empty())
    {
    this->m_OutputImage = NULL;
    }
  else if (this->m_FileNames.size() == 1)
    {
    this->m_OutputImage = this->ReadImage(this->m_FileNames[0]);
    }
  else
    {
    this->m_OutputImage = this->ReadImageSeries(this->m_FileNames);
    }
}

}
