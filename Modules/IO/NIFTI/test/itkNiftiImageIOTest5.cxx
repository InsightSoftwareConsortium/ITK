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


#include "itkNiftiImageIOTest.h"


template <typename PixelType, unsigned TType>
int
SlopeInterceptTest()
{
  //
  // fill out a nifti image and write it.
  const char *  filename = "SlopeIntercept.nii";
  nifti_image * niftiImage = nifti_simple_init_nim();
  niftiImage->fname = (char *)malloc(strlen(filename) + 1);
  if (niftiImage->fname == nullptr)
  {
    std::cerr << "Failed to allocate memory for filename, length requested " << strlen(filename) + 1 << std::endl;
    return EXIT_FAILURE;
  }
  strcpy(niftiImage->fname, filename);
  niftiImage->nifti_type = 1;
  niftiImage->iname = (char *)malloc(strlen(filename) + 1);
  if (niftiImage->iname == nullptr)
  {
    free(niftiImage->fname);
    std::cerr << "Failed to allocate memory for filename, length requested " << strlen(filename) + 1 << std::endl;
    return EXIT_FAILURE;
  }
  strcpy(niftiImage->iname, filename);
  niftiImage->dim[0] = niftiImage->ndim = 3;
  niftiImage->nx = niftiImage->dim[1] = 8;
  niftiImage->ny = niftiImage->dim[2] = 8;
  niftiImage->nz = niftiImage->dim[3] = 4;
  niftiImage->nvox = 256;
  niftiImage->dx = niftiImage->pixdim[1] = niftiImage->dy = niftiImage->pixdim[2] = niftiImage->dz =
    niftiImage->pixdim[3] = 1.0;
  niftiImage->nu = 1;
  niftiImage->datatype = TType;
  niftiImage->nbyper = sizeof(PixelType);
  niftiImage->scl_slope = 1.0 / 256.0;
  niftiImage->scl_inter = 0.0;
  niftiImage->sform_code = NIFTI_XFORM_SCANNER_ANAT;
  niftiImage->qform_code = NIFTI_XFORM_ALIGNED_ANAT;
  niftiImage->qfac = 1;
  mat44 matrix;
  for (unsigned i = 0; i < 4; i++)
  {
    for (unsigned j = 0; j < 4; j++)
    {
      matrix.m[i][j] = (i == j) ? 1.0 : 0.0;
    }
  }
  niftiImage->qto_xyz = matrix;
  niftiImage->sto_xyz = matrix;
  niftiImage->sto_ijk = matrix;
  niftiImage->qto_ijk = matrix;
  nifti_mat44_to_quatern(matrix,
                         &(niftiImage->quatern_b),
                         &(niftiImage->quatern_c),
                         &(niftiImage->quatern_d),
                         &(niftiImage->qoffset_x),
                         &(niftiImage->qoffset_y),
                         &(niftiImage->qoffset_z),
                         nullptr,
                         nullptr,
                         nullptr,
                         &(niftiImage->qfac));
  niftiImage->data = malloc(sizeof(PixelType) * 256);
  for (unsigned i = 0; i < 256; i++)
  {
    static_cast<PixelType *>(niftiImage->data)[i] = i;
  }
  nifti_image_write(niftiImage);
  nifti_image_free(niftiImage);
  //
  // read the image back in
  using ImageType = typename itk::Image<float, 3>;
  typename ImageType::Pointer image;
  try
  {
    image = itk::IOTestHelper::ReadImage<ImageType>(std::string(filename));
  }
  catch (...)
  {
    itk::IOTestHelper::Remove(filename);
    return EXIT_FAILURE;
  }
  using IteratorType = typename itk::ImageRegionIterator<ImageType>;
  IteratorType it(image, image->GetLargestPossibleRegion());
  it.GoToBegin();
  double maxerror = 0.0;
  for (unsigned i = 0; i < 256; i++, ++it)
  {
    if (it.IsAtEnd())
    {
      return EXIT_FAILURE;
    }
    if (!Equal(it.Value(), static_cast<float>(i) / 256.0))
    {
      //      return EXIT_FAILURE;
      double error = std::abs(it.Value() - (static_cast<double>(i) / 256.0));
      if (error > maxerror)
      {
        maxerror = error;
      }
    }
  }
  std::cerr << "Max error " << maxerror << std::endl;
  itk::IOTestHelper::Remove(filename);
  return maxerror > 0.00001 ? EXIT_FAILURE : EXIT_SUCCESS;
}

template <typename PixelType>
int
SlopeInterceptWriteTest()
{
  //
  // fill out an image and write it as nifti.
  const char * filename = "SlopeIntercept.nii";
  using OutputImageType = itk::Image<PixelType, 3>;
  typename OutputImageType::RegionType region;
  typename OutputImageType::IndexType  start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;
  typename OutputImageType::SizeType size;
  size[0] = 8;
  size[1] = 8;
  size[2] = 4;
  region.SetSize(size);
  region.SetIndex(start);
  typename OutputImageType::Pointer outputimage = OutputImageType::New();
  outputimage->SetRegions(region);
  outputimage->Allocate();
  using OutputIteratorType = itk::ImageRegionIterator<OutputImageType>;
  OutputIteratorType itout(outputimage, outputimage->GetLargestPossibleRegion());
  itout.GoToBegin();
  for (unsigned i = 0; i < 256; i++, ++itout)
  {
    if (itout.IsAtEnd())
    {
      return EXIT_FAILURE;
    }
    itout.Set(static_cast<PixelType>(i));
  }
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  typename WriterType::Pointer writer = WriterType::New();
  itk::NiftiImageIO::Pointer   niftiImageIO(itk::NiftiImageIO::New());
  niftiImageIO->SetRescaleSlope(1.0 / 256.0);
  niftiImageIO->SetRescaleIntercept(-10.0);
  writer->SetImageIO(niftiImageIO);
  writer->SetFileName(filename);
  writer->SetInput(outputimage);
  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "Exception Object caught: " << std::endl << err << std::endl;
    throw;
  }
  //
  // read the image back in
  using ImageType = itk::Image<float, 3>;
  typename ImageType::Pointer image;
  try
  {
    image = itk::IOTestHelper::ReadImage<ImageType>(std::string(filename));
  }
  catch (...)
  {
    itk::IOTestHelper::Remove(filename);
    return EXIT_FAILURE;
  }
  using IteratorType = itk::ImageRegionIterator<ImageType>;
  IteratorType it(image, image->GetLargestPossibleRegion());
  it.GoToBegin();
  double maxerror = 0.0;
  for (unsigned i = 0; i < 256; i++, ++it)
  {
    if (it.IsAtEnd())
    {
      return EXIT_FAILURE;
    }
    if (!Equal(it.Value(), static_cast<float>(i) / 256.0 - 10.0))
    {
      //      return EXIT_FAILURE;
      double error = std::abs(it.Value() - (static_cast<double>(i) / 256.0 - 10.0));
      if (error > maxerror)
      {
        maxerror = error;
      }
    }
  }
  std::cerr << "Max error " << maxerror << std::endl;
  itk::IOTestHelper::Remove(filename);
  return maxerror > 0.00001 ? EXIT_FAILURE : EXIT_SUCCESS;
}

//
// test vector images
int
itkNiftiImageIOTest5(int ac, char * av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if (ac > 1)
  {
    char * testdir = *++av;
    itksys::SystemTools::ChangeDirectory(testdir);
  }
  else
  {
    return EXIT_FAILURE;
  }
  int success(0);
  success |= SlopeInterceptTest<unsigned char, NIFTI_TYPE_UINT8>();
  success |= SlopeInterceptTest<short, NIFTI_TYPE_INT16>();
  success |= SlopeInterceptTest<unsigned short, NIFTI_TYPE_UINT16>();
  success |= SlopeInterceptTest<int, NIFTI_TYPE_INT32>();
  success |= SlopeInterceptTest<unsigned int, NIFTI_TYPE_UINT32>();
  success |= SlopeInterceptTest<long long, NIFTI_TYPE_INT64>();
  success |= SlopeInterceptTest<unsigned long long, NIFTI_TYPE_UINT64>();
  success |= SlopeInterceptWriteTest<unsigned char>();
  success |= SlopeInterceptWriteTest<short>();
  success |= SlopeInterceptWriteTest<unsigned short>();
  success |= SlopeInterceptWriteTest<int>();
  success |= SlopeInterceptWriteTest<unsigned int>();
  success |= SlopeInterceptWriteTest<long long>();
  success |= SlopeInterceptWriteTest<unsigned long long>();
  return success;
}
