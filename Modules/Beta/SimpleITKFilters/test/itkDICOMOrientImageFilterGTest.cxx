//
// Created by Lowekamp, Bradley (NIH/NIAID) [C] on 1/10/20.
//


#include "itkGTest.h"
#include "itkDICOMOrientImageFilter.h"
#include "itkImage.h"
#include <sstream>


TEST(DICOMOrientImageFilter, NoOp)
{
  using ImageType = itk::Image<float, 3>;
  auto filter = itk::DICOMOrientImageFilter<ImageType>::New();
}
