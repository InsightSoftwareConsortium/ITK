# ==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# ==========================================================================*/

#
#  Verify FFT-based convolution in Python.
#
#  This checks that necessary FFT factories have been registered by default
#  and synchronized across modules so that they are available to filters in
#  the FFTConvolution Python module.
#
#  See `itkObjectFactoryBase.h` and `itkSingletonIndex.h` for more information.
#
import itk
from sys import argv
import os

itk.auto_progress(2)

dim = 2
PixelType = itk.F
ImageType = itk.Image[PixelType, dim]
ComplexImageType = itk.Image[itk.complex[PixelType], dim]

input_image = itk.imread(argv[1], pixel_type=PixelType)
kernel_image = itk.imread(argv[2], pixel_type=PixelType)

assert type(input_image) == ImageType
assert type(kernel_image) == ImageType

# FFTConvolutionImageFilter will fail if FFT factories are not available in
# ITKConvolution
output_image = itk.fft_convolution_image_filter(input_image, kernel_image=kernel_image)

itk.imwrite(output_image, argv[3])
