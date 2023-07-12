#==========================================================================
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
#==========================================================================*/

import math
import argparse

import itk

parser = argparse.ArgumentParser(description="Estimate three back-scatter coefficients.")
parser.add_argument("-i", "--input-image", required=True)
parser.add_argument("-o", "--output-image", required=True)
parser.add_argument("-a", "--azimuth-angular-separation", type=float, required=True)
parser.add_argument("-e", "--elevation-angular-separation", type=float, required=True)
parser.add_argument("-r", "--radius-sample-size", type=float, required=True)
parser.add_argument("-f", "--first-sample-distance", type=float, required=True)
parser.add_argument("-t", "--interpolation-type", choices=["nearest", "linear", "sinc"], default="linear")
args = parser.parse_args()

itk.auto_progress(2)

pixel_type = itk.UC
dimension = 3
image_type = itk.PhasedArray3DSpecialCoordinatesImage[pixel_type]
reader = itk.ImageFileReader[image_type].New()
reader.SetFileName(args.input_image)
reader.Update()
image = reader.GetOutput()
image.DisconnectPipeline()

image.SetAzimuthAngularSeparation(args.azimuth_angular_separation)
image.SetElevationAngularSeparation(args.elevation_angular_separation)
image.SetFirstSampleDistance(args.first_sample_distance)
image.SetRadiusSampleSize(args.radius_sample_size)
print(image)

print("Verify resampling works with PhasedArray3DSpecialCoordinatesImage input")
output_size = [128] * dimension
output_spacing = [0.2] * dimension
origin0 = -1 * output_spacing[0] * output_size[0] / 2.0
origin1 = -1 * output_spacing[1] * output_size[1] / 2.0
origin2 = 0.0;
output_origin = [origin0, origin1, origin2]

if args.interpolation_type == "nearest":
  interpolator = itk.NearestNeighborInterpolateImageFunction.New(image)
elif args.interpolation_type == "linear":
  interpolator = itk.LinearInterpolateImageFunction.New(image)
elif args.interpolation_type == "sinc":
  window_type = itk.LanczosWindowFunction[dimension]
  interpolator = itk.WindowedSincInterpolateImageFunction[type(image), dimension, window_type].New()

result = itk.resample_image_filter(
  image,
  size=output_size,
  output_spacing=output_spacing,
  output_origin=output_origin,
  interpolator=interpolator,
  )
itk.imwrite(result, args.output_image, compression=True)
print(f"Result image written to: {args.output_image}")
