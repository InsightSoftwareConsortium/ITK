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

import sys

import itk

#
#  Check input parameters
#  INPUTS(fixedImage):  {BrainProtonDensitySliceBorder20.png}
#  INPUTS(movingImage): {BrainProtonDensitySliceShifted13x17y.png}
#
if len(sys.argv) < 4:
    print("Missing Parameters")
    print(
        "Usage: ImageRegistration3.py fixed_image_file moving_image_file output_image_file"
    )
    sys.exit(1)
fixed_image_file = sys.argv[1]
moving_image_file = sys.argv[2]
output_image_file = sys.argv[3]

#
#  Define data types
#
PixelType = itk.ctype("float")

#
#  Read the fixed and moving images using filenames
#  from the command line arguments
#
fixed_image = itk.imread(fixed_image_file, PixelType)
moving_image = itk.imread(moving_image_file, PixelType)
Dimension = fixed_image.GetImageDimension()

#
# Create the spatial transform we will optimize.
#
initial_transform = itk.TranslationTransform[itk.D, Dimension].New()

#
# Create the matching metric we will use to compare the images during
# optimization.
matching_metric = itk.MeanSquaresImageToImageMetricv4[
    type(fixed_image), type(moving_image)
].New()

#
# Create the optimizer we will use for optimization.
#
optimizer = itk.RegularStepGradientDescentOptimizerv4[itk.D].New()
optimizer.SetLearningRate(4)
optimizer.SetMinimumStepLength(0.001)
optimizer.SetRelaxationFactor(0.5)
optimizer.SetNumberOfIterations(100)
#
# Iteration Observer
#
def iteration_update():
    metric_value = optimizer.GetValue()
    current_parameters = optimizer.GetCurrentPosition()
    parameter_list = [current_parameters[i] for i in range(len(current_parameters))]
    print(f"Metric: {metric_value:.8g}  \tParameters: {parameter_list}")


iteration_command = itk.PyCommand.New()
iteration_command.SetCommandCallable(iteration_update)
optimizer.AddObserver(itk.IterationEvent(), iteration_command)

#
# One level registration process without shrinking and smoothing.
#
registration = itk.ImageRegistrationMethodv4.New(
    fixed_image=fixed_image,
    moving_image=moving_image,
    optimizer=optimizer,
    metric=matching_metric,
    initial_transform=initial_transform,
)
registration.SetNumberOfLevels(1)
registration.SetSmoothingSigmasPerLevel([0])
registration.SetShrinkFactorsPerLevel([1])

#
# Execute the registration
#
registration.Update()


#
# Get the final parameters of the transformation
#
final_parameters = registration.GetOutput().Get().GetParameters()

print("\nFinal Registration Parameters: ")
print(f"Translation X =  {final_parameters[0]:f}")
print(f"Translation Y =  {final_parameters[1]:f}")


#
# Now, we use the final transform for resampling the
# moving image.
#
resampled_moving = itk.resample_image_filter(
    moving_image,
    transform=registration.GetTransform(),
    use_reference_image=True,
    reference_image=fixed_image,
    default_pixel_value=100,
)

#
# Cast to 8-bit unsigned integer pixels, supported by the PNG file format
#
OutputPixelType = itk.ctype("unsigned char")
resampled_cast = resampled_moving.astype(OutputPixelType)

#
#  Write the resampled image
#
itk.imwrite(resampled_cast, output_image_file)
