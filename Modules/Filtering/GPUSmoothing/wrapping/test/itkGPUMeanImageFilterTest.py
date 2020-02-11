#==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/
import itk
import sys

if len(sys.argv) < 3:
    print('Usage: ' + sys.argv[0] + ' inputFile outputFile [numberOfDimensions]')
    sys.exit(1)
input_file = sys.argv[1]
output_file = sys.argv[2]
Dimension = 2
if len(sys.argv) > 3:
    Dimension = int(sys.argv[3])

# Testing GPU Mean Image Filter
InputPixelType = itk.F
OutputPixelType = itk.F

InputImageType = itk.Image[InputPixelType, Dimension]
OutputImageType = itk.Image[OutputPixelType, Dimension]
InputGPUImageType = itk.GPUImage[InputPixelType, Dimension]
OutputGPUImageType = itk.GPUImage[OutputPixelType, Dimension]

input_image = itk.imread(input_file, InputPixelType)
input_gpu_image = itk.cast_image_filter(input_image, in_place=False,
        ttype=(InputImageType, InputGPUImageType))
input_gpu_image.UpdateBuffers()

MeanFilterType = itk.MeanImageFilter[InputImageType, OutputImageType]
GPUMeanFilterType = itk.GPUMeanImageFilter[InputGPUImageType, OutputGPUImageType]

# test 1~8 work units for CPU
for number_of_work_units in range(1,9):
    cpu_filter = MeanFilterType.New()
    cpu_timer = itk.TimeProbe()

    cpu_timer.Start()

    cpu_filter.SetNumberOfWorkUnits(number_of_work_units)

    cpu_filter.SetInput(input_image)
    cpu_filter.Update()

    cpu_timer.Stop()

    print("CPU MeanFilter took {0} seconds with {1} work units.\n".format(cpu_timer.GetMean(),
                cpu_filter.GetNumberOfWorkUnits()))


gpu_filter = GPUMeanFilterType.New()

gpu_timer = itk.TimeProbe()
gpu_timer.Start()

gpu_filter.SetInput(input_gpu_image)
gpu_filter.Update()

gpu_filter.GetOutput().UpdateBuffers() # synchronization point (GPU->CPU memcpy)

gpu_timer.Stop()

print("GPU MeanFilter took {0} seconds.\n".format(gpu_timer.GetMean()))

output_image = itk.cast_image_filter(gpu_filter.GetOutput(),
        ttype=(OutputGPUImageType, OutputImageType))
output_gpu_image = gpu_filter.GetOutput()
itk.imwrite(output_image, output_file)
