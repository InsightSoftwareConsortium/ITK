import sys
from pathlib import Path

import itk
import numpy as np

if len(sys.argv) < 8:
    print(
        "Usage: "
        + sys.argv[0]
        + " OutputFilename SpeedFilename PathFilename TerminationValue NumberOfIterations StepLengthFactor StepLengthRelax"
    )
    sys.exit(1)
output_filename = sys.argv[1]
speed_filename = sys.argv[2]
path_filename = sys.argv[3]
termination_value = float(sys.argv[4])
number_of_iterations = int(sys.argv[5])
step_length_factor = float(sys.argv[6])
step_length_relax = float(sys.argv[7])

Dimension = 2

PointType = itk.Point[itk.D, Dimension]
PathInformationType = itk.SpeedFunctionPathInformation[PointType]

path_information = PathInformationType.New()
with open(path_filename) as fp:
    for line in fp:
        line = line.replace("Path: ", "")
        line = line.replace("[", "").strip()
        points = line.split("]")[:-1]
        for index, point in enumerate(points):
            point_float = PointType()
            point_float[0] = float(point.split(",")[0])
            point_float[1] = float(point.split(",")[1])
            print(point_float)
            if index == 0:
                path_information.SetStartPoint(point_float)
            elif index == len(points) - 1:
                path_information.SetEndPoint(point_float)
            else:
                path_information.AddWayPoint(point_float)

# path_information2 = PathInformationType.New()
# start = PointType()
# start[0] = 202.
# start[1] = 369.
# path_information2.SetStartPoint(start)
# way1 = PointType()
# way1[0] = 201
# way1[1] = 335
# path_information2.AddWayPoint(way1)
# end = PointType()
# end[0] = 165
# end[1] = 326
# path_information2.SetEndPoint(end)

speed_image = itk.imread(speed_filename, itk.F)

interpolator = itk.LinearInterpolateImageFunction.New(speed_image)
interpolator.SetInputImage(speed_image)

cost_function = itk.SingleImageCostFunction[type(speed_image)].New(
    interpolator=interpolator
)
cost_function.SetInterpolator(interpolator)

spacing = list(speed_image.GetSpacing())
min_spacing = min(spacing)

optimizer = itk.RegularStepGradientDescentOptimizer.New(
    number_of_iterations=number_of_iterations,
    maximum_step_length=1.0 * step_length_factor * min_spacing,
    minimum_step_length=0.5 * step_length_factor * min_spacing,
    relaxation_factor=step_length_relax,
)

path_filter = itk.SpeedFunctionToPathFilter.New(
    speed_image,
    cost_function=cost_function,
    optimizer=optimizer,
    termination_value=termination_value,
)
path_filter.SetInput(speed_image)
path_filter.AddPathInformation(path_information)
path_filter.Update()

number_of_paths = path_filter.GetNumberOfOutputs()
print("Number of paths: " + str(number_of_paths))
for ii in range(number_of_paths):
    path = path_filter.GetOutput(ii)
    number_of_vertices = path.GetVertexList().Size()
    print(f"Number of Path {ii} vertices: {number_of_vertices}")
assert number_of_vertices != 0

path_filter.GetNumberOfOutputs()
vl = path.GetVertexList()
path_filter.GetOptimizer()
arrival = path_filter.GetCurrentArrivalFunction()
print(np.where(itk.array_from_image(arrival) < 100.0))
