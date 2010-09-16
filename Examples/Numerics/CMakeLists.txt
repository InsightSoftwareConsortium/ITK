#
# Examples on the use of Numerics classes.
#
project(NumericsExamples)
include_regular_expression("^.*$")

add_executable(FourierDescriptors1 FourierDescriptors1.cxx )
target_link_libraries(FourierDescriptors1 ITKIO)
