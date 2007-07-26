#
# Examples on the use of Numerics classes.
#
PROJECT(NumericsExamples)
INCLUDE_REGULAR_EXPRESSION("^.*$")

ADD_EXECUTABLE(FourierDescriptors1 FourierDescriptors1.cxx )
TARGET_LINK_LIBRARIES(FourierDescriptors1 ITKIO)
