SUBDIRS(vxl)

INCLUDE_DIRECTORIES(
../../..
../../../Code/Common
../../../Code/Numerics
../../../Code/Numerics/vxl
../../../Code/Numerics/vxl/vcl
)

LINK_DIRECTORIES(
../../../Code/Numerics/vxl
)

LINK_LIBRARIES(
VXLNumerics
)

TESTS(
itkNumericsHeaderTest
itkNumericsTest
)

