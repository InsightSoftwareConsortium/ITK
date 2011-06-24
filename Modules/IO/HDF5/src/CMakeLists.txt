set(ITK-IO-HDF5_SRC
itkHDF5ImageIOFactory.cxx
itkHDF5ImageIO.cxx
)

add_library(ITK-IO-HDF5 ${ITK-IO-HDF5_SRC})

target_link_libraries(ITK-IO-HDF5
${ITK-HDF5_LIBRARIES}
${ITK-IO-Base_LIBRARIES}
${TEST_DEPENDS_LIBRARIES}
${ITK-TestKernel_LIBRARIES}
itkhdf5
itkhdf5_cpp
)

itk_module_target(ITK-IO-HDF5)
