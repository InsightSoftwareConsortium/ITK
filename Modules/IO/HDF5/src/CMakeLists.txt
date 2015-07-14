set(ITKIOHDF5_SRC
itkHDF5ImageIOFactory.cxx
itkHDF5ImageIO.cxx
)

add_library(ITKIOHDF5 ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOHDF5_SRC})

itk_module_link_dependencies()
target_link_libraries(ITKIOHDF5 LINK_PUBLIC ${TEST_DEPENDS_LIBRARIES} ${ITKTestKernel_LIBRARIES})

itk_module_target(ITKIOHDF5)
