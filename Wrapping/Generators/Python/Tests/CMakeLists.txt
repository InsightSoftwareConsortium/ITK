# TODO: drop -E option once all filters/spatial objects/cost functions are actually wrapped
itk_python_add_test(NAME NonBlockingPythonFilterCoverage COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/wrappingCoverage.py -E -b Filter -e ${CMAKE_CURRENT_SOURCE_DIR}/exclude-filters.txt ${ITK_INCLUDE_DIRS})
itk_python_add_test(NAME NonBlockingPythonSpatialObjectCoverage COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/wrappingCoverage.py -E -b SpatialObject ${ITK_INCLUDE_DIRS})
if(ITK_BUILD_DEFAULT_MODULES)
  itk_python_add_test(NAME NonBlockingPythonCostFunctionCoverage COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/wrappingCoverage.py -E -b CostFunction ${ITK_INCLUDE_DIRS})
  itk_python_add_test(NAME PythonOptimizerCoverage COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/wrappingCoverage.py -b Optimizer ${ITK_INCLUDE_DIRS})
endif()

itk_python_add_test(NAME PythonImageIOCoverage COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/wrappingCoverage.py -b ImageIO -e ${CMAKE_CURRENT_SOURCE_DIR}/exclude-imageio.txt ${ITK_INCLUDE_DIRS})
itk_python_add_test(NAME PythonTransformCoverage COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/wrappingCoverage.py -b Transform -e ${CMAKE_CURRENT_SOURCE_DIR}/exclude-transform.txt ${ITK_INCLUDE_DIRS})

itk_python_add_test(NAME PythonFindEmptyClasses COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/findEmptyClasses.py)
itk_python_add_test(NAME PythonGetNameOfClass COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/getNameOfClass.py)
itk_python_add_test(NAME PythonTiming COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/timing.py)
itk_python_add_test(NAME PythonBuildOptions COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/build_options.py)
itk_python_add_test(NAME PythonVerifyGetOutputAPIConsistency COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/verifyGetOutputAPIConsistency.py)
itk_python_add_test(NAME PythonVerifyTTypeAPIConsistency COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/verifyTTypeAPIConsistency.py)
itk_python_add_test(NAME PythonTypeTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/PythonTypeTest.py)
itk_python_add_test(NAME PythonComplex COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/complex.py)
itk_python_add_test(NAME PythonHelperFunctions COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/helpers.py)
itk_python_add_test(NAME PythonLazyModule COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/lazy.py)
itk_python_add_test(NAME PythonNoLazyModule COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/nolazy.py)
itk_python_add_test(NAME PythonNoDefaultFactories COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/nodefaultfactories.py)
itk_python_add_test(NAME PythonDICOMSeries COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/dicomSeries.py
              DATA{${ITK_DATA_ROOT}/Input/DicomSeries/Image0075.dcm}
              DATA{${ITK_DATA_ROOT}/Input/DicomSeries/Image0076.dcm}
              DATA{${ITK_DATA_ROOT}/Input/DicomSeries/Image0077.dcm})
itk_python_add_test(NAME PyImageFilterTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/PyImageFilterTest.py)
itk_python_add_test(NAME PythonConvertMetaData COMMAND
  ${CMAKE_CURRENT_SOURCE_DIR}/test_metadata.py
  DATA{${WrapITK_SOURCE_DIR}/images/r16slice.nii.gz}
  DATA{${WrapITK_SOURCE_DIR}/images/sourceImage.nii.gz}
  DATA{${WrapITK_SOURCE_DIR}/images/warp2D.nii.gz}
  DATA{${WrapITK_SOURCE_DIR}/images/warp3D.nii.gz}
  )

# some tests will fail if dim=2 and unsigned short are not wrapped
INTERSECTION(WRAP_2 2 "${ITK_WRAP_IMAGE_DIMS}")
if(ITK_WRAP_unsigned_char AND WRAP_2)
  if(ITK_BUILD_DEFAULT_MODULES)
    itk_python_add_test(NAME PythonTypemapsTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/PythonTypemapsTest.py)
    itk_python_add_test(NAME PythonTemplateTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/PythonTemplateTest.py)
    itk_python_add_test(NAME PythonModifiedTimeTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/ModifiedTime.py)
    itk_python_add_test(NAME PythonTemplatedPipelineTest
      TEST_DRIVER_ARGS
        --compare ${ITK_TEST_OUTPUT_DIR}/templated_pipeline.png DATA{${WrapITK_SOURCE_DIR}/images/templated_pipeline.png}
      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/templated_pipeline.py
        DATA{${WrapITK_SOURCE_DIR}/images/2th_cthead1.png}
        ${ITK_TEST_OUTPUT_DIR}/templated_pipeline.png
        10
      )
    itk_python_add_test(NAME PythonModuleToModuleTest
      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/module2module.py
        DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
        5
      )
    itk_python_add_test(NAME PythonExtrasTest
      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/extras.py
        DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
        DATA{../../../../Testing/Data/Input/mushroom.vtk}
        DATA{../../../../Testing/Data/Input/LinearTransform.h5}
        ${ITK_TEST_OUTPUT_DIR}/out.png
        ${ITK_TEST_OUTPUT_DIR}/extras-mushroom.vtk
        ${ITK_TEST_OUTPUT_DIR}
        ${ITK_TEST_OUTPUT_DIR}/LinearTransformWritten.h5
      )
    itk_python_add_test(NAME PythonXarrayTest
      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test_xarray.py
        DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      )
  endif()
endif()

UNIQUE(types "${WRAP_ITK_SCALAR};UC")
# signed char can't be used to store an image with values up to 255
list(REMOVE_ITEM types SC)
foreach(d ${ITK_WRAP_IMAGE_DIMS})
  foreach(t ${types})
    itk_python_add_test(NAME PythonSimplePipeline${t}${d}
      TEST_DRIVER_ARGS
        --compare ${ITK_TEST_OUTPUT_DIR}/simple_pipeline${t}${d}.nrrd DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/simple_pipeline.py
        ${t}
        ${d}
        DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
        ${ITK_TEST_OUTPUT_DIR}/simple_pipeline${t}${d}.nrrd
    )
  endforeach()
endforeach()


# Test the lazyloading in a multiprocessing environment
itk_python_add_test(NAME PythonMultiprocessLazyLoad
      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/multiprocess_lazy_loading.py
        DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png} DATA{${WrapITK_SOURCE_DIR}/images/templated_pipeline.png} DATA{${WrapITK_SOURCE_DIR}/images/2th_cthead1.png}
    )

# Test proper reading and writing of VariableLengthVector
itk_python_add_test(NAME PythonVLVReadWriteTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/TestVLV.seg.nrrd DATA{${WrapITK_SOURCE_DIR}/images/TestVLV.seg.nrrd}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/readWriteVLV.py
      DATA{${WrapITK_SOURCE_DIR}/images/TestVLV.seg.nrrd}
      ${ITK_TEST_OUTPUT_DIR}/TestVLV.seg.nrrd
      59 85 58 5
    )
