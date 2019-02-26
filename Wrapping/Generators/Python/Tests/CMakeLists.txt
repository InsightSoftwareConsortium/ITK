# TODO: drop -E option once all filters/spatial objects/cost functions are actually wrapped
itk_python_add_test(NAME NonBlockingPythonFilterCoverage COMMAND wrappingCoverage.py -E -b Filter -e ${CMAKE_CURRENT_SOURCE_DIR}/exclude-filters.txt ${ITK_INCLUDE_DIRS})
itk_python_add_test(NAME NonBlockingPythonSpatialObjectCoverage COMMAND wrappingCoverage.py -E -b SpatialObject ${ITK_INCLUDE_DIRS})
if(ITK_BUILD_DEFAULT_MODULES)
  itk_python_add_test(NAME NonBlockingPythonCostFunctionCoverage COMMAND wrappingCoverage.py -E -b CostFunction ${ITK_INCLUDE_DIRS})
  itk_python_add_test(NAME PythonOptimizerCoverage COMMAND wrappingCoverage.py -b Optimizer ${ITK_INCLUDE_DIRS})
endif()

itk_python_add_test(NAME PythonImageIOCoverage COMMAND wrappingCoverage.py -b ImageIO -e ${CMAKE_CURRENT_SOURCE_DIR}/exclude-imageio.txt ${ITK_INCLUDE_DIRS})
itk_python_add_test(NAME PythonTransformCoverage COMMAND wrappingCoverage.py -b Transform -e ${CMAKE_CURRENT_SOURCE_DIR}/exclude-transform.txt ${ITK_INCLUDE_DIRS})

execute_process(COMMAND ${PYTHON_EXECUTABLE} -c "import numpy"
  RESULT_VARIABLE _have_numpy_return_code
  OUTPUT_QUIET
  ERROR_QUIET
  )

# These two tests can use up all of system memory when the are executed.
# TODO: find the root cause and re-enable them.  See ITK-3006 on issues.itk.org.
#itk_python_add_test(PythonFindEmptyClasses findEmptyClasses.py)
if(_have_numpy_return_code EQUAL 0 AND ITK_BUILD_DEFAULT_MODULES)
  # Currently unreliably fails. See Issue #96
  # itk_python_add_test(NAME PythonGetNameOfClass COMMAND getNameOfClass.py)

  itk_python_add_test(NAME PythonTiming COMMAND timing.py)
endif()
itk_python_add_test(NAME PythonVerifyAPIConsistency COMMAND verifyAPIConsistency.py)
itk_python_add_test(NAME PythonComplex COMMAND complex.py)
itk_python_add_test(NAME PythonHelperFunctions COMMAND helpers.py)

# some tests will fail if dim=2 and unsigned short are not wrapped
INTERSECTION(WRAP_2 2 "${ITK_WRAP_IMAGE_DIMS}")
if(ITK_WRAP_unsigned_char AND WRAP_2)
  if(ITK_BUILD_DEFAULT_MODULES)
    itk_python_add_test(NAME PythonTypemapsTest COMMAND PythonTypemapsTest.py)
    itk_python_add_test(NAME PythonTemplateTest COMMAND PythonTemplateTest.py)
    itk_python_add_test(NAME PythonModifiedTimeTest COMMAND ModifiedTime.py)
    itk_python_add_test(NAME PythonTemplatedPipelineTest
      TEST_DRIVER_ARGS
        --compare ${ITK_TEST_OUTPUT_DIR}/templated_pipeline.png DATA{${WrapITK_SOURCE_DIR}/images/templated_pipeline.png}
      COMMAND templated_pipeline.py
        DATA{${WrapITK_SOURCE_DIR}/images/2th_cthead1.png}
        ${ITK_TEST_OUTPUT_DIR}/templated_pipeline.png
        10
      )
    itk_python_add_test(NAME PythonModuleToModuleTest
      COMMAND module2module.py
        DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
        5
      )
    if(_have_numpy_return_code EQUAL 0)
      itk_python_add_test(NAME PythonExtrasTest
        COMMAND extras.py
          DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/out.png ${ITK_TEST_OUTPUT_DIR}
        )
    endif()
  endif()

#   itk_python_add_test(NAME PythonStrelFromImageGrayscaleDilateImageFilter
#     StrelFromImageGrayscaleDilateImageFilter.py
#     DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
#     StrelFromImageGrayscaleDilateImageFilter.png
#     DATA{${WrapITK_SOURCE_DIR}/images/StrelFromImage.png}
#     --compare StrelFromImageGrayscaleDilateImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/StrelFromImageGrayscaleDilateImageFilter.png}
#   )

#   itk_python_add_test(NAME PythonFlatStructuringElementBall
#     FlatStructuringElement.py
#     Ball-5.png
#     Ball
#     5
#     --compare Ball-5.png DATA{${WrapITK_SOURCE_DIR}/images/Ball-5.png}
#   )

#   itk_python_add_test(NAME PythonFlatStructuringElementBox
#     FlatStructuringElement.py
#     Box-5.png
#     Box
#     5
#     --compare Box-5.png DATA{${WrapITK_SOURCE_DIR}/images/Box-5.png}
#   )

#   itk_python_add_test(NAME PythonFlatStructuringElementFromImage
#     FlatStructuringElement.py
#     StrelFromImage.png
#     FromImage
#     DATA{${WrapITK_SOURCE_DIR}/images/StrelFromImage.png}
#     --compare StrelFromImage.png DATA{${WrapITK_SOURCE_DIR}/images/StrelFromImage.png}
#   )
endif()

if(ITK_WRAP_float AND WRAP_2)
#   itk_python_add_test(NAME  PythonGeodesicActiveContourWhiteMatterTest
#     --compare GeodesicActiveContourWhiteMatterTest.png
#             DATA{${WrapITK_SOURCE_DIR}/images/GeodesicActiveContourWhiteMatterTest.png}
#     --
#     GeodesicActiveContourImageFilter.py
#     DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
#     GeodesicActiveContourWhiteMatterTest.png
#     56 92 5.0 1.0 -0.3 2.0 10.0
#   )

#   itk_python_add_test(NAME  PythonGeodesicActiveContourGrayMatterTest
#     --compare GeodesicActiveContourGrayMatterTest.png
#             DATA{${WrapITK_SOURCE_DIR}/images/GeodesicActiveContourGrayMatterTest.png}
#     --
#     GeodesicActiveContourImageFilter.py
#     DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
#     GeodesicActiveContourGrayMatterTest.png
#     40 90 5.0 .5 -0.3 2.0 10.0
#   )
endif()


UNIQUE(types "${WRAP_ITK_SCALAR};UC")
# signed char can't be used to store an image with values up to 255
list(REMOVE_ITEM types SC)
foreach(d ${ITK_WRAP_IMAGE_DIMS})
  foreach(t ${types})
    itk_python_add_test(NAME PythonSimplePipeline${t}${d}
      TEST_DRIVER_ARGS
        --compare ${ITK_TEST_OUTPUT_DIR}/simple_pipeline${t}${d}.nrrd DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      COMMAND simple_pipeline.py
        ${t}
        ${d}
        DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
        ${ITK_TEST_OUTPUT_DIR}/simple_pipeline${t}${d}.nrrd
    )
  endforeach()
endforeach()
