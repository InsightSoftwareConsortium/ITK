#
#  Examples on the use of Iterators.
#
PROJECT(IteratorExamples)
INCLUDE_REGULAR_EXPRESSION("^.*$")

ADD_EXECUTABLE(ImageRegionIterator ImageRegionIterator.cxx)
TARGET_LINK_LIBRARIES(ImageRegionIterator ITKCommon ITKIO ITKMetaIO itkzlib itkpng)

ADD_EXECUTABLE(ImageRegionIteratorWithIndex ImageRegionIteratorWithIndex.cxx)
TARGET_LINK_LIBRARIES(ImageRegionIteratorWithIndex ITKCommon ITKIO ITKMetaIO itkzlib itkpng)

ADD_EXECUTABLE(ImageLinearIteratorWithIndex ImageLinearIteratorWithIndex.cxx)
TARGET_LINK_LIBRARIES(ImageLinearIteratorWithIndex ITKCommon ITKIO ITKMetaIO itkzlib itkpng)

ADD_EXECUTABLE(ImageSliceIteratorWithIndex ImageSliceIteratorWithIndex.cxx)
TARGET_LINK_LIBRARIES(ImageSliceIteratorWithIndex ITKCommon ITKIO ITKMetaIO itkzlib itkpng)

ADD_EXECUTABLE(ImageRandomConstIteratorWithIndex ImageRandomConstIteratorWithIndex.cxx)
TARGET_LINK_LIBRARIES(ImageRandomConstIteratorWithIndex ITKCommon ITKIO ITKMetaIO itkzlib itkpng)

SET(ITERATORS_EXAMPLES ${CXX_TEST_PATH}/IteratorsExamples)

SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Iterators)
SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

ADD_TEST(ImageLinearIteratorWithIndexTest ${ITERATORS_EXAMPLES}
  --compare ${BASELINE}/ImageLinearIteratorWithIndexTest.png
            ${TEMP}/ImageLinearIteratorWithIndexTest.png
  ImageLinearIteratorWithIndexTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/ImageLinearIteratorWithIndexTest.png
)

ADD_TEST(ImageRegionIteratorWithIndexTest ${ITERATORS_EXAMPLES}
  --compare ${BASELINE}/ImageRegionIteratorWithIndexTest.png
            ${TEMP}/ImageRegionIteratorWithIndexTest.png
  ImageRegionIteratorWithIndexTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/ImageRegionIteratorWithIndexTest.png
)

ADD_TEST(ImageRegionIteratorTest ${ITERATORS_EXAMPLES}
  --compare ${BASELINE}/ImageRegionIteratorTest.png
            ${TEMP}/ImageRegionIteratorTest.png
  ImageRegionIteratorTest
        ${ITK_SOURCE_DIR}/Examples/Data/FatMRISlice.png
        ${TEMP}/ImageRegionIteratorTest.png
        20 70 210 140
)

ADD_TEST(ImageSliceIteratorWithIndexTest ${ITERATORS_EXAMPLES}
  --compare ${BASELINE}/ImageSliceIteratorWithIndexTest.mha
            ${TEMP}/ImageSliceIteratorWithIndexTest.mha
  ImageSliceIteratorWithIndexTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensity3Slices.mha
        ${TEMP}/ImageSliceIteratorWithIndexTest.mha
        2
)


ADD_EXECUTABLE(IteratorsExamples IteratorsExamples.cxx )
TARGET_LINK_LIBRARIES(IteratorsExamples ITKIO ITKBasicFilters ITKCommon)
