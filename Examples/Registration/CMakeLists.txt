
PROJECT(ImageRegistration)

ADD_EXECUTABLE(ImageRegistration1 ImageRegistration1.cxx )
TARGET_LINK_LIBRARIES(ImageRegistration1 ITKCommon ITKNumerics VXLNumerics)

