#include <iostream>
#include <fstream>
#include "CoherenceEnhancingDiffusionCommandLine.h"

int CoherenceEnhancingDiffusionTest(int argc, char **argv)
{
    try {
        CoherenceEnhancingDiffusionCommandLine::Execute(argc, argv);
    } catch (itk::ExceptionObject& e) {
        std::cerr << "ITK Exception : " << e.GetDescription() << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}

/*
int LinearAnisotropicDiffusionTest(int argc, char **argv)
{
    try {
        LinearAnisotropicDiffusionCommandLine::Execute(argc, argv);
    } catch (itk::ExceptionObject& e) {
        std::cerr << "ITK Exception : " << e.GetDescription() << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}*/