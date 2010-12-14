cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(itk-template-module)
find_package(itk-base 4.0 EXACT REQUIRED)
itk_module_impl()
