QT     += opengl xml widgets gui
CONFIG += c++11
CONFIG += debug

LIB_DIR      = ../lib
BUILD_DIR    = ../build

SOURCES     += $${LIB_DIR}/glew.c \
               gui/MainWindow.cpp \
               gui/GLWidget.cpp \
               prim/DiffusionCurve.cpp \
               libfmm/FastMultipole.cpp \
               main.cpp
HEADERS     += gui/MainWindow.hpp \
               gui/GLWidget.hpp
FORMS       += gui/MainWindow.ui
RESOURCES   += gui/MainWindow.qrc

INCLUDEPATH += $${LIB_DIR} $${BUILD_DIR}/DiffusionCurves
OBJECTS_DIR  = $${BUILD_DIR}/DiffusionCurves
RCC_DIR      = $${BUILD_DIR}/DiffusionCurves
MOC_DIR      = $${BUILD_DIR}/DiffusionCurves
UI_DIR       = $${BUILD_DIR}/DiffusionCurves
LIBS        += -L$${BUILD_DIR} -lQGLViewer -lGLU

DESTDIR = ../
TARGET  = diffusioncurves
