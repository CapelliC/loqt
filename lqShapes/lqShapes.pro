#-------------------------------------------------
#
# Project created by QtCreator 2016-03-11T08:59:04
#
#-------------------------------------------------

QT       += widgets

TARGET = lqShapes
TEMPLATE = lib

DEFINES += LQSHAPES_LIBRARY

SOURCES += lqShapes.cpp

HEADERS += lqShapes.h\
        lqshapes_global.h

unix {
    target.path = /usr/lib
    INSTALLS += target
}
