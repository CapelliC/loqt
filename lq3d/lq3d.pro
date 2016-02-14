#--------------------------------------------------
# lq3D.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Author        : Carlo Capelli
# Copyright (C): 2013,2014,2015,2016

QT += core gui widgets 3dcore 3drender 3dinput

TARGET = lq3d
TEMPLATE = lib
CONFIG += C++11

DEFINES += LQ3D_LIBRARY

SOURCES += \
    lq3d.cpp \
    lq3dContext.cpp \
    lq3dObj.cpp \
    lq3dScene.cpp \
    lq3dView.cpp

HEADERS += \
    lq3d.h \
    lq3d_global.h \
    lq3d_configure.h \
    lq3dContext.h \
    lq3dObj.h \
    lq3dScene.h \
    lq3dView.h

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqUty/release/ -llqUty
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqUty/debug/ -llqUty
else:unix: LIBS += -L$$OUT_PWD/../lqUty/ -llqUty

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../pqConsole/release/ -lpqConsole
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../pqConsole/debug/ -lpqConsole
else:unix: LIBS += -L$$OUT_PWD/../pqConsole/ -lpqConsole

INCLUDEPATH += $$PWD/../pqConsole
DEPENDPATH += $$PWD/../pqConsole

OTHER_FILES +=

RESOURCES +=
