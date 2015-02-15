#--------------------------------------------------
# lqXDot.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Copyright (C): 2013,2014,2015

QT += core gui widgets

TARGET = lqXDot
TEMPLATE = lib
QMAKE_CXXFLAGS += -std=c++0x

DEFINES += LQXDOT_LIBRARY

unix {
    CONFIG += link_pkgconfig

    DEFINES += WITH_CGRAPH
    # latest download (2013/11/29) refuses to compile without this useless define
    DEFINES += HAVE_STRING_H

    PKGCONFIG += libcgraph libgvc
}

SOURCES += \
    lqContextGraph.cpp \
    lqAobj.cpp \
    lqXDot.cpp \
    lqXDotView.cpp \
    lqXDotScene.cpp \
    lqGvSynCol.cpp \
    make_nop.cpp

HEADERS += \
    lqXDot_global.h \
    lqContextGraph.h \
    lqAobj.h \
    lqXDot.h \
    lqXDotView.h \
    lqXDotScene.h \
    lqGvSynCol.h \
    lqXDot_configure.h \
    make_nop.h

unix:!symbian {
    maemo5 {
        target.path = /opt/usr/lib
    } else {
        target.path = /usr/lib
    }
    INSTALLS += target
}

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqUty/release/ -llqUty
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqUty/debug/ -llqUty
else:symbian: LIBS += -llqUty
else:unix: LIBS += -L$$OUT_PWD/../lqUty/ -llqUty

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty

OTHER_FILES +=

RESOURCES +=
