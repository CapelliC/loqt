#--------------------------------------------------
# lqXDot.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Author        : Carlo Capelli
# Copyright (C) : 2013,2014,2015,2016

#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

QT += core gui widgets svg

TARGET = lqXDot
TEMPLATE = lib
CONFIG += C++11

DEFINES += LQXDOT_LIBRARY

unix {
    CONFIG += link_pkgconfig

    DEFINES += WITH_CGRAPH
    # latest download (2013/11/29) refuses to compile without this useless define
    DEFINES += HAVE_STRING_H

    PKGCONFIG += libcgraph libgvc
}

DEFINES += QT_NO_OPENGL

SOURCES += \
    lqContextGraph.cpp \
    lqAobj.cpp \
    lqXDot.cpp \
    lqXDotView.cpp \
    lqXDotScene.cpp \
    lqGvSynCol.cpp \
    make_nop.cpp \
    SvgView.cpp

HEADERS += \
    lqXDot_global.h \
    lqContextGraph.h \
    lqAobj.h \
    lqXDot.h \
    lqXDotView.h \
    lqXDotScene.h \
    lqGvSynCol.h \
    lqXDot_configure.h \
    make_nop.h \
    SvgView.h

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
