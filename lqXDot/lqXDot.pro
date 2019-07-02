#--------------------------------------------------
# lqXDot.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Author       : Carlo Capelli
# Copyright (C): 2013,2014,2015,2016,2017,2018,2019

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

include(../loqt.pri)

QT += svg

TARGET = lqXDot
TEMPLATE = lib

# enable dependency tracking
CONFIG += create_prl

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty

DEFINES += LQXDOT_LIBRARY

unix {
    CONFIG += link_pkgconfig
    PKGCONFIG += libcgraph libgvc

    DEFINES += WITH_CGRAPH
    # latest download (2013/11/29) refuses to compile without this useless define
    # DEFINES += HAVE_STRING_H
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
