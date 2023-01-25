#--------------------------------------------------
# pqGraphviz.pro: SWI-Prolog / Graphviz / Qt
#--------------------------------------------------
# interface SWI-Prolog and lqXDot
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

QT += concurrent

TARGET = pqGraphviz
TEMPLATE = lib

DEFINES += PQGRAPHVIZ_LIBRARY

# enable dependency tracking
CONFIG += create_prl

unix {
    # because SWI-Prolog is built from source
    CONFIG += link_pkgconfig
    PKGCONFIG += swipl libcgraph libgvc

    DEFINES += WITH_CGRAPH

    # latest download (2013/11/29) refuses to compile without this useless define
    # DEFINES += HAVE_STRING_H
}

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty/debug
LIBS += -L$$OUT_PWD/../lqUty/debug -llqUty

INCLUDEPATH += $$PWD/../lqXDot
DEPENDPATH += $$PWD/../lqXDot/debug
LIBS += -L$$OUT_PWD/../lqXDot/debug -llqXDot

INCLUDEPATH += $$PWD/../pqConsole
DEPENDPATH += $$PWD/../pqConsole/debug
LIBS += -L$$OUT_PWD/../pqConsole/debug -lpqConsole

SOURCES += \
    pqGraphviz.cpp \
    pqDocView.cpp

HEADERS += \
    pqGraphviz.h \
    pqGraphviz_global.h \
    pqDocView.h

OTHER_FILES += \
    prolog/gv_uty.pl \
    test/genealogy/genealogy.pl \
    test/genealogy/familiari.pl \
    test/odbc/odbc_schema.pl \
    test/xref/paths_prefix.pl \
    test/xref/file_xref.pl \
    test/odbc/graph_schema.pl \
    prolog/termtree.pl \
    test/genealogy/familiari.pdf \
    test/genealogy/pqGraphviz_emu.pl \
    test/genealogy/allocator.pl

RESOURCES += \
    pqGraphviz.qrc
